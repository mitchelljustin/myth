use std::assert_matches::debug_assert_matches;
use std::num::{ParseFloatError, ParseIntError};

use thiserror::Error;

use TransformError::InvalidFloatLiteral;

use crate::ast;
use crate::parse::transform::TransformError::InvalidIntegerLiteral;
use crate::parse::{Rule, Span};

#[derive(Error, Debug)]
pub enum TransformError {
    #[error("invalid integer literal '{0}': {1}")]
    InvalidIntegerLiteral(String, ParseIntError),
    #[error("invalid float literal '{0}': {1}")]
    InvalidFloatLiteral(String, ParseFloatError),
}

type TransformResult<T> = Result<ast::Ast<T>, TransformError>;
type Pair<'a> = pest::iterators::Pair<'a, Rule>;

pub fn create<T>(pair: &Pair, v: T) -> TransformResult<T> {
    Ok(ast::Ast {
        span: Span {
            rule: pair.as_rule(),
            source: pair.as_str().to_string(),
            line_col: pair.line_col(),
        },
        v: Box::new(v),
    })
}

pub fn transform_library(pair: Pair) -> TransformResult<ast::Library> {
    let rule = pair.as_rule();
    match rule {
        Rule::Library => {
            let items = pair
                .clone()
                .into_inner()
                .map(transform_item)
                .collect::<Result<Vec<_>, _>>()?;
            create(&pair, ast::Library { items })
        }
        _ => unreachable!("{rule:?}"),
    }
}

pub fn transform_item(pair: Pair) -> TransformResult<ast::Item> {
    let pair = pair.into_inner().next().expect("item");
    let rule = pair.as_rule();
    match rule {
        Rule::FunctionDef => {
            let mut inner = pair.clone().into_inner();
            let [name, params, ret_type_or_body] = inner.next_chunk().expect("function def");
            let return_type;
            let body;
            if matches!(ret_type_or_body.as_rule(), Rule::Block) {
                return_type = None;
                body = ret_type_or_body;
            } else {
                return_type = Some(ret_type_or_body);
                body = inner.next().unwrap();
            }
            let name = transform_ident(name)?;
            let params = params
                .into_inner()
                .map(transform_param_decl)
                .collect::<Result<Vec<_>, _>>()?;
            let return_type = return_type
                .map(|return_type| transform_path(return_type))
                .transpose()?;
            let body = transform_block(body)?;
            create(
                &pair,
                ast::Item::FunctionDef(create(
                    &pair,
                    ast::FunctionDef {
                        name,
                        params,
                        return_type,
                        body,
                    },
                )?),
            )
        }
        _ => unreachable!("{rule:?}"),
    }
}

pub fn transform_param_decl(pair: Pair) -> TransformResult<ast::ParamDecl> {
    let mut inner = pair.clone().into_inner();
    create(
        &pair,
        ast::ParamDecl {
            name: transform_ident(inner.next().expect("param name"))?,
            ty: transform_path(inner.next().expect("param ty"))?,
        },
    )
}

pub fn transform_path(pair: Pair) -> TransformResult<ast::Path> {
    debug_assert_matches!(pair.as_rule(), Rule::Path);
    let components = pair
        .clone()
        .into_inner()
        .map(transform_ident)
        .collect::<Result<Vec<_>, _>>()?;
    create(&pair, ast::Path(components))
}

pub fn transform_block(pair: Pair) -> TransformResult<ast::Block> {
    let pair = pair.into_inner().next().expect("block");
    create(
        &pair,
        ast::Block(
            pair.clone()
                .into_inner()
                .map(transform_statement)
                .collect::<Result<Vec<_>, _>>()?,
        ),
    )
}

pub fn transform_statement(pair: Pair) -> TransformResult<ast::Statement> {
    let pair = pair.into_inner().next().expect("statement");
    let rule = pair.as_rule();
    match rule {
        Rule::Assignment => {
            let [target, ty, value] = pair.clone().into_inner().next_chunk().unwrap();
            let target = transform_expression(target)?;
            let ty = transform_path(ty)?;
            let value = transform_expression(value)?;
            create(
                &pair,
                ast::Statement::Assignment(create(&pair, ast::Assignment { value, target, ty })?),
            )
        }
        Rule::IfStmt => {
            unimplemented!()
        }
        Rule::BreakStmt => {
            unimplemented!()
        }
        Rule::ContinueStmt => {
            unimplemented!()
        }
        Rule::ReturnStmt => {
            unimplemented!()
        }
        Rule::Expression => create(
            &pair,
            ast::Statement::Expression(transform_expression(
                pair.clone().into_inner().next().expect("swag"),
            )?),
        ),
        _ => unreachable!("transform_statement {rule:?}"),
    }
}

pub fn transform_operator(pair: &Pair) -> ast::Operator {
    match pair.as_rule() {
        Rule::EqEq => ast::Operator::EqEq,
        Rule::NotEq => ast::Operator::NotEq,
        Rule::LessThan => ast::Operator::Lt,
        Rule::GreaterThan => ast::Operator::Gt,
        Rule::LessThanEq => ast::Operator::Lte,
        Rule::GreaterThanEq => ast::Operator::Gte,
        Rule::Asterisk => ast::Operator::Mul,
        Rule::Slash => ast::Operator::Div,
        Rule::Percent => ast::Operator::Rem,
        Rule::Plus => ast::Operator::Add,
        Rule::Minus => ast::Operator::Sub,
        rule => unreachable!("transform_operator {rule:?}"),
    }
}

pub fn transform_expression(pair: Pair) -> TransformResult<ast::Expression> {
    let rule = pair.as_rule();
    match rule {
        Rule::Expression | Rule::LeafExpr | Rule::GroupedExpr | Rule::LValue => {
            transform_expression(pair.into_inner().next().unwrap())
        }
        Rule::PathExpr => {
            let path = transform_path(pair.clone().into_inner().next().unwrap())?;
            create(&pair, ast::Expression::Path(path))
        }
        Rule::LiteralExpr => {
            let literal = pair.clone().into_inner().next().unwrap();
            let inner =
                match literal.as_rule() {
                    Rule::StringLiteral => ast::Literal::String(literal.as_str().to_string()),
                    Rule::IntegerLiteral => {
                        let value = literal.as_str().parse().map_err(|err| {
                            InvalidIntegerLiteral(literal.as_str().to_string(), err)
                        })?;
                        ast::Literal::Integer(value)
                    }
                    Rule::FloatLiteral => {
                        let value = literal.as_str().parse().map_err(|err| {
                            InvalidFloatLiteral(literal.as_str().to_string(), err)
                        })?;
                        ast::Literal::Float(value)
                    }
                    Rule::BooleanLiteral => {
                        let value = match literal.into_inner().next().unwrap().as_rule() {
                            Rule::True => true,
                            Rule::False => true,
                            _ => unreachable!(),
                        };
                        ast::Literal::Bool(value)
                    }
                    rule => unreachable!("literal {rule:?}"),
                };
            create(&pair, ast::Expression::Literal(create(&pair, inner)?))
        }
        Rule::OrExpr | Rule::AndExpr | Rule::ComparisonExpr | Rule::FactorExpr | Rule::TermExpr => {
            let mut inner = pair.clone().into_inner();
            let mut lhs = transform_expression(inner.next().unwrap())?;
            for [op, rhs] in inner.array_chunks() {
                let operator = transform_operator(&op);
                let rhs = transform_expression(rhs)?;
                let bin_expr = ast::Ast::<ast::BinaryExpr> {
                    span: Span {
                        rule,
                        line_col: lhs.span.line_col,
                        source: format!(
                            "{} {} {}",
                            &lhs.span.source,
                            op.as_str(),
                            &rhs.span.source
                        ),
                    },
                    v: Box::new(ast::BinaryExpr { lhs, rhs, operator }),
                };
                lhs = create(&pair, ast::Expression::BinaryExpr(bin_expr))?;
            }
            Ok(lhs)
        }
        _ => unreachable!("transform_expression {rule:?}"),
    }
}

pub fn transform_ident(pair: Pair) -> TransformResult<ast::Ident> {
    create(&pair, ast::Ident(pair.as_str().to_string()))
}
