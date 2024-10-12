use std::assert_matches::debug_assert_matches;
use std::num::{ParseFloatError, ParseIntError};

use thiserror::Error;

use TransformError::InvalidFloatLiteral;

use crate::ast;
use crate::ast::Operator;
use crate::parse::transform::TransformError::{InvalidIntegerLiteral, InvalidLvalue};
use crate::parse::{Rule, Span};

#[derive(Error, Debug)]
pub enum TransformError {
    #[error("invalid integer literal '{0}': {1}")]
    InvalidIntegerLiteral(String, ParseIntError),
    #[error("invalid float literal '{0}': {1}")]
    InvalidFloatLiteral(String, ParseFloatError),
    #[error("invalid lvalue: '{0}'")]
    InvalidLvalue(String),
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
            let pairs = pair.clone().into_inner().collect::<Vec<_>>();
            let items = pairs
                .clone()
                .into_iter()
                .filter(|pair| pair.as_rule() != Rule::EOI)
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
            let return_type =
                return_type.map_or_else(|| create(&pair, ast::Type::None), transform_type)?;
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
            ty: transform_type(inner.next().expect("param ty"))?,
        },
    )
}

pub fn transform_block(pair: Pair) -> TransformResult<ast::Block> {
    let pair = pair.into_inner().next().expect("block");
    let mut has_last_semi = false;
    let mut statements = vec![];
    for pair in pair.clone().into_inner() {
        if pair.as_rule() == Rule::LastSemi {
            has_last_semi = true;
            continue;
        }
        let statement = transform_statement(pair)?;
        statements.push(statement);
    }
    create(
        &pair,
        ast::Block {
            statements,
            has_last_semi,
        },
    )
}

pub fn transform_statement(pair: Pair) -> TransformResult<ast::Statement> {
    let pair = pair.into_inner().next().expect("statement");
    let rule = pair.as_rule();
    match rule {
        Rule::VariableDecl => {
            let [name, ty, init_value] = pair.clone().into_inner().next_chunk().unwrap();
            let name = transform_ident(name)?;
            let ty = transform_type(ty)?;
            let init_value = transform_expression(init_value)?;
            create(
                &pair,
                ast::Statement::VariableDecl(create(
                    &pair,
                    ast::VariableDecl {
                        init_value,
                        name,
                        ty,
                    },
                )?),
            )
        }
        Rule::Assignment => {
            let [lvalue, value] = pair.clone().into_inner().next_chunk().unwrap();
            debug_assert_eq!(lvalue.as_rule(), Rule::LValue);
            let lvalue = lvalue.into_inner().next().unwrap();
            let target = create(&lvalue, match lvalue.as_rule() {
                Rule::VariableRef => {
                    ast::LValue::VariableRef(create(&lvalue, ast::VariableRef {
                        name: transform_ident(lvalue.clone())?,
                    })?)
                }
                Rule::DerefExpr => {
                    let lvalue_expr = transform_expression(lvalue.clone())?;
                    let ast::Expression::Unary(unary) = *lvalue_expr.v else {
                        return Err(InvalidLvalue(lvalue.as_str().to_string()));
                    };
                    if unary.v.operator != Operator::Deref {
                        return Err(InvalidLvalue(lvalue.as_str().to_string()));
                    };
                    ast::LValue::Deref(unary.v.target)
                }
                rule => unreachable!("{rule:?}"),
            })?;
            let value = transform_expression(value)?;
            create(&pair, ast::Statement::Assignment(create(&pair, ast::Assignment {
                target,
                value,
            })?))
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
        Rule::Expression => {
            let expression =
                transform_expression(pair.clone().into_inner().next().expect("expression"))?;
            create(&pair, ast::Statement::Expression(expression))
        }
        _ => unreachable!("transform_statement {rule:?}"),
    }
}

pub fn transform_operator(pair: &Pair) -> Operator {
    match pair.as_rule() {
        Rule::EqEq => Operator::EqEq,
        Rule::NotEq => Operator::NotEq,
        Rule::LessThan => Operator::Lt,
        Rule::GreaterThan => Operator::Gt,
        Rule::LessThanEq => Operator::Lte,
        Rule::GreaterThanEq => Operator::Gte,
        Rule::Asterisk => Operator::Mul,
        Rule::Slash => Operator::Div,
        Rule::Percent => Operator::Rem,
        Rule::Plus => Operator::Add,
        Rule::Minus => Operator::Sub,
        Rule::DotDotLt => Operator::RangeExclusive,
        rule => unreachable!("transform_operator {rule:?}"),
    }
}

pub fn transform_expression(pair: Pair) -> TransformResult<ast::Expression> {
    let rule = pair.as_rule();
    match rule {
        Rule::Expression | Rule::LeafExpr | Rule::GroupedExpr | Rule::LValue => {
            transform_expression(pair.into_inner().next().unwrap())
        }
        Rule::VariableRef => {
            let name = transform_ident(pair.clone().into_inner().next().unwrap())?;
            create(
                &pair,
                ast::Expression::VariableRef(create(&pair, ast::VariableRef { name })?),
            )
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
        Rule::OrExpr
        | Rule::AndExpr
        | Rule::ComparisonExpr
        | Rule::FactorExpr
        | Rule::TermExpr
        | Rule::RangeExpr => {
            let mut inner = pair.clone().into_inner();
            let mut lhs = transform_expression(inner.next().unwrap())?;
            for [op, rhs] in inner.array_chunks() {
                let operator = transform_operator(&op);
                let rhs = transform_expression(rhs)?;
                let bin_expr = ast::Ast::<ast::Binary> {
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
                    v: Box::new(ast::Binary { lhs, rhs, operator }),
                };
                lhs = create(&pair, ast::Expression::Binary(bin_expr))?;
            }
            Ok(lhs)
        }
        Rule::NewExpr => {
            let ty = pair.clone().into_inner().next().unwrap();
            let ty = transform_type(ty)?;
            create(&pair, ast::Expression::New(create(&pair, ast::New { ty })?))
        }
        Rule::DerefExpr => {
            let mut iter = pair.clone().into_inner().rev();
            let mut expr = transform_expression(iter.next().unwrap())?;
            for _ in iter {
                expr = create(
                    &pair,
                    ast::Expression::Unary(create(
                        &pair,
                        ast::Unary {
                            target: expr,
                            operator: Operator::Deref,
                        },
                    )?),
                )?;
            }
            Ok(expr)
        }
        Rule::CallExpr => {
            let mut inner = pair.clone().into_inner();
            let mut expr = transform_expression(inner.next().expect("callee"))?;
            for arg_list in inner {
                let args = arg_list
                    .into_inner()
                    .map(transform_expression)
                    .collect::<Result<_, _>>()?;
                expr = create(
                    &pair,
                    ast::Expression::Call(create(&pair, ast::Call { callee: expr, args })?),
                )?;
            }
            Ok(expr)
        }
        Rule::IfExpr => {
            let mut inner = pair.clone().into_inner();
            let [condition, then_body] = inner.next_chunk().unwrap();
            let else_body = inner.next();
            let condition = transform_expression(condition)?;
            let then_body = transform_block(then_body)?;
            let else_body = else_body.map(transform_block).transpose()?;
            create(
                &pair,
                ast::Expression::If(create(
                    &pair,
                    ast::If {
                        condition,
                        then_body,
                        else_body,
                    },
                )?),
            )
        }
        _ => unreachable!("transform_expression {rule:?}"),
    }
}

pub fn transform_type(pair: Pair) -> TransformResult<ast::Type> {
    debug_assert_matches!(pair.as_rule(), Rule::Type);
    let pair = pair.into_inner().next().unwrap();
    let type_ast = match pair.as_rule() {
        Rule::TypePrimitive => {
            let primitive = match pair.clone().into_inner().next().unwrap().as_rule() {
                Rule::TypeI32 => ast::TypePrimitive::I32,
                Rule::TypeI64 => ast::TypePrimitive::I64,
                Rule::TypeF64 => ast::TypePrimitive::F64,
                Rule::TypeRange => ast::TypePrimitive::Range,
                Rule::TypeString => ast::TypePrimitive::String,
                _ => unreachable!(),
            };
            ast::Type::Primitive(primitive)
        }
        Rule::TypePointer => {
            let inner = transform_type(pair.clone().into_inner().next().unwrap())?;
            ast::Type::Pointer(inner)
        }
        rule => unreachable!("transform_type {rule:?}"),
    };
    create(&pair, type_ast)
}

pub fn transform_ident(pair: Pair) -> TransformResult<ast::Ident> {
    create(&pair, ast::Ident(pair.as_str().to_string()))
}
