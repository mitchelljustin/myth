use thiserror::Error;

use crate::ast;
use crate::parse::{Rule, Span};

#[derive(Error, Debug)]
pub enum TransformError {}

type TransformResult<T> = Result<ast::AST<T>, TransformError>;
type Pair<'a> = pest::iterators::Pair<'a, Rule>;

pub fn create<T>(pair: &Pair, v: T) -> TransformResult<T> {
    Ok(ast::AST {
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
            let [name, params, return_type, body] = pair
                .clone()
                .into_inner()
                .next_chunk()
                .expect("function def");
            let name = transform_ident(name)?;
            let params = params
                .into_inner()
                .map(transform_param_decl)
                .collect::<Result<Vec<_>, _>>()?;
            let return_type = transform_path(return_type)?;
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
    let components = pair
        .clone()
        .into_inner()
        .map(transform_ident)
        .collect::<Result<Vec<_>, _>>()?;
    create(&pair, ast::Path(components))
}

pub fn transform_block(pair: Pair) -> TransformResult<ast::Block> {
    create(&pair, ast::Block(vec![]))
}

pub fn transform_ident(pair: Pair) -> TransformResult<ast::Ident> {
    create(&pair, ast::Ident(pair.as_str().to_string()))
}
