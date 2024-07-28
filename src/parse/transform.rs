use crate::ast;
use crate::parse::Rule;
use pest::iterators::Pair;
use thiserror::Error;

#[derive(Error, Debug)]
pub enum TransformError {}

type TransformResult = Result<ast::AST, TransformError>;

pub fn transform_library(pair: Pair<Rule>) -> TransformResult {
    let rule = pair.as_rule();
    match rule {
        Rule::Library => Ok(ast::AST {}),
        _ => unreachable!("{rule:?}"),
    }
}
