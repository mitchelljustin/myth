use pest::iterators::Pair;
use pest::Parser;
use thiserror::Error;

use crate::ast;

#[derive(pest_derive::Parser)]
#[grammar = "src/parse/myth.pest"]
struct MythParser;

#[derive(Error, Debug)]
pub enum Error {
    #[error("Pest parsing failed: {0}")]
    PestParseFailed(pest::error::Error<Rule>),
    #[error("Transform into AST failed: {0}")]
    TransformFailed(TransformError),
}

#[derive(Error, Debug)]
pub enum TransformError {}

type TransformResult = Result<ast::AST, TransformError>;

pub fn parse(source: impl AsRef<str>) -> Result<ast::AST, Error> {
    let mut root_pair =
        MythParser::parse(Rule::Library, source.as_ref()).map_err(Error::PestParseFailed)?;
    let ast =
        transform_library(root_pair.next().expect("no library")).map_err(Error::TransformFailed)?;
    Ok(ast)
}

pub fn transform_library(pair: Pair<Rule>) -> TransformResult {
    let rule = pair.as_rule();
    match rule {
        Rule::Library => Ok(ast::AST {}),
        _ => unreachable!("{rule:?}"),
    }
}

#[cfg(test)]
mod tests;
