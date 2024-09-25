use pest::Parser;
use thiserror::Error;

use crate::ast;

#[cfg(test)]
mod tests;
mod transform;

#[derive(pest_derive::Parser)]
#[grammar = "src/parse/myth.pest"]
struct MythParser;

#[derive(Error, Debug)]
pub enum Error {
    #[error("Pest parsing failed: {0}")]
    PestParseFailed(pest::error::Error<Rule>),
    #[error("Transform into AST failed: {0}")]
    TransformFailed(transform::TransformError),
}

pub fn parse(source: impl AsRef<str>) -> Result<ast::Ast<ast::Library>, Error> {
    let mut root_pair =
        MythParser::parse(Rule::Library, source.as_ref()).map_err(Error::PestParseFailed)?;
    let ast = transform::transform_library(root_pair.next().expect("no library"))
        .map_err(Error::TransformFailed)?;
    Ok(ast)
}

#[derive(Debug, Clone)]
pub struct Span {
    pub source: String,
    pub rule: Rule,
    pub line_col: (usize, usize),
}
