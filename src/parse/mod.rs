use crate::ast;
use pest::Parser;
use thiserror::Error;

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

pub fn parse(source: impl AsRef<str>) -> Result<ast::AST, Error> {
    let mut root_pair =
        MythParser::parse(Rule::Library, source.as_ref()).map_err(Error::PestParseFailed)?;
    let ast = transform::transform_library(root_pair.next().expect("no library"))
        .map_err(Error::TransformFailed)?;
    Ok(ast)
}

#[cfg(test)]
mod tests;
mod transform;
