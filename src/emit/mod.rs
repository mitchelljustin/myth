use thiserror::Error;

use crate::ast;

#[derive(Error, Debug)]
pub enum Error {}

pub fn emit_module(lib: ast::AST<ast::Library>) -> Result<wasm_encoder::Module, Error> {
    Ok(wasm_encoder::Module::new()) // TODO
}
