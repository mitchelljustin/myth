use thiserror::Error;
use wasm_encoder::{Module, ValType};

use compiler::Compiler;

use crate::ast;
use crate::ast::Ast;

mod analyzer;
mod compiler;

#[derive(Error, Debug)]
pub enum Error {
    #[error("no operator '{operator}' for {val_type:?}")]
    NoOperatorForValType {
        operator: ast::Operator,
        val_type: ValType,
    },
    #[error("illegal lvalue: '{expression:?}'")]
    InvalidLValue { expression: Ast<ast::Expression> },
    #[error("no such function: '{func_name}'")]
    NoSuchFunction { func_name: String },
    #[error("`if` and `else` have incompatible types: expected {expected}, found {actual}")]
    IfElseIncompatibleTypes { expected: String, actual: String },
}

type Result<T = ()> = std::result::Result<T, Error>;

pub fn compile(lib: Ast<ast::Library>) -> Result<Module> {
    let mut compiler = Compiler::new();
    compiler.compile_library(lib)?;
    Ok(compiler.finish())
}
