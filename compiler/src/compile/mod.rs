use thiserror::Error;
use wasm_encoder::ValType;

use crate::ast;
use crate::ast::{Ast, Binary};
use compiler::Compiler;
use std::collections::HashMap;
use std::fmt;
use ty::Ty;

mod analyzer;
mod compiler;
mod ty;
pub mod util;

#[derive(Error, Debug)]
pub enum Error {
    #[error("no operator '{operator}' for {val_type:?}")]
    NoOperatorForType {
        operator: ast::Operator,
        val_type: ValType,
    },
    #[error("illegal lvalue: '{expression:?}'")]
    InvalidLValue { expression: Ast<ast::Expression> },
    #[error("no such function: '{func_name}'")]
    NoSuchFunction { func_name: String },
    #[error("`if` and `else` have incompatible types: expected {expected}, found {actual}")]
    IfElseIncompatibleTypes { expected: String, actual: String },
    #[error("illegal binary expression: '{expression:?}'")]
    IllegalBinaryExpression { expression: Ast<Binary> },
    #[error("no such variable: '{variable:?}''")]
    NoSuchVariable { variable: String },
    #[error("illegal deref: {ty}")]
    IllegalDeref { ty: Ty },
}

type Result<T = ()> = std::result::Result<T, Error>;

pub fn compile(lib: Ast<ast::Library>) -> Result<Vec<u8>> {
    let mut compiler = Compiler::new();
    compiler.compile_library(lib)?;
    Ok(compiler.finish().finish())
}

#[derive(Default)]
pub struct CallFrame {
    func_name: String,
    params: HashMap<String, u32>,
    locals: HashMap<String, u32>,
}

struct FunctionScope {
    params: Vec<(String, Ty)>,
    return_type: Ty,
    locals: HashMap<String, Ty>,
}

struct Swag<'a> {
    swag: &'a dyn fmt::Write,
}
