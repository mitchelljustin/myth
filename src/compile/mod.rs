use thiserror::Error;
use wasm_encoder::{Module, ValType};

use crate::ast;
use crate::ast::{Ast, BinaryExpr};
use compiler::Compiler;
use std::collections::HashMap;

mod analyzer;
mod compiler;
pub mod util;

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
    #[error("illegal binary expression: '{expression:?}'")]
    IllegalBinaryExpression { expression: Ast<BinaryExpr> },
    #[error("no such variable: '{variable:?}''")]
    NoSuchVariable { variable: String },
}

type Result<T = ()> = std::result::Result<T, Error>;

pub fn compile(lib: Ast<ast::Library>) -> Result<Module> {
    let mut compiler = Compiler::new();
    compiler.compile_library(lib)?;
    Ok(compiler.finish())
}

#[derive(Default)]
pub struct CallFrame {
    func_name: String,
    params: HashMap<String, u32>,
    locals: HashMap<String, u32>,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Ty {
    Unit,
    I32,
    I64,
    F64,
    Bool,
}

impl Ty {
    pub fn val_type(&self) -> Option<ValType> {
        match self {
            Ty::Unit => None,
            Ty::I32 => Some(ValType::I32),
            Ty::I64 => Some(ValType::I64),
            Ty::F64 => Some(ValType::F64),
            Ty::Bool => Some(ValType::I32),
        }
    }
}

struct FunctionScope {
    params: Vec<(String, Ty)>,
    return_type: Ty,
    locals: HashMap<String, Ty>,
}
