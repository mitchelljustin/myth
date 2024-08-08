use crate::ast;
use crate::ast::{Assignment, Ast, Block, Expression, Literal, Statement};
use ast::Type;
use wasm_encoder::ValType;

pub fn extract_assignments(block: &Ast<Block>) -> Vec<&Ast<Assignment>> {
    block
        .v
        .statements
        .iter()
        .filter_map(|stmt| {
            if let Statement::Assignment(assn) = stmt.v.as_ref() {
                Some(assn)
            } else {
                None
            }
        })
        .collect()
}

pub fn ty_to_valtype(ty: &Ast<Type>) -> ValType {
    match ty.v.0.as_slice() {
        [builtin_ty] => match builtin_ty.v.0.as_str() {
            "i32" => ValType::I32,
            "i64" => ValType::I64,
            "f64" => ValType::F64,
            newtype => unimplemented!("newtype: {newtype:?}"),
        },
        path_type => unimplemented!("path type: {path_type:?}"),
    }
}

pub fn infer_block_result_type(block: &Ast<Block>) -> Option<ValType> {
    if block.v.has_last_semi {
        return None;
    }
    let last_stmt = block.v.statements.last()?;
    let Statement::Expression(expr) = last_stmt.v.as_ref() else {
        return None;
    };
    infer_expr_type(expr)
}

pub fn infer_expr_type(expr: &Ast<Expression>) -> Option<ValType> {
    match expr.v.as_ref() {
        Expression::BinaryExpr(bin_expr) => infer_expr_type(&bin_expr.v.lhs),
        Expression::Call(_) => todo!(),
        Expression::Literal(lit) => Some(match lit.v.as_ref() {
            Literal::String(_) => todo!(),
            Literal::Integer(_) => ValType::I32,
            Literal::Float(_) => ValType::F64,
            Literal::Bool(_) => ValType::I32,
        }),
        Expression::Path(_) => todo!(),
        Expression::IfExpr(if_expr) => infer_block_result_type(&if_expr.v.then_body),
    }
}
