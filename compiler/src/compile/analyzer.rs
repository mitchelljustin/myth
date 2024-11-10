use crate::ast::{Operator, Type, TypePrimitive};
use crate::compile::ty::Ty;
use crate::compile::Error::NoSuchFunction;
use crate::compile::{util, CallFrame, FunctionScope};
use crate::{
    ast, ast::Ast, ast::Block, ast::Expression, ast::FunctionDef, ast::Literal, ast::Statement,
    compile,
};
use std::collections::HashMap;

#[derive(Default)]
pub struct Analyzer {
    globals: HashMap<String, Ty>,
    functions: HashMap<String, FunctionScope>,
}

pub type TyResult = Result<Ty, compile::Error>;

impl Analyzer {
    pub(crate) fn init(&mut self) {}

    pub fn analyze_library(&mut self, lib: &Ast<ast::Library>) {
        for item in &lib.v.items {
            match item.v.as_ref() {
                ast::Item::FunctionDef(func_def) => {
                    self.analyze_func_def(func_def);
                }
                ast::Item::Use(_use_item) => {}
            }
        }
    }

    fn analyze_func_def(&mut self, func_def: &Ast<FunctionDef>) {
        let return_type = self.ast_type_to_ty(&func_def.v.return_type);
        let params: Vec<(String, Ty)> = func_def
            .v
            .params
            .iter()
            .map(|param_decl| {
                (
                    param_decl.v.name.v.0.clone(),
                    self.ast_type_to_ty(&param_decl.v.ty),
                )
            })
            .collect();
        let var_decls = util::extract_variable_decls(&func_def.v.body);
        let mut locals: HashMap<String, Ty> = var_decls
            .into_iter()
            .map(|var_decl| {
                (
                    var_decl.v.name.v.0.clone(),
                    self.ast_type_to_ty(&var_decl.v.ty),
                )
            })
            .collect();
        for (name, ty) in &params {
            locals.insert(name.clone(), ty.clone());
        }
        self.functions.insert(
            func_def.v.name.v.0.clone(),
            FunctionScope {
                params,
                return_type,
                locals,
            },
        );
    }

    pub fn ast_type_to_ty(&self, ast: &Ast<Type>) -> Ty {
        match &*ast.v {
            Type::None => Ty::Unit,
            Type::Primitive(primitive) => match primitive {
                TypePrimitive::I32 => Ty::I32,
                TypePrimitive::I64 => Ty::I64,
                TypePrimitive::F64 => Ty::F64,
                TypePrimitive::Range => Ty::Range,
                TypePrimitive::String => Ty::String,
            },
            Type::Pointer(inner) => Ty::Pointer(Box::new(self.ast_type_to_ty(inner))),
        }
    }

    pub fn resolve_block_result_type(&self, frame: &CallFrame, block: &Ast<Block>) -> TyResult {
        if block.v.has_last_semi {
            return Ok(Ty::Unit);
        }
        let Some(last_stmt) = block.v.statements.last() else {
            return Ok(Ty::Unit);
        };
        let Statement::Expression(expr) = last_stmt.v.as_ref() else {
            return Ok(Ty::Unit);
        };
        self.resolve_expr_type(frame, expr)
    }

    pub fn resolve_expr_type(&self, frame: &CallFrame, expr: &Ast<Expression>) -> TyResult {
        match expr.v.as_ref() {
            Expression::Unary(unary) => match unary.v.operator {
                Operator::Deref => {
                    let ty = self.resolve_expr_type(frame, &unary.v.target)?;
                    let Ty::Pointer(inner) = ty else {
                        return Err(compile::Error::IllegalDeref { ty });
                    };
                    Ok(*inner)
                }
                _ => unreachable!(),
            },
            Expression::Binary(bin_expr) => match bin_expr.v.operator {
                Operator::RangeExclusive => Ok(Ty::Range),
                _ => self.resolve_expr_type(frame, &bin_expr.v.lhs),
            },
            Expression::Call(call) => {
                let Expression::VariableRef(var_ref) = call.v.callee.v.as_ref() else {
                    unimplemented!();
                };
                let func_name = var_ref.v.name.v.0.as_str();
                let Some(function) = self.functions.get(func_name) else {
                    return Err(NoSuchFunction {
                        func_name: func_name.to_string(),
                    });
                };
                Ok(function.return_type.clone())
            }
            Expression::Literal(lit) => Ok(match lit.v.as_ref() {
                Literal::String(_) => Ty::String,
                Literal::Integer(_) => Ty::I32,
                Literal::Float(_) => Ty::F64,
                Literal::Bool(_) => Ty::Bool,
            }),
            Expression::VariableRef(var_ref) => {
                let name = &var_ref.v.name.v.0;
                let func = &self.functions[&frame.func_name];
                if let Some(ty) = func.locals.get(name) {
                    return Ok(ty.clone());
                }
                if let Some(ty) = self.globals.get(name) {
                    return Ok(ty.clone());
                }
                Err(compile::Error::NoSuchVariable {
                    variable: name.clone(),
                })
            }
            Expression::If(if_expr) => self.resolve_block_result_type(frame, &if_expr.v.then_body),
            Expression::New(new) => Ok(self.ast_type_to_ty(&new.v.ty)),
        }
    }
}
