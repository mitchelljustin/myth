use std::collections::HashMap;

use wasm_encoder::{
    CodeSection, DataSection, ExportKind, ExportSection, Function, FunctionSection, ImportSection,
    Instruction, Module, TypeSection, ValType,
};

use crate::ast::{Ast, Expression, Literal, Operator, Statement};
use crate::compile::Error;
use crate::compile::Error::{InvalidLValue, NoOperatorForValType, NoSuchFunction};
use crate::{ast, compile};

#[derive(Default)]
struct CallFrame {
    params: HashMap<String, u32>,
    locals: HashMap<String, u32>,
}

#[derive(Default)]
pub struct Compiler {
    func_def_count: u32,
    functions: HashMap<String, u32>,
    call_frame: CallFrame,
    current_function: Option<Function>,
    import_section: ImportSection,
    data_section: DataSection,
    type_section: TypeSection,
    function_section: FunctionSection,
    export_section: ExportSection,
    code_section: CodeSection,
}

impl Compiler {
    pub fn new() -> Self {
        Self {
            ..Default::default()
        }
    }

    pub fn compile_library(&mut self, lib: Ast<ast::Library>) -> compile::Result {
        for item in lib.v.items {
            match *item.v {
                ast::Item::FunctionDef(func_def) => {
                    self.compile_func_def(func_def)?;
                }
            }
        }
        Ok(())
    }

    fn compile_func_def(&mut self, func_def: Ast<ast::FunctionDef>) -> compile::Result {
        let param_types = func_def
            .v
            .params
            .iter()
            .map(|param| Self::ty_to_valtype(&param.v.ty))
            .collect::<Vec<_>>();
        let params: HashMap<String, u32> = func_def
            .v
            .params
            .iter()
            .enumerate()
            .map(|(i, param)| (param.v.name.v.0.clone(), i as u32))
            .collect();
        if let Some(ret_type) = &func_def.v.return_type {
            self.type_section
                .function(param_types, [Self::ty_to_valtype(ret_type)]);
        } else {
            self.type_section.function(param_types, []);
        }
        self.function_section.function(self.func_def_count);
        self.call_frame = Default::default();
        let mut local_type_counts = HashMap::<ValType, u32>::new();
        let mut locals = HashMap::<String, u32>::new();
        let assignments = func_def
            .v
            .body
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
            .collect::<Vec<_>>();
        for (i, assn) in assignments.into_iter().enumerate() {
            let val_type = Self::ty_to_valtype(&assn.v.ty);
            *local_type_counts.entry(val_type).or_default() += 1;
            let target = &assn.v.target;
            let Expression::Path(path) = target.v.as_ref() else {
                return Err(Error::InvalidLValue {
                    expression: target.clone(),
                });
            };
            let [name] = path.v.0.as_slice() else {
                return Err(Error::InvalidLValue {
                    expression: target.clone(),
                });
            };
            locals.insert(name.v.0.clone(), (i + params.len()) as u32);
        }
        for (name, index) in params.iter() {
            locals.insert(name.clone(), *index);
        }
        self.call_frame = CallFrame { params, locals };
        self.current_function = Some(Function::new(
            local_type_counts.into_iter().map(|(k, v)| (v, k)),
        ));
        self.export_section.export(
            func_def.v.name.v.0.as_str(),
            ExportKind::Func,
            self.func_def_count,
        );
        self.compile_block(func_def.v.body)?;
        self.emit(&Instruction::End);
        self.code_section
            .function(self.current_function.as_ref().unwrap());
        self.func_def_count += 1;
        Ok(())
    }

    fn ty_to_valtype(ty: &Ast<ast::Type>) -> ValType {
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

    fn emit(&mut self, instruction: &Instruction) {
        self.current_function
            .as_mut()
            .unwrap()
            .instruction(instruction);
    }

    pub fn finish(&self) -> Module {
        let mut module = Module::new();
        module.section(&self.type_section);
        module.section(&self.import_section);
        module.section(&self.function_section);
        module.section(&self.export_section);
        module.section(&self.code_section);
        module.section(&self.data_section);
        module
    }

    fn compile_block(&mut self, block: Ast<ast::Block>) -> compile::Result {
        let statement_count = block.v.statements.len();
        for (i, stmt) in block.v.statements.into_iter().enumerate() {
            let keep_expr_result = if i == statement_count - 1 {
                !block.v.has_last_semi
            } else {
                false
            };
            self.compile_statement(stmt, keep_expr_result)?;
        }
        Ok(())
    }

    fn compile_statement(
        &mut self,
        statement: Ast<Statement>,
        keep_expr_result: bool,
    ) -> compile::Result {
        match *statement.v {
            Statement::Assignment(assn) => {
                let target = assn.v.target;
                let Expression::Path(path) = target.v.as_ref() else {
                    return Err(InvalidLValue { expression: target });
                };
                let [name] = path.v.0.as_slice() else {
                    return Err(InvalidLValue { expression: target });
                };
                let local_index = *self
                    .call_frame
                    .locals
                    .get(name.v.0.as_str())
                    .expect("no such local");

                self.compile_expression(assn.v.value)?;
                self.emit(&Instruction::LocalSet(local_index));
            }
            Statement::Expression(expr) => {
                self.compile_expression(expr)?;
                if !keep_expr_result {
                    self.emit(&Instruction::Drop);
                }
            }
            Statement::BreakStmt(_) => todo!(),
            Statement::ContinueStmt(_) => todo!(),
            Statement::ReturnStmt(_) => todo!(),
            Statement::IfStmt(_) => todo!(),
        }
        Ok(())
    }

    fn compile_expression(&mut self, expression: Ast<Expression>) -> compile::Result {
        match *expression.v {
            Expression::BinaryExpr(bin_expr) => {
                self.compile_expression(bin_expr.v.lhs)?;
                self.compile_expression(bin_expr.v.rhs)?;
                self.compile_operator(ValType::I32, bin_expr.v.operator)?;
            }
            Expression::Call(call) => {
                for arg in call.v.args {
                    self.compile_expression(arg)?;
                }
                match *call.v.callee.v {
                    Expression::Path(path) => {
                        let [name] = path.v.0.as_slice() else {
                            unimplemented!("path call");
                        };
                        let func_name = name.v.0.as_str();
                        let Some(func_index) = self.functions.get(func_name) else {
                            return Err(NoSuchFunction {
                                func_name: func_name.to_string(),
                            });
                        };
                        self.emit(&Instruction::Call(*func_index));
                    }
                    expr => unimplemented!("call: {expr:?}"),
                }
            }
            Expression::Literal(lit) => match *lit.v {
                Literal::Integer(int) => {
                    self.emit(&Instruction::I32Const(int as i32));
                }
                Literal::String(_) => todo!(),
                Literal::Float(_) => todo!(),
                Literal::Bool(_) => todo!(),
            },
            Expression::Path(path) => {
                let [name] = path.v.0.as_slice() else {
                    unimplemented!();
                };
                let local_index = self
                    .call_frame
                    .locals
                    .get(name.v.0.as_str())
                    .expect("cannot find local");
                self.emit(&Instruction::LocalGet(*local_index));
            }
        }
        Ok(())
    }

    fn compile_operator(&mut self, val_type: ValType, operator: Operator) -> compile::Result {
        let instruction = match (val_type, operator) {
            (ValType::I32, Operator::EqEq) => Instruction::I32Eq,
            (ValType::I32, Operator::NotEq) => Instruction::I32Ne,
            (ValType::I32, Operator::Lt) => Instruction::I32LtS,
            (ValType::I32, Operator::Gt) => Instruction::I32GtS,
            (ValType::I32, Operator::Lte) => Instruction::I32LeS,
            (ValType::I32, Operator::Gte) => Instruction::I32GeS,
            (ValType::I32, Operator::Mul) => Instruction::I32Mul,
            (ValType::I32, Operator::Div) => Instruction::I32DivS,
            (ValType::I32, Operator::Rem) => Instruction::I32RemS,
            (ValType::I32, Operator::Add) => Instruction::I32Add,
            (ValType::I32, Operator::Sub) => Instruction::I32Sub,
            (ValType::I64, Operator::EqEq) => Instruction::I64Eq,
            (ValType::I64, Operator::NotEq) => Instruction::I64Ne,
            (ValType::I64, Operator::Lt) => Instruction::I64LtS,
            (ValType::I64, Operator::Gt) => Instruction::I64GtS,
            (ValType::I64, Operator::Lte) => Instruction::I64LeS,
            (ValType::I64, Operator::Gte) => Instruction::I64GeS,
            (ValType::I64, Operator::Mul) => Instruction::I64Mul,
            (ValType::I64, Operator::Div) => Instruction::I64DivS,
            (ValType::I64, Operator::Rem) => Instruction::I64RemS,
            (ValType::I64, Operator::Add) => Instruction::I64Add,
            (ValType::I64, Operator::Sub) => Instruction::I64Sub,
            (ValType::F64, Operator::EqEq) => Instruction::F64Eq,
            (ValType::F64, Operator::NotEq) => Instruction::F64Ne,
            (ValType::F64, Operator::Lt) => Instruction::F64Lt,
            (ValType::F64, Operator::Gt) => Instruction::F64Gt,
            (ValType::F64, Operator::Lte) => Instruction::F64Le,
            (ValType::F64, Operator::Gte) => Instruction::F64Ge,
            (ValType::F64, Operator::Mul) => Instruction::F64Mul,
            (ValType::F64, Operator::Div) => Instruction::F64Div,
            (ValType::F64, Operator::Add) => Instruction::F64Add,
            (ValType::F64, Operator::Sub) => Instruction::F64Sub,
            _ => {
                return Err(NoOperatorForValType { operator, val_type });
            }
        };
        self.emit(&instruction);
        Ok(())
    }
}
