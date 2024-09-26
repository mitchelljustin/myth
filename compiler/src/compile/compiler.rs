use std::collections::HashMap;

use wasm_encoder::{
    BlockType, CodeSection, DataSection, Encode, EntityType, ExportKind, ExportSection, Function,
    FunctionSection, ImportSection, Instruction, MemorySection, MemoryType, Module, TypeSection,
    ValType,
};

use crate::ast::{Ast, Expression, Literal, Operator, Statement};
use crate::compile::analyzer::Analyzer;
use crate::compile::util;
use crate::compile::CallFrame;
use crate::compile::Error::{
    IfElseIncompatibleTypes, InvalidLValue, NoOperatorForType, NoSuchFunction,
};
use crate::{ast, compile};

#[derive(Default)]
pub struct Compiler {
    code_buffer: Vec<u8>,
    analyzer: Analyzer,
    func_index: u32,
    func_name_to_index: HashMap<String, u32>,
    call_frame: CallFrame,
    import_section: ImportSection,
    data_section: DataSection,
    type_section: TypeSection,
    function_section: FunctionSection,
    export_section: ExportSection,
    code_section: CodeSection,
    strings: HashMap<String, u32>,
}

const GC_NEW: u32 = 0;
const GC_INC_REF: u32 = 1;
const GC_DEC_REF: u32 = 2;
const GC_GET_DATA: u32 = 3;

impl Compiler {
    pub fn new() -> Self {
        let mut compiler = Self {
            ..Default::default()
        };
        compiler.init();
        compiler
    }

    fn init(&mut self) {
        self.type_section.function([ValType::I32], [ValType::I32]);
        self.type_section.function([ValType::I32], []);
        self.type_section.function([ValType::I32], []);
        self.type_section.function([ValType::I32], [ValType::I32]);
        self.import_section
            .import("gc", "new", EntityType::Function(GC_NEW))
            .import("gc", "inc_ref", EntityType::Function(GC_INC_REF))
            .import("gc", "dec_ref", EntityType::Function(GC_DEC_REF))
            .import("gc", "get_data", EntityType::Function(GC_GET_DATA));
    }

    pub fn compile_library(&mut self, lib: Ast<ast::Library>) -> compile::Result {
        self.analyzer.analyze_library(&lib);
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
        let params: HashMap<String, u32> = func_def
            .v
            .params
            .iter()
            .enumerate()
            .map(|(i, param)| (param.v.name.v.0.clone(), i as u32))
            .collect();
        let mut local_type_counts = HashMap::<ValType, u32>::new();
        let mut locals = HashMap::<String, u32>::new();
        let func_body = func_def.v.body;
        let assignments = util::extract_assignments(&func_body);
        for (i, assn) in assignments.into_iter().enumerate() {
            let Some(val_type) = self.analyzer.ast_type_to_ty(&assn.v.ty).val_type() else {
                continue;
            };
            *local_type_counts.entry(val_type).or_default() += 1;
            let target = &assn.v.target;
            let Expression::VariableRef(var_ref) = target.v.as_ref() else {
                continue;
            };
            locals.insert(var_ref.v.name.v.0.clone(), (i + params.len()) as u32);
        }
        for (name, &index) in params.iter() {
            locals.insert(name.clone(), index);
        }
        // Set metadata
        let func_index = self.import_section.len() + self.function_section.len();
        let func_name = func_def.v.name.v.0;
        let param_types: Vec<ValType> = func_def
            .v
            .params
            .iter()
            .filter_map(|param| self.analyzer.ast_type_to_ty(&param.v.ty).val_type())
            .collect();
        let results = self
            .analyzer
            .ast_type_to_ty(&func_def.v.return_type)
            .val_type();
        self.type_section.function(param_types, results);
        self.function_section.function(func_index);
        self.export_section
            .export(func_name.as_str(), ExportKind::Func, func_index);

        self.func_name_to_index
            .insert(func_name.clone(), func_index);
        self.call_frame = CallFrame {
            func_name,
            params,
            locals,
        };
        self.code_buffer.clear();
        self.compile_block(func_body)?;
        self.emit(&Instruction::End);
        let mut function = Function::new(local_type_counts.into_iter().map(|(k, v)| (v, k)));
        function.raw(self.code_buffer.drain(..));
        self.code_section.function(&function);

        Ok(())
    }

    fn emit(&mut self, instruction: &Instruction) {
        instruction.encode(&mut self.code_buffer);
    }

    pub fn finish(self) -> Module {
        let mut module = Module::new();
        let mut memory = MemorySection::new();
        memory.memory(MemoryType {
            maximum: None,
            minimum: 1,
            memory64: false,
            shared: false,
            page_size_log2: None,
        });

        module.section(&self.type_section);
        module.section(&self.import_section);
        module.section(&self.function_section);
        module.section(&memory);
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
                let Expression::VariableRef(var_ref) = target.v.as_ref() else {
                    return Err(InvalidLValue { expression: target });
                };
                let local_index = *self
                    .call_frame
                    .locals
                    .get(var_ref.v.name.v.0.as_str())
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
        }
        Ok(())
    }

    fn compile_expression(&mut self, expression: Ast<Expression>) -> compile::Result {
        let ty = self
            .analyzer
            .resolve_expr_type(&self.call_frame, &expression)?;
        match *expression.v {
            Expression::BinaryExpr(bin_expr) => {
                self.compile_expression(bin_expr.v.lhs)?;
                self.compile_expression(bin_expr.v.rhs)?;
                self.compile_operator_val_type(ty.val_type().unwrap(), bin_expr.v.operator)?;
            }
            Expression::Call(call) => {
                for arg in call.v.args {
                    self.compile_expression(arg)?;
                }
                match *call.v.callee.v {
                    Expression::VariableRef(var_ref) => {
                        let func_name = var_ref.v.name.v.0.as_str();
                        let Some(&func_index) = self.func_name_to_index.get(func_name) else {
                            return Err(NoSuchFunction {
                                func_name: func_name.to_string(),
                            });
                        };
                        self.emit(&Instruction::Call(func_index));
                    }
                    expr => unimplemented!("call: {expr:?}"),
                }
            }
            Expression::Literal(lit) => match *lit.v {
                Literal::Integer(val) => {
                    self.emit(&Instruction::I32Const(val as i32));
                }
                Literal::String(value) => {
                    todo!()
                }
                Literal::Float(val) => {
                    self.emit(&Instruction::F64Const(val));
                }
                Literal::Bool(val) => {
                    self.emit(&Instruction::I32Const(if val { 1 } else { 0 }));
                }
            },
            Expression::VariableRef(var_ref) => {
                let local_index = self
                    .call_frame
                    .locals
                    .get(var_ref.v.name.v.0.as_str())
                    .expect("cannot find local");
                self.emit(&Instruction::LocalGet(*local_index));
            }
            Expression::IfExpr(if_expr) => {
                let ast::IfExpr {
                    condition,
                    then_body,
                    else_body,
                } = *if_expr.v;
                self.compile_expression(condition)?;
                let then_result_type = self
                    .analyzer
                    .resolve_block_result_type(&self.call_frame, &then_body)?;
                let else_body_result_type = else_body
                    .as_ref()
                    .map(|body| {
                        self.analyzer
                            .resolve_block_result_type(&self.call_frame, body)
                    })
                    .transpose()?;
                if let Some(else_result_type) = else_body_result_type {
                    if then_result_type != else_result_type {
                        return Err(IfElseIncompatibleTypes {
                            expected: format!("{then_result_type:?}"),
                            actual: format!("{else_result_type:?}"),
                        });
                    }
                };
                let block_type = match then_result_type.val_type() {
                    None => BlockType::Empty,
                    Some(ty) => BlockType::Result(ty),
                };
                self.emit(&Instruction::If(block_type));
                self.compile_block(then_body)?;
                if let Some(else_body) = else_body {
                    self.emit(&Instruction::Else);
                    self.compile_block(else_body)?;
                }
                self.emit(&Instruction::End);
            }
            Expression::New(new) => {
                let ty = self.analyzer.ast_type_to_ty(&new.v.ty);
                let size = ty.size();
                self.emit(&Instruction::I32Const(size));
                self.emit(&Instruction::Call(GC_NEW));
            }
        }
        Ok(())
    }

    fn compile_operator_val_type(
        &mut self,
        val_type: ValType,
        operator: Operator,
    ) -> compile::Result {
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
                return Err(NoOperatorForType { operator, val_type });
            }
        };
        self.emit(&instruction);
        Ok(())
    }
}
