use crate::ast::{Ast, Block, Statement, VariableDecl};

pub fn extract_variable_decls(block: &Ast<Block>) -> Vec<&Ast<VariableDecl>> {
    block
        .v
        .statements
        .iter()
        .filter_map(|stmt| match stmt.v.as_ref() {
            Statement::VariableDecl(decl) => Some(decl),
            _ => None,
        })
        .collect()
}
