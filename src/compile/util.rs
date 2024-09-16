use crate::ast::{Assignment, Ast, Block, Statement};

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
