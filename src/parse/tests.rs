use crate::ast;

#[test]
fn test_minimal() -> Result<(), crate::parse::Error> {
    let result = crate::parse::parse(include_str!("../../examples/minimal.myth"))?;
    assert!(matches!(result, ast::AST {}));
    Ok(())
}
