#[test]
fn test_minimal() -> Result<(), crate::parse::Error> {
    let result = crate::parse::parse(include_str!("../../examples/test.myth"))?;
    assert_eq!(result.v.items.len(), 1);
    println!("{result:#?}");
    Ok(())
}
