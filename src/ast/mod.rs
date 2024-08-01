use crate::parse;

#[derive(Debug, Clone)]
pub struct Library {
    pub(crate) items: Vec<AST<Item>>,
}

#[derive(Debug, Clone)]
pub enum Item {
    FunctionDef(AST<FunctionDef>),
}

#[derive(Debug, Clone)]
pub struct FunctionDef {
    pub(crate) name: AST<Ident>,
    pub(crate) params: Vec<AST<ParamDecl>>,
    pub(crate) return_type: AST<Type>,
    pub(crate) body: AST<Block>,
}

#[derive(Debug, Clone)]
pub struct ParamDecl {
    pub(crate) name: AST<Ident>,
    pub(crate) ty: AST<Type>,
}

#[derive(Debug, Clone)]
pub enum Statement {
    Assignment(AST<Assignment>),
    BreakStmt(AST<BreakStmt>),
    ContinueStmt(AST<ContinueStmt>),
    ReturnStmt(AST<ReturnStmt>),
    IfStmt(AST<IfStmt>),
    Expression(AST<Expression>),
}

#[derive(Debug, Clone)]
pub struct IfStmt {
    pub(crate) condition: AST<Expression>,
    pub(crate) if_body: AST<Block>,
    pub(crate) else_body: Option<AST<Block>>,
}

#[derive(Debug, Clone)]
pub struct Assignment {
    pub(crate) target: AST<Expression>,
    pub(crate) value: AST<Expression>,
}

#[derive(Debug, Clone)]
pub struct BreakStmt {}
#[derive(Debug, Clone)]
pub struct ContinueStmt {}
#[derive(Debug, Clone)]
pub struct ReturnStmt {
    pub(crate) retval: Option<AST<Expression>>,
}

#[derive(Debug, Clone)]
pub enum Expression {
    BinaryExpr(AST<BinaryExpr>),
    Call(AST<Call>),
    Literal(AST<Literal>),
    Path(AST<Path>),
}

#[derive(Debug, Clone)]
pub enum Literal {
    String(String),
    Integer(i64),
    Float(f64),
    Bool(bool),
}

pub type Type = Path;

#[derive(Debug, Clone)]
pub struct Path(pub(crate) Vec<AST<Ident>>);

#[derive(Debug, Clone)]
pub struct Call {
    pub(crate) callee: AST<Expression>,
    pub(crate) args: Vec<AST<Expression>>,
}

#[derive(Debug, Clone)]
pub struct BinaryExpr {
    pub(crate) lhs: AST<Expression>,
    pub(crate) rhs: AST<Expression>,
    pub(crate) operator: Operator,
}

#[derive(Debug, Clone)]
pub enum Operator {
    EqEq,
    NotEq,
    Lt,
    Gt,
    Lte,
    Gte,
    Mul,
    Div,
    Rem,
    Add,
    Sub,
}

#[derive(Debug, Clone)]
pub struct Block(pub(crate) Vec<AST<Statement>>);

#[derive(Debug, Clone)]
pub struct Ident(pub(crate) String);

#[derive(Debug, Clone)]
pub struct AST<T> {
    pub(crate) v: Box<T>,
    pub(crate) span: parse::Span,
}
