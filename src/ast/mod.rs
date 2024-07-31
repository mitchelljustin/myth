use crate::parse;

pub struct Library {
    pub(crate) items: Vec<AST<Item>>,
}

pub enum Item {
    FunctionDef(AST<FunctionDef>),
}

pub struct FunctionDef {
    pub(crate) name: AST<Ident>,
    pub(crate) params: Vec<AST<ParamDecl>>,
    pub(crate) return_type: AST<Type>,
    pub(crate) body: AST<Block>,
}

pub struct ParamDecl {
    pub(crate) name: AST<Ident>,
    pub(crate) ty: AST<Type>,
}

pub enum Statement {
    Assignment(AST<Assignment>),
    BreakStmt(AST<BreakStmt>),
    ContinueStmt(AST<ContinueStmt>),
    ReturnStmt(AST<ReturnStmt>),
    IfStmt(AST<IfStmt>),
    Expression(AST<Expression>),
}

pub struct IfStmt {
    pub(crate) condition: AST<Expression>,
    pub(crate) if_body: AST<Block>,
    pub(crate) else_body: Option<AST<Block>>,
}

pub struct Assignment {
    pub(crate) target: AST<Expression>,
    pub(crate) value: AST<Expression>,
}

pub struct BreakStmt {}
pub struct ContinueStmt {}
pub struct ReturnStmt {
    pub(crate) retval: Option<AST<Expression>>,
}

pub enum Expression {
    BinaryExpr(AST<BinaryExpr>),
    Call(AST<Call>),
    Literal(AST<Literal>),
    Path(AST<Path>),
}

pub enum Literal {
    String(String),
    Integer(i64),
    Float(f64),
    Bool(bool),
}

pub type Type = Path;

pub struct Path(pub(crate) Vec<AST<Ident>>);

pub struct Call {
    pub(crate) callee: AST<Expression>,
    pub(crate) args: Vec<AST<Expression>>,
}

pub struct BinaryExpr {
    pub(crate) lhs: AST<Expression>,
    pub(crate) rhs: AST<Expression>,
    pub(crate) operator: Operator,
}

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

pub struct Block(pub(crate) Vec<AST<Statement>>);

pub struct Ident(pub(crate) String);

pub struct AST<T> {
    pub(crate) v: Box<T>,
    pub(crate) span: parse::Span,
}
