use crate::parse;
use std::fmt;
use std::fmt::Formatter;

#[derive(Debug, Clone)]
pub struct Library {
    pub(crate) items: Vec<Ast<Item>>,
}

#[derive(Debug, Clone)]
pub enum Item {
    FunctionDef(Ast<FunctionDef>),
    Use(Ast<Use>),
}

#[derive(Debug, Clone)]
pub struct FunctionDef {
    pub(crate) name: Ast<Ident>,
    pub(crate) params: Vec<Ast<ParamDecl>>,
    pub(crate) return_type: Ast<Type>,
    pub(crate) body: Ast<Block>,
}

#[derive(Debug, Clone)]
pub struct Use {
    pub(crate) path: Vec<Ast<Ident>>,
}

#[derive(Debug, Clone)]
pub struct ParamDecl {
    pub(crate) name: Ast<Ident>,
    pub(crate) ty: Ast<Type>,
}

#[derive(Debug, Clone)]
pub enum Statement {
    VariableDecl(Ast<VariableDecl>),
    Assignment(Ast<Assignment>),
    BreakStmt(Ast<BreakStmt>),
    ContinueStmt(Ast<ContinueStmt>),
    ReturnStmt(Ast<ReturnStmt>),
    Expression(Ast<Expression>),
}

#[derive(Debug, Clone)]
pub struct If {
    pub(crate) condition: Ast<Expression>,
    pub(crate) then_body: Ast<Block>,
    pub(crate) else_body: Option<Ast<Block>>,
}

#[derive(Debug, Clone)]
pub struct VariableDecl {
    pub(crate) name: Ast<Ident>,
    pub(crate) ty: Ast<Type>,
    pub(crate) init_value: Ast<Expression>,
}

#[derive(Debug, Clone)]
pub struct Assignment {
    pub(crate) target: Ast<LValue>,
    pub(crate) value: Ast<Expression>,
}

#[derive(Debug, Clone)]
pub enum LValue {
    VariableRef(Ast<VariableRef>),
    Deref(Ast<Expression>),
}

#[derive(Debug, Clone)]
pub struct BreakStmt {}
#[derive(Debug, Clone)]
pub struct ContinueStmt {}
#[derive(Debug, Clone)]
pub struct ReturnStmt {
    pub(crate) retval: Option<Ast<Expression>>,
}

#[derive(Debug, Clone)]
pub enum Expression {
    Binary(Ast<Binary>),
    Unary(Ast<Unary>),
    Call(Ast<Call>),
    Literal(Ast<Literal>),
    New(Ast<New>),
    VariableRef(Ast<VariableRef>),
    If(Ast<If>),
}

#[derive(Debug, Clone)]
pub struct New {
    pub(crate) ty: Ast<Type>,
}

#[derive(Debug, Clone)]
pub struct Unary {
    pub(crate) operator: Operator,
    pub(crate) target: Ast<Expression>,
}

#[derive(Debug, Clone)]
pub enum Literal {
    String(String),
    Integer(i64),
    Float(f64),
    Bool(bool),
}

#[derive(Debug, Clone)]
pub enum Type {
    None,
    Primitive(TypePrimitive),
    Pointer(Ast<Type>),
}

#[derive(Debug, Clone)]
pub enum TypePrimitive {
    I32,
    I64,
    F64,
    Range,
    String,
}

#[derive(Debug, Clone)]
pub struct Call {
    pub(crate) callee: Ast<Expression>,
    pub(crate) args: Vec<Ast<Expression>>,
}

#[derive(Debug, Clone)]
pub struct Binary {
    pub(crate) lhs: Ast<Expression>,
    pub(crate) rhs: Ast<Expression>,
    pub(crate) operator: Operator,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
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
    RangeExclusive,
    Deref,
}

impl fmt::Display for Operator {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let as_str = match self {
            Operator::EqEq => "==",
            Operator::NotEq => "!=",
            Operator::Lt => "<",
            Operator::Gt => ">",
            Operator::Lte => "<=",
            Operator::Gte => ">=",
            Operator::Mul | Operator::Deref => "*",
            Operator::Div => "/",
            Operator::Rem => "%",
            Operator::Add => "+",
            Operator::Sub => "-",
            Operator::RangeExclusive => "..<",
        };
        f.write_str(as_str)
    }
}

#[derive(Debug, Clone)]
pub struct Block {
    pub(crate) statements: Vec<Ast<Statement>>,
    pub has_last_semi: bool,
}

#[derive(Debug, Clone)]
pub struct VariableRef {
    pub(crate) name: Ast<Ident>,
}

#[derive(Debug, Clone)]
pub struct Ident(pub(crate) String);

#[derive(Debug, Clone)]
pub struct Ast<T> {
    pub(crate) v: Box<T>,
    pub(crate) span: parse::Span,
}
