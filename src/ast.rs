use crate::{span::Span, token::Kind, value::Value};

#[derive(Debug)]
pub struct Program {
    pub statements: Vec<Statement>,
    pub span: Span,
}

#[derive(Debug)]
pub struct Statement {
    pub kind: StatementKind,
    pub span: Span,
}

#[derive(Debug)]
pub struct Expression {
    pub kind: ExpressionKind,
    pub span: Span,
}

#[derive(Debug)]
pub enum StatementKind {
    Expression(Expression),
    Block(Vec<Statement>),
    VariableDeclaration {
        declarations: Vec<VariableDeclarator>,
        mutable: bool,
    },
    FunctionDeclaration {
        name: String,
        parameters: Vec<String>,
        body: Box<Statement>,
    },
}

#[derive(Debug)]
pub enum ExpressionKind {
    Identifier(String),
    Literal(Value),
    Operation {
        left: Box<Expression>,
        operator: Kind,
        right: Box<Expression>,
    },
    Call {
        callee: Box<Expression>,
        arguments: Vec<Expression>,
    },
}

#[derive(Debug)]
pub struct VariableDeclarator {
    pub name: String,
    pub initialiser: Expression,
}
