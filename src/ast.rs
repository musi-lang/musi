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
        span: Span,
    },
    FunctionDeclaration {
        name: Box<str>,
        parameters: Vec<Box<str>>,
        body: Box<Statement>,
        span: Span,
    },
}

#[derive(Debug)]
pub enum ExpressionKind {
    Assignment {
        target: Box<Expression>,
        value: Box<Expression>,
        span: Span,
    },
    Identifier {
        name: Box<str>,
        span: Span,
    },
    Literal {
        value: Value,
        span: Span,
    },
    Operation {
        left: Box<Expression>,
        operator: Kind,
        right: Box<Expression>,
        span: Span,
    },
    Call {
        callee: Box<Expression>,
        arguments: Vec<Expression>,
        span: Span,
    },
}

#[derive(Debug)]
pub struct VariableDeclarator {
    pub name: Box<str>,
    pub initialiser: Expression,
    pub span: Span,
}
