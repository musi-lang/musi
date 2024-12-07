use crate::{span::Span, token::Kind, value::Value};

#[derive(Debug)]
pub struct Node<T> {
    pub kind: T,
    pub span: Span,
}

pub type Program = Node<ProgramKind>;
pub type Statement = Node<StatementKind>;
pub type Expression = Node<ExpressionKind>;

#[derive(Debug)]
pub struct ProgramKind {
    pub body: Vec<Statement>,
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
