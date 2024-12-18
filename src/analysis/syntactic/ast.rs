use crate::{
    analysis::lexical::token::TokenKind,
    core::{span::Span, value::Value},
};

#[non_exhaustive]
#[derive(Debug)]
pub struct Program {
    pub statements: Vec<Statement>,
    pub span: Span,
}

#[non_exhaustive]
#[derive(Debug)]
pub struct Statement {
    pub kind: StatementKind,
    pub span: Span,
}

#[non_exhaustive]
#[derive(Debug)]
pub enum StatementKind {
    Declaration(Declaration),
    Expression(Expression),
    While {
        condition: Box<Expression>,
        body: Box<Statement>,
    },
    Return(Option<Box<Expression>>),
}

#[non_exhaustive]
#[derive(Debug)]
pub enum Declaration {
    Variable {
        name: Box<str>,
        mutable: bool,
        type_annotation: Option<TypeAnnotation>,
        initialiser: Option<Expression>,
    },
    Function {
        name: Box<str>,
        parameters: Vec<Parameter>,
        return_type: Option<TypeAnnotation>,
        body: Box<Statement>,
    },
}

#[non_exhaustive]
#[derive(Debug)]
pub struct Expression {
    pub kind: ExpressionKind,
    pub type_annotation: Option<TypeAnnotation>,
    pub span: Span,
}

#[non_exhaustive]
#[derive(Debug)]
pub enum ExpressionKind {
    Literal(Value),
    Variable(Box<str>),
    Binary {
        left: Box<Expression>,
        operator: TokenKind,
        right: Box<Expression>,
    },
    Unary {
        operator: TokenKind,
        operand: Box<Expression>,
    },
    Call {
        callee: Box<Expression>,
        arguments: Vec<Expression>,
    },
    Lambda {
        parameters: Vec<Parameter>,
        body: Box<Expression>,
    },
    Assignment {
        target: Box<Expression>,
        value: Box<Expression>,
    },
    Block {
        statements: Vec<Statement>,
    },
    If {
        condition: Box<Expression>,
        then_branch: Box<Expression>,
        else_branch: Option<Box<Expression>>,
    },
}

#[non_exhaustive]
#[derive(Debug)]
pub struct Parameter {
    pub name: Box<str>,
    pub type_annotation: Option<TypeAnnotation>,
}

#[non_exhaustive]
#[derive(Debug)]
pub struct TypeAnnotation {
    pub name: Box<str>,
    pub parameters: Vec<TypeAnnotation>,
    pub span: Span,
}
