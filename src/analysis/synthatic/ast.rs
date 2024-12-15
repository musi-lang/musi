use crate::{
    analysis::lexical::token::Kind,
    core::{span::Span, value::Value},
};

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
        name: Box<str>,
        initialiser: Expression,
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
    Literal {
        value: Value,
        span: Span,
    },
    Identifier {
        name: Box<str>,
        span: Span,
    },
    Binary {
        left: Box<Expression>,
        operator: Kind,
        right: Box<Expression>,
        span: Span,
    },
    Unary {
        operator: Kind,
        operand: Box<Expression>,
        span: Span,
    },
    Assignment {
        target: Box<Expression>,
        value: Box<Expression>,
        span: Span,
    },
    Call {
        callee: Box<Expression>,
        arguments: Vec<Expression>,
        span: Span,
    },
    Block {
        statements: Vec<Statement>,
        scope_level: u8,
        span: Span,
    },
    Conditional {
        condition: Box<Expression>,
        consequence: Box<Expression>,
        alternative: Option<Box<Expression>>,
        span: Span,
    },
    Match {
        scrutinee: Box<Expression>,
        arms: Vec<MatchArm>,
        span: Span,
    },
}

#[derive(Debug)]
pub enum PatternKind {
    Literal(Value),
    Identifier(Box<str>),
    Tuple(Vec<PatternKind>),
    Constructor {
        name: Box<str>,
        fields: Vec<PatternKind>,
    },
    Wildcard,
}

#[derive(Debug)]
pub struct MatchArm {
    pub pattern: PatternKind,
    pub guard: Option<Box<Expression>>,
    pub body: Box<Expression>,
    pub span: Span,
}
