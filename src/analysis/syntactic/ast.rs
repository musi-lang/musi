use crate::{
    analysis::lexical::token::TokenKind,
    core::{span::Span, value::Value},
};

#[derive(Debug)]
#[non_exhaustive]
pub struct Program {
    pub statements: Vec<Statement>,
    pub span: Span,
}

#[derive(Debug)]
#[non_exhaustive]
pub struct Statement {
    pub kind: StatementKind,
    pub span: Span,
}

#[derive(Debug)]
#[non_exhaustive]
pub struct Expression {
    pub kind: ExpressionKind,
    pub span: Span,
}

#[derive(Debug)]
#[non_exhaustive]
pub enum StatementKind {
    Expression(Expression),
    Block(Vec<Statement>),
    VariableDeclaration {
        name: Box<str>,
        initialiser: Expression,
        mutable: bool,
        span: Span,
    },
    // FunctionDeclaration {
    //     name: Box<str>,
    //     parameters: Vec<Box<str>>,
    //     body: Box<Statement>,
    //     span: Span,
    // },
}

#[derive(Debug)]
#[non_exhaustive]
pub enum ExpressionKind {
    Assignment {
        target: Box<Expression>,
        value: Box<Expression>,
        span: Span,
    },
    Binary {
        left: Box<Expression>,
        operator: TokenKind,
        right: Box<Expression>,
        span: Span,
    },
    Block {
        statements: Vec<Statement>,
        nesting_depth: u8,
        span: Span,
    },
    Call {
        callee: Box<Expression>,
        arguments: Vec<Expression>,
        span: Span,
    },
    Conditional {
        condition: Box<Expression>,
        consequence: Box<Expression>,
        alternative: Option<Box<Expression>>,
        span: Span,
    },
    Identifier {
        name: Box<str>,
        span: Span,
    },
    Lambda {
        parameters: Vec<Box<str>>,
        body: Box<Expression>,
        span: Span,
    },
    Literal {
        value: Value,
        span: Span,
    },
    Match {
        scrutinee: Box<Expression>,
        arms: Vec<MatchArm>,
        span: Span,
    },
    Unary {
        operator: TokenKind,
        operand: Box<Expression>,
        span: Span,
    },
}

#[derive(Debug)]
#[non_exhaustive]
pub enum PatternKind {
    Literal(Value),
    Identifier(Box<str>),
    Tuple {
        elements: Vec<PatternKind>,
    },
    Constructor {
        name: Box<str>,
        fields: Vec<PatternKind>,
    },
    Wildcard,
}

#[derive(Debug)]
#[non_exhaustive]
pub struct MatchArm {
    pub pattern: PatternKind,
    pub guard: Option<Box<Expression>>,
    pub body: Box<Expression>,
    pub span: Span,
}
