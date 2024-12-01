use crate::{span::Span, token::Token, value::Value, visitor::Visitor};

pub enum Node {
    Expression(Box<ExpressionKind>),
    Statement(Box<StatementKind>),
    Declaration(Box<DeclarationKind>),
}

impl Node {
    pub fn accept<T>(&self, visitor: &mut dyn Visitor<T>) -> T {
        match self {
            Self::Expression(kind) => kind.accept(visitor),
            Self::Statement(kind) => kind.accept(visitor),
            Self::Declaration(kind) => kind.accept(visitor),
        }
    }

    pub const fn span(&self) -> Span {
        match self {
            Self::Expression(kind) => kind.span(),
            Self::Statement(kind) => kind.span(),
            Self::Declaration(kind) => kind.span(),
        }
    }
}

pub enum ExpressionKind {
    Literal {
        value: Value,
        span: Span,
    },
    Identifier {
        name: Token,
        span: Span,
    },
    BinaryOperation {
        left: Box<ExpressionKind>,
        operator: Token,
        right: Box<ExpressionKind>,
        span: Span,
    },
    UnaryOperation {
        operator: Token,
        operand: Box<ExpressionKind>,
        span: Span,
    },
    FunctionCall {
        callee: Box<ExpressionKind>,
        arguments: Vec<ExpressionKind>,
        span: Span,
    },
}

impl ExpressionKind {
    pub fn accept<T>(&self, visitor: &mut dyn Visitor<T>) -> T {
        match self {
            Self::Literal { value, span } => visitor.visit_literal(value, *span),
            Self::Identifier { name, span } => visitor.visit_identifier(name, *span),
            Self::BinaryOperation {
                left,
                operator,
                right,
                span,
            } => visitor.visit_binary_operation(left, operator, right, *span),
            Self::UnaryOperation {
                operator,
                operand,
                span,
            } => visitor.visit_unary_operation(operator, operand, *span),
            Self::FunctionCall {
                callee,
                arguments,
                span,
            } => visitor.visit_function_call(callee, arguments, *span),
        }
    }

    pub const fn span(&self) -> Span {
        match self {
            Self::Literal { span, .. }
            | Self::Identifier { span, .. }
            | Self::BinaryOperation { span, .. }
            | Self::UnaryOperation { span, .. }
            | Self::FunctionCall { span, .. } => *span,
        }
    }
}

pub enum StatementKind {
    ExpressionStatement {
        kind: ExpressionKind,
        span: Span,
    },
    Block {
        statements: Vec<StatementKind>,
        span: Span,
    },
    Conditional {
        condition: ExpressionKind,
        then_branch: Box<StatementKind>,
        else_branch: Option<Box<StatementKind>>,
        span: Span,
    },
}

impl StatementKind {
    pub fn accept<T>(&self, visitor: &mut dyn Visitor<T>) -> T {
        match self {
            Self::ExpressionStatement { kind, span } => {
                visitor.visit_expression_statement(kind, *span)
            }
            Self::Block { statements, span } => visitor.visit_block(statements, *span),
            Self::Conditional {
                condition,
                then_branch,
                else_branch,
                span,
            } => visitor.visit_conditional(condition, then_branch, else_branch, *span),
        }
    }

    pub const fn span(&self) -> Span {
        match self {
            Self::ExpressionStatement { span, .. }
            | Self::Block { span, .. }
            | Self::Conditional { span, .. } => *span,
        }
    }
}

pub enum DeclarationKind {
    Constant {
        name: Token,
        initialiser: ExpressionKind,
        span: Span,
    },
    Variable {
        name: Token,
        mutable: bool, // false='let', true='var'
        initialiser: Option<ExpressionKind>,
        span: Span,
    },
    Function {
        name: Token,
        parameters: Vec<Token>,
        body: Box<StatementKind>,
        span: Span,
    },
}

impl DeclarationKind {
    pub fn accept<T>(&self, visitor: &mut dyn Visitor<T>) -> T {
        match self {
            Self::Constant {
                name,
                initialiser,
                span,
            } => visitor.visit_constant(name, initialiser, *span),
            Self::Variable {
                name,
                mutable,
                initialiser,
                span,
            } => visitor.visit_variable(name, *mutable, initialiser, *span),
            Self::Function {
                name,
                parameters,
                body,
                span,
            } => visitor.visit_function(name, parameters, body, *span),
        }
    }

    pub const fn span(&self) -> Span {
        match self {
            Self::Constant { span, .. }
            | Self::Variable { span, .. }
            | Self::Function { span, .. } => *span,
        }
    }
}
