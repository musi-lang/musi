use crate::{
    token::Token,
    value::Value,
    visitor::{DeclarationVisitor, ExpressionVisitor, ProgramVisitor, StatementVisitor},
};

#[derive(Debug, Clone)]
pub struct Program {
    pub statements: Vec<Statement>,
}

impl Program {
    pub fn accept<T>(&self, visitor: &mut impl ProgramVisitor<T>) -> T {
        visitor.visit_program(&self.statements)
    }
}

#[derive(Debug, Clone)]
pub enum Declaration {
    Function {
        name: Token,
        parameters: Vec<Token>,
        body: Vec<Statement>,
    },
    Variable {
        name: Token,
        initialiser: Box<Expression>,
        mutable: bool,
    },
}

impl Declaration {
    pub fn accept<T>(&self, visitor: &mut impl DeclarationVisitor<T>) -> T {
        match self {
            Declaration::Function {
                name,
                parameters,
                body,
            } => visitor.visit_function(name, parameters, body),
            Declaration::Variable {
                name,
                initialiser,
                mutable,
            } => visitor.visit_variable(name, initialiser, *mutable),
        }
    }
}

#[derive(Debug, Clone)]
pub enum Expression {
    Binary {
        left: Box<Expression>,
        operator: Token,
        right: Box<Expression>,
    },
    Unary {
        operator: Token,
        operand: Box<Expression>,
    },
    Call {
        callee: Box<Expression>,
        arguments: Vec<Expression>,
    },
    Reference(Box<Expression>),
    If {
        condition: Box<Expression>,
        then_branch: Box<Expression>,
        else_branch: Option<Box<Expression>>,
    },
    Match {
        value: Box<Expression>,
        arms: Vec<MatchArm>,
    },
    Grouping(Box<Expression>),
    Identifier(Token),
    Literal(Value),
}

impl Expression {
    pub fn accept<T>(&self, visitor: &mut impl ExpressionVisitor<T>) -> T {
        match self {
            Expression::Binary {
                left,
                operator,
                right,
            } => visitor.visit_binary(left, operator, right),
            Expression::Unary { operator, operand } => visitor.visit_unary(operator, operand),
            Expression::Call { callee, arguments } => visitor.visit_call(callee, arguments),
            Expression::Reference(expr) => visitor.visit_reference(expr),
            Expression::If {
                condition,
                then_branch,
                else_branch,
            } => visitor.visit_if(condition, then_branch, else_branch.as_deref()),
            Expression::Match { value, arms } => visitor.visit_match(value, arms),
            Expression::Grouping(expr) => visitor.visit_grouping(expr),
            Expression::Identifier(name) => visitor.visit_identifier(name),
            Expression::Literal(value) => visitor.visit_literal(value),
        }
    }
}

#[derive(Debug, Clone)]
pub enum Statement {
    Declaration(Declaration),
    Expression(Expression),
    While {
        condition: Box<Expression>,
        body: Vec<Statement>,
    },
    RepeatUntil {
        body: Vec<Statement>,
        condition: Box<Expression>,
    },
    Break,
    Continue,
    Return {
        value: Option<Box<Expression>>,
    },
    Block(Vec<Statement>),
}

impl Statement {
    pub fn accept<T>(&self, visitor: &mut impl StatementVisitor<T>) -> T {
        match self {
            Statement::Declaration(decl) => visitor.visit_declaration(decl),
            Statement::Expression(expr) => visitor.visit_expression_stmt(expr),
            Statement::While { condition, body } => visitor.visit_while(condition, body),
            Statement::RepeatUntil { condition, body } => {
                visitor.visit_repeat_until(condition, body)
            }
            Statement::Break => visitor.visit_break(),
            Statement::Continue => visitor.visit_continue(),
            Statement::Return { value } => visitor.visit_return(value.as_deref()),
            Statement::Block(statements) => visitor.visit_block(statements),
        }
    }
}

#[derive(Debug, Clone)]
pub struct MatchArm {
    pub pattern: Box<Expression>,
    pub guard: Option<Box<Expression>>,
    pub body: Box<Expression>,
}
