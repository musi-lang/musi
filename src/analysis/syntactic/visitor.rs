use super::ast::{Expression, Program, Statement};

pub trait AstVisitor<T> {
    fn visit_program(&mut self, program: &Program) -> T;
    fn visit_statement(&mut self, statement: &Statement) -> T;
    fn visit_expression(&mut self, expression: &Expression) -> T;
}

impl Program {
    #[inline]
    pub fn accept<T, V: AstVisitor<T>>(&self, visitor: &mut V) -> T {
        visitor.visit_program(self)
    }
}

impl Statement {
    #[inline]
    pub fn accept<T, V: AstVisitor<T>>(&self, visitor: &mut V) -> T {
        visitor.visit_statement(self)
    }
}

impl Expression {
    #[inline]
    pub fn accept<T, V: AstVisitor<T>>(&self, visitor: &mut V) -> T {
        visitor.visit_expression(self)
    }
}
