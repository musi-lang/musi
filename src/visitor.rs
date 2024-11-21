use crate::{
    ast::{Declaration, Expression, MatchArm, Program, Statement},
    token::Token,
    value::Value,
};

pub trait ExpressionVisitor<T> {
    fn visit_binary(&mut self, left: &Expression, operator: &Token, right: &Expression) -> T;
    fn visit_unary(&mut self, operator: &Token, operand: &Expression) -> T;
    fn visit_call(&mut self, callee: &Expression, arguments: &[Expression]) -> T;
    fn visit_reference(&mut self, expression: &Expression) -> T;
    fn visit_if(
        &mut self,
        condition: &Expression,
        then_branch: &Expression,
        else_branch: Option<&Expression>,
    ) -> T;
    fn visit_match(&mut self, value: &Expression, arms: &[MatchArm]) -> T;
    fn visit_grouping(&mut self, expression: &Expression) -> T;
    fn visit_identifier(&mut self, name: &Token) -> T;
    fn visit_literal(&mut self, value: &Value) -> T;
}

pub trait StatementVisitor<T> {
    fn visit_declaration(&mut self, declaration: &Declaration) -> T;
    fn visit_expression_stmt(&mut self, expression: &Expression) -> T;
    fn visit_while(&mut self, condition: &Expression, body: &[Statement]) -> T;
    fn visit_repeat_until(&mut self, condition: &Expression, body: &[Statement]) -> T;
    fn visit_break(&mut self) -> T;
    fn visit_continue(&mut self) -> T;
    fn visit_return(&mut self, value: Option<&Expression>) -> T;
    fn visit_block(&mut self, statements: &[Statement]) -> T;
}

pub trait DeclarationVisitor<T> {
    fn visit_function(&mut self, name: &Token, parameters: &[Token], body: &[Statement]) -> T;
    fn visit_variable(&mut self, name: &Token, initialiser: &Expression, mutable: bool) -> T;
}

pub trait ProgramVisitor<T> {
    fn visit_program(&mut self, statements: &[Statement]) -> T;
}
