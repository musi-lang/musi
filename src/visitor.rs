use crate::{
    ast::{ExpressionKind, StatementKind},
    span::Span,
    token::Token,
    value::Value,
};

pub trait Visitor<T> {
    // Expression
    fn visit_literal(&mut self, value: &Value, span: Span) -> T;
    fn visit_identifier(&mut self, name: &Token, span: Span) -> T;
    fn visit_binary_operation(
        &mut self,
        left: &ExpressionKind,
        operator: &Token,
        right: &ExpressionKind,
        span: Span,
    ) -> T;
    fn visit_unary_operation(
        &mut self,
        operator: &Token,
        operand: &ExpressionKind,
        span: Span,
    ) -> T;
    fn visit_function_call(
        &mut self,
        callee: &ExpressionKind,
        arguments: &[ExpressionKind],
        span: Span,
    ) -> T;

    // Statement
    fn visit_expression_statement(&mut self, kind: &ExpressionKind, span: Span) -> T;
    fn visit_block(&mut self, statements: &[StatementKind], span: Span) -> T;
    fn visit_conditional(
        &mut self,
        condition: &ExpressionKind,
        then_branch: &StatementKind,
        else_branch: &Option<Box<StatementKind>>,
        span: Span,
    ) -> T;

    // Declaration
    fn visit_constant(&mut self, name: &Token, initialiser: &ExpressionKind, span: Span) -> T;
    fn visit_variable(
        &mut self,
        name: &Token,
        mutable: bool,
        initialiser: &Option<ExpressionKind>,
        span: Span,
    ) -> T;
    fn visit_function(
        &mut self,
        name: &Token,
        parameters: &[Token],
        body: &StatementKind,
        span: Span,
    ) -> T;
}
