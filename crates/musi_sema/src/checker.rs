use std::collections::HashSet;

use musi_ast::{AstArena, CondKind, ExprId, ExprKind, StmtKind};
use musi_core::{MusiResult, Span};

use crate::errors;

pub struct Checker<'a> {
    ast: &'a AstArena,
    in_loop: bool,
    in_fn: bool,
}

impl<'a> Checker<'a> {
    #[must_use]
    pub const fn new(ast: &'a AstArena) -> Self {
        Self {
            ast,
            in_loop: false,
            in_fn: false,
        }
    }

    /// # Errors
    /// Returns error if semantic validation fails.
    pub fn check_expr(&mut self, expr_id: ExprId) -> MusiResult<()> {
        let expr = self.ast.exprs.get(expr_id);
        let span = expr.span;

        match &expr.kind.clone() {
            ExprKind::Break(_) => self.check_break(span),
            ExprKind::Cycle => self.check_cycle(span),
            ExprKind::Return(_) => self.check_return(span),
            ExprKind::Block { stmts, expr } => self.check_block(stmts, *expr),
            ExprKind::If {
                cond,
                then_br,
                else_br,
            } => self.check_if(*cond, *then_br, *else_br),
            ExprKind::While { cond, guard, body } => self.check_while(*cond, *guard, *body),
            ExprKind::For {
                pat: _,
                iter,
                guard,
                body,
            } => self.check_for(*iter, *guard, *body),
            ExprKind::Match { scrutinee, cases } => self.check_match(*scrutinee, cases),
            ExprKind::Fn { body, .. } => self.check_fn(*body),
            ExprKind::Call { callee, args } => self.check_call(*callee, args),
            ExprKind::Binary { lhs, rhs, .. } => {
                self.check_expr(*lhs)?;
                self.check_expr(*rhs)
            }
            ExprKind::Unary { operand, .. } => self.check_expr(*operand),
            ExprKind::Tuple(elems) | ExprKind::Array(elems) => self.check_exprs(elems),
            ExprKind::Record { fields, .. } => self.check_record_fields(fields, span),
            ExprKind::Binding { init, .. } => self.check_expr(*init),
            ExprKind::Field { base, .. } | ExprKind::Index { base, .. } => self.check_expr(*base),
            ExprKind::Assign { target, value } => {
                self.check_expr(*target)?;
                self.check_expr(*value)
            }
            ExprKind::Defer(inner) | ExprKind::Unsafe(inner) => self.check_expr(*inner),
            _ => Ok(()),
        }
    }

    fn check_break(&self, span: Span) -> MusiResult<()> {
        if self.in_loop {
            Ok(())
        } else {
            Err(errors::break_outside_loop(span))
        }
    }

    fn check_cycle(&self, span: Span) -> MusiResult<()> {
        if self.in_loop {
            Ok(())
        } else {
            Err(errors::cycle_outside_loop(span))
        }
    }

    fn check_return(&self, span: Span) -> MusiResult<()> {
        if self.in_fn {
            Ok(())
        } else {
            Err(errors::return_outside_fn(span))
        }
    }

    fn check_block(&mut self, stmts: &[musi_ast::StmtId], tail: Option<ExprId>) -> MusiResult<()> {
        for stmt_id in stmts {
            let stmt = self.ast.stmts.get(*stmt_id);
            let StmtKind::Expr(expr_id) = &stmt.kind;
            self.check_expr(*expr_id)?;
        }
        if let Some(expr) = tail {
            self.check_expr(expr)?;
        }
        Ok(())
    }

    fn check_if(
        &mut self,
        cond: musi_ast::CondId,
        then_br: ExprId,
        else_br: Option<ExprId>,
    ) -> MusiResult<()> {
        self.check_cond(cond)?;
        self.check_expr(then_br)?;
        self.check_optional_expr(else_br)
    }

    fn check_cond(&mut self, cond_id: musi_ast::CondId) -> MusiResult<()> {
        let cond = self.ast.conds.get(cond_id);
        match &cond.kind.clone() {
            CondKind::Expr(expr_id) => self.check_expr(*expr_id),
            CondKind::Case { init, extra, .. } => {
                self.check_expr(*init)?;
                self.check_exprs(extra)
            }
        }
    }

    fn check_while(
        &mut self,
        cond: musi_ast::CondId,
        guard: Option<ExprId>,
        body: ExprId,
    ) -> MusiResult<()> {
        self.check_cond(cond)?;
        self.check_optional_expr(guard)?;
        self.with_loop(|this| this.check_expr(body))
    }

    fn check_for(&mut self, iter: ExprId, guard: Option<ExprId>, body: ExprId) -> MusiResult<()> {
        self.check_expr(iter)?;
        self.check_optional_expr(guard)?;
        self.with_loop(|this| this.check_expr(body))
    }

    fn check_optional_expr(&mut self, expr: Option<ExprId>) -> MusiResult<()> {
        if let Some(e) = expr {
            self.check_expr(e)?;
        }
        Ok(())
    }

    fn check_match(&mut self, scrutinee: ExprId, cases: &[musi_ast::MatchCase]) -> MusiResult<()> {
        self.check_expr(scrutinee)?;
        for case in cases {
            if let Some(guard) = case.guard {
                self.check_expr(guard)?;
            }
            self.check_expr(case.body)?;
        }
        Ok(())
    }

    fn check_fn(&mut self, body: ExprId) -> MusiResult<()> {
        self.with_fn(|this| this.check_expr(body))
    }

    fn check_call(&mut self, callee: ExprId, args: &[ExprId]) -> MusiResult<()> {
        self.check_expr(callee)?;
        self.check_exprs(args)
    }

    fn check_exprs(&mut self, exprs: &[ExprId]) -> MusiResult<()> {
        for expr in exprs {
            self.check_expr(*expr)?;
        }
        Ok(())
    }

    fn check_record_fields(&mut self, fields: &[musi_ast::Field], span: Span) -> MusiResult<()> {
        let mut seen = HashSet::new();
        for field in fields {
            if !seen.insert(field.name) {
                return Err(errors::duplicate_field(field.name, span));
            }
            if let Some(init) = field.init {
                self.check_expr(init)?;
            }
        }
        Ok(())
    }

    fn with_loop<F, R>(&mut self, f: F) -> R
    where
        F: FnOnce(&mut Self) -> R,
    {
        let prev = self.in_loop;
        self.in_loop = true;
        let result = f(self);
        self.in_loop = prev;
        result
    }

    fn with_fn<F, R>(&mut self, f: F) -> R
    where
        F: FnOnce(&mut Self) -> R,
    {
        let prev = self.in_fn;
        self.in_fn = true;
        let result = f(self);
        self.in_fn = prev;
        result
    }
}
