use std::collections::HashMap;

use musi_ast::{AstArena, ExprKind, Pat, PatKind, Prog};
use musi_core::{Span, Symbol};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BindingKind {
    ValBinding,
    VarBinding,
    Function,
    TypeDef,
    Parameter { mutable: bool },
}

#[derive(Debug, Clone)]
pub struct BindingInfo {
    pub kind: BindingKind,
    pub def_span: Span,
    pub usages: Vec<Span>,
}

pub struct BindingCollector<'a> {
    arena: &'a AstArena,
    bindings: HashMap<Symbol, BindingInfo>,
}

impl<'a> BindingCollector<'a> {
    pub fn new(arena: &'a AstArena) -> Self {
        Self {
            arena,
            bindings: HashMap::new(),
        }
    }

    pub fn collect(mut self, prog: &Prog) -> HashMap<Symbol, BindingInfo> {
        for stmt_id in &prog.stmts {
            let stmt = self.arena.stmts.get(*stmt_id);
            let musi_ast::StmtKind::Expr(expr_id) = &stmt.kind;
            self.visit_expr(*expr_id);
        }
        self.bindings
    }

    fn visit_expr(&mut self, expr_id: musi_ast::ExprId) {
        let expr = self.arena.exprs.get(expr_id);
        match &expr.kind {
            ExprKind::Ident(sym) => self.record_usage(*sym, expr.span),
            ExprKind::Binding {
                mutable, pat, init, ..
            } => {
                self.collect_binding_pat(*pat, *mutable);
                self.visit_expr(*init);
            }
            ExprKind::Fn { sig, body, .. } => self.visit_fn(sig, *body),
            ExprKind::RecordDef { name, .. } | ExprKind::ChoiceDef { name, .. } => {
                if let Some(n) = name {
                    self.register_type(*n, expr.span);
                }
            }
            ExprKind::TypeDef { name, .. } => self.register_type(*name, expr.span),
            ExprKind::Block { stmts, expr: tail } => self.visit_block(stmts, tail.as_ref()),
            ExprKind::If {
                then_br, else_br, ..
            } => self.visit_if(*then_br, else_br.as_ref()),
            ExprKind::While { body, .. } | ExprKind::For { body, .. } => self.visit_expr(*body),
            ExprKind::Match { scrutinee, cases } => self.visit_match(*scrutinee, cases),
            ExprKind::Call { callee, args } => self.visit_call(*callee, args),
            ExprKind::Binary { lhs, rhs, .. }
            | ExprKind::Assign {
                target: lhs,
                value: rhs,
            } => {
                self.visit_expr(*lhs);
                self.visit_expr(*rhs);
            }
            ExprKind::Unary { operand, .. } => self.visit_expr(*operand),
            ExprKind::Field { base, .. } | ExprKind::Index { base, .. } => self.visit_expr(*base),
            ExprKind::Return(Some(e))
            | ExprKind::Defer(e)
            | ExprKind::Unsafe(e)
            | ExprKind::Break(Some(e))
            | ExprKind::Propagate(e)
            | ExprKind::Force(e)
            | ExprKind::Deref(e) => self.visit_expr(*e),
            ExprKind::Tuple(es) | ExprKind::Array(es) => self.visit_exprs(es),
            ExprKind::Record { fields, .. } => {
                for f in fields {
                    if let Some(init) = f.init {
                        self.visit_expr(init);
                    }
                }
            }
            ExprKind::Range { start, end, .. } => {
                self.visit_expr(*start);
                if let Some(e) = end {
                    self.visit_expr(*e);
                }
            }
            _ => {}
        }
    }

    fn visit_fn(&mut self, sig: &musi_ast::FnSig, body: musi_ast::ExprId) {
        if let Some(name) = sig.name {
            self.register_binding(name, BindingKind::Function, sig.span);
        }
        for param in &sig.params {
            self.register_binding(
                param.name,
                BindingKind::Parameter {
                    mutable: param.mutable,
                },
                Span::new(0, 0),
            );
        }
        self.visit_expr(body);
    }

    fn register_type(&mut self, name: Symbol, span: Span) {
        self.register_binding(name, BindingKind::TypeDef, span);
    }

    fn register_binding(&mut self, name: Symbol, kind: BindingKind, def_span: Span) {
        let _ = self.bindings.insert(
            name,
            BindingInfo {
                kind,
                def_span,
                usages: Vec::new(),
            },
        );
    }

    fn record_usage(&mut self, sym: Symbol, span: Span) {
        if let Some(info) = self.bindings.get_mut(&sym) {
            info.usages.push(span);
        }
    }

    fn visit_block(&mut self, stmts: &[musi_ast::StmtId], tail: Option<&musi_ast::ExprId>) {
        for stmt_id in stmts {
            let stmt = self.arena.stmts.get(*stmt_id);
            let musi_ast::StmtKind::Expr(e) = &stmt.kind;
            self.visit_expr(*e);
        }
        if let Some(e) = tail {
            self.visit_expr(*e);
        }
    }

    fn visit_if(&mut self, then_br: musi_ast::ExprId, else_br: Option<&musi_ast::ExprId>) {
        self.visit_expr(then_br);
        if let Some(e) = else_br {
            self.visit_expr(*e);
        }
    }

    fn visit_match(&mut self, scrutinee: musi_ast::ExprId, cases: &[musi_ast::MatchCase]) {
        self.visit_expr(scrutinee);
        for case in cases {
            self.visit_expr(case.body);
        }
    }

    fn visit_call(&mut self, callee: musi_ast::ExprId, args: &[musi_ast::ExprId]) {
        self.visit_expr(callee);
        self.visit_exprs(args);
    }

    fn visit_exprs(&mut self, exprs: &[musi_ast::ExprId]) {
        for e in exprs {
            self.visit_expr(*e);
        }
    }

    fn collect_binding_pat(&mut self, pat_id: musi_ast::PatId, mutable: bool) {
        let pat = self.arena.pats.get(pat_id);
        self.collect_pat_inner(pat, mutable);
    }

    fn collect_pat_inner(&mut self, pat: &Pat, mutable: bool) {
        match &pat.kind {
            PatKind::Ident(sym) => {
                let kind = if mutable {
                    BindingKind::VarBinding
                } else {
                    BindingKind::ValBinding
                };
                self.register_binding(*sym, kind, pat.span);
            }
            PatKind::Tuple(pats)
            | PatKind::Array(pats)
            | PatKind::Cons(pats)
            | PatKind::Or(pats) => {
                for p in pats {
                    self.collect_binding_pat(*p, mutable);
                }
            }
            PatKind::As { inner, binding } => {
                self.collect_binding_pat(*inner, mutable);
                let kind = if mutable {
                    BindingKind::VarBinding
                } else {
                    BindingKind::ValBinding
                };
                self.register_binding(*binding, kind, pat.span);
            }
            _ => {}
        }
    }
}

pub fn find_binding_at(
    bindings: &HashMap<Symbol, BindingInfo>,
    offset: u32,
) -> Option<(Symbol, &BindingInfo)> {
    for (sym, info) in bindings {
        if span_contains(info.def_span, offset) {
            return Some((*sym, info));
        }
        for usage in &info.usages {
            if span_contains(*usage, offset) {
                return Some((*sym, info));
            }
        }
    }
    None
}

const fn span_contains(span: Span, offset: u32) -> bool {
    offset >= span.lo && offset < span.hi
}
