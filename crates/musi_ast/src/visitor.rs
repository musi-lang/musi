use crate::{
    Attr, AttrArg, Cond, Expr, ExprKind, Field, FnSig, LitKind, MatchCase, Pat, PatKind, Prog,
    Stmt, StmtKind, SumCase, SumCaseItem, TemplatePart, Typ, TypKind,
};

pub trait Visitor: Sized {
    fn visit_prog(&mut self, prog: &Prog) {
        walk_prog(self, prog);
    }

    fn visit_stmt(&mut self, stmt: &Stmt) {
        walk_stmt(self, stmt);
    }

    fn visit_stmts(&mut self, stmts: &[Stmt]) {
        for s in stmts {
            self.visit_stmt(s);
        }
    }

    fn visit_expr(&mut self, expr: &Expr) {
        walk_expr(self, expr);
    }

    fn visit_expr_refs(&mut self, exprs: &[&Expr]) {
        for e in exprs {
            self.visit_expr(e);
        }
    }

    fn visit_exprs(&mut self, exprs: &[Expr]) {
        for e in exprs {
            self.visit_expr(e);
        }
    }

    fn visit_typ(&mut self, typ: &Typ) {
        walk_typ(self, typ);
    }

    fn visit_typs(&mut self, typs: &[Typ]) {
        for t in typs {
            self.visit_typ(t);
        }
    }

    fn visit_pat(&mut self, pat: &Pat) {
        walk_pat(self, pat);
    }

    fn visit_typ_refs(&mut self, typs: &[&Typ]) {
        for t in typs {
            self.visit_typ(t);
        }
    }

    fn visit_pats(&mut self, pats: &[Pat]) {
        for p in pats {
            self.visit_pat(p);
        }
    }

    fn visit_lit(&mut self, lit: &LitKind) {
        walk_lit(self, lit);
    }

    fn visit_cond(&mut self, cond: &Cond) {
        walk_cond(self, cond);
    }

    fn visit_field(&mut self, field: &Field) {
        walk_field(self, field);
    }

    fn visit_fn_sig(&mut self, sig: &FnSig) {
        walk_fn_sig(self, sig);
    }

    fn visit_match_case(&mut self, case: &MatchCase) {
        walk_match_case(self, case);
    }

    fn visit_sum_case(&mut self, case: &SumCase) {
        walk_sum_case(self, case);
    }

    fn visit_attr(&mut self, attr: &Attr) {
        walk_attr(self, attr);
    }

    fn visit_attrs(&mut self, attrs: &[Attr]) {
        for a in attrs {
            self.visit_attr(a);
        }
    }

    fn visit_fields(&mut self, fields: &[Field]) {
        for f in fields {
            self.visit_field(f);
        }
    }
}

pub fn walk_prog<V: Visitor>(v: &mut V, prog: &Prog) {
    v.visit_stmts(&prog.stmts);
}

pub fn walk_stmt<V: Visitor>(v: &mut V, stmt: &Stmt) {
    match &stmt.kind {
        StmtKind::Expr(expr) => v.visit_expr(expr),
    }
}

#[allow(clippy::too_many_lines)]
pub fn walk_expr<V: Visitor>(v: &mut V, expr: &Expr) {
    match &expr.kind {
        ExprKind::Lit(lit) => v.visit_lit(lit),
        ExprKind::Ident(_) | ExprKind::Cycle | ExprKind::Import(_) => {}
        ExprKind::Tuple(exprs) | ExprKind::Array(exprs) => v.visit_exprs(exprs),
        ExprKind::Record { fields, .. } => v.visit_fields(fields),
        ExprKind::Block { stmts, expr } => {
            v.visit_stmts(stmts);
            if let Some(e) = expr {
                v.visit_expr(e);
            }
        }
        ExprKind::If {
            cond,
            then_br,
            else_br,
        } => {
            v.visit_cond(cond);
            v.visit_expr(then_br);
            if let Some(e) = else_br {
                v.visit_expr(e);
            }
        }
        ExprKind::While { cond, body } => {
            v.visit_cond(cond);
            v.visit_expr(body);
        }
        ExprKind::For { pat, iter, body } => {
            v.visit_pat(pat);
            v.visit_expr_refs(&[iter, body]);
        }
        ExprKind::Match { scrutinee, cases } => {
            v.visit_expr(scrutinee);
            for c in cases {
                v.visit_match_case(c);
            }
        }
        ExprKind::Try {
            expr, else_body, ..
        } => {
            v.visit_expr(expr);
            if let Some(e) = else_body {
                v.visit_expr(e);
            }
        }
        ExprKind::Return(e) | ExprKind::Break(e) => {
            if let Some(e) = e {
                v.visit_expr(e);
            }
        }
        ExprKind::Defer(e) | ExprKind::Unsafe(e) | ExprKind::Deref(e) => {
            v.visit_expr(e);
        }
        ExprKind::Extern { fns, .. } => {
            for sig in fns {
                v.visit_fn_sig(sig);
            }
        }
        ExprKind::RecordDef { attrs, fields, .. } => {
            v.visit_attrs(attrs);
            v.visit_fields(fields);
        }
        ExprKind::SumDef { attrs, cases, .. } => {
            v.visit_attrs(attrs);
            for c in cases {
                v.visit_sum_case(c);
            }
        }
        ExprKind::Alias { attrs, ty, .. } => {
            v.visit_attrs(attrs);
            v.visit_typ(ty);
        }
        ExprKind::Fn {
            attrs, sig, body, ..
        } => {
            v.visit_attrs(attrs);
            v.visit_fn_sig(sig);
            v.visit_expr(body);
        }
        ExprKind::Bind { pat, ty, init, .. } => {
            v.visit_pat(pat);
            if let Some(t) = ty {
                v.visit_typ(t);
            }
            v.visit_expr(init);
        }
        ExprKind::Call { callee, args } => {
            v.visit_expr(callee);
            v.visit_exprs(args);
        }
        ExprKind::Index { base, index } => v.visit_expr_refs(&[base, index]),
        ExprKind::Field { base, .. } => {
            v.visit_expr(base);
        }
        ExprKind::Unary { operand, .. } => {
            v.visit_expr(operand);
        }
        ExprKind::Binary { lhs, rhs, .. } => v.visit_expr_refs(&[lhs, rhs]),
        ExprKind::Range { start, end, .. } => {
            v.visit_expr(start);
            if let Some(e) = end {
                v.visit_expr(e);
            }
        }
        ExprKind::Assign { target, value } => v.visit_expr_refs(&[target, value]),
    }
}

pub fn walk_typ<V: Visitor>(v: &mut V, typ: &Typ) {
    match &typ.kind {
        TypKind::Ident(_) => {}
        TypKind::App { args, .. } => v.visit_typs(args),
        TypKind::Optional(t) | TypKind::Ptr(t) => v.visit_typ(t),
        TypKind::Array { elem, .. } => v.visit_typ(elem),
        TypKind::Fn { param, ret } => v.visit_typ_refs(&[param, ret]),
        TypKind::Tuple(ts) => v.visit_typs(ts),
    }
}

pub fn walk_pat<V: Visitor>(v: &mut V, pat: &Pat) {
    match &pat.kind {
        PatKind::Ident(_) | PatKind::Wild | PatKind::Record { .. } => {}
        PatKind::Lit(lit) => v.visit_lit(lit),
        PatKind::Tuple(ps) | PatKind::Array(ps) | PatKind::Cons(ps) | PatKind::Or(ps) => {
            v.visit_pats(ps);
        }
        PatKind::Variant { ty_args, args, .. } => {
            v.visit_typs(ty_args);
            v.visit_pats(args);
        }
    }
}

pub fn walk_lit<V: Visitor>(v: &mut V, lit: &LitKind) {
    if let LitKind::Template(parts) = lit {
        for part in parts {
            if let TemplatePart::Expr(e) = part {
                v.visit_expr(e);
            }
        }
    }
}

pub fn walk_cond<V: Visitor>(v: &mut V, cond: &Cond) {
    match cond {
        Cond::Expr(e) => v.visit_expr(e),
        Cond::Case { pat, init, extra } => {
            v.visit_pat(pat);
            v.visit_expr(init);
            v.visit_exprs(extra);
        }
    }
}

pub fn walk_field<V: Visitor>(v: &mut V, field: &Field) {
    if let Some(t) = &field.ty {
        v.visit_typ(t);
    }
    if let Some(e) = &field.init {
        v.visit_expr(e);
    }
}

pub fn walk_fn_sig<V: Visitor>(v: &mut V, sig: &FnSig) {
    for p in &sig.params {
        v.visit_field(p);
    }
    if let Some(t) = &sig.ret {
        v.visit_typ(t);
    }
}

pub fn walk_match_case<V: Visitor>(v: &mut V, case: &MatchCase) {
    v.visit_pat(&case.pat);
    if let Some(g) = &case.guard {
        v.visit_expr(g);
    }
    v.visit_expr(&case.body);
}

pub fn walk_sum_case<V: Visitor>(v: &mut V, case: &SumCase) {
    for t in &case.ty_args {
        v.visit_typ(t);
    }
    for item in &case.fields {
        match item {
            SumCaseItem::Type(t) => v.visit_typ(t),
            SumCaseItem::Field(f) => v.visit_field(f),
        }
    }
}

pub fn walk_attr<V: Visitor>(v: &mut V, attr: &Attr) {
    for arg in &attr.args {
        walk_attr_arg(v, arg);
    }
}

pub fn walk_attr_arg<V: Visitor>(v: &mut V, arg: &AttrArg) {
    if let Some(e) = &arg.value {
        v.visit_expr(e);
    }
    if let Some(lit) = &arg.lit {
        v.visit_lit(lit);
    }
}
