use crate::{
    AstArena, Attr, AttrArg, ChoiceCase, ChoiceCaseItem, Cond, CondId, CondKind, Expr, ExprId,
    ExprIds, ExprKind, Field, FnSig, Ident, LitKind, MatchCase, Pat, PatId, PatIds, PatKind, Prog,
    Stmt, StmtId, StmtIds, StmtKind, TemplatePart, TyExpr, TyExprId, TyExprIds, TyExprKind,
};

pub trait AstVisitor: Sized {
    fn visit_prog(&mut self, arena: &AstArena, prog: &Prog) {
        walk_prog(self, arena, prog);
    }

    fn visit_stmt_id(&mut self, arena: &AstArena, id: StmtId) {
        let stmt = arena.stmts.get(id);
        walk_stmt(self, arena, stmt);
    }

    fn visit_stmt_ids(&mut self, arena: &AstArena, ids: &StmtIds) {
        for &id in ids {
            self.visit_stmt_id(arena, id);
        }
    }

    fn visit_expr_id(&mut self, arena: &AstArena, id: ExprId) {
        let expr = arena.exprs.get(id);
        walk_expr(self, arena, expr);
    }

    fn visit_expr_ids(&mut self, arena: &AstArena, ids: &ExprIds) {
        for &id in ids {
            self.visit_expr_id(arena, id);
        }
    }

    fn visit_ty_expr_id(&mut self, arena: &AstArena, id: TyExprId) {
        let ty_expr = arena.ty_exprs.get(id);
        walk_ty_expr(self, arena, ty_expr);
    }

    fn visit_ty_expr_ids(&mut self, arena: &AstArena, ids: &TyExprIds) {
        for &id in ids {
            self.visit_ty_expr_id(arena, id);
        }
    }

    fn visit_pat_id(&mut self, arena: &AstArena, id: PatId) {
        let pat = arena.pats.get(id);
        walk_pat(self, arena, pat);
    }

    fn visit_pat_ids(&mut self, arena: &AstArena, ids: &PatIds) {
        for &id in ids {
            self.visit_pat_id(arena, id);
        }
    }

    fn visit_cond_id(&mut self, arena: &AstArena, id: CondId) {
        let cond = arena.conds.get(id);
        walk_cond(self, arena, cond);
    }

    fn visit_lit(&mut self, arena: &AstArena, lit: &LitKind) {
        walk_lit(self, arena, lit);
    }

    fn visit_field(&mut self, arena: &AstArena, field: &Field) {
        walk_field(self, arena, field);
    }

    fn visit_fields(&mut self, arena: &AstArena, fields: &[Field]) {
        for f in fields {
            self.visit_field(arena, f);
        }
    }

    fn visit_fn_sig(&mut self, arena: &AstArena, sig: &FnSig) {
        walk_fn_sig(self, arena, sig);
    }

    fn visit_match_case(&mut self, arena: &AstArena, case: &MatchCase) {
        walk_match_case(self, arena, case);
    }

    fn visit_choice_case(&mut self, arena: &AstArena, case: &ChoiceCase) {
        walk_choice_case(self, arena, case);
    }

    fn visit_attr(&mut self, arena: &AstArena, attr: &Attr) {
        walk_attr(self, arena, attr);
    }

    fn visit_attrs(&mut self, arena: &AstArena, attrs: &[Attr]) {
        for a in attrs {
            self.visit_attr(arena, a);
        }
    }

    fn visit_ident(&mut self, _arena: &AstArena, _ident: Ident) {}
}

pub fn walk_prog<V: AstVisitor>(v: &mut V, arena: &AstArena, prog: &Prog) {
    v.visit_stmt_ids(arena, &prog.stmts);
}

pub fn walk_stmt<V: AstVisitor>(v: &mut V, arena: &AstArena, stmt: &Stmt) {
    match &stmt.kind {
        StmtKind::Expr(id) => v.visit_expr_id(arena, *id),
    }
}

#[allow(clippy::too_many_lines)]
pub fn walk_expr<V: AstVisitor>(v: &mut V, arena: &AstArena, expr: &Expr) {
    match &expr.kind {
        ExprKind::Lit(lit) => v.visit_lit(arena, lit),
        ExprKind::Ident(ident) => v.visit_ident(arena, *ident),
        ExprKind::Cycle | ExprKind::Import(_) => {}
        ExprKind::Tuple(ids) | ExprKind::Array(ids) => v.visit_expr_ids(arena, ids),
        ExprKind::Record { base, fields } => {
            if let Some(id) = base {
                v.visit_expr_id(arena, *id);
            }
            v.visit_fields(arena, fields);
        }
        ExprKind::Block { stmts, expr } => {
            v.visit_stmt_ids(arena, stmts);
            if let Some(id) = expr {
                v.visit_expr_id(arena, *id);
            }
        }
        ExprKind::If {
            cond,
            then_br,
            else_br,
        } => {
            v.visit_cond_id(arena, *cond);
            v.visit_expr_id(arena, *then_br);
            if let Some(id) = else_br {
                v.visit_expr_id(arena, *id);
            }
        }
        ExprKind::While { cond, body } => {
            v.visit_cond_id(arena, *cond);
            v.visit_expr_id(arena, *body);
        }
        ExprKind::For { pat, iter, body } => {
            v.visit_pat_id(arena, *pat);
            v.visit_expr_id(arena, *iter);
            v.visit_expr_id(arena, *body);
        }
        ExprKind::Match { scrutinee, cases } => {
            v.visit_expr_id(arena, *scrutinee);
            for c in cases {
                v.visit_match_case(arena, c);
            }
        }
        ExprKind::Return(e) | ExprKind::Break(e) => {
            if let Some(id) = e {
                v.visit_expr_id(arena, *id);
            }
        }
        ExprKind::Defer(id) | ExprKind::Unsafe(id) | ExprKind::Deref(id) => {
            v.visit_expr_id(arena, *id);
        }
        ExprKind::Extern { fns, .. } => {
            for sig in fns {
                v.visit_fn_sig(arena, sig);
            }
        }
        ExprKind::RecordDef {
            attrs,
            fields,
            name,
            ..
        } => {
            v.visit_attrs(arena, attrs);
            if let Some(id) = name {
                v.visit_ident(arena, *id);
            }
            v.visit_fields(arena, fields);
        }
        ExprKind::ChoiceDef {
            attrs, cases, name, ..
        } => {
            v.visit_attrs(arena, attrs);
            if let Some(id) = name {
                v.visit_ident(arena, *id);
            }
            for c in cases {
                v.visit_choice_case(arena, c);
            }
        }
        ExprKind::Alias {
            attrs, ty, name, ..
        } => {
            v.visit_attrs(arena, attrs);
            v.visit_ident(arena, *name);
            v.visit_ty_expr_id(arena, *ty);
        }
        ExprKind::Fn {
            attrs, sig, body, ..
        } => {
            v.visit_attrs(arena, attrs);
            if let Some(name) = sig.name {
                v.visit_ident(arena, name);
            }
            v.visit_fn_sig(arena, sig);
            v.visit_expr_id(arena, *body);
        }
        ExprKind::Bind { pat, ty, init, .. } => {
            v.visit_pat_id(arena, *pat);
            if let Some(id) = ty {
                v.visit_ty_expr_id(arena, *id);
            }
            v.visit_expr_id(arena, *init);
        }
        ExprKind::Call { callee, args } => {
            v.visit_expr_id(arena, *callee);
            v.visit_expr_ids(arena, args);
        }
        ExprKind::Index { base, index } => {
            v.visit_expr_id(arena, *base);
            v.visit_expr_id(arena, *index);
        }
        ExprKind::Field { base, .. } => {
            v.visit_expr_id(arena, *base);
        }
        ExprKind::Unary { operand, .. } => {
            v.visit_expr_id(arena, *operand);
        }
        ExprKind::Binary { lhs, rhs, .. } => {
            v.visit_expr_id(arena, *lhs);
            v.visit_expr_id(arena, *rhs);
        }
        ExprKind::Range { start, end, .. } => {
            v.visit_expr_id(arena, *start);
            if let Some(id) = end {
                v.visit_expr_id(arena, *id);
            }
        }
        ExprKind::Assign { target, value } => {
            v.visit_expr_id(arena, *target);
            v.visit_expr_id(arena, *value);
        }
    }
}

pub fn walk_ty_expr<V: AstVisitor>(v: &mut V, arena: &AstArena, ty_expr: &TyExpr) {
    match &ty_expr.kind {
        TyExprKind::Ident(_) => {}
        TyExprKind::App { args, .. } => v.visit_ty_expr_ids(arena, args),
        TyExprKind::Optional(id) | TyExprKind::Ptr(id) => v.visit_ty_expr_id(arena, *id),
        TyExprKind::Array { elem, .. } => v.visit_ty_expr_id(arena, *elem),
        TyExprKind::Fn { param, ret } => {
            v.visit_ty_expr_id(arena, *param);
            v.visit_ty_expr_id(arena, *ret);
        }
        TyExprKind::Tuple(ids) => v.visit_ty_expr_ids(arena, ids),
    }
}

pub fn walk_pat<V: AstVisitor>(v: &mut V, arena: &AstArena, pat: &Pat) {
    match &pat.kind {
        PatKind::Ident(ident) => v.visit_ident(arena, *ident),
        PatKind::Wild => {}
        PatKind::Record { base, .. } => {
            if let Some(id) = base {
                v.visit_expr_id(arena, *id);
            }
        }
        PatKind::Lit(lit) => v.visit_lit(arena, lit),
        PatKind::Tuple(ids) | PatKind::Array(ids) | PatKind::Cons(ids) | PatKind::Or(ids) => {
            v.visit_pat_ids(arena, ids);
        }
        PatKind::Choice {
            name,
            ty_args,
            args,
            ..
        } => {
            v.visit_ident(arena, *name);
            v.visit_ty_expr_ids(arena, ty_args);
            v.visit_pat_ids(arena, args);
        }
    }
}

pub fn walk_cond<V: AstVisitor>(v: &mut V, arena: &AstArena, cond: &Cond) {
    match &cond.kind {
        CondKind::Expr(id) => v.visit_expr_id(arena, *id),
        CondKind::Case { pat, init, extra } => {
            v.visit_pat_id(arena, *pat);
            v.visit_expr_id(arena, *init);
            v.visit_expr_ids(arena, extra);
        }
    }
}

pub fn walk_lit<V: AstVisitor>(v: &mut V, arena: &AstArena, lit: &LitKind) {
    if let LitKind::Template(parts) = lit {
        for part in parts {
            if let TemplatePart::Expr(id) = part {
                v.visit_expr_id(arena, *id);
            }
        }
    }
}

pub fn walk_field<V: AstVisitor>(v: &mut V, arena: &AstArena, field: &Field) {
    v.visit_ident(arena, field.name);
    if let Some(id) = field.ty {
        v.visit_ty_expr_id(arena, id);
    }
    if let Some(id) = field.init {
        v.visit_expr_id(arena, id);
    }
}

pub fn walk_fn_sig<V: AstVisitor>(v: &mut V, arena: &AstArena, sig: &FnSig) {
    for p in &sig.params {
        v.visit_field(arena, p);
    }
    if let Some(id) = sig.ret {
        v.visit_ty_expr_id(arena, id);
    }
}

pub fn walk_match_case<V: AstVisitor>(v: &mut V, arena: &AstArena, case: &MatchCase) {
    v.visit_pat_id(arena, case.pat);
    if let Some(id) = case.guard {
        v.visit_expr_id(arena, id);
    }
    v.visit_expr_id(arena, case.body);
}

pub fn walk_choice_case<V: AstVisitor>(v: &mut V, arena: &AstArena, case: &ChoiceCase) {
    v.visit_ident(arena, case.name);
    v.visit_ty_expr_ids(arena, &case.ty_args);
    for item in &case.fields {
        match item {
            ChoiceCaseItem::Type(id) => v.visit_ty_expr_id(arena, *id),
            ChoiceCaseItem::Field(f) => v.visit_field(arena, f),
        }
    }
}

pub fn walk_attr<V: AstVisitor>(v: &mut V, arena: &AstArena, attr: &Attr) {
    for arg in &attr.args {
        walk_attr_arg(v, arena, arg);
    }
}

pub fn walk_attr_arg<V: AstVisitor>(v: &mut V, arena: &AstArena, arg: &AttrArg) {
    if let Some(id) = arg.value {
        v.visit_expr_id(arena, id);
    }
    if let Some(lit) = &arg.lit {
        v.visit_lit(arena, lit);
    }
}
