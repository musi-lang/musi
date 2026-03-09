//! AST visitor trait and walk functions.

#[cfg(test)]
mod tests;

use std::ops::ControlFlow;

use music_shared::Idx;

use crate::attr::{Attr, AttrValue};
use crate::decl::{ClassMember, EffectOp};
use crate::expr::{Arg, ArrayElem, Expr, LetFields, MatchArm, Param, PwGuard, RecField};
use crate::lit::{FStrPart, Lit};
use crate::pat::Pat;
use crate::ty::{Constraint, EffectItem, Ty};
use crate::{AstArenas, Stmt};

/// Visitor trait for traversing the AST.
///
/// Default implementations call the corresponding `walk_*` function, which
/// recurses into child nodes in source order.
pub trait AstVisitor {
    /// The type returned on early exit.
    type Break;

    fn visit_expr(&mut self, idx: Idx<Expr>, ctx: &AstArenas) -> ControlFlow<Self::Break> {
        walk_expr(self, idx, ctx)
    }

    fn visit_expr_list(
        &mut self,
        exprs: &[Idx<Expr>],
        ctx: &AstArenas,
    ) -> ControlFlow<Self::Break> {
        for expr in exprs {
            self.visit_expr(*expr, ctx)?;
        }
        ControlFlow::Continue(())
    }

    fn visit_ty(&mut self, idx: Idx<Ty>, ctx: &AstArenas) -> ControlFlow<Self::Break> {
        walk_ty(self, idx, ctx)
    }

    fn visit_pat(&mut self, idx: Idx<Pat>, ctx: &AstArenas) -> ControlFlow<Self::Break> {
        walk_pat(self, idx, ctx)
    }

    fn visit_pat_list(&mut self, pats: &[Idx<Pat>], ctx: &AstArenas) -> ControlFlow<Self::Break> {
        for pat in pats {
            self.visit_pat(*pat, ctx)?;
        }
        ControlFlow::Continue(())
    }

    fn visit_stmt(&mut self, stmt: &Stmt, ctx: &AstArenas) -> ControlFlow<Self::Break> {
        walk_stmt(self, stmt, ctx)
    }
}

/// Walk an expression, visiting all children in source order.
pub fn walk_expr<V: AstVisitor + ?Sized>(
    v: &mut V,
    idx: Idx<Expr>,
    ctx: &AstArenas,
) -> ControlFlow<V::Break> {
    match &ctx.exprs[idx] {
        // -- literals & names ------------------------------------------------
        Expr::Lit { lit, .. } => walk_expr_lit(v, lit, ctx),
        Expr::Name { .. } | Expr::Error { .. } | Expr::Import { .. } | Expr::Export { .. } => {
            ControlFlow::Continue(())
        }

        // -- grouping --------------------------------------------------------
        Expr::Paren { inner: e, .. } | Expr::Field { object: e, .. } => v.visit_expr(*e, ctx),
        Expr::Tuple { elems, .. } => {
            for &e in elems {
                v.visit_expr(e, ctx)?;
            }
            ControlFlow::Continue(())
        }
        Expr::Block { stmts, tail, .. } => walk_expr_block(v, stmts, *tail, ctx),

        // -- bindings --------------------------------------------------------
        Expr::Let { fields, body, .. } => {
            walk_let_fields(v, fields, ctx)?;
            if let Some(b) = *body {
                v.visit_expr(b, ctx)?;
            }
            ControlFlow::Continue(())
        }
        Expr::Binding { fields, .. } => walk_let_fields(v, fields, ctx),

        // -- functions -------------------------------------------------------
        Expr::Fn {
            params,
            ret_ty,
            body,
            ..
        } => {
            walk_params(v, params, ctx)?;
            if let Some(ty) = *ret_ty {
                v.visit_ty(ty, ctx)?;
            }
            v.visit_expr(*body, ctx)
        }
        Expr::Call { callee, args, .. } => walk_expr_call(v, *callee, args, ctx),

        // -- access & update -------------------------------------------------
        Expr::Index { object, index, .. } => v.visit_expr_list(&[*object, *index], ctx),
        Expr::Update { base, fields, .. } => {
            v.visit_expr(*base, ctx)?;
            walk_rec_fields(v, fields, ctx)
        }

        // -- constructors ----------------------------------------------------
        Expr::Record { fields, .. } => walk_rec_fields(v, fields, ctx),
        Expr::Array { elems, .. } => walk_expr_array(v, elems, ctx),
        Expr::Variant { args, .. } => {
            for &a in args {
                v.visit_expr(a, ctx)?;
            }
            ControlFlow::Continue(())
        }

        // -- operators -------------------------------------------------------
        Expr::BinOp { left, right, .. } => v.visit_expr_list(&[*left, *right], ctx),
        Expr::UnaryOp { operand, .. } => v.visit_expr(*operand, ctx),

        // -- conditionals ----------------------------------------------------
        Expr::Piecewise { arms, .. } => {
            for arm in arms {
                if let PwGuard::When { expr, .. } = arm.guard {
                    v.visit_expr(expr, ctx)?;
                }
                v.visit_expr(arm.result, ctx)?;
            }
            ControlFlow::Continue(())
        }
        Expr::Match {
            scrutinee, arms, ..
        } => walk_expr_match(v, *scrutinee, arms, ctx),

        // -- control flow ----------------------------------------------------
        Expr::Return { value, .. } => {
            if let Some(val) = *value {
                v.visit_expr(val, ctx)?;
            }
            ControlFlow::Continue(())
        }

        // -- quantification --------------------------------------------------
        Expr::Quantified {
            constraints, body, ..
        } => {
            walk_constraints(v, constraints, ctx)?;
            v.visit_expr(*body, ctx)
        }

        // -- module ----------------------------------------------------------
        Expr::Annotated { attrs, inner, .. } => {
            walk_attrs_values(v, attrs, ctx)?;
            v.visit_expr(*inner, ctx)
        }

        // -- declarations ----------------------------------------------------
        Expr::Class {
            constraints,
            members,
            ..
        }
        | Expr::Given {
            constraints,
            members,
            ..
        } => {
            walk_constraints(v, constraints, ctx)?;
            walk_class_members(v, members, ctx)
        }

        Expr::Effect { ops, .. } => walk_effect_ops(v, ops, ctx),
    }
}

/// Walk a type node, visiting all children in source order.
pub fn walk_ty<V: AstVisitor + ?Sized>(
    v: &mut V,
    idx: Idx<Ty>,
    ctx: &AstArenas,
) -> ControlFlow<V::Break> {
    match &ctx.tys[idx] {
        Ty::Var { .. } | Ty::Error { .. } => ControlFlow::Continue(()),

        Ty::Named { args, .. } => {
            for &a in args {
                v.visit_ty(a, ctx)?;
            }
            ControlFlow::Continue(())
        }
        Ty::Option { inner, .. } | Ty::Ref { inner, .. } => v.visit_ty(*inner, ctx),

        Ty::Fn {
            params,
            ret,
            effects,
            ..
        } => {
            for &p in params {
                v.visit_ty(p, ctx)?;
            }
            v.visit_ty(*ret, ctx)?;
            if let Some(eff) = effects {
                for item in &eff.effects {
                    if let EffectItem::Named { arg: Some(a), .. } = item {
                        v.visit_ty(*a, ctx)?;
                    }
                }
            }
            ControlFlow::Continue(())
        }
        Ty::Product { fields, .. }
        | Ty::Sum {
            variants: fields, ..
        } => {
            for &f in fields {
                v.visit_ty(f, ctx)?;
            }
            ControlFlow::Continue(())
        }
        Ty::Record { fields, .. } => {
            for field in fields {
                v.visit_ty(field.ty, ctx)?;
                if let Some(def) = field.default {
                    v.visit_expr(def, ctx)?;
                }
            }
            ControlFlow::Continue(())
        }
        Ty::Refine { base, pred, .. } => {
            v.visit_ty(*base, ctx)?;
            v.visit_expr(*pred, ctx)
        }
        Ty::Array { elem, .. } => v.visit_ty(*elem, ctx),

        Ty::Quantified {
            constraints, body, ..
        } => {
            walk_constraints(v, constraints, ctx)?;
            v.visit_ty(*body, ctx)
        }
    }
}

/// Walk a pattern node, visiting all children in source order.
pub fn walk_pat<V: AstVisitor + ?Sized>(
    v: &mut V,
    idx: Idx<Pat>,
    ctx: &AstArenas,
) -> ControlFlow<V::Break> {
    match &ctx.pats[idx] {
        Pat::Wild { .. } | Pat::Lit { .. } | Pat::Error { .. } => ControlFlow::Continue(()),

        Pat::Bind { inner, .. } => {
            if let Some(i) = *inner {
                v.visit_pat(i, ctx)?;
            }
            ControlFlow::Continue(())
        }
        Pat::Variant { args, .. } => {
            for &a in args {
                v.visit_pat(a, ctx)?;
            }
            ControlFlow::Continue(())
        }
        Pat::Record { fields, .. } => {
            for field in fields {
                if let Some(p) = field.pat {
                    v.visit_pat(p, ctx)?;
                }
            }
            ControlFlow::Continue(())
        }
        Pat::Tuple { elems, .. } | Pat::Array { elems, .. } => {
            for &e in elems {
                v.visit_pat(e, ctx)?;
            }
            ControlFlow::Continue(())
        }
        Pat::Or { left, right, .. } => v.visit_pat_list(&[*left, *right], ctx),
    }
}

/// Walk a statement, visiting its expression.
pub fn walk_stmt<V: AstVisitor + ?Sized>(
    v: &mut V,
    stmt: &Stmt,
    ctx: &AstArenas,
) -> ControlFlow<V::Break> {
    v.visit_expr(stmt.expr, ctx)
}

fn walk_let_fields<V: AstVisitor + ?Sized>(
    v: &mut V,
    fields: &LetFields,
    ctx: &AstArenas,
) -> ControlFlow<V::Break> {
    v.visit_pat(fields.pat, ctx)?;
    if let Some(ty) = fields.ty {
        v.visit_ty(ty, ctx)?;
    }
    v.visit_expr(fields.value, ctx)
}

fn walk_params<V: AstVisitor + ?Sized>(
    v: &mut V,
    params: &[Param],
    ctx: &AstArenas,
) -> ControlFlow<V::Break> {
    for param in params {
        if let Some(ty) = param.ty {
            v.visit_ty(ty, ctx)?;
        }
        if let Some(def) = param.default {
            v.visit_expr(def, ctx)?;
        }
    }
    ControlFlow::Continue(())
}

fn walk_rec_fields<V: AstVisitor + ?Sized>(
    v: &mut V,
    fields: &[RecField],
    ctx: &AstArenas,
) -> ControlFlow<V::Break> {
    for field in fields {
        match field {
            RecField::Named { value, .. } => {
                if let Some(val) = *value {
                    v.visit_expr(val, ctx)?;
                }
            }
            RecField::Spread { expr, .. } => {
                v.visit_expr(*expr, ctx)?;
            }
        }
    }
    ControlFlow::Continue(())
}

fn walk_constraints<V: AstVisitor + ?Sized>(
    v: &mut V,
    constraints: &[Constraint],
    ctx: &AstArenas,
) -> ControlFlow<V::Break> {
    for c in constraints {
        for &a in &c.bound.args {
            v.visit_ty(a, ctx)?;
        }
    }
    ControlFlow::Continue(())
}

fn walk_class_members<V: AstVisitor + ?Sized>(
    v: &mut V,
    members: &[ClassMember],
    ctx: &AstArenas,
) -> ControlFlow<V::Break> {
    for member in members {
        match member {
            ClassMember::Fn { sig, default, .. } => {
                walk_params(v, &sig.params, ctx)?;
                if let Some(ty) = sig.ret {
                    v.visit_ty(ty, ctx)?;
                }
                if let Some(def) = *default {
                    v.visit_expr(def, ctx)?;
                }
            }
            ClassMember::Law { body, .. } => {
                v.visit_expr(*body, ctx)?;
            }
        }
    }
    ControlFlow::Continue(())
}

fn walk_expr_lit<V: AstVisitor + ?Sized>(
    v: &mut V,
    lit: &Lit,
    ctx: &AstArenas,
) -> ControlFlow<V::Break> {
    if let Lit::FStr { parts, .. } = lit {
        for part in parts {
            if let FStrPart::Interpolated { expr, .. } = part {
                v.visit_expr(*expr, ctx)?;
            }
        }
    }
    ControlFlow::Continue(())
}

fn walk_effect_ops<V: AstVisitor + ?Sized>(
    v: &mut V,
    ops: &[EffectOp],
    ctx: &AstArenas,
) -> ControlFlow<V::Break> {
    for op in ops {
        v.visit_ty(op.ty, ctx)?;
    }
    ControlFlow::Continue(())
}

fn walk_expr_block<V: AstVisitor + ?Sized>(
    v: &mut V,
    stmts: &[Idx<Expr>],
    tail: Option<Idx<Expr>>,
    ctx: &AstArenas,
) -> ControlFlow<V::Break> {
    for &s in stmts {
        v.visit_expr(s, ctx)?;
    }
    if let Some(t) = tail {
        v.visit_expr(t, ctx)?;
    }
    ControlFlow::Continue(())
}

fn walk_expr_call<V: AstVisitor + ?Sized>(
    v: &mut V,
    callee: Idx<Expr>,
    args: &[Arg],
    ctx: &AstArenas,
) -> ControlFlow<V::Break> {
    v.visit_expr(callee, ctx)?;
    for arg in args {
        match *arg {
            Arg::Pos { expr, .. } => v.visit_expr(expr, ctx)?,
            Arg::Hole { .. } => {}
        }
    }
    ControlFlow::Continue(())
}

fn walk_expr_array<V: AstVisitor + ?Sized>(
    v: &mut V,
    elems: &[ArrayElem],
    ctx: &AstArenas,
) -> ControlFlow<V::Break> {
    for elem in elems {
        match *elem {
            ArrayElem::Elem { expr, .. } | ArrayElem::Spread { expr, .. } => {
                v.visit_expr(expr, ctx)?;
            }
        }
    }
    ControlFlow::Continue(())
}

fn walk_expr_match<V: AstVisitor + ?Sized>(
    v: &mut V,
    scrutinee: Idx<Expr>,
    arms: &[MatchArm],
    ctx: &AstArenas,
) -> ControlFlow<V::Break> {
    v.visit_expr(scrutinee, ctx)?;
    for arm in arms {
        walk_attrs_values(v, &arm.attrs, ctx)?;
        v.visit_pat(arm.pat, ctx)?;
        if let Some(g) = arm.guard {
            v.visit_expr(g, ctx)?;
        }
        v.visit_expr(arm.result, ctx)?;
    }
    ControlFlow::Continue(())
}

fn walk_attrs_values<V: AstVisitor + ?Sized>(
    v: &mut V,
    attrs: &[Attr],
    ctx: &AstArenas,
) -> ControlFlow<V::Break> {
    for attr in attrs {
        if let Some(AttrValue::Lit { lit, .. }) = &attr.value {
            walk_expr_lit(v, lit, ctx)?;
        }
    }
    ControlFlow::Continue(())
}
