//! AST visitor trait and walk functions.

#[cfg(test)]
mod tests;

use std::ops::ControlFlow;

use crate::attr::{Attr, AttrValue};
use crate::decl::{ClassMember, EffectOp, ForeignDecl};
use crate::expr::{
    Arg, ArrayElem, EffectItem, Expr, HandlerOp, InstanceBody, LetFields, MatchArm, Param, PwArm,
    PwGuard, RecDefField, RecField,
};
use crate::lit::{FStrPart, Lit};
use crate::pat::Pat;
use crate::ty_param::Constraint;
use crate::{AstArenas, ExprIdx, PatIdx, Stmt};

/// Visitor trait for traversing the AST.
///
/// Default implementations call the corresponding `walk_*` function, which
/// recurses into child nodes in source order.
pub trait AstVisitor {
    /// The type returned on early exit.
    type Break;

    fn visit_expr(&mut self, idx: ExprIdx, ctx: &AstArenas) -> ControlFlow<Self::Break> {
        walk_expr(self, idx, ctx)
    }

    fn visit_expr_list(&mut self, exprs: &[ExprIdx], ctx: &AstArenas) -> ControlFlow<Self::Break> {
        for expr in exprs {
            self.visit_expr(*expr, ctx)?;
        }
        ControlFlow::Continue(())
    }

    fn visit_pat(&mut self, idx: PatIdx, ctx: &AstArenas) -> ControlFlow<Self::Break> {
        walk_pat(self, idx, ctx)
    }

    fn visit_pat_list(&mut self, pats: &[PatIdx], ctx: &AstArenas) -> ControlFlow<Self::Break> {
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
    idx: ExprIdx,
    ctx: &AstArenas,
) -> ControlFlow<V::Break> {
    match &ctx.exprs[idx] {
        Expr::Lit { lit, .. } => walk_expr_lit(v, lit, ctx),
        Expr::Name { .. } | Expr::Error { .. } | Expr::Import { .. } | Expr::Export { .. } => {
            ControlFlow::Continue(())
        }

        Expr::Paren { inner: e, .. } | Expr::Field { object: e, .. } => v.visit_expr(*e, ctx),
        Expr::Tuple { elems, .. } => {
            for &e in elems {
                v.visit_expr(e, ctx)?;
            }
            ControlFlow::Continue(())
        }
        Expr::Block { stmts, tail, .. } => walk_expr_block(v, stmts, *tail, ctx),

        Expr::Let { fields, .. } => walk_expr_let(v, fields, ctx),
        Expr::Binding { fields, .. } => walk_let_fields(v, fields, ctx),

        Expr::Fn {
            params,
            ret_ty,
            body,
            ..
        } => walk_expr_fn(v, params, *ret_ty, *body, ctx),
        Expr::Call { callee, args, .. } => walk_expr_call(v, *callee, args, ctx),

        Expr::Index { object, index, .. } => v.visit_expr_list(&[*object, *index], ctx),
        Expr::Update { base, fields, .. } => {
            v.visit_expr(*base, ctx)?;
            walk_rec_fields(v, fields, ctx)
        }

        Expr::Record { fields, .. } => walk_rec_fields(v, fields, ctx),
        Expr::Array { elems, .. } => walk_expr_array(v, elems, ctx),
        Expr::Variant { args, .. } => {
            for &a in args {
                v.visit_expr(a, ctx)?;
            }
            ControlFlow::Continue(())
        }
        Expr::Choice { body, .. } => v.visit_expr(*body, ctx),
        Expr::RecordDef { fields, .. } => walk_rec_def_fields(v, fields, ctx),

        Expr::BinOp { left, right, .. } => v.visit_expr_list(&[*left, *right], ctx),
        Expr::UnaryOp { operand, .. } | Expr::Need { operand, .. } => v.visit_expr(*operand, ctx),

        Expr::Piecewise { arms, .. } => walk_expr_piecewise(v, arms, ctx),
        Expr::Match {
            scrutinee, arms, ..
        } => walk_expr_match(v, *scrutinee, arms, ctx),

        Expr::Return { value, .. } | Expr::Resume { value, .. } => {
            if let Some(val) = *value {
                v.visit_expr(val, ctx)?;
            }
            ControlFlow::Continue(())
        }

        Expr::Annotated { attrs, inner, .. } => {
            walk_attrs_values(v, attrs, ctx)?;
            v.visit_expr(*inner, ctx)
        }

        Expr::Class {
            constraints,
            members,
            ..
        } => {
            walk_constraints(v, constraints, ctx)?;
            walk_class_members(v, members, ctx)
        }
        Expr::Instance {
            target,
            constraints,
            body,
            ..
        } => {
            v.visit_expr(*target, ctx)?;
            walk_constraints(v, constraints, ctx)?;
            match body {
                InstanceBody::Manual { members } => walk_class_members(v, members, ctx),
                InstanceBody::Via { delegate, .. } => v.visit_expr(*delegate, ctx),
            }
        }
        Expr::Effect { ops, .. } => walk_effect_ops(v, ops, ctx),
        Expr::Foreign { decls, .. } => walk_foreign_decls(v, decls, ctx),

        Expr::TypeCheck { operand, ty, .. } => {
            v.visit_expr(*operand, ctx)?;
            v.visit_expr(*ty, ctx)
        }

        Expr::Handle {
            effect_ty,
            ops,
            body,
            ..
        } => walk_expr_handle(v, *effect_ty, ops, *body, ctx),

        Expr::TypeApp { .. }
        | Expr::FnType { .. }
        | Expr::OptionType { .. }
        | Expr::ProductType { .. }
        | Expr::SumType { .. }
        | Expr::ArrayType { .. }
        | Expr::PiType { .. } => walk_type_expr(v, idx, ctx),
    }
}

/// Walk a type expression node, visiting all children in source order.
fn walk_type_expr<V: AstVisitor + ?Sized>(
    v: &mut V,
    idx: ExprIdx,
    ctx: &AstArenas,
) -> ControlFlow<V::Break> {
    match &ctx.exprs[idx] {
        Expr::TypeApp { callee, args, .. } => {
            v.visit_expr(*callee, ctx)?;
            v.visit_expr_list(args, ctx)
        }
        Expr::FnType {
            params,
            ret,
            effects,
            ..
        } => {
            v.visit_expr_list(params, ctx)?;
            v.visit_expr(*ret, ctx)?;
            if let Some(eff) = effects {
                for item in &eff.effects {
                    if let EffectItem::Named { arg: Some(a), .. } = item {
                        v.visit_expr(*a, ctx)?;
                    }
                }
            }
            ControlFlow::Continue(())
        }
        Expr::OptionType { inner, .. } => v.visit_expr(*inner, ctx),
        Expr::ProductType { fields, .. }
        | Expr::SumType {
            variants: fields, ..
        } => v.visit_expr_list(fields, ctx),
        Expr::ArrayType { elem, .. } => v.visit_expr(*elem, ctx),
        Expr::PiType { param_ty, body, .. } => {
            v.visit_expr(*param_ty, ctx)?;
            v.visit_expr(*body, ctx)
        }
        _ => ControlFlow::Continue(()),
    }
}

/// Walk a pattern node, visiting all children in source order.
pub fn walk_pat<V: AstVisitor + ?Sized>(
    v: &mut V,
    idx: PatIdx,
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
    walk_constraints(v, &fields.constraints, ctx)?;
    if let Some(ty) = fields.ty {
        v.visit_expr(ty, ctx)?;
    }
    if let Some(val) = fields.value {
        v.visit_expr(val, ctx)?;
    }
    ControlFlow::Continue(())
}

fn walk_params<V: AstVisitor + ?Sized>(
    v: &mut V,
    params: &[Param],
    ctx: &AstArenas,
) -> ControlFlow<V::Break> {
    for param in params {
        if let Some(ty) = param.ty {
            v.visit_expr(ty, ctx)?;
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
        v.visit_expr(c.bound, ctx)?;
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
                    v.visit_expr(ty, ctx)?;
                }
                if let Some(def) = *default {
                    v.visit_expr(def, ctx)?;
                }
            }
            ClassMember::Law { params, body, .. } => {
                walk_params(v, params, ctx)?;
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
        v.visit_expr(op.ty, ctx)?;
    }
    ControlFlow::Continue(())
}

fn walk_expr_block<V: AstVisitor + ?Sized>(
    v: &mut V,
    stmts: &[ExprIdx],
    tail: Option<ExprIdx>,
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
    callee: ExprIdx,
    args: &[Arg],
    ctx: &AstArenas,
) -> ControlFlow<V::Break> {
    v.visit_expr(callee, ctx)?;
    for arg in args {
        match *arg {
            Arg::Pos { expr, .. } | Arg::Spread { expr, .. } => v.visit_expr(expr, ctx)?,
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
    scrutinee: ExprIdx,
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

fn walk_foreign_decls<V: AstVisitor + ?Sized>(
    v: &mut V,
    decls: &[ForeignDecl],
    ctx: &AstArenas,
) -> ControlFlow<V::Break> {
    for decl in decls {
        if let ForeignDecl::Fn { ty, .. } = decl {
            v.visit_expr(*ty, ctx)?;
        }
    }
    ControlFlow::Continue(())
}

fn walk_rec_def_fields<V: AstVisitor + ?Sized>(
    v: &mut V,
    fields: &[RecDefField],
    ctx: &AstArenas,
) -> ControlFlow<V::Break> {
    for field in fields {
        v.visit_expr(field.ty, ctx)?;
        if let Some(def) = field.default {
            v.visit_expr(def, ctx)?;
        }
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

fn walk_expr_let<V: AstVisitor + ?Sized>(
    v: &mut V,
    fields: &LetFields,
    ctx: &AstArenas,
) -> ControlFlow<V::Break> {
    walk_let_fields(v, fields, ctx)
}

fn walk_expr_fn<V: AstVisitor + ?Sized>(
    v: &mut V,
    params: &[Param],
    ret_ty: Option<ExprIdx>,
    body: ExprIdx,
    ctx: &AstArenas,
) -> ControlFlow<V::Break> {
    walk_params(v, params, ctx)?;
    if let Some(ty) = ret_ty {
        v.visit_expr(ty, ctx)?;
    }
    v.visit_expr(body, ctx)
}

fn walk_expr_piecewise<V: AstVisitor + ?Sized>(
    v: &mut V,
    arms: &[PwArm],
    ctx: &AstArenas,
) -> ControlFlow<V::Break> {
    for arm in arms {
        if let PwGuard::When { expr, .. } = arm.guard {
            v.visit_expr(expr, ctx)?;
        }
        v.visit_expr(arm.result, ctx)?;
    }
    ControlFlow::Continue(())
}

fn walk_expr_handle<V: AstVisitor + ?Sized>(
    v: &mut V,
    effect_ty: ExprIdx,
    ops: &[HandlerOp],
    body: ExprIdx,
    ctx: &AstArenas,
) -> ControlFlow<V::Break> {
    v.visit_expr(effect_ty, ctx)?;
    for op in ops {
        walk_params(v, &op.params, ctx)?;
        v.visit_expr(op.body, ctx)?;
    }
    v.visit_expr(body, ctx)
}
