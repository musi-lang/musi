use musi_ast::{LitKind, TyExprId};
use musi_basic::span::Span;

use crate::ty_repr::{TyRepr, TyReprKind};

use super::BindCtx;
use super::ty::resolve_ty_expr;

pub fn try_coerce_lit(lit: &LitKind, target: &TyRepr) -> Option<TyRepr> {
    match (lit, &target.kind) {
        (LitKind::Int(_), TyReprKind::Nat(_) | TyReprKind::Int(_))
        | (LitKind::Real(_), TyReprKind::Float(_)) => Some(target.clone()),
        (LitKind::Int(v), TyReprKind::Bool) if *v == 0 || *v == 1 => Some(target.clone()),
        _ => None,
    }
}

pub fn ensure_bool(ctx: &mut BindCtx<'_>, ty: &TyRepr, span: Span) {
    ctx.unify_or_err(ty, &TyRepr::bool(), span);
}

pub fn fresh_var_or(ctx: &mut BindCtx<'_>, ann: Option<TyExprId>) -> TyRepr {
    match ann {
        Some(id) => resolve_ty_expr(ctx, id),
        None => ctx.unifier.fresh_var(),
    }
}
