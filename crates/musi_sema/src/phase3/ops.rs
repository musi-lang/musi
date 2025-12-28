use musi_ast::LitKind;

use crate::ty_repr::{TyRepr, TyReprKind};

pub fn try_coerce_lit(lit: &LitKind, target: &TyRepr) -> Option<TyRepr> {
    match (lit, &target.kind) {
        (LitKind::Int(_), TyReprKind::Nat(_) | TyReprKind::Int(_))
        | (LitKind::Real(_), TyReprKind::Float(_)) => Some(target.clone()),
        (LitKind::Int(v), TyReprKind::Bool) if *v == 0 || *v == 1 => Some(target.clone()),
        _ => None,
    }
}
