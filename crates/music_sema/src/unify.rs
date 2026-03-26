use music_found::Span;

use crate::env::TypeEnv;
use crate::errors::{SemaError, SemaErrorKind};
use crate::types::{SemaTypeId, Ty, TyVarId};

/// Unifies two types, binding unification variables as needed.
///
/// Returns the unified type on success, or a `SemaError` on failure.
/// The `span` is attached to any errors produced.
///
/// # Errors
///
/// Returns `SemaError` with `CannotUnify`, `TypeMismatch`, or `OccursCheck`
/// when the types are incompatible or would create an infinite type.
pub fn unify(
    env: &mut TypeEnv,
    a: SemaTypeId,
    b: SemaTypeId,
    span: Span,
) -> Result<SemaTypeId, SemaError> {
    let a = env.resolve_var(a);
    let b = env.resolve_var(b);

    if a == b {
        return Ok(a);
    }

    let ty_a = env.types.get(a).clone();
    let ty_b = env.types.get(b).clone();

    match (&ty_a, &ty_b) {
        (Ty::Var(v), _) => unify_var(env, *v, b, a, span),
        (_, Ty::Var(v)) => unify_var(env, *v, a, b, span),

        // Any is consistent with anything; Unit == Unit returns a
        (Ty::Any, _) => Ok(b),
        (_, Ty::Any) | (Ty::Unit, Ty::Unit) => Ok(a),

        // Empty <: everything
        (Ty::Empty, _) => Ok(b),
        // everything <: Unknown
        (_, Ty::Unknown) => Ok(a),

        (Ty::Builtin(x), Ty::Builtin(y)) if x == y => Ok(a),
        (Ty::Builtin(_), Ty::Builtin(_)) => Err(mismatch(a, b, span)),

        (Ty::Arrow { param: p1, ret: r1 }, Ty::Arrow { param: p2, ret: r2 }) => {
            unify_arrow(env, (*p1, *r1), (*p2, *r2), span)
        }

        (Ty::Tuple(xs), Ty::Tuple(ys)) => unify_tuple(env, xs, ys, span),

        (Ty::Record { fields: f1 }, Ty::Record { fields: f2 }) => unify_record(env, f1, f2, span),

        (Ty::Union(xs), _) => {
            for &x in xs {
                let _unified = unify(env, x, b, span)?;
            }
            Ok(b)
        }

        (Ty::Mut(x), Ty::Mut(y)) => {
            let inner = unify(env, *x, *y, span)?;
            Ok(env.intern(Ty::Mut(inner)))
        }

        (Ty::Param(x), Ty::Param(y)) if x == y => Ok(a),

        (Ty::Array(x), Ty::Array(y)) => {
            let inner = unify(env, *x, *y, span)?;
            Ok(env.intern(Ty::Array(inner)))
        }

        (Ty::List(x), Ty::List(y)) => {
            let inner = unify(env, *x, *y, span)?;
            Ok(env.intern(Ty::List(inner)))
        }

        _ => Err(cannot_unify(a, b, span)),
    }
}

/// Binds a unification variable to a type after an occurs check.
/// `bind_to` is the type the variable should be bound to.
/// `return_id` is the `SemaTypeId` to return on success (the non-var side).
fn unify_var(
    env: &mut TypeEnv,
    var: TyVarId,
    bind_to: SemaTypeId,
    _var_id: SemaTypeId,
    span: Span,
) -> Result<SemaTypeId, SemaError> {
    if occurs_check(env, var, bind_to) {
        return Err(SemaError {
            kind: SemaErrorKind::OccursCheck { var },
            span,
            context: None,
        });
    }
    env.bind_var(var, bind_to);
    Ok(bind_to)
}

/// Unifies two arrow types with contravariant parameters and covariant returns.
fn unify_arrow(
    env: &mut TypeEnv,
    (p1, r1): (SemaTypeId, SemaTypeId),
    (p2, r2): (SemaTypeId, SemaTypeId),
    span: Span,
) -> Result<SemaTypeId, SemaError> {
    let param = unify(env, p2, p1, span)?; // contravariant
    let ret = unify(env, r1, r2, span)?; // covariant
    Ok(env.intern(Ty::Arrow { param, ret }))
}

/// Unifies two tuple types pairwise, requiring equal length.
fn unify_tuple(
    env: &mut TypeEnv,
    xs: &[SemaTypeId],
    ys: &[SemaTypeId],
    span: Span,
) -> Result<SemaTypeId, SemaError> {
    if xs.len() != ys.len() {
        return Err(SemaError {
            kind: SemaErrorKind::ArityMismatch {
                expected: xs.len(),
                found: ys.len(),
            },
            span,
            context: Some("tuple unification"),
        });
    }
    let mut unified = Vec::with_capacity(xs.len());
    for (&x, &y) in xs.iter().zip(ys.iter()) {
        unified.push(unify(env, x, y, span)?);
    }
    Ok(env.intern(Ty::Tuple(unified)))
}

/// Unifies two record types with width subtyping (f2's fields must exist in f1).
fn unify_record(
    env: &mut TypeEnv,
    f1: &[(music_found::Symbol, SemaTypeId)],
    f2: &[(music_found::Symbol, SemaTypeId)],
    span: Span,
) -> Result<SemaTypeId, SemaError> {
    let mut unified_fields = f1.to_vec();
    for &(name, ty_b_field) in f2 {
        if let Some(pos) = unified_fields.iter().position(|(n, _)| *n == name) {
            let unified_field = unify(env, unified_fields[pos].1, ty_b_field, span)?;
            unified_fields[pos].1 = unified_field;
        } else {
            return Err(SemaError {
                kind: SemaErrorKind::UndefinedField { field: name },
                span,
                context: Some("record unification"),
            });
        }
    }
    Ok(env.intern(Ty::Record {
        fields: unified_fields,
    }))
}

/// Returns `true` if `var` occurs anywhere in the type referenced by `ty`.
fn occurs_check(env: &TypeEnv, var: TyVarId, ty: SemaTypeId) -> bool {
    let resolved = env.resolve_var(ty);
    match env.types.get(resolved) {
        Ty::Var(v) => *v == var,
        Ty::Arrow { param, ret } => occurs_check(env, var, *param) || occurs_check(env, var, *ret),
        Ty::EffectArrow {
            param,
            ret,
            effects,
        } => {
            occurs_check(env, var, *param)
                || occurs_check(env, var, *ret)
                || effects.iter().any(|&e| occurs_check(env, var, e))
        }
        Ty::Tuple(elems) => elems.iter().any(|&e| occurs_check(env, var, e)),
        Ty::Record { fields } => fields.iter().any(|&(_, f)| occurs_check(env, var, f)),
        Ty::Choice { variants } => variants
            .iter()
            .any(|(_, payload)| payload.is_some_and(|p| occurs_check(env, var, p))),
        Ty::Array(inner) | Ty::List(inner) | Ty::Mut(inner) => occurs_check(env, var, *inner),
        Ty::Union(members) => members.iter().any(|&m| occurs_check(env, var, m)),
        Ty::App(base, args) => {
            occurs_check(env, var, *base) || args.iter().any(|&a| occurs_check(env, var, a))
        }
        Ty::Builtin(_)
        | Ty::Param(_)
        | Ty::Any
        | Ty::Unknown
        | Ty::Empty
        | Ty::Unit
        | Ty::Class(_)
        | Ty::Effect(_) => false,
    }
}

const fn mismatch(expected: SemaTypeId, found: SemaTypeId, span: Span) -> SemaError {
    SemaError {
        kind: SemaErrorKind::TypeMismatch { expected, found },
        span,
        context: None,
    }
}

const fn cannot_unify(left: SemaTypeId, right: SemaTypeId, span: Span) -> SemaError {
    SemaError {
        kind: SemaErrorKind::CannotUnify { left, right },
        span,
        context: None,
    }
}

#[cfg(test)]
#[allow(clippy::unwrap_used, clippy::panic)]
mod tests;
