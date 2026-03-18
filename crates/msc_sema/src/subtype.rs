//! Subtyping relation for the Musi type system.

#[cfg(test)]
mod tests;

use msc_shared::Arena;

use crate::types::{Quantifier, RecordField, SumVariant, Type, TypeIdx};
use crate::unify::UnifyTable;
use crate::well_known::WellKnown;
use crate::{DefId, TyVarId};

struct SubCtx<'a> {
    types: &'a Arena<Type>,
    unify: &'a UnifyTable,
    well_known: &'a WellKnown,
}

/// Returns `true` if `sub` is a subtype of `sup` under the current unification state.
///
/// The subtyping lattice:
/// - `Never` is the bottom type (subtype of everything)
/// - `Unknown` is the top type (supertype of everything)
/// - `Any` does NOT participate in the lattice
/// - `Error` absorbs (poison) in both directions
#[must_use]
pub fn is_subtype(
    sub: TypeIdx,
    sup: TypeIdx,
    types: &Arena<Type>,
    unify: &UnifyTable,
    well_known: &WellKnown,
) -> bool {
    let ctx = SubCtx {
        types,
        unify,
        well_known,
    };
    is_subtype_inner(sub, sup, &ctx)
}

fn is_subtype_inner(sub: TypeIdx, sup: TypeIdx, ctx: &SubCtx) -> bool {
    let sub = ctx.unify.resolve(sub, ctx.types);
    let sup = ctx.unify.resolve(sup, ctx.types);

    if is_any_type(sub, ctx) || is_any_type(sup, ctx) {
        return false;
    }

    if sub == sup {
        return true;
    }

    match (&ctx.types[sub], &ctx.types[sup]) {
        (Type::Error, _) | (_, Type::Error) => true,
        (Type::Named { def, args }, _) if *def == ctx.well_known.never && args.is_empty() => true,
        (_, Type::Named { def, args }) if *def == ctx.well_known.unknown && args.is_empty() => true,
        (Type::Named { def: d1, args: a1 }, Type::Named { def: d2, args: a2 }) => {
            is_subtype_named(*d1, a1, *d2, a2, ctx)
        }
        (
            Type::Fn {
                params: p1,
                ret: r1,
                ..
            },
            Type::Fn {
                params: p2,
                ret: r2,
                ..
            },
        ) => is_subtype_fn(p1, *r1, p2, *r2, ctx),
        (Type::Record { fields: f1, .. }, Type::Record { fields: f2, .. }) => {
            is_subtype_record(f1, f2, ctx)
        }
        (Type::Universe { level: a }, Type::Universe { level: b }) => a <= b,
        (Type::Tuple { elems: e1 }, Type::Tuple { elems: e2 }) => is_subtype_tuple(e1, e2, ctx),
        (Type::Ref { inner: i1 }, Type::Ref { inner: i2 }) => is_subtype_ref(*i1, *i2, ctx),
        (Type::AnonSum { variants: v1 }, Type::AnonSum { variants: v2 }) => {
            is_subtype_anonsum(v1, v2, ctx)
        }
        (Type::Array { elem: e1, len: l1 }, Type::Array { elem: e2, len: l2 }) => {
            is_subtype_array(*e1, *l1, *e2, *l2, ctx)
        }
        (Type::Sum { variants: v1 }, Type::Sum { variants: v2 }) => is_subtype_sum(v1, v2, ctx),
        (
            Type::Quantified {
                kind: k1,
                params: p1,
                body: b1,
                ..
            },
            Type::Quantified {
                kind: k2,
                params: p2,
                body: b2,
                ..
            },
        ) => is_subtype_quantified(*k1, p1, *b1, *k2, p2, *b2, ctx),
        (
            Type::Pi {
                param_ty: pt1,
                body: b1,
                ..
            },
            Type::Pi {
                param_ty: pt2,
                body: b2,
                ..
            },
        ) => is_subtype_pi(*pt1, *b1, *pt2, *b2, ctx),
        _ => false,
    }
}

fn is_any_type(ty: TypeIdx, ctx: &SubCtx) -> bool {
    matches!(
        &ctx.types[ty],
        Type::Named { def, args } if *def == ctx.well_known.any && args.is_empty()
    )
}

fn is_subtype_named(d1: DefId, a1: &[TypeIdx], d2: DefId, a2: &[TypeIdx], ctx: &SubCtx) -> bool {
    if d1 != d2 || a1.len() != a2.len() {
        return false;
    }
    a1.iter()
        .copied()
        .zip(a2.iter().copied())
        .all(|(a, b)| is_subtype_inner(a, b, ctx))
}

fn is_subtype_fn(p1: &[TypeIdx], r1: TypeIdx, p2: &[TypeIdx], r2: TypeIdx, ctx: &SubCtx) -> bool {
    if p1.len() != p2.len() {
        return false;
    }
    p1.iter()
        .zip(p2.iter())
        .all(|(&param_sub, &param_sup)| is_subtype_inner(param_sup, param_sub, ctx))
        && is_subtype_inner(r1, r2, ctx)
}

fn is_subtype_record(f1: &[RecordField], f2: &[RecordField], ctx: &SubCtx) -> bool {
    f2.iter().all(|sup_field| {
        f1.iter()
            .find(|sub_field| sub_field.name == sup_field.name)
            .is_some_and(|sub_field| is_subtype_inner(sub_field.ty, sup_field.ty, ctx))
    })
}

fn is_subtype_tuple(e1: &[TypeIdx], e2: &[TypeIdx], ctx: &SubCtx) -> bool {
    if e1.len() != e2.len() {
        return false;
    }
    e1.iter()
        .copied()
        .zip(e2.iter().copied())
        .all(|(a, b)| is_subtype_inner(a, b, ctx))
}

fn is_subtype_ref(i1: TypeIdx, i2: TypeIdx, ctx: &SubCtx) -> bool {
    let resolved_i1 = ctx.unify.resolve(i1, ctx.types);
    let resolved_i2 = ctx.unify.resolve(i2, ctx.types);
    resolved_i1 == resolved_i2
}

fn is_subtype_anonsum(v1: &[TypeIdx], v2: &[TypeIdx], ctx: &SubCtx) -> bool {
    if v1.len() != v2.len() {
        return false;
    }
    v1.iter()
        .copied()
        .zip(v2.iter().copied())
        .all(|(a, b)| is_subtype_inner(a, b, ctx))
}

fn is_subtype_array(
    e1: TypeIdx,
    l1: Option<u32>,
    e2: TypeIdx,
    l2: Option<u32>,
    ctx: &SubCtx,
) -> bool {
    l1 == l2 && is_subtype_inner(e1, e2, ctx)
}

fn is_subtype_sum(v1: &[SumVariant], v2: &[SumVariant], ctx: &SubCtx) -> bool {
    if v1.len() != v2.len() {
        return false;
    }
    v1.iter().zip(v2.iter()).all(|(sv1, sv2)| {
        sv1.name == sv2.name
            && sv1.fields.len() == sv2.fields.len()
            && sv1
                .fields
                .iter()
                .copied()
                .zip(sv2.fields.iter().copied())
                .all(|(f1, f2)| is_subtype_inner(f1, f2, ctx))
    })
}

fn is_subtype_quantified(
    k1: Quantifier,
    p1: &[TyVarId],
    b1: TypeIdx,
    k2: Quantifier,
    p2: &[TyVarId],
    b2: TypeIdx,
    ctx: &SubCtx,
) -> bool {
    if k1 != k2 || p1.len() != p2.len() {
        return false;
    }
    is_subtype_inner(b1, b2, ctx)
}

fn is_subtype_pi(pt1: TypeIdx, b1: TypeIdx, pt2: TypeIdx, b2: TypeIdx, ctx: &SubCtx) -> bool {
    is_subtype_inner(pt2, pt1, ctx) && is_subtype_inner(b1, b2, ctx)
}
