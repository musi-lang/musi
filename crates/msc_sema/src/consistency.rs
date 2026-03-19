//! Consistency relation for gradual typing.
//!
//! Consistency is symmetric but NOT transitive. `Any` is consistent with
//! every type, enabling gradual typing boundaries.

#[cfg(test)]
mod tests;

use msc_shared::Arena;

use crate::types::{SumVariant, Type, TypeIdx};
use crate::unify::UnifyTable;
use crate::well_known::WellKnown;
use crate::DefId;

fn is_named_consistent(
    d1: DefId,
    a1: &[TypeIdx],
    d2: DefId,
    a2: &[TypeIdx],
    types: &Arena<Type>,
    unify: &UnifyTable,
    well_known: &WellKnown,
) -> bool {
    d1 == d2
        && a1.len() == a2.len()
        && a1
            .iter()
            .zip(a2.iter())
            .all(|(&x, &y)| is_consistent(x, y, types, unify, well_known))
}

fn is_fn_consistent(
    p1: &[TypeIdx],
    r1: TypeIdx,
    p2: &[TypeIdx],
    r2: TypeIdx,
    types: &Arena<Type>,
    unify: &UnifyTable,
    well_known: &WellKnown,
) -> bool {
    p1.len() == p2.len()
        && p1
            .iter()
            .zip(p2.iter())
            .all(|(&x, &y)| is_consistent(x, y, types, unify, well_known))
        && is_consistent(r1, r2, types, unify, well_known)
}

fn is_tuple_consistent(
    e1: &[TypeIdx],
    e2: &[TypeIdx],
    types: &Arena<Type>,
    unify: &UnifyTable,
    well_known: &WellKnown,
) -> bool {
    e1.len() == e2.len()
        && e1
            .iter()
            .zip(e2.iter())
            .all(|(&x, &y)| is_consistent(x, y, types, unify, well_known))
}

fn is_sum_consistent(
    v1: &[SumVariant],
    v2: &[SumVariant],
    types: &Arena<Type>,
    unify: &UnifyTable,
    well_known: &WellKnown,
) -> bool {
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
                .all(|(f1, f2)| is_consistent(f1, f2, types, unify, well_known))
    })
}

/// Returns `true` if types `a` and `b` are consistent.
///
/// Consistency governs cast boundaries in the gradual type system:
/// - `Any ~ t` for all `t` (and `t ~ Any`) - the core gradual typing rule
/// - `t ~ t` - reflexive
/// - `Error ~ t` - poison absorbs
/// - Structural: `Fn(P1, R1) ~ Fn(P2, R2)` iff `P1 ~ P2` and `R1 ~ R2`
/// - Record: pointwise on matching fields
/// - NOT transitive - each call is independent
#[must_use]
pub fn is_consistent(
    a: TypeIdx,
    b: TypeIdx,
    types: &Arena<Type>,
    unify: &UnifyTable,
    well_known: &WellKnown,
) -> bool {
    let a = unify.resolve(a, types);
    let b = unify.resolve(b, types);
    if a == b {
        return true;
    }

    match (&types[a], &types[b]) {
        (Type::Error, _) | (_, Type::Error) => true,
        (Type::Named { def, args }, _) if *def == well_known.any && args.is_empty() => true,
        (_, Type::Named { def, args }) if *def == well_known.any && args.is_empty() => true,
        (Type::Named { def: d1, args: a1 }, Type::Named { def: d2, args: a2 }) => {
            is_named_consistent(*d1, a1, *d2, a2, types, unify, well_known)
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
        ) => is_fn_consistent(p1, *r1, p2, *r2, types, unify, well_known),
        (Type::Tuple { elems: e1 }, Type::Tuple { elems: e2 }) => {
            is_tuple_consistent(e1, e2, types, unify, well_known)
        }
        (Type::Record { fields: f1, .. }, Type::Record { fields: f2, .. }) => {
            f1.iter().all(|field1| {
                f2.iter()
                    .find(|field2| field2.name == field1.name)
                    .is_none_or(|field2| {
                        is_consistent(field1.ty, field2.ty, types, unify, well_known)
                    })
            })
        }
        (Type::Array { elem: e1, len: l1 }, Type::Array { elem: e2, len: l2 }) => {
            l1 == l2 && is_consistent(*e1, *e2, types, unify, well_known)
        }
        (Type::Ref { inner: i1 }, Type::Ref { inner: i2 }) => {
            is_consistent(*i1, *i2, types, unify, well_known)
        }
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
        ) => {
            is_consistent(*pt1, *pt2, types, unify, well_known)
                && is_consistent(*b1, *b2, types, unify, well_known)
        }
        (Type::Universe { level: a }, Type::Universe { level: b }) => a == b,
        (Type::Sum { variants: v1 }, Type::Sum { variants: v2 }) => {
            is_sum_consistent(v1, v2, types, unify, well_known)
        }
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
        ) => {
            if k1 != k2 || p1.len() != p2.len() {
                return false;
            }
            is_consistent(*b1, *b2, types, unify, well_known)
        }
        _ => false,
    }
}
