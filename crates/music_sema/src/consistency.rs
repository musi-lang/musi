//! Consistency relation for gradual typing.
//!
//! Consistency is symmetric but NOT transitive. `Any` is consistent with
//! every type, enabling gradual typing boundaries.

#[cfg(test)]
mod tests;

use music_shared::Arena;

use crate::types::{Type, TypeIdx};
use crate::unify::UnifyTable;
use crate::well_known::WellKnown;

/// Returns `true` if types `a` and `b` are consistent.
///
/// Consistency governs cast boundaries in the gradual type system:
/// - `Any ~ t` for all `t` (and `t ~ Any`) — the core gradual typing rule
/// - `t ~ t` — reflexive
/// - `Error ~ t` — poison absorbs
/// - Structural: `Fn(P1, R1) ~ Fn(P2, R2)` iff `P1 ~ P2` and `R1 ~ R2`
/// - Record: pointwise on matching fields
/// - NOT transitive — each call is independent
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

        // Core gradual typing rule: Any ~ t for all t
        (Type::Named { def, args }, _) if *def == well_known.any && args.is_empty() => true,
        (_, Type::Named { def, args }) if *def == well_known.any && args.is_empty() => true,

        // Structural consistency
        (Type::Named { def: d1, args: a1 }, Type::Named { def: d2, args: a2 }) => {
            d1 == d2
                && a1.len() == a2.len()
                && a1
                    .iter()
                    .zip(a2.iter())
                    .all(|(&x, &y)| is_consistent(x, y, types, unify, well_known))
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
        ) => {
            let (p1, r1, p2, r2) = (p1.clone(), *r1, p2.clone(), *r2);
            p1.len() == p2.len()
                && p1
                    .iter()
                    .zip(p2.iter())
                    .all(|(&x, &y)| is_consistent(x, y, types, unify, well_known))
                && is_consistent(r1, r2, types, unify, well_known)
        }

        (Type::Tuple { elems: e1 }, Type::Tuple { elems: e2 }) => {
            let (e1, e2) = (e1.clone(), e2.clone());
            e1.len() == e2.len()
                && e1
                    .iter()
                    .zip(e2.iter())
                    .all(|(&x, &y)| is_consistent(x, y, types, unify, well_known))
        }

        (Type::Record { fields: f1, .. }, Type::Record { fields: f2, .. }) => {
            let (f1, f2) = (f1.clone(), f2.clone());
            // Pointwise on matching fields
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

        _ => false,
    }
}
