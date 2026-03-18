//! Subtyping relation for the Musi type system.

#[cfg(test)]
mod tests;

use music_shared::Arena;

use crate::types::{Type, TypeIdx};
use crate::unify::UnifyTable;
use crate::well_known::WellKnown;

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
    let sub = unify.resolve(sub, types);
    let sup = unify.resolve(sup, types);

    // Any does not participate in the subtyping lattice — check before reflexivity
    // so that `Any <: Any` is also rejected.
    match (&types[sub], &types[sup]) {
        (Type::Named { def, args }, _) if *def == well_known.any && args.is_empty() => {
            return false;
        }
        (_, Type::Named { def, args }) if *def == well_known.any && args.is_empty() => {
            return false;
        }
        _ => {}
    }

    // Reflexivity.
    if sub == sup {
        return true;
    }

    match (&types[sub], &types[sup]) {
        // Error absorbs in both directions.
        (Type::Error, _) | (_, Type::Error) => true,

        // Never is the bottom type: Never <: t for all t.
        (Type::Named { def, args }, _) if *def == well_known.never && args.is_empty() => true,

        // Unknown is the top type: t <: Unknown for all t.
        (_, Type::Named { def, args }) if *def == well_known.unknown && args.is_empty() => true,

        // Invariant named types: same def, args pairwise equal (not subtype, equality).
        (Type::Named { def: d1, args: a1 }, Type::Named { def: d2, args: a2 }) => {
            if d1 != d2 || a1.len() != a2.len() {
                return false;
            }
            a1.iter()
                .copied()
                .zip(a2.iter().copied())
                .all(|(a, b)| is_subtype(a, b, types, unify, well_known))
        }

        // Function subtyping: contravariant params, covariant return.
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
            if p1.len() != p2.len() {
                return false;
            }
            let (p1, r1, p2, r2) = (p1.clone(), *r1, p2.clone(), *r2);
            // Params are contravariant: p2 <: p1.
            p1.iter().zip(p2.iter()).all(|(&param_sub, &param_sup)| {
                is_subtype(param_sup, param_sub, types, unify, well_known)
            }) && is_subtype(r1, r2, types, unify, well_known)
        }

        // Record width + depth subtyping: sub must have at least the fields of sup,
        // and for each matching field the sub field type must be a subtype of the sup field type.
        (Type::Record { fields: f1, .. }, Type::Record { fields: f2, .. }) => {
            let (f1, f2) = (f1.clone(), f2.clone());
            f2.iter().all(|sup_field| {
                f1.iter()
                    .find(|sub_field| sub_field.name == sup_field.name)
                    .is_some_and(|sub_field| {
                        is_subtype(sub_field.ty, sup_field.ty, types, unify, well_known)
                    })
            })
        }

        // Universe level subtyping: Universe(a) <: Universe(b) iff a <= b.
        (Type::Universe { level: a }, Type::Universe { level: b }) => a <= b,

        // Tuple subtyping: same length, pairwise covariant.
        (Type::Tuple { elems: e1 }, Type::Tuple { elems: e2 }) => {
            if e1.len() != e2.len() {
                return false;
            }
            e1.iter()
                .copied()
                .zip(e2.iter().copied())
                .all(|(a, b)| is_subtype(a, b, types, unify, well_known))
        }

        // Ref is invariant: Ref(a) <: Ref(b) iff a == b after resolve.
        (Type::Ref { inner: i1 }, Type::Ref { inner: i2 }) => {
            let resolved_i1 = unify.resolve(*i1, types);
            let resolved_i2 = unify.resolve(*i2, types);
            resolved_i1 == resolved_i2
        }

        // AnonSum subtyping: pairwise if same length.
        (Type::AnonSum { variants: v1 }, Type::AnonSum { variants: v2 }) => {
            if v1.len() != v2.len() {
                return false;
            }
            v1.iter()
                .copied()
                .zip(v2.iter().copied())
                .all(|(a, b)| is_subtype(a, b, types, unify, well_known))
        }

        // Array subtyping: same length and covariant element type.
        (Type::Array { elem: e1, len: l1 }, Type::Array { elem: e2, len: l2 }) => {
            l1 == l2 && is_subtype(*e1, *e2, types, unify, well_known)
        }

        // Var: resolve through unification table and retry.
        // (Already resolved at the top via unify.resolve, so a remaining Var is unbound.)
        // Unbound vars have no subtype relationship with non-vars.
        _ => false,
    }
}
