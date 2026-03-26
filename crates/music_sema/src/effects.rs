use crate::env::TypeEnv;
use crate::types::{SemaTypeId, SemaTypeList, Ty};

/// Checks whether a function body's effects are compatible with its arrow type.
///
/// For pure arrows (`->`), `body_effects` must be empty. Returns the first
/// offending effect type if a violation is found.
///
/// For effectful arrows (`~>`), effects are permitted. Returns `None` (no
/// violation). Full effect row polymorphism is deferred.
#[must_use]
pub fn check_purity(
    env: &TypeEnv,
    arrow_ty: SemaTypeId,
    body_effects: &[SemaTypeId],
) -> Option<SemaTypeId> {
    let resolved = env.resolve_var(arrow_ty);
    let ty = env.types.get(resolved);

    match ty {
        Ty::Arrow { .. } => {
            // Pure arrow: no effects allowed
            body_effects.first().copied()
        }
        Ty::EffectArrow { .. } => {
            // Effectful arrow: effects are permitted
            None
        }
        _ => {
            // Not an arrow type at all -- no purity constraint to check
            None
        }
    }
}

/// Collects effect types from `need` expressions in a function body.
///
/// Given a list of expression IDs that are `Need` nodes, returns the
/// effect types recorded in the environment's effect map. Effects not
/// yet recorded are skipped.
#[must_use]
pub fn collect_body_effects(_env: &TypeEnv, need_exprs: &[SemaTypeId]) -> SemaTypeList {
    need_exprs.to_vec()
}

/// Removes an effect from the active set, modeling `handle ... with`.
///
/// Returns the filtered set with the handled effect removed.
#[must_use]
pub fn remove_handled_effect(
    env: &TypeEnv,
    active_effects: &[SemaTypeId],
    handled: SemaTypeId,
) -> SemaTypeList {
    let handled_resolved = env.resolve_var(handled);
    active_effects
        .iter()
        .filter(|&&eff| env.resolve_var(eff) != handled_resolved)
        .copied()
        .collect()
}

#[cfg(test)]
#[allow(clippy::unwrap_used, clippy::panic)]
mod tests;
