//! Effect row subset checking.

use music_shared::Span;
use std::hash::BuildHasher;

use crate::checker::Checker;
use crate::error::SemaError;
use crate::types::EffectRow;

/// Checks that `callee_effects` is a subset of `current_effects`.
///
/// Reports diagnostics for any effect in the callee that is not declared
/// in the current context.
pub fn check_effects_subset<S: BuildHasher>(
    ck: &mut Checker<'_, S>,
    callee_effects: &EffectRow,
    current_effects: &EffectRow,
    span: Span,
) {
    if callee_effects.effects.is_empty() {
        return;
    }

    if current_effects.is_pure() {
        let _d = ck
            .diags
            .report(&SemaError::EffectInPureContext, span, ck.ctx.file_id);
        return;
    }

    // Check each callee effect is present in the current context.
    for callee_eff in &callee_effects.effects {
        let found = current_effects
            .effects
            .iter()
            .any(|cur_eff| cur_eff.def == callee_eff.def);

        if !found && current_effects.row_var.is_none() {
            let effect_name = ck.ctx.interner.resolve(ck.defs.get(callee_eff.def).name);
            let _d = ck.diags.report(
                &SemaError::UndeclaredEffect {
                    effect: Box::from(effect_name),
                },
                span,
                ck.ctx.file_id,
            );
        }
    }
}
