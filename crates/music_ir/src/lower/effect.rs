//! Effect handler lowering: semantic `EffectRow` -> `IrEffectMask`,
//! `EffectPush`/`EffectPop`/`EffectDo` emission.

use music_sema::types::EffectRow;
use music_sema::well_known::WellKnownEffects;

use crate::types::IrEffectMask;

/// Convert a semantic `EffectRow` to an `IrEffectMask` bitmask.
///
/// Well-known effects are mapped to fixed bit positions:
///   IO = bit 0, Async = bit 1, State = bit 2, Throw = bit 3.
///
/// Polymorphic row variables are ignored (treated as pure) — they only
/// matter for static checking, not runtime dispatch.
#[must_use]
pub fn lower_effect_row(row: &EffectRow, effects: &WellKnownEffects) -> IrEffectMask {
    if row.is_pure() {
        return IrEffectMask::PURE;
    }

    let mut mask: u16 = 0;
    for entry in &row.effects {
        if entry.def == effects.io {
            mask |= 1 << 0;
        } else if entry.def == effects.async_eff {
            mask |= 1 << 1;
        } else if entry.def == effects.state {
            mask |= 1 << 2;
        } else if entry.def == effects.throw {
            mask |= 1 << 3;
        }
        // Unknown effects are silently ignored in the bitmask;
        // the semantic checker already validated them.
    }
    IrEffectMask(mask)
}
