//! §13 effect push/pop/do/resume/abort dispatch.

use musi_bc::Opcode;

use crate::error::VmError;
use crate::loader::{HandlerEntry, LoadedEffect};
use crate::vm::{EffFrame, Frame};

/// Dispatch §13 effect opcodes.
pub fn exec(
    op: Opcode,
    operand: u32,
    frame: &mut Frame,
    effects: &[LoadedEffect],
    handlers: &[HandlerEntry],
) -> Result<EffectAction, VmError> {
    match op {
        Opcode::EFF_PSH => exec_eff_psh(operand, frame, handlers),
        Opcode::EFF_POP => {
            let effect_id = u8::try_from(operand).map_err(|_| VmError::Malformed {
                desc: "eff.pop effect_id overflow".into(),
            })?;
            if let Some(pos) = frame
                .eff_stack
                .iter()
                .rposition(|f| f.effect_id == effect_id)
            {
                let _ = frame.eff_stack.remove(pos);
            }
            Ok(EffectAction::Continue)
        }
        Opcode::EFF_DO => Ok(exec_eff_do(operand, frame, effects)),
        Opcode::EFF_RES => Ok(EffectAction::Resume),
        Opcode::EFF_RES_C => Ok(EffectAction::Continue),
        Opcode::EFF_ABT => {
            frame.eff_stack.clear();
            Ok(EffectAction::Abort)
        }
        _ => Ok(EffectAction::NotHandled),
    }
}

fn exec_eff_psh(
    operand: u32,
    frame: &mut Frame,
    handlers: &[HandlerEntry],
) -> Result<EffectAction, VmError> {
    let effect_id = u8::try_from(operand).map_err(|_| VmError::Malformed {
        desc: "eff.psh effect_id overflow".into(),
    })?;
    let handler_fn_id = handlers
        .iter()
        .find(|h| h.effect_id == effect_id)
        .map_or(0, |h| h.handler_fn_id);
    frame.eff_stack.push(EffFrame {
        effect_id,
        handler_fn_id,
    });
    Ok(EffectAction::Continue)
}

fn exec_eff_do(op_id: u32, frame: &Frame, effects: &[LoadedEffect]) -> EffectAction {
    let search_id = resolve_effect_id(op_id, effects);
    let search_id_u8 = u8::try_from(search_id & 0xFF).unwrap_or(u8::MAX);

    // Search current frame first.
    if let Some(eff_frame) = frame
        .eff_stack
        .iter()
        .rev()
        .find(|f| f.effect_id == search_id_u8)
    {
        return EffectAction::DoEffect {
            handler_fn_id: eff_frame.handler_fn_id,
        };
    }

    // Not found in current frame — request cross-frame search.
    EffectAction::CrossFrameSearch {
        effect_id: search_id_u8,
    }
}

/// Resolve an `op_id` to an `effect_id` by searching the effects table.
/// Falls back to treating `op_id` as `effect_id` for simple single-op effects.
pub fn resolve_effect_id(op_id: u32, effects: &[LoadedEffect]) -> u32 {
    effects
        .iter()
        .find(|eff| eff.ops.iter().any(|op| op.id == op_id))
        .map_or(op_id, |eff| eff.id)
}

/// What the effect dispatcher wants the main loop to do after handling an opcode.
#[derive(Debug)]
pub enum EffectAction {
    /// Opcode not in this group — try the next dispatcher.
    NotHandled,
    /// Normal execution continues.
    Continue,
    /// Call handler function with the operand stack arguments.
    DoEffect { handler_fn_id: u32 },
    /// Handler not found in current frame — search entire call stack.
    CrossFrameSearch { effect_id: u8 },
    /// Effect aborted — unwind to nearest handler.
    Abort,
    /// Resume a captured continuation (`EFF_RES`).
    Resume,
}
