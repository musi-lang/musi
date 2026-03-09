//! §13 effect push/pop/do/resume/abort dispatch.

use crate::error::VmError;
use crate::loader::{HandlerEntry, LoadedEffect};
use crate::vm::{EffFrame, Frame};

// Opcode constants.
const EFF_PSH: u8 = 0x4C;
const EFF_POP: u8 = 0x4D;
const EFF_RES_C: u8 = 0x66;
const EFF_ABT: u8 = 0x67;
const EFF_DO: u8 = 0xCA;
const EFF_RES: u8 = 0xCB;

/// Dispatch §13 effect opcodes.
pub fn exec(
    op: u8,
    operand: u32,
    frame: &mut Frame,
    effects: &[LoadedEffect],
    handlers: &[HandlerEntry],
) -> Result<EffectAction, VmError> {
    match op {
        EFF_PSH => exec_eff_psh(operand, frame, handlers),
        EFF_POP => {
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
        EFF_DO => exec_eff_do(operand, frame, effects),
        EFF_RES => Err(VmError::Unimplemented {
            desc: "eff.res (caller-side resume) not yet implemented",
        }),
        EFF_RES_C => {
            let _val = frame.stack.pop().ok_or_else(|| VmError::Malformed {
                desc: "eff.res.c on empty stack".into(),
            })?;
            Ok(EffectAction::Continue)
        }
        EFF_ABT => {
            frame.eff_stack.clear();
            Ok(EffectAction::Abort)
        }
        _ => Ok(EffectAction::NotHandled),
    }
}

/// Push an effect handler frame, looking up the `handler_fn_id` from the
/// current function's handler table.
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

/// Handle `EFF_DO`: look up which effect owns the given `op_id`, then find
/// the handler in the effect stack.
fn exec_eff_do(
    op_id: u32,
    frame: &Frame,
    effects: &[LoadedEffect],
) -> Result<EffectAction, VmError> {
    // Find the effect that contains this op_id.
    let effect_id_for_op = effects
        .iter()
        .find(|eff| eff.ops.iter().any(|op| op.id == op_id))
        .map(|eff| eff.id);

    // Fallback: treat op_id as effect_id (backward-compatible with
    // simple single-operation effects).
    let search_id = effect_id_for_op.unwrap_or(op_id);

    let search_id_u8 = u8::try_from(search_id & 0xFF).unwrap_or(u8::MAX);

    frame
        .eff_stack
        .iter()
        .rev()
        .find(|f| f.effect_id == search_id_u8)
        .copied()
        .map_or_else(
            || {
                Err(VmError::NoHandler {
                    effect_id: search_id_u8,
                })
            },
            |eff_frame| {
                Ok(EffectAction::DoEffect {
                    handler_fn_id: eff_frame.handler_fn_id,
                })
            },
        )
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
    /// Effect aborted — unwind to nearest handler.
    Abort,
}
