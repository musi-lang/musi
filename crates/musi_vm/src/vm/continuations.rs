//! Continuation mark/save/resume dispatch.

use musi_bc::Opcode;

use crate::error::VmError;
use crate::loader::{HandlerEntry, LoadedEffect};
use crate::vm::{ContMarker, Frame};

/// Dispatch continuation opcodes.
pub fn exec(
    op: Opcode,
    operand: u32,
    frame: &mut Frame,
    effects: &[LoadedEffect],
    handlers: &[HandlerEntry],
) -> Result<ContAction, VmError> {
    match op {
        Opcode::CNT_MRK => exec_cont_mark(operand, frame, handlers),
        Opcode::CNT_UMK => {
            let effect_id = u8::try_from(operand).map_err(|_| VmError::Malformed {
                desc: "cont.unmark effect_id overflow".into(),
            })?;
            if let Some(pos) = frame
                .marker_stack
                .iter()
                .rposition(|f| f.effect_id == effect_id)
            {
                let _ = frame.marker_stack.remove(pos);
            }
            Ok(ContAction::Continue)
        }
        Opcode::CNT_SAV => Ok(exec_cont_save(operand, frame, effects)),
        Opcode::CNT_RSM => Ok(ContAction::Resume),
        _ => Ok(ContAction::NotHandled),
    }
}

fn exec_cont_mark(
    operand: u32,
    frame: &mut Frame,
    handlers: &[HandlerEntry],
) -> Result<ContAction, VmError> {
    let effect_id = u8::try_from(operand).map_err(|_| VmError::Malformed {
        desc: "cont.mark effect_id overflow".into(),
    })?;
    let handler_fn_id = handlers
        .iter()
        .find(|h| h.effect_id == effect_id)
        .map_or(0, |h| h.handler_fn_id);
    frame.marker_stack.push(ContMarker {
        effect_id,
        handler_fn_id,
    });
    Ok(ContAction::Continue)
}

fn exec_cont_save(op_id: u32, frame: &Frame, effects: &[LoadedEffect]) -> ContAction {
    let search_id = resolve_marker_id(op_id, effects);
    let search_id_u8 = u8::try_from(search_id & 0xFF).unwrap_or(u8::MAX);

    // Search current frame first.
    if let Some(marker) = frame
        .marker_stack
        .iter()
        .rev()
        .find(|f| f.effect_id == search_id_u8)
    {
        return ContAction::Dispatch {
            handler_fn_id: marker.handler_fn_id,
        };
    }

    // Not found in current frame — request cross-frame search.
    ContAction::CrossFrameSearch {
        effect_id: search_id_u8,
        op_id,
    }
}

/// Resolve an `op_id` to a marker id by searching the effects table.
/// Falls back to treating `op_id` as marker id for simple single-op effects.
pub fn resolve_marker_id(op_id: u32, effects: &[LoadedEffect]) -> u32 {
    effects
        .iter()
        .find(|eff| eff.ops.iter().any(|op| op.id == op_id))
        .map_or(op_id, |eff| eff.id)
}

/// What the continuation dispatcher wants the main loop to do after handling an opcode.
#[derive(Debug)]
pub enum ContAction {
    /// Opcode not in this group — try the next dispatcher.
    NotHandled,
    /// Normal execution continues.
    Continue,
    /// Call handler function with the operand stack arguments.
    Dispatch { handler_fn_id: u32 },
    /// Handler not found in current frame — search entire call stack.
    CrossFrameSearch { effect_id: u8, op_id: u32 },
    /// Resume a captured continuation (`CONT_RESUME`).
    Resume,
}
