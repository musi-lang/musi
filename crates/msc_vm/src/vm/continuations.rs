//! Effect handler dispatch (`EFF_HDL` / `EFF_NEED` / `EFF_RES` / `EFF_POP`).

use msc_bc::Opcode;

use crate::VmResult;
use crate::error::VmError;
use crate::loader::{HandlerEntry, LoadedEffect};
use crate::vm::{ContMarker, Frame};

/// Dispatch effect opcodes.
pub fn exec(
    op: Opcode,
    operand: u32,
    frame: &mut Frame,
    effects: &[LoadedEffect],
    handlers: &[HandlerEntry],
) -> VmResult<ContAction> {
    match op {
        Opcode::EFF_HDL => exec_eff_hdl(operand, frame, handlers),
        Opcode::EFF_POP => {
            // FI16 operand = effect_id.
            let effect_id = u8::try_from(operand & 0xFF).map_err(|_| VmError::Malformed {
                desc: "eff.pop effect_id overflow".into(),
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
        Opcode::EFF_NEED => {
            // FI8x2: op_id in high byte, arity in low byte.
            let op_id = (operand >> 8) & 0xFF;
            Ok(exec_eff_need(op_id, frame, effects))
        }
        Opcode::EFF_RES => Ok(ContAction::Resume),
        _ => Ok(ContAction::NotHandled),
    }
}

fn exec_eff_hdl(
    operand: u32,
    frame: &mut Frame,
    handlers: &[HandlerEntry],
) -> VmResult<ContAction> {
    // FI16 operand = effect_id.
    let effect_id = u8::try_from(operand & 0xFF).map_err(|_| VmError::Malformed {
        desc: "eff.hdl effect_id overflow".into(),
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

fn exec_eff_need(op_id: u32, frame: &Frame, effects: &[LoadedEffect]) -> ContAction {
    let search_id = resolve_marker_id(op_id, effects);
    let search_id_u8 = u8::try_from(search_id & 0xFF).unwrap_or(u8::MAX);

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

    ContAction::CrossFrameSearch {
        effect_id: search_id_u8,
        op_id,
    }
}

/// Resolve an `op_id` to the containing effect's id via the effect table.
///
/// Falls back to treating `op_id` as the marker id for single-op effects.
pub fn resolve_marker_id(op_id: u32, effects: &[LoadedEffect]) -> u32 {
    effects
        .iter()
        .find(|eff| eff.ops.iter().any(|op| u32::from(op.id) == op_id))
        .map_or(op_id, |eff| u32::from(eff.id))
}

/// What the continuation dispatcher wants the main loop to do next.
#[derive(Debug)]
pub enum ContAction {
    /// Opcode not handled by this group - try the next dispatcher.
    NotHandled,
    /// Normal execution continues.
    Continue,
    /// Call handler function with operand stack arguments.
    Dispatch { handler_fn_id: u32 },
    /// Handler not found in current frame - search the entire call stack.
    CrossFrameSearch { effect_id: u8, op_id: u32 },
    /// Resume a captured continuation (`EFF_RES`).
    Resume,
}
