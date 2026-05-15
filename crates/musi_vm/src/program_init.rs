use music_seam::{GlobalId, Instruction, Opcode, Operand, ProcedureId};

use crate::Value;
use crate::program::LoadedProcedure;
pub fn build_global_init_image(
    procedures: &[LoadedProcedure],
    global_count: usize,
    entry: Option<ProcedureId>,
) -> Option<Box<[Value]>> {
    let entry = entry?;
    let assignments = simple_global_assignments(procedures, entry)?;
    let mut globals = vec![Value::Unit; global_count];
    for (global, value) in assignments {
        let slot = usize::try_from(global.raw()).ok()?;
        *globals.get_mut(slot)? = Value::Int(value);
    }
    Some(globals.into_boxed_slice())
}

fn simple_global_assignments(
    procedures: &[LoadedProcedure],
    procedure: ProcedureId,
) -> Option<Vec<(GlobalId, i64)>> {
    let procedure = procedures.get(usize::try_from(procedure.raw()).ok()?)?;
    if let Some(assignment) = simple_global_init_assignment(&procedure.instructions) {
        return Some(vec![assignment]);
    }
    let mut assignments = Vec::new();
    let mut chunks = procedure.instructions.chunks_exact(2);
    for chunk in &mut chunks {
        let [call, store] = chunk else {
            return None;
        };
        let (Opcode::Call, Operand::Procedure(callee)) = (call.opcode, &call.operand) else {
            return None;
        };
        if !matches!(
            (store.opcode, &store.operand),
            (Opcode::StLoc, Operand::Local(_))
        ) {
            return None;
        }
        assignments.extend(simple_global_assignments(procedures, *callee)?);
    }
    let [ret] = chunks.remainder() else {
        return None;
    };
    (ret.opcode == Opcode::Ret).then_some(assignments)
}

fn simple_global_init_assignment(instructions: &[Instruction]) -> Option<(GlobalId, i64)> {
    let [load, store, unit, ret] = instructions else {
        return None;
    };
    let (Opcode::LdCI4, Operand::I16(value)) = (load.opcode, &load.operand) else {
        return None;
    };
    let (Opcode::StGlob, Operand::Global(global)) = (store.opcode, &store.operand) else {
        return None;
    };
    if matches!(
        (unit.opcode, &unit.operand),
        (Opcode::LdCI4, Operand::I16(0))
    ) && ret.opcode == Opcode::Ret
    {
        Some((*global, i64::from(*value)))
    } else {
        None
    }
}
