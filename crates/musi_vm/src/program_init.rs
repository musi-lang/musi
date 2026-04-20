use music_seam::{GlobalId, Instruction, Opcode, Operand, ProcedureId};

use crate::Value;
use crate::program::{LoadedProcedure, RuntimeKernel};

fn decode_value_clause_add(procedure: &LoadedProcedure) -> Option<i16> {
    let [load, smi, add, ret] = procedure.instructions.as_ref() else {
        return None;
    };
    if procedure.params != 1 {
        return None;
    }
    if !matches!(
        (load.opcode, &load.operand),
        (Opcode::LdLoc, Operand::Local(0))
    ) {
        return None;
    }
    let (Opcode::LdSmi, Operand::I16(value)) = (smi.opcode, &smi.operand) else {
        return None;
    };
    if add.opcode == Opcode::IAdd && ret.opcode == Opcode::Ret {
        Some(*value)
    } else {
        None
    }
}

fn decode_op_clause_resume(procedure: &LoadedProcedure) -> Option<i16> {
    let [smi, resume, ret] = procedure.instructions.as_ref() else {
        return None;
    };
    if procedure.params != 1 {
        return None;
    }
    let (Opcode::LdSmi, Operand::I16(value)) = (smi.opcode, &smi.operand) else {
        return None;
    };
    if resume.opcode == Opcode::EffResume && ret.opcode == Opcode::Ret {
        Some(*value)
    } else {
        None
    }
}

pub fn specialize_runtime_kernels(procedures: &mut [LoadedProcedure]) {
    let kernels = procedures
        .iter()
        .map(|procedure| match procedure.runtime_kernel {
            Some(RuntimeKernel::InlineEffectResumeClauses {
                value_clause,
                op_clause,
            }) => inline_effect_resume_kernel(procedures, value_clause, op_clause)
                .or(procedure.runtime_kernel),
            other => other,
        })
        .collect::<Vec<_>>();
    for (procedure, kernel) in procedures.iter_mut().zip(kernels) {
        procedure.runtime_kernel = kernel;
    }
}

fn inline_effect_resume_kernel(
    procedures: &[LoadedProcedure],
    value_clause: ProcedureId,
    op_clause: ProcedureId,
) -> Option<RuntimeKernel> {
    let value_clause = procedures.get(usize::try_from(value_clause.raw()).ok()?)?;
    let op_clause = procedures.get(usize::try_from(op_clause.raw()).ok()?)?;
    Some(RuntimeKernel::InlineEffectResume {
        value_add: decode_value_clause_add(value_clause)?,
        resume_value: decode_op_clause_resume(op_clause)?,
    })
}

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
    let (Opcode::LdSmi, Operand::I16(value)) = (load.opcode, &load.operand) else {
        return None;
    };
    let (Opcode::StGlob, Operand::Global(global)) = (store.opcode, &store.operand) else {
        return None;
    };
    if matches!(
        (unit.opcode, &unit.operand),
        (Opcode::LdSmi, Operand::I16(0))
    ) && ret.opcode == Opcode::Ret
    {
        Some((*global, i64::from(*value)))
    } else {
        None
    }
}
