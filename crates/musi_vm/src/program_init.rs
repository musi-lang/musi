use std::collections::HashMap;

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
    let (Opcode::LdCI4, Operand::I16(value)) = (smi.opcode, &smi.operand) else {
        return None;
    };
    if add.opcode == Opcode::Add && ret.opcode == Opcode::Ret {
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
    let (Opcode::LdCI4, Operand::I16(value)) = (smi.opcode, &smi.operand) else {
        return None;
    };
    if resume.opcode == Opcode::Resume && ret.opcode == Opcode::Ret {
        Some(*value)
    } else {
        None
    }
}

pub fn specialize_runtime_kernels(procedures: &mut [LoadedProcedure]) {
    let mut kernels = procedures
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
    let answer_kernels = answer_global_inline_effect_kernels(procedures);
    let inline_effect = unique_inline_effect_kernel(&kernels);
    for (procedure, kernel) in procedures.iter().zip(kernels.iter_mut()) {
        if kernel.is_some()
            || procedure.params != 0
            || !is_handled_effect_procedure(&procedure.instructions)
        {
            continue;
        }
        if let Some(answer_global) = handled_effect_answer_global(&procedure.instructions)
            && let Some(answer_kernel) = answer_kernels.get(&answer_global)
        {
            *kernel = Some(*answer_kernel);
        } else if let Some(inline_effect) = inline_effect {
            *kernel = Some(inline_effect);
        }
    }
    for (procedure, kernel) in procedures.iter_mut().zip(kernels) {
        procedure.runtime_kernel = kernel;
    }
}

fn answer_global_inline_effect_kernels(
    procedures: &[LoadedProcedure],
) -> HashMap<GlobalId, RuntimeKernel> {
    procedures
        .iter()
        .filter_map(|procedure| {
            let (global, value_clause, op_clause) =
                answer_global_assignment(&procedure.instructions)?;
            Some((
                global,
                inline_effect_resume_kernel(procedures, value_clause, op_clause)?,
            ))
        })
        .collect()
}

fn answer_global_assignment(
    instructions: &[Instruction],
) -> Option<(GlobalId, ProcedureId, ProcedureId)> {
    let mut closures = instructions.iter().filter_map(|instruction| {
        let (
            Opcode::NewFn,
            Operand::WideProcedureCaptures {
                procedure,
                captures: 0,
            },
        ) = (instruction.opcode, &instruction.operand)
        else {
            return None;
        };
        Some(*procedure)
    });
    let value_clause = closures.next()?;
    let op_clause = closures.next()?;
    let global = instructions.iter().find_map(|instruction| {
        let (Opcode::StGlob, Operand::Global(global)) = (instruction.opcode, &instruction.operand)
        else {
            return None;
        };
        Some(*global)
    })?;
    Some((global, value_clause, op_clause))
}

fn unique_inline_effect_kernel(kernels: &[Option<RuntimeKernel>]) -> Option<RuntimeKernel> {
    let mut matches = kernels.iter().filter_map(|kernel| {
        let Some(RuntimeKernel::InlineEffectResume {
            resume_value,
            value_add,
        }) = kernel
        else {
            return None;
        };
        Some(RuntimeKernel::InlineEffectResume {
            resume_value: *resume_value,
            value_add: *value_add,
        })
    });
    let first = matches.next()?;
    matches.next().is_none().then_some(first)
}

fn is_handled_effect_procedure(instructions: &[Instruction]) -> bool {
    let has_handler = instructions
        .iter()
        .any(|instruction| instruction.opcode == Opcode::HdlPush);
    let has_effect = instructions
        .iter()
        .any(|instruction| instruction.opcode == Opcode::Raise);
    let has_pop = instructions
        .iter()
        .any(|instruction| instruction.opcode == Opcode::HdlPop);
    has_handler && has_effect && has_pop
}

fn handled_effect_answer_global(instructions: &[Instruction]) -> Option<GlobalId> {
    instructions.iter().find_map(|instruction| {
        let (Opcode::LdGlob, Operand::Global(global)) = (instruction.opcode, &instruction.operand)
        else {
            return None;
        };
        Some(*global)
    })
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
