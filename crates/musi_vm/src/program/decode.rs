use music_seam::{Artifact, CodeEntry, Instruction, Opcode, Operand, ProcedureId};

use crate::program_kernel::decode_runtime_kernel;

use super::{
    CompareOp, LabelIndexMap, LoadedProcedure, RuntimeBranchTable, RuntimeBranchTableList,
    RuntimeCallMode, RuntimeCallShape, RuntimeFusedOp, RuntimeInstruction, RuntimeInstructionList,
    VmResult,
};

pub(super) fn build_procedures(artifact: &Artifact) -> VmResult<Box<[LoadedProcedure]>> {
    let procedure_shapes = artifact
        .procedures
        .iter()
        .map(|(_, procedure)| RuntimeCallShape::new(procedure.params, procedure.locals))
        .collect::<Vec<_>>()
        .into_boxed_slice();
    artifact
        .procedures
        .iter()
        .map(|(procedure_id, procedure)| {
            let procedure_name: Box<str> = artifact.string_text(procedure.name).into();
            let mut labels = LabelIndexMap::new();
            let mut instructions = Vec::new();
            for entry in &procedure.code {
                match entry {
                    CodeEntry::Label(label) => {
                        let _ = labels.insert(label.id, instructions.len());
                    }
                    CodeEntry::Instruction(instruction) => {
                        let _ = &procedure_name;
                        instructions.push(instruction.clone());
                    }
                }
            }
            let instructions = instructions.into_boxed_slice();
            let runtime_instructions = decode_runtime_instructions(
                procedure_id,
                procedure.params,
                &instructions,
                &labels,
                &procedure_shapes,
            );
            let runtime_branch_tables = decode_runtime_branch_tables(&instructions, &labels);
            let runtime_kernel =
                decode_runtime_kernel(procedure.params, &instructions, &runtime_instructions);
            Ok(LoadedProcedure::new(
                procedure_name,
                procedure.params,
                procedure.locals,
                instructions,
                runtime_instructions,
                runtime_branch_tables,
                runtime_kernel,
            )
            .with_labels(labels))
        })
        .collect::<VmResult<Vec<_>>>()
        .map(Vec::into_boxed_slice)
}

fn decode_runtime_instructions(
    current_procedure: ProcedureId,
    param_count: u16,
    instructions: &[Instruction],
    labels: &LabelIndexMap,
    procedure_shapes: &[RuntimeCallShape],
) -> RuntimeInstructionList {
    instructions
        .iter()
        .enumerate()
        .map(|(index, instruction)| {
            decode_runtime_instruction(
                current_procedure,
                param_count,
                index,
                instruction,
                instructions,
                labels,
                procedure_shapes,
            )
        })
        .collect::<Vec<_>>()
        .into()
}

fn decode_runtime_branch_tables(
    instructions: &[Instruction],
    labels: &LabelIndexMap,
) -> RuntimeBranchTableList {
    instructions
        .iter()
        .map(|instruction| {
            let Operand::BranchTable(branch_labels) = &instruction.operand else {
                return None;
            };
            let targets = branch_labels
                .iter()
                .map(|label| labels.get(label).copied())
                .collect::<Vec<_>>()
                .into_boxed_slice();
            Some(RuntimeBranchTable::new(targets))
        })
        .collect::<Vec<_>>()
        .into_boxed_slice()
}

fn decode_runtime_instruction(
    current_procedure: ProcedureId,
    param_count: u16,
    index: usize,
    instruction: &Instruction,
    instructions: &[Instruction],
    labels: &LabelIndexMap,
    procedure_shapes: &[RuntimeCallShape],
) -> RuntimeInstruction {
    let runtime = RuntimeInstruction::new(index, instruction);
    if let Some(fused) =
        decode_fused_op(current_procedure, param_count, index, instructions, labels)
    {
        return runtime.with_fused(fused);
    }
    match instruction.opcode {
        Opcode::Br | Opcode::BrFalse => decode_branch_target(runtime, &instruction.operand, labels),
        Opcode::Call | Opcode::TailCall => decode_call(
            runtime,
            index,
            instruction,
            instructions,
            labels,
            procedure_shapes,
        ),
        Opcode::HdlPush => decode_handler_pop(runtime, index, instructions),
        Opcode::Ceq | Opcode::Cne | Opcode::CltS | Opcode::CgtS | Opcode::CleS | Opcode::CgeS => {
            decode_compare_branch(runtime, index, instructions, labels)
        }
        _ => runtime,
    }
}

fn decode_handler_pop(
    runtime: RuntimeInstruction,
    index: usize,
    instructions: &[Instruction],
) -> RuntimeInstruction {
    find_matching_handler_pop(index.saturating_add(1), instructions)
        .map_or(runtime, |pop_ip| runtime.with_branch_target(pop_ip))
}

fn find_matching_handler_pop(start: usize, instructions: &[Instruction]) -> Option<usize> {
    let mut depth = 0usize;
    for (index, instruction) in instructions.iter().enumerate().skip(start) {
        match instruction.opcode {
            Opcode::HdlPush => depth = depth.saturating_add(1),
            Opcode::HdlPop if depth == 0 => return Some(index),
            Opcode::HdlPop => depth = depth.saturating_sub(1),
            _ => {}
        }
    }
    None
}

fn decode_fused_op(
    current_procedure: ProcedureId,
    param_count: u16,
    index: usize,
    instructions: &[Instruction],
    labels: &LabelIndexMap,
) -> Option<RuntimeFusedOp> {
    decode_local_smi_compare_self_tail_dec_acc(
        current_procedure,
        param_count,
        index,
        instructions,
        labels,
    )
    .or_else(|| decode_self_tail_dec_acc(current_procedure, param_count, index, instructions))
    .or_else(|| decode_local_seq2_get_add_set(index, instructions))
    .or_else(|| decode_local_seq2_const_set(index, instructions))
    .or_else(|| decode_local_seq2_get_add(index, instructions))
    .or_else(|| decode_local_data_new1_init(index, instructions))
    .or_else(|| decode_local_copy_add_smi(index, instructions))
    .or_else(|| decode_local_data_tag_branch_table(index, instructions))
    .or_else(|| decode_local_data_get_const_store(index, instructions))
    .or_else(|| decode_local_smi_compare_branch(index, instructions, labels))
}

fn decode_local_smi_compare_self_tail_dec_acc(
    current_procedure: ProcedureId,
    param_count: u16,
    index: usize,
    instructions: &[Instruction],
    labels: &LabelIndexMap,
) -> Option<RuntimeFusedOp> {
    let RuntimeFusedOp::LocalSmiCompareBranch {
        local: compare_local,
        smi: compare_smi,
        compare,
        target,
        fallthrough,
    } = decode_local_smi_compare_branch(index, instructions, labels)?
    else {
        return None;
    };
    let RuntimeFusedOp::SelfTailDecAcc {
        dec_local,
        dec_smi,
        acc_local,
        add_local,
        param_count,
    } = decode_self_tail_dec_acc(current_procedure, param_count, target, instructions)?
    else {
        return None;
    };
    let (mirror_local, loop_ip) = copied_match_local(index, instructions, dec_local, compare_local);
    Some(RuntimeFusedOp::LocalSmiCompareSelfTailDecAcc {
        compare_local,
        compare_smi,
        compare,
        fallthrough,
        dec_local,
        dec_smi,
        acc_local,
        add_local,
        param_count,
        mirror_local,
        loop_ip,
    })
}

fn copied_match_local(
    index: usize,
    instructions: &[Instruction],
    source: u16,
    dest: u16,
) -> (Option<u16>, usize) {
    let Some(prefix_index) = index.checked_sub(2) else {
        return (None, 0);
    };
    let Some([load, store]) = instruction_window::<2>(prefix_index, instructions) else {
        return (None, 0);
    };
    let (Opcode::LdLoc, Operand::Local(load_local)) = (load.opcode, &load.operand) else {
        return (None, 0);
    };
    let (Opcode::StLoc, Operand::Local(store_local)) = (store.opcode, &store.operand) else {
        return (None, 0);
    };
    if *load_local == source && *store_local == dest {
        (Some(dest), index)
    } else {
        (None, 0)
    }
}

fn decode_self_tail_dec_acc(
    current_procedure: ProcedureId,
    param_count: u16,
    index: usize,
    instructions: &[Instruction],
) -> Option<RuntimeFusedOp> {
    let [dec_load, dec_step, dec_op, acc_load, add_load, add_op, call] =
        instruction_window::<7>(index, instructions)?;
    let (Opcode::LdLoc, Operand::Local(dec_local)) = (dec_load.opcode, &dec_load.operand) else {
        return None;
    };
    let (Opcode::LdCI4, Operand::I16(dec_smi)) = (dec_step.opcode, &dec_step.operand) else {
        return None;
    };
    if dec_op.opcode != Opcode::Sub {
        return None;
    }
    let (Opcode::LdLoc, Operand::Local(acc_local)) = (acc_load.opcode, &acc_load.operand) else {
        return None;
    };
    let (Opcode::LdLoc, Operand::Local(add_local)) = (add_load.opcode, &add_load.operand) else {
        return None;
    };
    if add_op.opcode != Opcode::Add {
        return None;
    }
    let (Opcode::Call | Opcode::TailCall, Operand::Procedure(procedure)) =
        (call.opcode, &call.operand)
    else {
        return None;
    };
    if *procedure != current_procedure {
        return None;
    }
    Some(RuntimeFusedOp::SelfTailDecAcc {
        dec_local: *dec_local,
        dec_smi: *dec_smi,
        acc_local: *acc_local,
        add_local: *add_local,
        param_count,
    })
}

fn decode_local_smi_compare_branch(
    index: usize,
    instructions: &[Instruction],
    labels: &LabelIndexMap,
) -> Option<RuntimeFusedOp> {
    let [first, second, compare, branch] = instruction_window::<4>(index, instructions)?;
    let (Opcode::LdLoc, Operand::Local(local)) = (first.opcode, &first.operand) else {
        return None;
    };
    let (Opcode::LdCI4, Operand::I16(smi)) = (second.opcode, &second.operand) else {
        return None;
    };
    let compare = compare_op(compare.opcode)?;
    let (Opcode::BrFalse, Operand::Label(label)) = (branch.opcode, &branch.operand) else {
        return None;
    };
    let target = labels.get(label).copied()?;
    Some(RuntimeFusedOp::LocalSmiCompareBranch {
        local: *local,
        smi: *smi,
        compare,
        target,
        fallthrough: index.saturating_add(4),
    })
}

fn decode_local_data_new1_init(
    index: usize,
    instructions: &[Instruction],
) -> Option<RuntimeFusedOp> {
    let [
        field_load,
        tag_load,
        data_new,
        data_store,
        zero_load,
        zero_store,
        data_reload,
        match_store,
    ] = instruction_window::<8>(index, instructions)?;
    let (Opcode::LdLoc, Operand::Local(field_local)) = (field_load.opcode, &field_load.operand)
    else {
        return None;
    };
    let (Opcode::LdCI4, Operand::I16(tag)) = (tag_load.opcode, &tag_load.operand) else {
        return None;
    };
    let (Opcode::NewObj, Operand::TypeLen { ty, len: 1 }) = (data_new.opcode, &data_new.operand)
    else {
        return None;
    };
    let (Opcode::StLoc, Operand::Local(data_local)) = (data_store.opcode, &data_store.operand)
    else {
        return None;
    };
    let (Opcode::LdCI4, Operand::I16(zero)) = (zero_load.opcode, &zero_load.operand) else {
        return None;
    };
    let (Opcode::StLoc, Operand::Local(zero_local)) = (zero_store.opcode, &zero_store.operand)
    else {
        return None;
    };
    let (Opcode::LdLoc, Operand::Local(reloaded)) = (data_reload.opcode, &data_reload.operand)
    else {
        return None;
    };
    let (Opcode::StLoc, Operand::Local(match_local)) = (match_store.opcode, &match_store.operand)
    else {
        return None;
    };
    if data_local != reloaded || zero_local != match_local {
        return None;
    }
    Some(RuntimeFusedOp::LocalNewObj1Init {
        field_local: *field_local,
        tag: *tag,
        ty: *ty,
        data_local: *data_local,
        match_local: *match_local,
        zero: *zero,
        fallthrough: index.saturating_add(8),
    })
}

fn decode_local_copy_add_smi(index: usize, instructions: &[Instruction]) -> Option<RuntimeFusedOp> {
    let [load, store, reload, smi, add] = instruction_window::<5>(index, instructions)?;
    let (Opcode::LdLoc, Operand::Local(source)) = (load.opcode, &load.operand) else {
        return None;
    };
    let (Opcode::StLoc, Operand::Local(dest)) = (store.opcode, &store.operand) else {
        return None;
    };
    let (Opcode::LdLoc, Operand::Local(reload)) = (reload.opcode, &reload.operand) else {
        return None;
    };
    if dest != reload {
        return None;
    }
    let (Opcode::LdCI4, Operand::I16(smi)) = (smi.opcode, &smi.operand) else {
        return None;
    };
    if add.opcode != Opcode::Add {
        return None;
    }
    Some(RuntimeFusedOp::LocalCopyAddSmi {
        source: *source,
        dest: *dest,
        smi: *smi,
        fallthrough: index.saturating_add(5),
    })
}

fn decode_local_seq2_const_set(
    index: usize,
    instructions: &[Instruction],
) -> Option<RuntimeFusedOp> {
    let [
        load,
        first,
        second,
        value,
        set,
        scratch_value,
        scratch_store,
    ] = instruction_window::<7>(index, instructions)?;
    let (Opcode::LdLoc, Operand::Local(local)) = (load.opcode, &load.operand) else {
        return None;
    };
    let (Opcode::LdCI4, Operand::I16(first)) = (first.opcode, &first.operand) else {
        return None;
    };
    let (Opcode::LdCI4, Operand::I16(second)) = (second.opcode, &second.operand) else {
        return None;
    };
    let (Opcode::LdCI4, Operand::I16(value)) = (value.opcode, &value.operand) else {
        return None;
    };
    let (Opcode::StElem, Operand::I16(2)) = (set.opcode, &set.operand) else {
        return None;
    };
    let (Opcode::LdCI4, Operand::I16(scratch_value)) =
        (scratch_value.opcode, &scratch_value.operand)
    else {
        return None;
    };
    let (Opcode::StLoc, Operand::Local(scratch)) = (scratch_store.opcode, &scratch_store.operand)
    else {
        return None;
    };
    Some(RuntimeFusedOp::LocalSeq2ConstSet {
        local: *local,
        first: *first,
        second: *second,
        value: *value,
        scratch: *scratch,
        scratch_value: *scratch_value,
        fallthrough: index.saturating_add(7),
    })
}

fn decode_local_seq2_get_add_set(
    index: usize,
    instructions: &[Instruction],
) -> Option<RuntimeFusedOp> {
    let [
        target_load,
        target_first,
        target_second,
        source_load,
        source_first,
        source_second,
        get,
        add_value,
        add,
        set,
        scratch_value,
        scratch_store,
    ] = instruction_window::<12>(index, instructions)?;
    let (Opcode::LdLoc, Operand::Local(target)) = (target_load.opcode, &target_load.operand) else {
        return None;
    };
    let (Opcode::LdCI4, Operand::I16(target_first)) = (target_first.opcode, &target_first.operand)
    else {
        return None;
    };
    let (Opcode::LdCI4, Operand::I16(target_second)) =
        (target_second.opcode, &target_second.operand)
    else {
        return None;
    };
    let (Opcode::LdLoc, Operand::Local(source)) = (source_load.opcode, &source_load.operand) else {
        return None;
    };
    let (Opcode::LdCI4, Operand::I16(source_first)) = (source_first.opcode, &source_first.operand)
    else {
        return None;
    };
    let (Opcode::LdCI4, Operand::I16(source_second)) =
        (source_second.opcode, &source_second.operand)
    else {
        return None;
    };
    if !matches!(
        (get.opcode, &get.operand),
        (Opcode::LdElem, Operand::I16(2))
    ) {
        return None;
    }
    let (Opcode::LdCI4, Operand::I16(add_value)) = (add_value.opcode, &add_value.operand) else {
        return None;
    };
    if add.opcode != Opcode::Add {
        return None;
    }
    if !matches!(
        (set.opcode, &set.operand),
        (Opcode::StElem, Operand::I16(2))
    ) {
        return None;
    }
    let (Opcode::LdCI4, Operand::I16(scratch_value)) =
        (scratch_value.opcode, &scratch_value.operand)
    else {
        return None;
    };
    let (Opcode::StLoc, Operand::Local(scratch)) = (scratch_store.opcode, &scratch_store.operand)
    else {
        return None;
    };
    Some(RuntimeFusedOp::LocalSeq2GetAddSet {
        target: *target,
        target_first: *target_first,
        target_second: *target_second,
        source: *source,
        source_first: *source_first,
        source_second: *source_second,
        add: *add_value,
        scratch: *scratch,
        scratch_value: *scratch_value,
        fallthrough: index.saturating_add(12),
    })
}

fn decode_local_seq2_get_add(index: usize, instructions: &[Instruction]) -> Option<RuntimeFusedOp> {
    let [
        left_load,
        left_first,
        left_second,
        left_get,
        right_load,
        right_first,
        right_second,
        right_get,
        add,
    ] = instruction_window::<9>(index, instructions)?;
    let (Opcode::LdLoc, Operand::Local(left)) = (left_load.opcode, &left_load.operand) else {
        return None;
    };
    let (Opcode::LdCI4, Operand::I16(left_first)) = (left_first.opcode, &left_first.operand) else {
        return None;
    };
    let (Opcode::LdCI4, Operand::I16(left_second)) = (left_second.opcode, &left_second.operand)
    else {
        return None;
    };
    if !matches!(
        (left_get.opcode, &left_get.operand),
        (Opcode::LdElem, Operand::I16(2))
    ) {
        return None;
    }
    let (Opcode::LdLoc, Operand::Local(right)) = (right_load.opcode, &right_load.operand) else {
        return None;
    };
    let (Opcode::LdCI4, Operand::I16(right_first)) = (right_first.opcode, &right_first.operand)
    else {
        return None;
    };
    let (Opcode::LdCI4, Operand::I16(right_second)) = (right_second.opcode, &right_second.operand)
    else {
        return None;
    };
    if !matches!(
        (right_get.opcode, &right_get.operand),
        (Opcode::LdElem, Operand::I16(2))
    ) {
        return None;
    }
    if add.opcode != Opcode::Add {
        return None;
    }
    Some(RuntimeFusedOp::LocalSeq2GetAdd {
        left: *left,
        left_first: *left_first,
        left_second: *left_second,
        right: *right,
        right_first: *right_first,
        right_second: *right_second,
        fallthrough: index.saturating_add(9),
    })
}

fn decode_local_data_tag_branch_table(
    index: usize,
    instructions: &[Instruction],
) -> Option<RuntimeFusedOp> {
    let [load, tag, branch] = instruction_window::<3>(index, instructions)?;
    let (Opcode::LdLoc, Operand::Local(local)) = (load.opcode, &load.operand) else {
        return None;
    };
    if tag.opcode != Opcode::LdFld || branch.opcode != Opcode::BrTbl {
        return None;
    }
    Some(RuntimeFusedOp::LocalLdFldBranchTable {
        local: *local,
        branch_table: index.saturating_add(2),
    })
}

fn decode_local_data_get_const_store(
    index: usize,
    instructions: &[Instruction],
) -> Option<RuntimeFusedOp> {
    let [load, field, get, store] = instruction_window::<4>(index, instructions)?;
    let (Opcode::LdLoc, Operand::Local(source)) = (load.opcode, &load.operand) else {
        return None;
    };
    let (Opcode::LdCI4, Operand::I16(field)) = (field.opcode, &field.operand) else {
        return None;
    };
    if get.opcode != Opcode::LdFld {
        return None;
    }
    let (Opcode::StLoc, Operand::Local(dest)) = (store.opcode, &store.operand) else {
        return None;
    };
    Some(RuntimeFusedOp::LocalLdFldConstStore {
        source: *source,
        field: *field,
        dest: *dest,
        fallthrough: index.saturating_add(4),
    })
}

fn instruction_window<const N: usize>(
    index: usize,
    instructions: &[Instruction],
) -> Option<&[Instruction; N]> {
    instructions
        .get(index..index.checked_add(N)?)?
        .try_into()
        .ok()
}

fn decode_branch_target(
    runtime: RuntimeInstruction,
    operand: &Operand,
    labels: &LabelIndexMap,
) -> RuntimeInstruction {
    let Operand::Label(label) = operand else {
        return runtime;
    };
    labels
        .get(label)
        .copied()
        .map_or(runtime, |target| runtime.with_branch_target(target))
}

fn decode_call(
    runtime: RuntimeInstruction,
    index: usize,
    instruction: &Instruction,
    instructions: &[Instruction],
    labels: &LabelIndexMap,
    procedure_shapes: &[RuntimeCallShape],
) -> RuntimeInstruction {
    let runtime = if matches!(instruction.opcode, Opcode::TailCall)
        || call_is_tail_position(index, instructions, labels)
    {
        runtime.with_call_mode(RuntimeCallMode::Tail)
    } else {
        runtime
    };
    let Operand::Procedure(procedure) = instruction.operand else {
        return runtime;
    };
    let Some(shape) = procedure_shapes
        .get(usize::try_from(procedure.raw()).unwrap_or(usize::MAX))
        .copied()
    else {
        return runtime;
    };
    runtime.with_call_shape(shape)
}

fn decode_compare_branch(
    runtime: RuntimeInstruction,
    index: usize,
    instructions: &[Instruction],
    labels: &LabelIndexMap,
) -> RuntimeInstruction {
    let Some(compare) = compare_op(runtime.opcode) else {
        return runtime;
    };
    let Some(next) = instructions.get(index.saturating_add(1)) else {
        return runtime;
    };
    if !matches!(next.opcode, Opcode::BrFalse) {
        return runtime;
    }
    let Operand::Label(label) = next.operand else {
        return runtime;
    };
    labels.get(&label).copied().map_or(runtime, |target| {
        runtime.with_compare_branch(compare, target)
    })
}

fn call_is_tail_position(
    index: usize,
    instructions: &[Instruction],
    labels: &LabelIndexMap,
) -> bool {
    let next_index = index.saturating_add(1);
    let Some(next) = instructions.get(next_index) else {
        return false;
    };
    if matches!(next.opcode, Opcode::Ret) {
        return true;
    }
    if !matches!(next.opcode, Opcode::Br) {
        return false;
    }
    let Operand::Label(label) = next.operand else {
        return false;
    };
    let Some(target) = labels.get(&label).copied() else {
        return false;
    };
    instructions
        .get(target)
        .is_some_and(|instruction| matches!(instruction.opcode, Opcode::Ret))
}

const fn compare_op(opcode: Opcode) -> Option<CompareOp> {
    match opcode {
        Opcode::Ceq => Some(CompareOp::Eq),
        Opcode::Cne => Some(CompareOp::Ne),
        Opcode::CltS => Some(CompareOp::Lt),
        Opcode::CgtS => Some(CompareOp::Gt),
        Opcode::CleS => Some(CompareOp::Le),
        Opcode::CgeS => Some(CompareOp::Ge),
        _ => None,
    }
}
