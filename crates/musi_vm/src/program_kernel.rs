use music_seam::{Instruction, Opcode, Operand};

use crate::program::{RuntimeFusedOp, RuntimeInstruction, RuntimeKernel};

pub fn decode_runtime_kernel(
    param_count: u16,
    instructions: &[Instruction],
    runtime_instructions: &[RuntimeInstruction],
) -> Option<RuntimeKernel> {
    decode_direct_int_wrapper_call(instructions)
        .or_else(|| decode_int_tail_accumulator_kernel(runtime_instructions, instructions))
        .or_else(|| decode_inline_effect_resume_kernel(instructions))
        .or_else(|| decode_seq2_mutation_kernel(runtime_instructions))
        .or_else(|| decode_data_construct_match_add_kernel(runtime_instructions))
        .or_else(|| decode_int_arg_add_smi_kernel(param_count, instructions))
}

fn decode_direct_int_wrapper_call(instructions: &[Instruction]) -> Option<RuntimeKernel> {
    let [load, constant, call, ret] = instruction_window::<4>(0, instructions)?;
    let (Opcode::LdLoc, Operand::Local(arg_local)) = (load.opcode, &load.operand) else {
        return None;
    };
    let (Opcode::LdSmi, Operand::I16(const_arg)) = (constant.opcode, &constant.operand) else {
        return None;
    };
    let (Opcode::Call | Opcode::CallTail, Operand::Procedure(procedure)) =
        (call.opcode, &call.operand)
    else {
        return None;
    };
    if ret.opcode != Opcode::Ret || instructions.len() != 4 {
        return None;
    }
    Some(RuntimeKernel::DirectIntWrapperCall {
        arg_local: *arg_local,
        const_arg: *const_arg,
        procedure: *procedure,
    })
}

fn decode_int_tail_accumulator_kernel(
    runtime_instructions: &[RuntimeInstruction],
    instructions: &[Instruction],
) -> Option<RuntimeKernel> {
    let fused = runtime_instructions.iter().find_map(|instruction| {
        let Some(RuntimeFusedOp::LocalSmiCompareSelfTailDecAcc {
            compare_local,
            compare_smi,
            compare,
            dec_local,
            dec_smi,
            acc_local,
            add_local,
            ..
        }) = instruction.fused
        else {
            return None;
        };
        Some((
            compare_local,
            compare_smi,
            compare,
            dec_local,
            dec_smi,
            acc_local,
            add_local,
        ))
    })?;
    let return_local = decode_return_local(instructions).unwrap_or(fused.5);
    Some(RuntimeKernel::IntTailAccumulator {
        compare_local: fused.0,
        compare_smi: fused.1,
        compare: fused.2,
        dec_local: fused.3,
        dec_smi: fused.4,
        acc_local: fused.5,
        add_local: fused.6,
        return_local,
    })
}

fn decode_return_local(instructions: &[Instruction]) -> Option<u16> {
    instructions.windows(2).find_map(|window| {
        let [load, ret] = window else {
            return None;
        };
        let (Opcode::LdLoc, Operand::Local(local)) = (load.opcode, &load.operand) else {
            return None;
        };
        (ret.opcode == Opcode::Ret).then_some(*local)
    })
}

fn decode_data_construct_match_add_kernel(
    runtime_instructions: &[RuntimeInstruction],
) -> Option<RuntimeKernel> {
    let field_local = runtime_instructions.iter().find_map(|instruction| {
        let Some(RuntimeFusedOp::LocalDataNew1Init { field_local, .. }) = instruction.fused else {
            return None;
        };
        Some(field_local)
    })?;
    let smi = runtime_instructions.iter().find_map(|instruction| {
        let Some(RuntimeFusedOp::LocalCopyAddSmi { smi, .. }) = instruction.fused else {
            return None;
        };
        Some(smi)
    })?;
    Some(RuntimeKernel::DataConstructMatchAdd {
        source: field_local,
        smi,
    })
}

fn decode_seq2_mutation_kernel(
    runtime_instructions: &[RuntimeInstruction],
) -> Option<RuntimeKernel> {
    let init = runtime_instructions.iter().find_map(|instruction| {
        let Some(fused @ RuntimeFusedOp::LocalSeq2ConstSet { .. }) = instruction.fused else {
            return None;
        };
        Some(fused)
    })?;
    let update = runtime_instructions.iter().find_map(|instruction| {
        let Some(fused @ RuntimeFusedOp::LocalSeq2GetAddSet { .. }) = instruction.fused else {
            return None;
        };
        Some(fused)
    })?;
    let finish = runtime_instructions.iter().find_map(|instruction| {
        let Some(fused @ RuntimeFusedOp::LocalSeq2GetAdd { .. }) = instruction.fused else {
            return None;
        };
        Some(fused)
    })?;
    Some(RuntimeKernel::Seq2Mutation {
        init,
        update,
        finish,
    })
}

fn decode_inline_effect_resume_kernel(instructions: &[Instruction]) -> Option<RuntimeKernel> {
    let mut procedures = instructions.iter().filter_map(|instruction| {
        let (
            Opcode::ClsNew,
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
    let value_clause = procedures.next()?;
    let op_clause = procedures.next()?;
    let has_handler = instructions
        .iter()
        .any(|instruction| instruction.opcode == Opcode::HdlPush);
    let has_effect = instructions
        .iter()
        .any(|instruction| instruction.opcode == Opcode::EffInvk);
    let has_pop = instructions
        .iter()
        .any(|instruction| instruction.opcode == Opcode::HdlPop);
    if has_handler && has_effect && has_pop {
        Some(RuntimeKernel::InlineEffectResume {
            value_clause,
            op_clause,
        })
    } else {
        None
    }
}

fn decode_int_arg_add_smi_kernel(
    param_count: u16,
    instructions: &[Instruction],
) -> Option<RuntimeKernel> {
    if param_count != 1 {
        return None;
    }
    if let Some(kernel) = decode_stored_closure_int_arg_add_smi_kernel(instructions) {
        return Some(kernel);
    }
    instructions.windows(7).find_map(|window| {
        let [
            smi_load,
            smi_store,
            capture_load,
            closure_new,
            arg_load,
            call,
            ret,
        ] = window
        else {
            return None;
        };
        let (Opcode::LdSmi, Operand::I16(smi)) = (smi_load.opcode, &smi_load.operand) else {
            return None;
        };
        let (Opcode::StLoc, Operand::Local(smi_local)) = (smi_store.opcode, &smi_store.operand)
        else {
            return None;
        };
        let (Opcode::LdLoc, Operand::Local(capture_local)) =
            (capture_load.opcode, &capture_load.operand)
        else {
            return None;
        };
        if smi_local != capture_local {
            return None;
        }
        if !matches!(
            (closure_new.opcode, &closure_new.operand),
            (
                Opcode::ClsNew,
                Operand::WideProcedureCaptures { captures: 1, .. }
            )
        ) {
            return None;
        }
        let (Opcode::LdLoc, Operand::Local(arg_local)) = (arg_load.opcode, &arg_load.operand)
        else {
            return None;
        };
        if !matches!(call.opcode, Opcode::Call | Opcode::CallTail) || ret.opcode != Opcode::Ret {
            return None;
        }
        Some(RuntimeKernel::IntArgAddSmi {
            arg_local: *arg_local,
            smi: *smi,
        })
    })
}

fn decode_stored_closure_int_arg_add_smi_kernel(
    instructions: &[Instruction],
) -> Option<RuntimeKernel> {
    let [
        smi_load,
        smi_store,
        _zero_load_a,
        _zero_store_a,
        capture_load,
        closure_new,
        closure_store,
        _zero_load_b,
        _zero_store_b,
        callee_load,
        arg_load,
        call,
        ret,
    ] = instruction_window::<13>(0, instructions)?;
    let (Opcode::LdSmi, Operand::I16(smi)) = (smi_load.opcode, &smi_load.operand) else {
        return None;
    };
    let (Opcode::StLoc, Operand::Local(smi_local)) = (smi_store.opcode, &smi_store.operand) else {
        return None;
    };
    let (Opcode::LdLoc, Operand::Local(capture_local)) =
        (capture_load.opcode, &capture_load.operand)
    else {
        return None;
    };
    if smi_local != capture_local {
        return None;
    }
    if !matches!(
        (closure_new.opcode, &closure_new.operand),
        (
            Opcode::ClsNew,
            Operand::WideProcedureCaptures { captures: 1, .. }
        )
    ) {
        return None;
    }
    let (Opcode::StLoc, Operand::Local(closure_local)) =
        (closure_store.opcode, &closure_store.operand)
    else {
        return None;
    };
    let (Opcode::LdLoc, Operand::Local(callee_local)) = (callee_load.opcode, &callee_load.operand)
    else {
        return None;
    };
    if closure_local != callee_local {
        return None;
    }
    let (Opcode::LdLoc, Operand::Local(arg_local)) = (arg_load.opcode, &arg_load.operand) else {
        return None;
    };
    if !matches!(call.opcode, Opcode::Call | Opcode::CallTail) || ret.opcode != Opcode::Ret {
        return None;
    }
    Some(RuntimeKernel::IntArgAddSmi {
        arg_local: *arg_local,
        smi: *smi,
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
