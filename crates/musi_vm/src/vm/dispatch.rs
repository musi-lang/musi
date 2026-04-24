use std::cmp::Ordering;
use std::mem;

use music_seam::{Instruction, Opcode, ProcedureId, TypeId};

use crate::{VmIndexSpace, VmStackKind, VmValueKind};

use super::state::{CallFrame, CallFrameList, EffectHandler, EffectHandlerList, StepOutcome};
use super::{
    CompareOp, GcRef, OperandShape, RuntimeCallMode, RuntimeCallShape, RuntimeFusedOp,
    RuntimeInstruction, RuntimeOperand, Value, ValueList, VmError, VmErrorKind, VmResult,
};

use super::Vm;

impl Vm {
    pub(crate) fn invoke_procedure_from_args_shape(
        &mut self,
        module_slot: usize,
        procedure: ProcedureId,
        args: &[Value],
        param_count: usize,
        local_count: usize,
    ) -> VmResult<Value> {
        self.push_frame_from_arg_slice_with_shape(
            module_slot,
            procedure,
            args,
            param_count,
            local_count,
        )?;
        self.run_current_state()
    }

    pub(crate) fn invoke_procedure_with_prefix_args_shape(
        &mut self,
        module_slot: usize,
        procedure: ProcedureId,
        prefix: &[Value],
        args: &[Value],
        shape: RuntimeCallShape,
    ) -> VmResult<Value> {
        self.push_frame_with_prefix_and_args_shape(module_slot, procedure, prefix, args, shape)?;
        self.run_current_state()
    }

    pub(crate) fn invoke_procedure_in_context(
        &mut self,
        module_slot: usize,
        procedure: ProcedureId,
        args: ValueList,
        base_depth: usize,
    ) -> VmResult<Value> {
        self.push_frame(module_slot, procedure, args)?;
        let saved_return_depth = self.return_depth;
        self.return_depth = Some(base_depth);
        let result = self.run_current_state();
        self.return_depth = saved_return_depth;
        result
    }

    pub(crate) fn invoke_procedure_in_context_from_args_shape(
        &mut self,
        module_slot: usize,
        procedure: ProcedureId,
        args: &[Value],
        base_depth: usize,
        param_count: usize,
        local_count: usize,
    ) -> VmResult<Value> {
        self.push_frame_from_arg_slice_with_shape(
            module_slot,
            procedure,
            args,
            param_count,
            local_count,
        )?;
        let saved_return_depth = self.return_depth;
        self.return_depth = Some(base_depth);
        let result = self.run_current_state();
        self.return_depth = saved_return_depth;
        result
    }

    pub(crate) fn invoke_procedure_in_context_with_prefix_args_shape(
        &mut self,
        module_slot: usize,
        procedure: ProcedureId,
        prefix: &[Value],
        args: &[Value],
        base_depth: usize,
        shape: RuntimeCallShape,
    ) -> VmResult<Value> {
        self.push_frame_with_prefix_and_args_shape(module_slot, procedure, prefix, args, shape)?;
        let saved_return_depth = self.return_depth;
        self.return_depth = Some(base_depth);
        let result = self.run_current_state();
        self.return_depth = saved_return_depth;
        result
    }

    pub(crate) fn invoke_continuation(
        &mut self,
        continuation: GcRef,
        value: Value,
    ) -> VmResult<Value> {
        let saved_frames = mem::take(&mut self.frames);
        let saved_handlers = mem::take(&mut self.handlers);
        let saved_active_resumes = mem::take(&mut self.active_resumes);
        let saved_target = self.continuation_target_handler;
        let saved_return_depth = self.return_depth;

        let (frames, handlers, target_handler) = {
            let continuation = self.heap.continuation(continuation)?;
            let frames = continuation
                .frames
                .iter()
                .cloned()
                .map(CallFrame::from)
                .collect::<CallFrameList>();
            let handlers = continuation
                .handlers
                .iter()
                .cloned()
                .map(EffectHandler::from)
                .collect::<EffectHandlerList>();
            let target_handler = continuation
                .handlers
                .first()
                .map(|handler| handler.handler_id);
            (frames, handlers, target_handler)
        };

        self.frames = frames;
        self.handlers = handlers;
        self.active_resumes = Vec::new();
        self.continuation_target_handler = target_handler;
        self.return_depth = None;
        self.refresh_frame_runtime_codes()?;
        let result = match self.frames.last_mut() {
            Some(frame) => {
                frame.stack.push(value);
                self.run_current_state()
            }
            None => Err(VmError::new(VmErrorKind::InvalidProgramShape {
                detail: "continuation frame list is empty".into(),
            })),
        };

        self.frames = saved_frames;
        self.handlers = saved_handlers;
        self.active_resumes = saved_active_resumes;
        self.continuation_target_handler = saved_target;
        self.return_depth = saved_return_depth;
        result
    }

    pub(crate) fn run_current_state(&mut self) -> VmResult<Value> {
        if self.options.instruction_budget.is_some() {
            self.run_current_state_budgeted()
        } else {
            self.run_current_state_unbudgeted()
        }
    }

    fn run_current_state_budgeted(&mut self) -> VmResult<Value> {
        loop {
            let instruction = self.next_runtime_instruction()?;
            match self.execute_runtime_instr(&instruction)? {
                StepOutcome::Continue => {}
                StepOutcome::Return(value) => return Ok(value),
            }
        }
    }

    fn run_current_state_unbudgeted(&mut self) -> VmResult<Value> {
        loop {
            let instruction = self.next_runtime_instruction_unbudgeted()?;
            match self.execute_runtime_instr(&instruction)? {
                StepOutcome::Continue => {}
                StepOutcome::Return(value) => return Ok(value),
            }
        }
    }

    pub(crate) fn execute_runtime_instr(
        &mut self,
        runtime: &RuntimeInstruction,
    ) -> VmResult<StepOutcome> {
        if let Some(fused) = runtime.fused {
            return self.exec_fused(fused);
        }
        if let Some((compare, target)) = runtime.compare_branch {
            return self.exec_compare_branch(compare, target);
        }
        match runtime.opcode {
            Opcode::LdLoc => self.exec_fast_ldloc(runtime),
            Opcode::StLoc => self.exec_fast_stloc(runtime),
            Opcode::LdGlob => self.exec_fast_ldglob(runtime),
            Opcode::StGlob => self.exec_fast_stglob(runtime),
            Opcode::LdC => self.exec_fast_ldconst(runtime),
            Opcode::LdCI4 => self.exec_fast_ldsmi(runtime),
            Opcode::Add => self.exec_fast_int_op(runtime, i64::checked_add),
            Opcode::Sub => self.exec_fast_int_op(runtime, i64::checked_sub),
            Opcode::Mul => self.exec_fast_int_op(runtime, i64::checked_mul),
            Opcode::DivS => self.exec_fast_int_op(runtime, i64::checked_div),
            Opcode::RemS => self.exec_fast_int_op(runtime, i64::checked_rem),
            Opcode::Br => self.exec_fast_br(runtime),
            Opcode::BrFalse => self.exec_fast_brfalse(runtime),
            Opcode::Call => self.execute_runtime_call(runtime),
            Opcode::TailCall => self.exec_fast_tail_call(runtime),
            Opcode::CallInd | Opcode::NewFn => self.exec_fast_call_edge(runtime),
            Opcode::Ret => self.return_from_frame(),
            Opcode::NewArr | Opcode::LdElem | Opcode::StElem => self.exec_fast_seq(runtime),
            Opcode::NewObj | Opcode::LdFld | Opcode::StFld => self.exec_fast_data(runtime),
            Opcode::HdlPush | Opcode::HdlPop | Opcode::Raise | Opcode::Resume => {
                self.exec_fast_effect(runtime)
            }
            _ => {
                let instruction =
                    if let Some(instruction) = runtime.operand.to_instruction(runtime.opcode) {
                        instruction
                    } else {
                        self.current_raw_instruction(runtime.raw_index)?
                    };
                self.execute_instr(&instruction)
            }
        }
    }

    fn execute_runtime_call(&mut self, runtime: &RuntimeInstruction) -> VmResult<StepOutcome> {
        if let RuntimeOperand::Procedure(_) = runtime.operand {
            self.exec_fast_call(runtime)
        } else {
            let instruction = self.current_raw_instruction(runtime.raw_index)?;
            self.execute_instr(&instruction)
        }
    }

    pub(crate) fn execute_instr(&mut self, instruction: &Instruction) -> VmResult<StepOutcome> {
        match instruction.opcode {
            Opcode::LdLoc
            | Opcode::StLoc
            | Opcode::LdGlob
            | Opcode::StGlob
            | Opcode::LdC
            | Opcode::LdCI4
            | Opcode::LdStr => self.exec_load_store(instruction),
            Opcode::Add
            | Opcode::Sub
            | Opcode::Mul
            | Opcode::DivS
            | Opcode::RemS
            | Opcode::Ceq
            | Opcode::Cne
            | Opcode::CltS
            | Opcode::CgtS
            | Opcode::CleS
            | Opcode::CgeS
            | Opcode::And
            | Opcode::Or
            | Opcode::Xor
            | Opcode::Not => self.exec_scalar(instruction),
            Opcode::Br | Opcode::BrFalse | Opcode::BrTbl => self.exec_branch(instruction),
            Opcode::Call if matches!(instruction.operand, music_seam::Operand::I16(_)) => {
                self.exec_type(instruction)
            }
            Opcode::Call | Opcode::CallInd | Opcode::TailCall | Opcode::Ret | Opcode::NewFn => {
                self.exec_call(instruction)
            }
            Opcode::NewArr | Opcode::LdElem | Opcode::StElem | Opcode::LdLen => {
                self.exec_seq(instruction)
            }
            Opcode::NewObj | Opcode::LdFld | Opcode::StFld => self.exec_data(instruction),
            Opcode::LdType | Opcode::IsInst | Opcode::Cast => self.exec_type(instruction),
            Opcode::HdlPush | Opcode::HdlPop | Opcode::Raise | Opcode::Resume => {
                self.exec_effect(instruction)
            }
            Opcode::CallFfi | Opcode::LdFfi | Opcode::MdlLoad | Opcode::MdlGet => {
                self.exec_host_edge(instruction)
            }
            _ => Err(Self::invalid_dispatch(instruction, "general")),
        }
    }
}

impl Vm {
    fn exec_fused(&mut self, fused: RuntimeFusedOp) -> VmResult<StepOutcome> {
        match fused {
            RuntimeFusedOp::LocalSmiCompareBranch { .. }
            | RuntimeFusedOp::LocalSmiCompareSelfTailDecAcc { .. }
            | RuntimeFusedOp::SelfTailDecAcc { .. } => self.exec_fused_control(fused),
            RuntimeFusedOp::LocalLdFldBranchTable { .. }
            | RuntimeFusedOp::LocalLdFldConstStore { .. }
            | RuntimeFusedOp::LocalNewObj1Init { .. }
            | RuntimeFusedOp::LocalCopyAddSmi { .. } => self.exec_fused_data(fused),
            RuntimeFusedOp::LocalSeq2ConstSet { .. }
            | RuntimeFusedOp::LocalSeq2GetAddSet { .. }
            | RuntimeFusedOp::LocalSeq2GetAdd { .. } => self.exec_fused_seq(fused),
        }
    }

    fn exec_fused_control(&mut self, fused: RuntimeFusedOp) -> VmResult<StepOutcome> {
        match fused {
            RuntimeFusedOp::LocalSmiCompareBranch {
                local,
                smi,
                compare,
                target,
                fallthrough,
            } => {
                let left = self.fast_local_int(local)?;
                if compare_int_for_branch(compare, left, i64::from(smi)) {
                    self.jump_to_ip(fallthrough)?;
                } else {
                    self.jump_to_ip(target)?;
                }
                Ok(StepOutcome::Continue)
            }
            RuntimeFusedOp::LocalSmiCompareSelfTailDecAcc {
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
            } => {
                let compare_value = self.fast_local_int(compare_local)?;
                if compare_int_for_branch(compare, compare_value, i64::from(compare_smi)) {
                    self.jump_to_ip(fallthrough)?;
                } else {
                    self.exec_self_tail_dec_acc(SelfTailDecAccPlan {
                        dec_local,
                        dec_smi,
                        acc_local,
                        add_local,
                        param_count,
                        mirror_local,
                        loop_ip,
                    })?;
                }
                Ok(StepOutcome::Continue)
            }
            RuntimeFusedOp::SelfTailDecAcc {
                dec_local,
                dec_smi,
                acc_local,
                add_local,
                param_count,
            } => {
                self.exec_self_tail_dec_acc(SelfTailDecAccPlan {
                    dec_local,
                    dec_smi,
                    acc_local,
                    add_local,
                    param_count,
                    mirror_local: None,
                    loop_ip: 0,
                })?;
                Ok(StepOutcome::Continue)
            }
            _ => Err(fused_dispatch_error("control")),
        }
    }

    fn exec_fused_data(&mut self, fused: RuntimeFusedOp) -> VmResult<StepOutcome> {
        match fused {
            RuntimeFusedOp::LocalLdFldBranchTable {
                local,
                branch_table,
            } => self.exec_local_data_tag_branch_table(local, branch_table),
            RuntimeFusedOp::LocalLdFldConstStore {
                source,
                field,
                dest,
                fallthrough,
            } => self.exec_local_data_get_const_store(source, field, dest, fallthrough),
            RuntimeFusedOp::LocalNewObj1Init {
                field_local,
                tag,
                ty,
                data_local,
                match_local,
                zero,
                fallthrough,
            } => self.exec_local_data_new1_init(NewObjInitPlan {
                field_local,
                tag,
                ty,
                data_local,
                match_local,
                zero,
                fallthrough,
            }),
            RuntimeFusedOp::LocalCopyAddSmi {
                source,
                dest,
                smi,
                fallthrough,
            } => self.exec_local_copy_add_smi(source, dest, smi, fallthrough),
            _ => Err(fused_dispatch_error("data")),
        }
    }

    fn exec_fused_seq(&mut self, fused: RuntimeFusedOp) -> VmResult<StepOutcome> {
        match fused {
            RuntimeFusedOp::LocalSeq2ConstSet {
                local,
                first,
                second,
                value,
                scratch,
                scratch_value,
                fallthrough,
            } => self.exec_local_seq2_const_set(Seq2ConstSetPlan {
                local,
                first,
                second,
                value,
                scratch,
                scratch_value,
                fallthrough,
            }),
            RuntimeFusedOp::LocalSeq2GetAddSet {
                target,
                target_first,
                target_second,
                source,
                source_first,
                source_second,
                add,
                scratch,
                scratch_value,
                fallthrough,
            } => self.exec_local_seq2_get_add_set(Seq2GetAddSetPlan {
                target,
                target_first,
                target_second,
                source,
                source_first,
                source_second,
                add,
                scratch,
                scratch_value,
                fallthrough,
            }),
            RuntimeFusedOp::LocalSeq2GetAdd {
                left,
                left_first,
                left_second,
                right,
                right_first,
                right_second,
                fallthrough,
            } => self.exec_local_seq2_get_add(Seq2GetAddPlan {
                left,
                left_first,
                left_second,
                right,
                right_first,
                right_second,
                fallthrough,
            }),
            _ => Err(fused_dispatch_error("sequence")),
        }
    }

    fn exec_self_tail_dec_acc(&mut self, plan: SelfTailDecAccPlan) -> VmResult {
        let SelfTailDecAccPlan {
            dec_local,
            dec_smi,
            acc_local,
            add_local,
            param_count,
            mirror_local,
            loop_ip,
        } = plan;
        let frame = self.current_frame_mut()?;
        let dec_index = usize::from(dec_local);
        let acc_index = usize::from(acc_local);
        let add_index = usize::from(add_local);
        let len = frame.locals.len();
        if dec_index >= len {
            return Err(local_index_error(dec_local, len));
        }
        if acc_index >= len {
            return Err(local_index_error(acc_local, len));
        }
        if add_index >= len {
            return Err(local_index_error(add_local, len));
        }
        let dec_value = int_from_value(&frame.locals[dec_index])?;
        let acc_value = int_from_value(&frame.locals[acc_index])?;
        let add_value = int_from_value(&frame.locals[add_index])?;
        let next_dec = dec_value
            .checked_sub(i64::from(dec_smi))
            .ok_or_else(int_overflow_error)?;
        let next_acc = acc_value
            .checked_add(add_value)
            .ok_or_else(int_overflow_error)?;
        frame.locals[dec_index] = Value::Int(next_dec);
        frame.locals[acc_index] = Value::Int(next_acc);
        let param_count = usize::from(param_count);
        if frame.locals.len() > param_count {
            for local in &mut frame.locals[param_count..] {
                *local = Value::Unit;
            }
        }
        if let Some(mirror_local) = mirror_local {
            let mirror_index = usize::from(mirror_local);
            if mirror_index >= len {
                return Err(local_index_error(mirror_local, len));
            }
            frame.locals[mirror_index] = Value::Int(next_dec);
        }
        frame.stack.clear();
        frame.set_ip(loop_ip);
        Ok(())
    }

    fn exec_local_data_new1_init(&mut self, plan: NewObjInitPlan) -> VmResult<StepOutcome> {
        let NewObjInitPlan {
            field_local,
            tag,
            ty,
            data_local,
            match_local,
            zero,
            fallthrough,
        } = plan;
        let field = self.local_value(field_local)?;
        let data = self.alloc_data_owned(ty, i64::from(tag), vec![field].into())?;
        self.store_local(data_local, data.clone())?;
        self.store_local(match_local, Value::Int(i64::from(zero)))?;
        self.store_local(match_local, data)?;
        self.jump_to_ip(fallthrough)?;
        Ok(StepOutcome::Continue)
    }

    fn exec_local_copy_add_smi(
        &mut self,
        source: u16,
        dest: u16,
        smi: i16,
        fallthrough: usize,
    ) -> VmResult<StepOutcome> {
        let value = self.local_value(source)?;
        let int = int_from_value(&value)?;
        self.store_local(dest, value)?;
        let result = int
            .checked_add(i64::from(smi))
            .ok_or_else(int_overflow_error)?;
        self.current_frame_mut()?.stack.push(Value::Int(result));
        self.jump_to_ip(fallthrough)?;
        Ok(StepOutcome::Continue)
    }

    fn local_value(&self, local: u16) -> VmResult<Value> {
        let frame = self.current_frame()?;
        let index = usize::from(local);
        let Some(value) = frame.locals.get(index) else {
            return Err(local_index_error(local, frame.locals.len()));
        };
        Ok(value.clone())
    }

    fn exec_local_seq2_const_set(&mut self, plan: Seq2ConstSetPlan) -> VmResult<StepOutcome> {
        let Seq2ConstSetPlan {
            local,
            first,
            second,
            value,
            scratch,
            scratch_value,
            fallthrough,
        } = plan;
        let seq = self.local_seq(local)?;
        self.set_seq2(
            seq,
            i64::from(first),
            i64::from(second),
            Value::Int(i64::from(value)),
        )?;
        self.store_local(scratch, Value::Int(i64::from(scratch_value)))?;
        self.current_frame_mut()?.stack.push(Value::Seq(seq));
        self.jump_to_ip(fallthrough)?;
        Ok(StepOutcome::Continue)
    }

    fn exec_local_seq2_get_add_set(&mut self, plan: Seq2GetAddSetPlan) -> VmResult<StepOutcome> {
        let Seq2GetAddSetPlan {
            target,
            target_first,
            target_second,
            source,
            source_first,
            source_second,
            add,
            scratch,
            scratch_value,
            fallthrough,
        } = plan;
        let source_seq = self.local_seq(source)?;
        let value = self.seq2_int(
            source_seq,
            i64::from(source_first),
            i64::from(source_second),
        )?;
        let value = value
            .checked_add(i64::from(add))
            .ok_or_else(int_overflow_error)?;
        let target_seq = self.local_seq(target)?;
        self.set_seq2(
            target_seq,
            i64::from(target_first),
            i64::from(target_second),
            Value::Int(value),
        )?;
        self.store_local(scratch, Value::Int(i64::from(scratch_value)))?;
        self.current_frame_mut()?.stack.push(Value::Seq(target_seq));
        self.jump_to_ip(fallthrough)?;
        Ok(StepOutcome::Continue)
    }

    fn exec_local_seq2_get_add(&mut self, plan: Seq2GetAddPlan) -> VmResult<StepOutcome> {
        let Seq2GetAddPlan {
            left,
            left_first,
            left_second,
            right,
            right_first,
            right_second,
            fallthrough,
        } = plan;
        let left_seq = self.local_seq(left)?;
        let left_value = self.seq2_int(left_seq, i64::from(left_first), i64::from(left_second))?;
        let right_seq = self.local_seq(right)?;
        let right_value =
            self.seq2_int(right_seq, i64::from(right_first), i64::from(right_second))?;
        let value = left_value
            .checked_add(right_value)
            .ok_or_else(int_overflow_error)?;
        self.current_frame_mut()?.stack.push(Value::Int(value));
        self.jump_to_ip(fallthrough)?;
        Ok(StepOutcome::Continue)
    }

    fn local_seq(&self, local: u16) -> VmResult<GcRef> {
        let frame = self.current_frame()?;
        let index = usize::from(local);
        let Some(value) = frame.locals.get(index) else {
            return Err(local_index_error(local, frame.locals.len()));
        };
        Self::expect_seq(value.clone())
    }

    fn seq2_int(&self, seq: GcRef, first: i64, second: i64) -> VmResult<i64> {
        let row = self.seq2_row(seq, first)?;
        let len = self.heap.sequence_len(row)?;
        let slot = sequence_index(second, len)?;
        self.heap.sequence_int_at(row, slot)
    }

    fn set_seq2(&mut self, seq: GcRef, first: i64, second: i64, value: Value) -> VmResult {
        let row = self.seq2_row(seq, first)?;
        let len = self.heap.sequence_len(row)?;
        let slot = sequence_index(second, len)?;
        self.heap.sequence_set(row, slot, value)
    }

    fn seq2_row(&self, seq: GcRef, first: i64) -> VmResult<GcRef> {
        let len = self.heap.sequence_len(seq)?;
        let slot = usize::try_from(first).unwrap_or(usize::MAX);
        let Ok(value) = self.heap.sequence_get_cloned(seq, slot) else {
            return Err(VmError::new(VmErrorKind::InvalidSequenceIndex {
                index: first,
                len,
            }));
        };
        match value {
            Value::Seq(row) => Ok(row),
            other => Err(Self::invalid_value_kind(VmValueKind::Seq, &other)),
        }
    }

    fn store_local(&mut self, local: u16, value: Value) -> VmResult {
        let frame = self.current_frame_mut()?;
        let index = usize::from(local);
        let len = frame.locals.len();
        let Some(slot) = frame.locals.get_mut(index) else {
            return Err(local_index_error(local, len));
        };
        *slot = value;
        Ok(())
    }

    fn exec_local_data_tag_branch_table(
        &mut self,
        local: u16,
        branch_table: usize,
    ) -> VmResult<StepOutcome> {
        let tag = {
            let frame = self.current_frame()?;
            let index = usize::from(local);
            let Some(value) = frame.locals.get(index) else {
                return Err(VmError::new(VmErrorKind::IndexOutOfBounds {
                    space: VmIndexSpace::Local,
                    owner: None,
                    index: i64::from(local),
                    len: frame.locals.len(),
                }));
            };
            let data = Self::expect_data(value.clone())?;
            self.heap.data(data)?.tag
        };
        self.branch_table_jump_at(branch_table, tag)?;
        Ok(StepOutcome::Continue)
    }

    fn exec_local_data_get_const_store(
        &mut self,
        source: u16,
        field: i16,
        dest: u16,
        fallthrough: usize,
    ) -> VmResult<StepOutcome> {
        let field_value = {
            let frame = self.current_frame()?;
            let source_index = usize::from(source);
            let Some(value) = frame.locals.get(source_index) else {
                return Err(VmError::new(VmErrorKind::IndexOutOfBounds {
                    space: VmIndexSpace::Local,
                    owner: None,
                    index: i64::from(source),
                    len: frame.locals.len(),
                }));
            };
            let data = Self::expect_data(value.clone())?;
            let data_ref = self.heap.data(data)?;
            let slot = usize::try_from(field).unwrap_or(usize::MAX);
            data_ref.fields.get(slot).cloned().ok_or_else(|| {
                VmError::new(VmErrorKind::InvalidDataIndex {
                    index: i64::from(field),
                    len: data_ref.fields.len(),
                })
            })?
        };
        let frame = self.current_frame_mut()?;
        let dest_index = usize::from(dest);
        let len = frame.locals.len();
        let Some(local) = frame.locals.get_mut(dest_index) else {
            return Err(VmError::new(VmErrorKind::IndexOutOfBounds {
                space: VmIndexSpace::Local,
                owner: None,
                index: i64::from(dest),
                len,
            }));
        };
        *local = field_value;
        frame.set_ip(fallthrough);
        Ok(StepOutcome::Continue)
    }

    fn fast_local_int(&self, slot: u16) -> VmResult<i64> {
        let frame = self.current_frame()?;
        local_int_from_frame(frame, slot)
    }
}

impl Vm {
    fn exec_fast_ldloc(&mut self, runtime: &RuntimeInstruction) -> VmResult<StepOutcome> {
        let RuntimeOperand::Local(slot) = runtime.operand else {
            let instruction = self.current_raw_instruction(runtime.raw_index)?;
            return Err(Self::invalid_operand(&instruction));
        };
        let frame = self.current_frame_mut()?;
        let index = usize::from(slot);
        let Some(value) = frame.locals.get(index).cloned() else {
            return Err(VmError::new(VmErrorKind::IndexOutOfBounds {
                space: VmIndexSpace::Local,
                owner: None,
                index: i64::from(slot),
                len: frame.locals.len(),
            }));
        };
        frame.stack.push(value);
        Ok(StepOutcome::Continue)
    }

    fn exec_fast_stloc(&mut self, runtime: &RuntimeInstruction) -> VmResult<StepOutcome> {
        let RuntimeOperand::Local(slot) = runtime.operand else {
            let instruction = self.current_raw_instruction(runtime.raw_index)?;
            return Err(Self::invalid_operand(&instruction));
        };
        let frame = self.current_frame_mut()?;
        let value = frame.stack.pop().ok_or_else(|| {
            VmError::new(VmErrorKind::StackEmpty {
                stack: VmStackKind::Operand,
            })
        })?;
        let index = usize::from(slot);
        let len = frame.locals.len();
        let Some(local) = frame.locals.get_mut(index) else {
            return Err(VmError::new(VmErrorKind::IndexOutOfBounds {
                space: VmIndexSpace::Local,
                owner: None,
                index: i64::from(slot),
                len,
            }));
        };
        *local = value;
        Ok(StepOutcome::Continue)
    }

    fn exec_fast_ldsmi(&mut self, runtime: &RuntimeInstruction) -> VmResult<StepOutcome> {
        let RuntimeOperand::I16(value) = runtime.operand else {
            let instruction = self.current_raw_instruction(runtime.raw_index)?;
            return Err(Self::invalid_operand(&instruction));
        };
        self.current_frame_mut()?
            .stack
            .push(Value::Int(i64::from(value)));
        Ok(StepOutcome::Continue)
    }

    fn exec_fast_ldglob(&mut self, runtime: &RuntimeInstruction) -> VmResult<StepOutcome> {
        let RuntimeOperand::Global(slot) = runtime.operand else {
            let instruction = self.current_raw_instruction(runtime.raw_index)?;
            return Err(Self::invalid_operand(&instruction));
        };
        let module_slot = self.current_module_slot()?;
        let raw_slot = usize::try_from(slot.raw()).unwrap_or(usize::MAX);
        let value = self
            .module(module_slot)?
            .globals
            .get(raw_slot)
            .cloned()
            .ok_or_else(|| {
                VmError::new(VmErrorKind::IndexOutOfBounds {
                    space: VmIndexSpace::Global,
                    owner: None,
                    index: i64::try_from(raw_slot).unwrap_or(i64::MAX),
                    len: self
                        .module(module_slot)
                        .map_or(0, |module| module.globals.len()),
                })
            })?;
        self.current_frame_mut()?.stack.push(value);
        Ok(StepOutcome::Continue)
    }

    fn exec_fast_stglob(&mut self, runtime: &RuntimeInstruction) -> VmResult<StepOutcome> {
        let RuntimeOperand::Global(slot) = runtime.operand else {
            let instruction = self.current_raw_instruction(runtime.raw_index)?;
            return Err(Self::invalid_operand(&instruction));
        };
        let value = self.pop_value()?;
        let module_slot = self.current_module_slot()?;
        let globals = &mut self.module_mut(module_slot)?.globals;
        let raw_slot = usize::try_from(slot.raw()).unwrap_or(usize::MAX);
        let len = globals.len();
        let Some(global) = globals.get_mut(raw_slot) else {
            return Err(VmError::new(VmErrorKind::IndexOutOfBounds {
                space: VmIndexSpace::Global,
                owner: None,
                index: i64::try_from(raw_slot).unwrap_or(i64::MAX),
                len,
            }));
        };
        *global = value;
        Ok(StepOutcome::Continue)
    }

    fn exec_fast_ldconst(&mut self, runtime: &RuntimeInstruction) -> VmResult<StepOutcome> {
        let RuntimeOperand::Constant(constant) = runtime.operand else {
            let instruction = self.current_raw_instruction(runtime.raw_index)?;
            return Err(Self::invalid_operand(&instruction));
        };
        let module_slot = self.current_module_slot()?;
        let constant_value = self
            .module(module_slot)?
            .program
            .artifact()
            .constants
            .get(constant)
            .value
            .clone();
        let value = self.constant_value(module_slot, &constant_value)?;
        self.current_frame_mut()?.stack.push(value);
        Ok(StepOutcome::Continue)
    }

    fn exec_fast_int_op(
        &mut self,
        runtime: &RuntimeInstruction,
        op: impl FnOnce(i64, i64) -> Option<i64>,
    ) -> VmResult<StepOutcome> {
        let frame = self.current_frame()?;
        let stack_len = frame.stack.len();
        let has_int_args = stack_len >= 2
            && matches!(frame.stack.get(stack_len - 1), Some(Value::Int(_)))
            && matches!(frame.stack.get(stack_len - 2), Some(Value::Int(_)));
        if !has_int_args {
            let instruction = self.current_raw_instruction(runtime.raw_index)?;
            return self.execute_instr(&instruction);
        }
        let frame = self.current_frame_mut()?;
        let right = pop_int_from_stack(frame)?;
        let left = pop_int_from_stack(frame)?;
        let result = op(left, right).ok_or_else(|| {
            VmError::new(VmErrorKind::ArithmeticFailed {
                detail: "signed integer overflow".into(),
            })
        })?;
        frame.stack.push(Value::Int(result));
        Ok(StepOutcome::Continue)
    }

    fn exec_fast_br(&mut self, runtime: &RuntimeInstruction) -> VmResult<StepOutcome> {
        let Some(target) = runtime.branch_target else {
            let instruction = self.current_raw_instruction(runtime.raw_index)?;
            return self.exec_branch(&instruction);
        };
        self.jump_to_ip(target)?;
        Ok(StepOutcome::Continue)
    }

    fn exec_fast_brfalse(&mut self, runtime: &RuntimeInstruction) -> VmResult<StepOutcome> {
        let Some(target) = runtime.branch_target else {
            let instruction = self.current_raw_instruction(runtime.raw_index)?;
            return self.exec_branch(&instruction);
        };
        let cond = self.pop_value()?;
        match self.bool_flag(&cond) {
            Some(false) => self.jump_to_ip(target)?,
            Some(true) => {}
            None => return Err(Self::invalid_value_kind(VmValueKind::Bool, &cond)),
        }
        Ok(StepOutcome::Continue)
    }

    fn exec_fast_call(&mut self, runtime: &RuntimeInstruction) -> VmResult<StepOutcome> {
        let RuntimeOperand::Procedure(procedure) = runtime.operand else {
            let instruction = self.current_raw_instruction(runtime.raw_index)?;
            return Err(Self::invalid_operand(&instruction));
        };
        let module_slot = self.current_module_slot()?;
        if runtime.call_mode == RuntimeCallMode::Tail && self.options.stack_frame_limit.is_none() {
            if let Some(shape) = runtime.call_shape {
                self.replace_frame_from_stack_with_shape(module_slot, procedure, shape)?;
            } else {
                self.replace_frame_from_stack(module_slot, procedure)?;
            }
        } else if let Some(shape) = runtime.call_shape {
            self.push_frame_from_stack_with_shape(module_slot, procedure, shape)?;
        } else {
            self.push_frame_from_stack(module_slot, procedure)?;
        }
        Ok(StepOutcome::Continue)
    }

    fn exec_fast_tail_call(&mut self, runtime: &RuntimeInstruction) -> VmResult<StepOutcome> {
        let RuntimeOperand::Procedure(procedure) = runtime.operand else {
            let instruction = self.current_raw_instruction(runtime.raw_index)?;
            return Err(Self::invalid_operand(&instruction));
        };
        let module_slot = self.current_module_slot()?;
        if let Some(shape) = runtime.call_shape {
            self.replace_frame_from_stack_with_shape(module_slot, procedure, shape)?;
        } else {
            self.replace_frame_from_stack(module_slot, procedure)?;
        }
        Ok(StepOutcome::Continue)
    }

    fn exec_compare_branch(&mut self, compare: CompareOp, target: usize) -> VmResult<StepOutcome> {
        let right = self.pop_value()?;
        let left = self.pop_value()?;
        if self.compare_for_branch(compare, &left, &right)? {
            self.skip_next_instruction()?;
        } else {
            self.jump_to_ip(target)?;
        }
        Ok(StepOutcome::Continue)
    }
}

impl Vm {
    fn compare_for_branch(
        &self,
        compare: CompareOp,
        left: &Value,
        right: &Value,
    ) -> VmResult<bool> {
        match compare {
            CompareOp::Eq => Ok(self.values_equal(left, right)),
            CompareOp::Ne => Ok(!self.values_equal(left, right)),
            CompareOp::Lt => self.compare_order_for_branch(left, right, Ordering::is_lt),
            CompareOp::Gt => self.compare_order_for_branch(left, right, Ordering::is_gt),
            CompareOp::Le => {
                self.compare_order_for_branch(left, right, |ordering| !ordering.is_gt())
            }
            CompareOp::Ge => {
                self.compare_order_for_branch(left, right, |ordering| !ordering.is_lt())
            }
        }
    }

    fn compare_order_for_branch(
        &self,
        left: &Value,
        right: &Value,
        op: impl FnOnce(Ordering) -> bool,
    ) -> VmResult<bool> {
        let ordering = match (left, right) {
            (Value::Int(left), Value::Int(right)) => left.cmp(right),
            (Value::Nat(left), Value::Nat(right)) => left.cmp(right),
            (Value::Float(left), Value::Float(right)) => left.total_cmp(right),
            (Value::String(left), Value::String(right)) => {
                self.heap.string(*left)?.cmp(self.heap.string(*right)?)
            }
            _ => return Err(Self::invalid_value_kind(left.kind(), right)),
        };
        Ok(op(ordering))
    }

    fn current_frame(&self) -> VmResult<&CallFrame> {
        self.frames.last().ok_or_else(|| {
            VmError::new(VmErrorKind::StackEmpty {
                stack: VmStackKind::CallFrame,
            })
        })
    }

    fn current_frame_mut(&mut self) -> VmResult<&mut CallFrame> {
        self.frames.last_mut().ok_or_else(|| {
            VmError::new(VmErrorKind::StackEmpty {
                stack: VmStackKind::CallFrame,
            })
        })
    }

    pub(crate) fn invalid_operand(instruction: &Instruction) -> VmError {
        VmError::new(VmErrorKind::InvalidOperandForOpcode {
            opcode: instruction.opcode,
            found: OperandShape::from(&instruction.operand),
        })
    }

    pub(crate) fn invalid_dispatch(instruction: &Instruction, family: &str) -> VmError {
        VmError::new(VmErrorKind::InvalidProgramShape {
            detail: format!(
                "opcode `{}` reached `{family}` executor",
                instruction.opcode.mnemonic()
            )
            .into(),
        })
    }
}

#[derive(Clone, Copy)]
struct SelfTailDecAccPlan {
    dec_local: u16,
    dec_smi: i16,
    acc_local: u16,
    add_local: u16,
    param_count: u16,
    mirror_local: Option<u16>,
    loop_ip: usize,
}

#[derive(Clone, Copy)]
struct NewObjInitPlan {
    field_local: u16,
    tag: i16,
    ty: TypeId,
    data_local: u16,
    match_local: u16,
    zero: i16,
    fallthrough: usize,
}

#[derive(Clone, Copy)]
struct Seq2ConstSetPlan {
    local: u16,
    first: i16,
    second: i16,
    value: i16,
    scratch: u16,
    scratch_value: i16,
    fallthrough: usize,
}

#[derive(Clone, Copy)]
struct Seq2GetAddSetPlan {
    target: u16,
    target_first: i16,
    target_second: i16,
    source: u16,
    source_first: i16,
    source_second: i16,
    add: i16,
    scratch: u16,
    scratch_value: i16,
    fallthrough: usize,
}

#[derive(Clone, Copy)]
struct Seq2GetAddPlan {
    left: u16,
    left_first: i16,
    left_second: i16,
    right: u16,
    right_first: i16,
    right_second: i16,
    fallthrough: usize,
}

#[inline]
fn int_from_value(value: &Value) -> VmResult<i64> {
    match value {
        Value::Int(value) => Ok(*value),
        Value::Nat(number) => i64::try_from(*number)
            .map_err(|_| Vm::invalid_value_kind(VmValueKind::Int, &Value::Nat(*number))),
        other => Err(Vm::invalid_value_kind(VmValueKind::Int, other)),
    }
}

#[inline]
fn int_overflow_error() -> VmError {
    VmError::new(VmErrorKind::ArithmeticFailed {
        detail: "signed integer overflow".into(),
    })
}

#[inline]
fn sequence_index(index: i64, len: usize) -> VmResult<usize> {
    let Ok(slot) = usize::try_from(index) else {
        return Err(sequence_index_error(index, len));
    };
    if slot >= len {
        return Err(sequence_index_error(index, len));
    }
    Ok(slot)
}

#[inline]
const fn sequence_index_error(index: i64, len: usize) -> VmError {
    VmError::new(VmErrorKind::InvalidSequenceIndex { index, len })
}

fn fused_dispatch_error(family: &str) -> VmError {
    VmError::new(VmErrorKind::InvalidProgramShape {
        detail: format!("fused opcode reached `{family}` executor").into(),
    })
}

#[inline]
fn local_index_error(slot: u16, len: usize) -> VmError {
    VmError::new(VmErrorKind::IndexOutOfBounds {
        space: VmIndexSpace::Local,
        owner: None,
        index: i64::from(slot),
        len,
    })
}

#[inline]
const fn compare_int_for_branch(compare: CompareOp, left: i64, right: i64) -> bool {
    match compare {
        CompareOp::Eq => left == right,
        CompareOp::Ne => left != right,
        CompareOp::Lt => left < right,
        CompareOp::Gt => left > right,
        CompareOp::Le => left <= right,
        CompareOp::Ge => left >= right,
    }
}

fn pop_int_from_stack(frame: &mut CallFrame) -> VmResult<i64> {
    let value = frame.stack.pop().ok_or_else(|| {
        VmError::new(VmErrorKind::StackEmpty {
            stack: VmStackKind::Operand,
        })
    })?;
    match value {
        Value::Int(value) => Ok(value),
        Value::Nat(value) => i64::try_from(value)
            .map_err(|_| Vm::invalid_value_kind(VmValueKind::Int, &Value::Nat(value))),
        other => Err(Vm::invalid_value_kind(VmValueKind::Int, &other)),
    }
}

fn local_int_from_frame(frame: &CallFrame, slot: u16) -> VmResult<i64> {
    let index = usize::from(slot);
    let Some(value) = frame.locals.get(index) else {
        return Err(VmError::new(VmErrorKind::IndexOutOfBounds {
            space: VmIndexSpace::Local,
            owner: None,
            index: i64::from(slot),
            len: frame.locals.len(),
        }));
    };
    match value {
        Value::Int(value) => Ok(*value),
        Value::Nat(number) => i64::try_from(*number)
            .map_err(|_| Vm::invalid_value_kind(VmValueKind::Int, &Value::Nat(*number))),
        other => Err(Vm::invalid_value_kind(VmValueKind::Int, other)),
    }
}
