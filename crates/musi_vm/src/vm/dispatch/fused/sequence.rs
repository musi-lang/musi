use super::super::int::int_overflow_error;
use super::super::shape_error::fused_dispatch_error;
use super::super::*;

impl Vm {
    pub(super) fn exec_fused_seq(&mut self, fused: RuntimeFusedOp) -> VmResult<StepOutcome> {
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
