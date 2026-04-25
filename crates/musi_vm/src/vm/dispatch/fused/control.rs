use super::super::frame::local_index_error;
use super::super::int::{compare_int_for_branch, int_from_value, int_overflow_error};
use super::super::shape_error::fused_dispatch_error;
use super::super::*;

impl Vm {
    pub(super) fn exec_fused_control(&mut self, fused: RuntimeFusedOp) -> VmResult<StepOutcome> {
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
