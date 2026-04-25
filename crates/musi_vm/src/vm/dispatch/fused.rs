use super::*;

mod control;
mod data;
mod locals;
mod sequence;

impl Vm {
    pub(super) fn exec_fused(&mut self, fused: RuntimeFusedOp) -> VmResult<StepOutcome> {
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
}
