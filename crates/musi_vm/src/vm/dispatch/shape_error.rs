use super::*;

impl Vm {
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

pub(super) fn fused_dispatch_error(family: &str) -> VmError {
    VmError::new(VmErrorKind::InvalidProgramShape {
        detail: format!("fused opcode reached `{family}` executor").into(),
    })
}
