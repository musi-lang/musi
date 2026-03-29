use super::*;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Instruction {
    pub opcode: Opcode,
    pub operand: Operand,
}

impl Instruction {
    #[must_use]
    pub const fn basic(opcode: Opcode) -> Self {
        Self {
            opcode,
            operand: Operand::None,
        }
    }

    #[must_use]
    pub const fn with_u8(opcode: Opcode, value: u8) -> Self {
        Self {
            opcode,
            operand: Operand::U8(value),
        }
    }

    #[must_use]
    pub const fn with_u16(opcode: Opcode, value: u16) -> Self {
        Self {
            opcode,
            operand: Operand::U16(value),
        }
    }

    #[must_use]
    pub const fn with_i16(opcode: Opcode, value: i16) -> Self {
        Self {
            opcode,
            operand: Operand::I16(value),
        }
    }

    #[must_use]
    pub const fn with_wide(opcode: Opcode, primary: u16, secondary: u8) -> Self {
        Self {
            opcode,
            operand: Operand::Wide(primary, secondary),
        }
    }

    #[must_use]
    pub const fn with_type_len(opcode: Opcode, type_id: u16, length: u16) -> Self {
        Self {
            opcode,
            operand: Operand::TypeLen(type_id, length),
        }
    }

    #[must_use]
    pub const fn with_effect(opcode: Opcode, effect_id: u16, op_id: u16) -> Self {
        Self {
            opcode,
            operand: Operand::Effect(effect_id, op_id),
        }
    }

    #[must_use]
    pub const fn with_effect_jump(opcode: Opcode, effect_id: u16, op_id: u16, jump: i16) -> Self {
        Self {
            opcode,
            operand: Operand::EffectJump(effect_id, op_id, jump),
        }
    }

    #[must_use]
    pub const fn with_table(opcode: Opcode, offsets: BranchOffsets) -> Self {
        Self {
            opcode,
            operand: Operand::BranchTable(offsets),
        }
    }
}
