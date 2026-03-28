use crate::opcode::Opcode;

/// A single SEAM bytecode instruction: an opcode paired with its operand.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Instruction {
    pub opcode: Opcode,
    pub operand: Operand,
}

/// The operand payload for a SEAM instruction.
///
/// Most opcodes use `None`, `U8`, `U16`, or `I16`. Compound operands exist
/// for closure creation (`Wide`) , tagged array construction (`Tagged`),
/// and branch tables (`Table`).
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Operand {
    /// No operand (stack-only instructions).
    None,
    /// Single byte: local index, upvalue index, arity.
    U8(u8),
    /// Signed 16-bit: branch offsets, small integer immediates.
    I16(i16),
    /// Unsigned 16-bit: constant pool index, global index, array length.
    U16(u16),
    /// Two-part operand: `cls.new fn_ref(u16) + upval_count(u8)`.
    Wide(u16, u8),
    /// Two-part operand: `arr.newt tag(u8) + length(u16)`.
    Tagged(u8, u16),
    /// Two-part operand: `perf effect_id(u16) + op_id(u16)`.
    Effect(u16, u16),
    /// Two-part operand: `hndl.push index(u16) + jump_offset(i16)`.
    IndexedJump(u16, i16),
    /// Three-part operand: `hndl.push effect_id(u16) + op_id(u16) + jump_offset(i16)`.
    EffectJump(u16, u16, i16),
    /// Variable-length: `br.tbl` offset table.
    Table(Vec<i16>),
}

impl Instruction {
    /// An instruction with no operand.
    #[must_use]
    pub const fn simple(opcode: Opcode) -> Self {
        Self {
            opcode,
            operand: Operand::None,
        }
    }

    /// An instruction with a single `u8` operand.
    #[must_use]
    pub const fn with_u8(opcode: Opcode, val: u8) -> Self {
        Self {
            opcode,
            operand: Operand::U8(val),
        }
    }

    /// An instruction with a `u16` operand.
    #[must_use]
    pub const fn with_u16(opcode: Opcode, val: u16) -> Self {
        Self {
            opcode,
            operand: Operand::U16(val),
        }
    }

    /// An instruction with an `i16` operand.
    #[must_use]
    pub const fn with_i16(opcode: Opcode, val: i16) -> Self {
        Self {
            opcode,
            operand: Operand::I16(val),
        }
    }

    /// An instruction with a wide operand (u16 + u8).
    #[must_use]
    pub const fn with_wide(opcode: Opcode, primary: u16, secondary: u8) -> Self {
        Self {
            opcode,
            operand: Operand::Wide(primary, secondary),
        }
    }

    /// An instruction with a tagged operand (u8 tag + u16 length).
    #[must_use]
    pub const fn with_tagged(opcode: Opcode, tag: u8, length: u16) -> Self {
        Self {
            opcode,
            operand: Operand::Tagged(tag, length),
        }
    }

    /// An instruction with an effect operand (u16 + u16).
    #[must_use]
    pub const fn with_effect(opcode: Opcode, effect_id: u16, op_id: u16) -> Self {
        Self {
            opcode,
            operand: Operand::Effect(effect_id, op_id),
        }
    }

    /// An instruction with an indexed jump operand (u16 index + i16 jump offset).
    #[must_use]
    pub const fn with_indexed_jump(opcode: Opcode, idx: u16, jump: i16) -> Self {
        Self {
            opcode,
            operand: Operand::IndexedJump(idx, jump),
        }
    }

    /// An instruction with an effect jump operand (u16 + u16 + i16).
    #[must_use]
    pub const fn with_effect_jump(opcode: Opcode, effect_id: u16, op_id: u16, jump: i16) -> Self {
        Self {
            opcode,
            operand: Operand::EffectJump(effect_id, op_id, jump),
        }
    }

    /// An instruction with a variable-length offset table.
    #[must_use]
    pub const fn with_table(opcode: Opcode, offsets: Vec<i16>) -> Self {
        Self {
            opcode,
            operand: Operand::Table(offsets),
        }
    }
}

#[cfg(test)]
#[allow(clippy::unwrap_used, clippy::panic)]
mod tests;
