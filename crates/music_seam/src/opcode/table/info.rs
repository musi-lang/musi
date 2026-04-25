use crate::instruction::OperandShape;

use super::super::{Opcode, OpcodeFamily, OpcodeWire};

pub(super) const EXTENDED_OPCODE_PREFIX: u8 = 0xFF;

pub(in crate::opcode) struct OpcodeInfo {
    pub(in crate::opcode) opcode: Opcode,
    pub(in crate::opcode) family: OpcodeFamily,
    pub(in crate::opcode) mnemonic: &'static str,
    pub(in crate::opcode) operand_shape: OperandShape,
    pub(in crate::opcode) wire: OpcodeWire,
}

pub(super) const fn opcode_info(
    opcode: Opcode,
    family: OpcodeFamily,
    mnemonic: &'static str,
    operand_shape: OperandShape,
    code: u8,
) -> OpcodeInfo {
    OpcodeInfo {
        opcode,
        family,
        mnemonic,
        operand_shape,
        wire: OpcodeWire::Core(code),
    }
}
