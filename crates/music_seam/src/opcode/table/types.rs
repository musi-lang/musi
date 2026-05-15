use crate::instruction::OperandShape;

use super::super::{Opcode, OpcodeFamily};
use super::info::{OpcodeInfo, opcode_info};

pub(super) const OPCODES: &[OpcodeInfo] = &[
    opcode_info(
        Opcode::LdType,
        OpcodeFamily::Type,
        "ld.type",
        OperandShape::Type,
        0x80,
    ),
    opcode_info(
        Opcode::IsInst,
        OpcodeFamily::Type,
        "is.inst",
        OperandShape::Type,
        0x82,
    ),
    opcode_info(
        Opcode::Cast,
        OpcodeFamily::Type,
        "cast",
        OperandShape::Type,
        0x83,
    ),
];
