use crate::instruction::OperandShape;

use super::super::{Opcode, OpcodeFamily};
use super::info::{OpcodeInfo, opcode_info};

pub(super) const OPCODES: &[OpcodeInfo] = &[
    opcode_info(
        Opcode::LdC,
        OpcodeFamily::Core,
        "ld.c",
        OperandShape::Constant,
        0x09,
    ),
    opcode_info(
        Opcode::LdCI4,
        OpcodeFamily::Core,
        "ld.c.i4",
        OperandShape::I16,
        0x0A,
    ),
    opcode_info(
        Opcode::LdStr,
        OpcodeFamily::Core,
        "ld.str",
        OperandShape::String,
        0x0E,
    ),
];
