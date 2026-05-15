use crate::instruction::OperandShape;

use super::super::{Opcode, OpcodeFamily};
use super::info::{OpcodeInfo, opcode_info};

pub(super) const OPCODES: &[OpcodeInfo] = &[
    opcode_info(
        Opcode::LdModDyn,
        OpcodeFamily::Module,
        "ld.mod.dyn",
        OperandShape::None,
        0xB2,
    ),
    opcode_info(
        Opcode::LdExpDyn,
        OpcodeFamily::Module,
        "ld.exp.dyn",
        OperandShape::String,
        0xB3,
    ),
];
