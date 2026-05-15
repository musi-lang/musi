use crate::instruction::OperandShape;

use super::super::{Opcode, OpcodeFamily};
use super::info::{OpcodeInfo, opcode_info};

pub(super) const OPCODES: &[OpcodeInfo] = &[
    opcode_info(
        Opcode::Br,
        OpcodeFamily::Branch,
        "br",
        OperandShape::Label,
        0x42,
    ),
    opcode_info(
        Opcode::BrZ,
        OpcodeFamily::Branch,
        "br.z",
        OperandShape::Label,
        0x44,
    ),
    opcode_info(
        Opcode::BrTbl,
        OpcodeFamily::Branch,
        "br.tbl",
        OperandShape::BranchTable,
        0x45,
    ),
    opcode_info(
        Opcode::Ret,
        OpcodeFamily::Branch,
        "ret",
        OperandShape::None,
        0x47,
    ),
];
