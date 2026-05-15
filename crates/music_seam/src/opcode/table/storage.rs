use crate::instruction::OperandShape;

use super::super::{Opcode, OpcodeFamily};
use super::info::{OpcodeInfo, opcode_info};

pub(super) const OPCODES: &[OpcodeInfo] = &[
    opcode_info(
        Opcode::LdLoc,
        OpcodeFamily::Storage,
        "ld.loc",
        OperandShape::Local,
        0x12,
    ),
    opcode_info(
        Opcode::StLoc,
        OpcodeFamily::Storage,
        "st.loc",
        OperandShape::Local,
        0x13,
    ),
    opcode_info(
        Opcode::LdGlob,
        OpcodeFamily::Storage,
        "ld.glob",
        OperandShape::Global,
        0x14,
    ),
    opcode_info(
        Opcode::StGlob,
        OpcodeFamily::Storage,
        "st.glob",
        OperandShape::Global,
        0x15,
    ),
    opcode_info(
        Opcode::LdFld,
        OpcodeFamily::Storage,
        "ld.fld",
        OperandShape::I16,
        0x16,
    ),
    opcode_info(
        Opcode::StFld,
        OpcodeFamily::Storage,
        "st.fld",
        OperandShape::I16,
        0x17,
    ),
];
