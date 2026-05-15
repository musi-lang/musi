use crate::instruction::OperandShape;

use super::super::{Opcode, OpcodeFamily};
use super::info::{OpcodeInfo, opcode_info};

pub(super) const OPCODES: &[OpcodeInfo] = &[
    opcode_info(
        Opcode::NewObj,
        OpcodeFamily::Object,
        "new.obj",
        OperandShape::TypeLen,
        0x70,
    ),
    opcode_info(
        Opcode::NewArr,
        OpcodeFamily::Object,
        "new.arr",
        OperandShape::TypeLen,
        0x71,
    ),
    opcode_info(
        Opcode::LdElem,
        OpcodeFamily::Object,
        "ld.elem",
        OperandShape::None,
        0x73,
    ),
    opcode_info(
        Opcode::StElem,
        OpcodeFamily::Object,
        "st.elem",
        OperandShape::None,
        0x74,
    ),
    opcode_info(
        Opcode::LdLen,
        OpcodeFamily::Object,
        "ld.len",
        OperandShape::None,
        0x76,
    ),
];
