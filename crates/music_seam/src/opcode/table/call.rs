use crate::instruction::OperandShape;

use super::super::{Opcode, OpcodeFamily};
use super::info::{OpcodeInfo, opcode_info};

pub(super) const OPCODES: &[OpcodeInfo] = &[
    opcode_info(
        Opcode::Call,
        OpcodeFamily::Call,
        "call",
        OperandShape::Procedure,
        0x50,
    ),
    opcode_info(
        Opcode::CallInd,
        OpcodeFamily::Call,
        "call.ind",
        OperandShape::None,
        0x51,
    ),
    opcode_info(
        Opcode::CallFfi,
        OpcodeFamily::Call,
        "call.ffi",
        OperandShape::Foreign,
        0x55,
    ),
    opcode_info(
        Opcode::TailCall,
        OpcodeFamily::Call,
        "call.tail",
        OperandShape::Procedure,
        0x56,
    ),
    opcode_info(
        Opcode::NewFn,
        OpcodeFamily::Call,
        "new.fn",
        OperandShape::WideProcedureCaptures,
        0x5D,
    ),
    opcode_info(
        Opcode::LdFfi,
        OpcodeFamily::Call,
        "ld.ffi",
        OperandShape::Foreign,
        0x61,
    ),
];
