use crate::instruction::OperandShape;

use super::super::{Opcode, OpcodeFamily};
use super::info::{OpcodeInfo, opcode_info};

pub(super) const OPCODES: &[OpcodeInfo] = &[
    opcode_info(
        Opcode::HdlPush,
        OpcodeFamily::Effect,
        "hdl.push",
        OperandShape::EffectId,
        0xA0,
    ),
    opcode_info(
        Opcode::HdlPop,
        OpcodeFamily::Effect,
        "hdl.pop",
        OperandShape::None,
        0xA1,
    ),
    opcode_info(
        Opcode::Raise,
        OpcodeFamily::Effect,
        "raise",
        OperandShape::Effect,
        0xA2,
    ),
    opcode_info(
        Opcode::Resume,
        OpcodeFamily::Effect,
        "resume",
        OperandShape::None,
        0xA3,
    ),
    opcode_info(
        Opcode::DropCont,
        OpcodeFamily::Effect,
        "drop.cont",
        OperandShape::None,
        0xA4,
    ),
];
