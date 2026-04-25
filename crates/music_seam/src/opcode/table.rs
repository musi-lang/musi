use crate::instruction::OperandShape;

use self::info::{EXTENDED_OPCODE_PREFIX, OpcodeInfo};
use super::*;

mod branch;
mod call;
mod core;
mod effects;
mod info;
mod module;
mod object;
mod refs;
mod scalar;
mod storage;
mod types;

use branch::OPCODES as BRANCH_OPCODES;
use call::OPCODES as CALL_OPCODES;
use core::OPCODES as CORE_OPCODES;
use effects::OPCODES as EFFECT_OPCODES;
use module::OPCODES as MODULE_OPCODES;
use object::OPCODES as OBJECT_OPCODES;
use refs::OPCODES as REF_OPCODES;
use scalar::OPCODES as SCALAR_OPCODES;
use storage::OPCODES as STORAGE_OPCODES;
use types::OPCODES as TYPE_OPCODES;

const OPCODE_TABLES: &[&[OpcodeInfo]] = &[
    CORE_OPCODES,
    STORAGE_OPCODES,
    SCALAR_OPCODES,
    BRANCH_OPCODES,
    CALL_OPCODES,
    OBJECT_OPCODES,
    TYPE_OPCODES,
    REF_OPCODES,
    EFFECT_OPCODES,
    MODULE_OPCODES,
];

impl Opcode {
    #[must_use]
    pub fn family(self) -> OpcodeFamily {
        self.info().family
    }
    #[must_use]
    pub fn mnemonic(self) -> &'static str {
        self.info().mnemonic
    }
    #[must_use]
    pub fn operand_shape(self) -> OperandShape {
        self.info().operand_shape
    }
    #[must_use]
    pub fn wire_code(self) -> u16 {
        match self.info().wire {
            OpcodeWire::Core(code) => u16::from(code),
            OpcodeWire::Extended(code) => code,
        }
    }
    #[must_use]
    pub fn wire(self) -> OpcodeWire {
        self.info().wire
    }
    #[must_use]
    pub const fn extended_opcode_prefix() -> u8 {
        EXTENDED_OPCODE_PREFIX
    }
    #[must_use]
    pub fn from_mnemonic(text: &str) -> Option<Self> {
        opcode_infos()
            .find(|opcode_spec| opcode_spec.mnemonic == text)
            .map(|opcode_spec| opcode_spec.opcode)
    }
    #[must_use]
    pub fn from_wire_code(code: u16) -> Option<Self> {
        opcode_infos()
            .find(|opcode_spec| match opcode_spec.wire {
                OpcodeWire::Core(core_wire) => u16::from(core_wire) == code,
                OpcodeWire::Extended(extended) => extended == code,
            })
            .map(|opcode_spec| opcode_spec.opcode)
    }
    fn info(self) -> &'static OpcodeInfo {
        opcode_infos()
            .find(|opcode_spec| opcode_spec.opcode == self)
            .unwrap_or_else(first_opcode_info)
    }
}

pub(super) fn opcode_infos() -> impl Iterator<Item = &'static OpcodeInfo> {
    OPCODE_TABLES.iter().flat_map(|table| table.iter())
}

#[cfg(test)]
pub(super) fn opcode_info_count() -> usize {
    OPCODE_TABLES.iter().map(|table| table.len()).sum()
}

fn first_opcode_info() -> &'static OpcodeInfo {
    &OPCODE_TABLES[0][0]
}
