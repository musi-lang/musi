//! `musi_bytecode` — shared `.msbc` bytecode format definitions.
//!
//! Defines the opcode set, instruction encoding functions, and CRC-32
//! checksum used by both the `music_emit` compiler crate and the
//! `musi_vm` runtime crate.

mod crc32;
mod opcode;

pub use crc32::crc32_slice;
pub use opcode::{
    Opcode, encode_i32, encode_no_operand, encode_u8, encode_u16, encode_u32, instr_len,
};
