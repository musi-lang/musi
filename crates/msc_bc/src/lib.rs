//! `msc_bc` - shared `.muse` bytecode format definitions.
//!
//! Defines the opcode set, instruction encoding functions, and CRC-32
//! checksum used by both the `msc_emit` compiler crate and the
//! `msc_vm` runtime crate.

mod crc32;
mod disasm;
mod opcode;

pub use crc32::crc32_slice;
pub use disasm::disassemble;
pub use opcode::{
    Opcode, encode_i8, encode_i32, encode_no_operand, encode_u8, encode_u16, encode_u32,
    encode_wid, encode_wid_u16, encode_wid_u32, instr_len, pack_id_arity, pack_tag_arity_u16,
    unpack_id_arity, unpack_tag_arity_u16, unpack_tag_arity_u32, widened_operand_size,
    zone_operand_size,
};
