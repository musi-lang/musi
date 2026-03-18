//! `msc_bc` - shared `.muse` bytecode format definitions.
//!
//! Defines the SEAM opcode set, instruction encoding functions, and CRC-32
//! checksum used by both the `msc_emit` compiler crate and the
//! `msc_vm` runtime crate.

mod crc32;
mod disasm;
mod encoding;
mod opcode;

pub use crc32::crc32_slice;
pub use disasm::disassemble;
pub use encoding::{encode_f0, encode_fi8, encode_fi8x2, encode_fi16, encode_fi24};
pub use opcode::{Format, OPCODE_NAMES, Opcode, format, instr_len};
