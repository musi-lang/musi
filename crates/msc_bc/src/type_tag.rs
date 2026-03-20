//! Type tag constants for the Musi bytecode type section (§11.3).
//!
//! Every primitive and compound type emitted into a `.seam` binary carries one
//! of these single-byte tags.  Both the emitter (`msc_emit`) and the VM
//! (`msc_vm`) reference this module so the values stay in sync automatically.

pub const TAG_UNIT: u8 = 0x01;
pub const TAG_BOOL: u8 = 0x02;
pub const TAG_I8: u8 = 0x03;
pub const TAG_I16: u8 = 0x04;
pub const TAG_I32: u8 = 0x05;
pub const TAG_I64: u8 = 0x06;
pub const TAG_U8: u8 = 0x07;
pub const TAG_U16: u8 = 0x08;
pub const TAG_U32: u8 = 0x09;
pub const TAG_U64: u8 = 0x0A;
pub const TAG_F32: u8 = 0x0B;
pub const TAG_F64: u8 = 0x0C;
pub const TAG_RUNE: u8 = 0x0D;
pub const TAG_PTR: u8 = 0x0E;
pub const TAG_ARR: u8 = 0x0F;
pub const TAG_PRODUCT: u8 = 0x10;
pub const TAG_SUM: u8 = 0x11;
pub const TAG_FN: u8 = 0x12;
pub const TAG_REF: u8 = 0x13;
pub const TAG_ANY: u8 = 0x14;
pub const TAG_CSTRUCT: u8 = 0x15;
