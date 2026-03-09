//! Opcode definitions and instruction encoding.
//!
//! High two bits of opcode determine instruction length:
//! - `0x00–0x3F`: 1 byte (no operand)
//! - `0x40–0x7F`: 2 bytes (u8 operand)
//! - `0x80–0xBF`: 3 bytes (u16 LE operand)
//! - `0xC0–0xFF`: 5 bytes (u32 LE operand)

/// A bytecode opcode (newtype around the raw byte value).
///
/// Using a tuple struct rather than a `#[repr(u8)]` enum avoids `as` casts
/// while retaining named constants.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Opcode(pub u8);

#[allow(dead_code)]
impl Opcode {
    // §0  Control / Stack
    pub const NOP: Self = Self(0x00);
    pub const HLT: Self = Self(0x01);
    pub const RET: Self = Self(0x02);
    pub const RET_U: Self = Self(0x03);
    pub const UNR: Self = Self(0x04);
    pub const BRK: Self = Self(0x05);
    pub const DUP: Self = Self(0x06);
    pub const POP: Self = Self(0x07);
    pub const SWP: Self = Self(0x08);

    // §1  Integer Arithmetic (overflow wraps)
    pub const I_ADD: Self = Self(0x10);
    pub const I_ADD_UN: Self = Self(0x11);
    pub const I_SUB: Self = Self(0x12);
    pub const I_SUB_UN: Self = Self(0x13);
    pub const I_MUL: Self = Self(0x14);
    pub const I_MUL_UN: Self = Self(0x15);
    pub const I_DIV: Self = Self(0x16);
    pub const I_DIV_UN: Self = Self(0x17);
    pub const I_REM: Self = Self(0x18);
    pub const I_REM_UN: Self = Self(0x19);
    pub const I_NEG: Self = Self(0x1A);

    // §2  Float Arithmetic (IEEE 754)
    pub const F_ADD: Self = Self(0x20);
    pub const F_SUB: Self = Self(0x21);
    pub const F_MUL: Self = Self(0x22);
    pub const F_DIV: Self = Self(0x23);
    pub const F_REM: Self = Self(0x24);
    pub const F_NEG: Self = Self(0x25);

    // §3  Bitwise / Logical
    pub const B_AND: Self = Self(0x30);
    pub const B_OR: Self = Self(0x31);
    pub const B_XOR: Self = Self(0x32);
    pub const B_NOT: Self = Self(0x33);
    pub const B_SHL: Self = Self(0x34);
    pub const B_SHR: Self = Self(0x36);
    pub const B_SHR_UN: Self = Self(0x37);

    // §4  Equality
    pub const CMP_EQ: Self = Self(0x3B);
    pub const CMP_NE: Self = Self(0x3C);

    // §5  Locals / Constants / Structures (u8 operand)
    pub const LD_LOC: Self = Self(0x40);
    pub const ST_LOC: Self = Self(0x41);
    pub const LD_CST: Self = Self(0x42);
    pub const ST_FLD: Self = Self(0x43);
    pub const MK_PRD: Self = Self(0x44);
    pub const GET_FLD: Self = Self(0x45);
    pub const MK_VAR: Self = Self(0x46);
    pub const GET_PAY: Self = Self(0x47);
    pub const CMP_TAG: Self = Self(0x48);
    pub const CNV_WDN: Self = Self(0x49);
    pub const CNV_WDN_UN: Self = Self(0x4A);
    pub const CNV_NRW: Self = Self(0x4B);
    pub const EFF_PSH: Self = Self(0x4C);
    pub const EFF_POP: Self = Self(0x4D);

    // §6  Ordered Comparison (no operand)
    pub const CMP_LT: Self = Self(0x50);
    pub const CMP_LT_UN: Self = Self(0x51);
    pub const CMP_LE: Self = Self(0x52);
    pub const CMP_LE_UN: Self = Self(0x53);
    pub const CMP_GT: Self = Self(0x54);
    pub const CMP_GT_UN: Self = Self(0x55);
    pub const CMP_GE: Self = Self(0x56);
    pub const CMP_GE_UN: Self = Self(0x57);

    // §7  Float Comparison
    pub const CMP_F_EQ: Self = Self(0x58);
    pub const CMP_F_NE: Self = Self(0x59);
    pub const CMP_F_LT: Self = Self(0x5A);
    pub const CMP_F_LE: Self = Self(0x5B);
    pub const CMP_F_GT: Self = Self(0x5C);
    pub const CMP_F_GE: Self = Self(0x5D);

    // §8  Conversion
    pub const CNV_ITF: Self = Self(0x5E);
    pub const CNV_FTI: Self = Self(0x5F);
    pub const CNV_TRM: Self = Self(0x60);

    // §9  Structural / Array / Effects
    pub const GET_TAG: Self = Self(0x61);
    pub const GET_LEN: Self = Self(0x62);
    pub const LD_IDX: Self = Self(0x63);
    pub const ST_IDX: Self = Self(0x64);
    pub const FRE: Self = Self(0x65);
    pub const EFF_RES_C: Self = Self(0x66);
    pub const EFF_ABT: Self = Self(0x67);
    pub const TSK_AWT: Self = Self(0x68);
    /// Indirect (dynamic) call through a closure or fn value. u8 = arg count.
    pub const INV_DYN: Self = Self(0x69);

    // §10  Wide Locals / Jumps (u16 operand)
    pub const LD_LOC_W: Self = Self(0x80);
    pub const ST_LOC_W: Self = Self(0x81);
    pub const LD_CST_W: Self = Self(0x82);
    pub const ST_FLD_W: Self = Self(0x83);
    pub const MK_VAR_W: Self = Self(0x84);
    pub const CMP_TAG_W: Self = Self(0x85);
    pub const JMP: Self = Self(0x86);
    pub const JMP_T: Self = Self(0x87);
    pub const JMP_F: Self = Self(0x88);

    // §11  Invocation (u32 operand)
    pub const INV: Self = Self(0xC0);
    pub const INV_EFF: Self = Self(0xC1);
    pub const INV_TAL: Self = Self(0xC2);
    pub const INV_TAL_EFF: Self = Self(0xC3);

    // §12  Globals / Allocation (u32 operand)
    pub const LD_GLB: Self = Self(0xC4);
    pub const ST_GLB: Self = Self(0xC5);
    pub const MK_ARR: Self = Self(0xC6);
    pub const ALC_REF: Self = Self(0xC7);
    pub const ALC_MAN: Self = Self(0xC8);
    pub const ALC_ARN: Self = Self(0xC9);

    // §13  Effects (u32 operand)
    pub const EFF_DO: Self = Self(0xCA);
    pub const EFF_RES: Self = Self(0xCB);

    // §14  Concurrency (u32 operand)
    pub const TSK_SPN: Self = Self(0xCC);
    pub const TSK_CHS: Self = Self(0xCD);
    pub const TSK_CHR: Self = Self(0xCE);
    pub const TSK_CMK: Self = Self(0xCF);

    // §15  Wide Jumps (u32 operand)
    pub const JMP_W: Self = Self(0xD0);
    pub const JMP_T_W: Self = Self(0xD1);
    pub const JMP_F_W: Self = Self(0xD2);
}

/// Encode a no-operand instruction into `buf`.
pub fn encode_no_operand(buf: &mut Vec<u8>, op: Opcode) {
    buf.push(op.0);
}

/// Encode a u8-operand instruction into `buf`.
pub fn encode_u8(buf: &mut Vec<u8>, op: Opcode, operand: u8) {
    buf.push(op.0);
    buf.push(operand);
}

/// Encode a u16-operand instruction (LE) into `buf`.
pub fn encode_u16(buf: &mut Vec<u8>, op: Opcode, operand: u16) {
    buf.push(op.0);
    buf.extend_from_slice(&operand.to_le_bytes());
}

/// Encode a u32-operand instruction (LE) into `buf`.
pub fn encode_u32(buf: &mut Vec<u8>, op: Opcode, operand: u32) {
    buf.push(op.0);
    buf.extend_from_slice(&operand.to_le_bytes());
}

/// Encode an i32-operand instruction (LE) into `buf`.
pub fn encode_i32(buf: &mut Vec<u8>, op: Opcode, operand: i32) {
    buf.push(op.0);
    buf.extend_from_slice(&operand.to_le_bytes());
}
