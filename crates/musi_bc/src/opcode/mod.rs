//! Opcode definitions and instruction encoding.
//!
//! High two bits of opcode determine instruction length:
//! - `0x00–0x3F`: 1 byte (no operand)
//! - `0x40–0x7F`: 2 bytes (u8 operand)
//! - `0x80–0xBF`: 3 bytes (u16 LE operand)
//! - `0xC0–0xFD`: 5 bytes (u32 LE operand)
//!
//! Special bytes:
//! - `0xFE` (`WID`): Wide prefix — widens next opcode's operand by one step
//! - `0xFF` (`EXT`): Extension prefix — next byte is an extended opcode

use core::fmt;

/// A bytecode opcode (newtype around the raw byte value).
///
/// Using a tuple struct rather than a `#[repr(u8)]` enum avoids `as` casts
/// while retaining named constants.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Opcode(pub u8);

impl Opcode {
    // ── Zone 0 — No operand (0x00–0x3F) ──

    // §CTL — Control (0x00–0x05)
    pub const NOP: Self = Self(0x00);
    pub const HLT: Self = Self(0x01);
    pub const RET: Self = Self(0x02);
    pub const RET_UT: Self = Self(0x03);
    pub const UNR: Self = Self(0x04);
    pub const BRK: Self = Self(0x05);

    // §STK — Stack (0x06–0x09)
    pub const DUP: Self = Self(0x06);
    pub const POP: Self = Self(0x07);
    pub const SWP: Self = Self(0x08);
    pub const LD_UT: Self = Self(0x09);

    // §DAT — Data (0x0A–0x0F)
    pub const LD_TAG: Self = Self(0x0A);
    pub const LD_LEN: Self = Self(0x0B);
    pub const LD_IDX: Self = Self(0x0C);
    pub const ST_IDX: Self = Self(0x0D);
    pub const TSK_AWT: Self = Self(0x0E);
    // 0x0F: reserved

    // §INT — Signed integer arithmetic (0x10–0x17)
    pub const INT_ADD: Self = Self(0x10);
    pub const INT_SUB: Self = Self(0x11);
    pub const INT_MUL: Self = Self(0x12);
    pub const INT_DIV: Self = Self(0x13);
    pub const INT_REM: Self = Self(0x14);
    pub const INT_NEG: Self = Self(0x15);
    // 0x16–0x17: reserved

    // §NAT — Unsigned integer arithmetic (0x18–0x1F)
    pub const NAT_ADD: Self = Self(0x18);
    pub const NAT_SUB: Self = Self(0x19);
    pub const NAT_MUL: Self = Self(0x1A);
    pub const NAT_DIV: Self = Self(0x1B);
    pub const NAT_REM: Self = Self(0x1C);
    // 0x1D–0x1F: reserved

    // §FLT — Float arithmetic (0x20–0x27)
    pub const FLT_ADD: Self = Self(0x20);
    pub const FLT_SUB: Self = Self(0x21);
    pub const FLT_MUL: Self = Self(0x22);
    pub const FLT_DIV: Self = Self(0x23);
    pub const FLT_REM: Self = Self(0x24);
    pub const FLT_NEG: Self = Self(0x25);
    // 0x26–0x27: reserved

    // §BIT — Bitwise / logical (0x28–0x2F)
    pub const BIT_AND: Self = Self(0x28);
    pub const BIT_OR: Self = Self(0x29);
    pub const BIT_XOR: Self = Self(0x2A);
    pub const BIT_NOT: Self = Self(0x2B);
    pub const BIT_SHL: Self = Self(0x2C);
    pub const BIT_SHR: Self = Self(0x2D);
    pub const BIT_SRU: Self = Self(0x2E);
    // 0x2F: reserved

    // §CMP — Comparison (0x30–0x3D)
    pub const CMP_EQ: Self = Self(0x30);
    pub const CMP_NE: Self = Self(0x31);
    pub const CMP_LT: Self = Self(0x32);
    pub const CMP_LE: Self = Self(0x33);
    pub const CMP_GT: Self = Self(0x34);
    pub const CMP_GE: Self = Self(0x35);
    pub const CMP_LTU: Self = Self(0x36);
    pub const CMP_LEU: Self = Self(0x37);
    pub const CMP_GTU: Self = Self(0x38);
    pub const CMP_GEU: Self = Self(0x39);
    pub const CMP_FLT: Self = Self(0x3A);
    pub const CMP_FLE: Self = Self(0x3B);
    pub const CMP_FGT: Self = Self(0x3C);
    pub const CMP_FGE: Self = Self(0x3D);

    // §CNV — Conversion (0x3E–0x3F)
    pub const CNV_ITF: Self = Self(0x3E);
    pub const CNV_FTI: Self = Self(0x3F);

    // ── Zone 1 — u8 operand (0x40–0x7F) ──

    // §LD — Load/store (0x40–0x46)
    pub const LD_LOC: Self = Self(0x40);
    pub const ST_LOC: Self = Self(0x41);
    pub const LD_CST: Self = Self(0x42);
    pub const LD_FLD: Self = Self(0x43);
    pub const ST_FLD: Self = Self(0x44);
    pub const LD_UPV: Self = Self(0x45);
    pub const LD_PAY: Self = Self(0x46);

    // §MK — Construction (0x48–0x4A)
    pub const MK_PRD: Self = Self(0x48);
    pub const CMP_TAG: Self = Self(0x49);
    pub const INV_DYN: Self = Self(0x4A);

    // §JMP — Short jumps (0x4C–0x4E)
    pub const JMP_SH: Self = Self(0x4C);
    pub const JIF_SH: Self = Self(0x4D);
    pub const JNF_SH: Self = Self(0x4E);

    // §EFF — Effect marks (0x50–0x51)
    pub const CNT_MRK: Self = Self(0x50);
    pub const CNT_UMK: Self = Self(0x51);

    // ── Zone 2 — u16 operand (0x80–0xBF) ──

    // §MK — Variant construction (0x80)
    /// Packed operand: `(tag_u8 << 8) | arity_u8`.
    /// With WID prefix: `(tag_u24 << 8) | arity_u8`.
    pub const MK_VAR: Self = Self(0x80);

    // ── Zone 3 — u32 operand (0xC0–0xFD) ──

    // §INV — Invocation (0xC0–0xC2)
    /// Packed operand: `(fn_id_u24 << 8) | arity_u8`.
    pub const INV: Self = Self(0xC0);
    /// Packed operand: `(fn_id_u24 << 8) | arity_u8`.
    pub const INV_TAL: Self = Self(0xC1);
    /// Packed operand: `(ffi_id_u24 << 8) | arity_u8`.
    pub const INV_FFI: Self = Self(0xC2);

    // §JMP — Long jumps (0xC4–0xC6)
    pub const JMP: Self = Self(0xC4);
    pub const JIF: Self = Self(0xC5);
    pub const JNF: Self = Self(0xC6);

    // §GLB — Globals (0xC8–0xC9)
    pub const LD_GLB: Self = Self(0xC8);
    pub const ST_GLB: Self = Self(0xC9);

    // §ALC — Allocation (0xCA–0xCC)
    pub const MK_ARR: Self = Self(0xCA);
    pub const ALC_REF: Self = Self(0xCB);
    pub const ALC_ARN: Self = Self(0xCC);

    // §CLO — Closures (0xCD)
    /// Packed operand: `(fn_id_u24 << 8) | upval_count_u8`.
    pub const MK_CLO: Self = Self(0xCD);

    // §CNT — Continuations (0xCE–0xCF)
    pub const CNT_SAV: Self = Self(0xCE);
    pub const CNT_RSM: Self = Self(0xCF);

    // §TSK — Tasks (0xD0–0xD3)
    pub const TSK_SPN: Self = Self(0xD0);
    pub const TSK_CHS: Self = Self(0xD1);
    pub const TSK_CHR: Self = Self(0xD2);
    pub const TSK_CMK: Self = Self(0xD3);

    // §TYP — Type operations (0xD4)
    pub const TYP_CHK: Self = Self(0xD4);

    // ── Prefix bytes ──

    /// Wide prefix — widens next opcode's operand by one step.
    pub const WID: u8 = 0xFE;
    /// Extension prefix — next byte is an extended opcode.
    pub const EXT: u8 = 0xFF;

    /// Human-readable name for this opcode, if known.
    #[must_use]
    pub fn name(self) -> Option<&'static str> {
        OPCODE_NAMES[usize::from(self.0)]
    }
}

const OPCODE_NAMES: [Option<&str>; 256] = {
    let mut t: [Option<&str>; 256] = [None; 256];

    // §CTL — Control
    t[0x00] = Some("nop");
    t[0x01] = Some("hlt");
    t[0x02] = Some("ret");
    t[0x03] = Some("ret.ut");
    t[0x04] = Some("unr");
    t[0x05] = Some("brk");

    // §STK — Stack
    t[0x06] = Some("dup");
    t[0x07] = Some("pop");
    t[0x08] = Some("swp");
    t[0x09] = Some("ld.ut");

    // §DAT — Data
    t[0x0A] = Some("ld.tag");
    t[0x0B] = Some("ld.len");
    t[0x0C] = Some("ld.idx");
    t[0x0D] = Some("st.idx");
    t[0x0E] = Some("tsk.awt");

    // §INT — Signed integer
    t[0x10] = Some("int.add");
    t[0x11] = Some("int.sub");
    t[0x12] = Some("int.mul");
    t[0x13] = Some("int.div");
    t[0x14] = Some("int.rem");
    t[0x15] = Some("int.neg");

    // §NAT — Unsigned integer
    t[0x18] = Some("nat.add");
    t[0x19] = Some("nat.sub");
    t[0x1A] = Some("nat.mul");
    t[0x1B] = Some("nat.div");
    t[0x1C] = Some("nat.rem");

    // §FLT — Float
    t[0x20] = Some("flt.add");
    t[0x21] = Some("flt.sub");
    t[0x22] = Some("flt.mul");
    t[0x23] = Some("flt.div");
    t[0x24] = Some("flt.rem");
    t[0x25] = Some("flt.neg");

    // §BIT — Bitwise / logical
    t[0x28] = Some("bit.and");
    t[0x29] = Some("bit.or");
    t[0x2A] = Some("bit.xor");
    t[0x2B] = Some("bit.not");
    t[0x2C] = Some("bit.shl");
    t[0x2D] = Some("bit.shr");
    t[0x2E] = Some("bit.sru");

    // §CMP — Comparison
    t[0x30] = Some("cmp.eq");
    t[0x31] = Some("cmp.ne");
    t[0x32] = Some("cmp.lt");
    t[0x33] = Some("cmp.le");
    t[0x34] = Some("cmp.gt");
    t[0x35] = Some("cmp.ge");
    t[0x36] = Some("cmp.lt.un");
    t[0x37] = Some("cmp.le.un");
    t[0x38] = Some("cmp.gt.un");
    t[0x39] = Some("cmp.ge.un");
    t[0x3A] = Some("cmp.fl.lt");
    t[0x3B] = Some("cmp.fl.le");
    t[0x3C] = Some("cmp.fl.gt");
    t[0x3D] = Some("cmp.fl.ge");

    // §CNV — Conversion
    t[0x3E] = Some("cnv.itf");
    t[0x3F] = Some("cnv.fti");

    // §LD — Load/store (u8)
    t[0x40] = Some("ld.loc");
    t[0x41] = Some("st.loc");
    t[0x42] = Some("ld.cst");
    t[0x43] = Some("ld.fld");
    t[0x44] = Some("st.fld");
    t[0x45] = Some("ld.upv");
    t[0x46] = Some("ld.pay");

    // §MK — Construction (u8)
    t[0x48] = Some("mk.prd");
    t[0x49] = Some("cmp.tag");
    t[0x4A] = Some("inv.dyn");

    // §JMP — Short jumps (u8)
    t[0x4C] = Some("jmp.sh");
    t[0x4D] = Some("jif.sh");
    t[0x4E] = Some("jnf.sh");

    // §EFF — Effect marks (u8)
    t[0x50] = Some("cnt.mrk");
    t[0x51] = Some("cnt.umk");

    // §MK — Variant (u16)
    t[0x80] = Some("mk.var");

    // §INV — Invocation (u32)
    t[0xC0] = Some("inv");
    t[0xC1] = Some("inv.tal");
    t[0xC2] = Some("inv.ffi");

    // §JMP — Long jumps (u32)
    t[0xC4] = Some("jmp");
    t[0xC5] = Some("jif");
    t[0xC6] = Some("jnf");

    // §GLB — Globals (u32)
    t[0xC8] = Some("ld.glb");
    t[0xC9] = Some("st.glb");

    // §ALC — Allocation (u32)
    t[0xCA] = Some("mk.arr");
    t[0xCB] = Some("alc.ref");
    t[0xCC] = Some("alc.arn");

    // §CLO — Closures (u32)
    t[0xCD] = Some("mk.clo");

    // §CNT — Continuations (u32)
    t[0xCE] = Some("cnt.sav");
    t[0xCF] = Some("cnt.rsm");

    // §TSK — Tasks (u32)
    t[0xD0] = Some("tsk.spn");
    t[0xD1] = Some("tsk.chs");
    t[0xD2] = Some("tsk.chr");
    t[0xD3] = Some("tsk.cmk");

    // §TYP — Type operations (u32)
    t[0xD4] = Some("typ.chk");

    t
};

impl fmt::Display for Opcode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.name() {
            Some(n) => f.write_str(n),
            None => write!(f, "0x{:02X}", self.0),
        }
    }
}

/// Instruction length from top-2-bits encoding.
///
/// `0x00..=0x3F` -> 1 byte, `0x40..=0x7F` -> 2 bytes,
/// `0x80..=0xBF` -> 3 bytes, `0xC0..=0xFD` -> 5 bytes.
///
/// For `WID` (0xFE) and `EXT` (0xFF), returns 1 (the prefix byte itself);
/// the caller must handle the subsequent opcode.
#[must_use]
pub const fn instr_len(op: u8) -> usize {
    match op {
        0xFE | 0xFF => 1,
        _ => match op >> 6 {
            0 => 1,
            1 => 2,
            2 => 3,
            _ => 5, // 3 -> 5
        },
    }
}

/// Operand size in bytes for a given zone (top-2-bits).
/// Zone 0 = 0, Zone 1 = 1, Zone 2 = 2, Zone 3 = 4.
#[must_use]
pub const fn zone_operand_size(zone: u8) -> usize {
    match zone {
        0 => 0,
        1 => 1,
        2 => 2,
        _ => 4,
    }
}

/// Widened operand size: none→u8 (1), u8→u16 (2), u16→u32 (4), u32→u32 (4).
#[must_use]
pub const fn widened_operand_size(zone: u8) -> usize {
    match zone {
        0 => 1,
        1 => 2,
        _ => 4,
    }
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

/// Encode an i8-operand instruction into `buf` (for short jumps).
pub fn encode_i8(buf: &mut Vec<u8>, op: Opcode, operand: i8) {
    buf.push(op.0);
    buf.push(u8::from_ne_bytes(operand.to_ne_bytes()));
}

/// Encode a WID-prefixed instruction. The operand is widened by one step.
pub fn encode_wid(buf: &mut Vec<u8>, op: Opcode, widened_operand_bytes: &[u8]) {
    buf.push(Opcode::WID);
    buf.push(op.0);
    buf.extend_from_slice(widened_operand_bytes);
}

/// Encode WID + u8-zone opcode with a u16 operand (widened from u8 to u16).
pub fn encode_wid_u16(buf: &mut Vec<u8>, op: Opcode, operand: u16) {
    buf.push(Opcode::WID);
    buf.push(op.0);
    buf.extend_from_slice(&operand.to_le_bytes());
}

/// Encode WID + u16-zone opcode with a u32 operand (widened from u16 to u32).
pub fn encode_wid_u32(buf: &mut Vec<u8>, op: Opcode, operand: u32) {
    buf.push(Opcode::WID);
    buf.push(op.0);
    buf.extend_from_slice(&operand.to_le_bytes());
}

/// Pack a u24 id and a u8 arity into a u32 operand.
/// Layout: `(id_u24 << 8) | arity_u8`.
#[must_use]
pub const fn pack_id_arity(id: u32, arity: u8) -> u32 {
    (id << 8) | u32::from_le_bytes([arity, 0, 0, 0])
}

/// Unpack a u32 operand into (`id_u24`, `arity_u8`).
#[must_use]
pub const fn unpack_id_arity(packed: u32) -> (u32, u8) {
    ((packed >> 8) & 0x00FF_FFFF, packed.to_le_bytes()[0])
}

/// Pack a u8 tag and a u8 arity into a u16 operand.
/// Layout: `(tag_u8 << 8) | arity_u8`.
#[must_use]
pub const fn pack_tag_arity_u16(tag: u8, arity: u8) -> u16 {
    u16::from_le_bytes([arity, tag])
}

/// Unpack a u16 operand into (`tag_u8`, `arity_u8`).
#[must_use]
pub const fn unpack_tag_arity_u16(packed: u16) -> (u8, u8) {
    (packed.to_le_bytes()[1], packed.to_le_bytes()[0])
}

/// Unpack a u32 operand as (`tag_u24`, `arity_u8`) for widened `MK_VAR`.
#[must_use]
pub const fn unpack_tag_arity_u32(packed: u32) -> (u32, u8) {
    unpack_id_arity(packed)
}

#[cfg(test)]
mod tests;
