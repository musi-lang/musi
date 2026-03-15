//! Opcode definitions and instruction encoding.
//!
//! High two bits of opcode determine instruction length:
//! - `0x00–0x3F`: 1 byte (no operand)
//! - `0x40–0x7F`: 2 bytes (u8 operand)
//! - `0x80–0xBF`: 3 bytes (u16 LE operand)
//! - `0xC0–0xFF`: 5 bytes (u32 LE operand)

use core::fmt;

/// A bytecode opcode (newtype around the raw byte value).
///
/// Using a tuple struct rather than a `#[repr(u8)]` enum avoids `as` casts
/// while retaining named constants.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Opcode(pub u8);

impl Opcode {
    // §0  Control / Stack (no operand)
    pub const NOP: Self = Self(0x00);
    pub const HLT: Self = Self(0x01);
    pub const RET: Self = Self(0x02);
    pub const RET_U: Self = Self(0x03);
    pub const UNR: Self = Self(0x04);
    pub const BRK: Self = Self(0x05);
    pub const DUP: Self = Self(0x06);
    pub const POP: Self = Self(0x07);
    pub const SWP: Self = Self(0x08);

    // §1  Structural (no operand)
    pub const LD_TAG: Self = Self(0x09);
    pub const LD_LEN: Self = Self(0x0A);
    pub const LD_IDX: Self = Self(0x0B);
    pub const ST_IDX: Self = Self(0x0C);
    pub const FRE: Self = Self(0x0D);
    pub const EFF_RES_C: Self = Self(0x0E);
    pub const EFF_ABT: Self = Self(0x0F);

    // §2  Integer Arithmetic (no operand, overflow wraps)
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
    pub const TSK_AWT: Self = Self(0x1B);

    // §3  Float Arithmetic (no operand, IEEE 754)
    pub const F_ADD: Self = Self(0x20);
    pub const F_SUB: Self = Self(0x21);
    pub const F_MUL: Self = Self(0x22);
    pub const F_DIV: Self = Self(0x23);
    pub const F_REM: Self = Self(0x24);
    pub const F_NEG: Self = Self(0x25);

    // §4  Bitwise / Logical (no operand)
    pub const B_AND: Self = Self(0x26);
    pub const B_OR: Self = Self(0x27);
    pub const B_XOR: Self = Self(0x28);
    pub const B_NOT: Self = Self(0x29);
    pub const B_SHL: Self = Self(0x2A);
    pub const B_SHR: Self = Self(0x2B);
    pub const B_SHR_UN: Self = Self(0x2C);

    // §5  Comparison (no operand)
    pub const CMP_EQ: Self = Self(0x2D);
    pub const CMP_NE: Self = Self(0x2E);
    pub const CMP_LT: Self = Self(0x2F);
    pub const CMP_LT_UN: Self = Self(0x30);
    pub const CMP_LE: Self = Self(0x31);
    pub const CMP_LE_UN: Self = Self(0x32);
    pub const CMP_GT: Self = Self(0x33);
    pub const CMP_GT_UN: Self = Self(0x34);
    pub const CMP_GE: Self = Self(0x35);
    pub const CMP_GE_UN: Self = Self(0x36);

    // §6  Float Comparison (no operand)
    pub const CMP_F_EQ: Self = Self(0x37);
    pub const CMP_F_NE: Self = Self(0x38);
    pub const CMP_F_LT: Self = Self(0x39);
    pub const CMP_F_LE: Self = Self(0x3A);
    pub const CMP_F_GT: Self = Self(0x3B);
    pub const CMP_F_GE: Self = Self(0x3C);

    // §7  Conversion (no operand)
    pub const CNV_ITF: Self = Self(0x3D);
    pub const CNV_FTI: Self = Self(0x3E);
    pub const CNV_TRM: Self = Self(0x3F);

    // §8  Locals / Constants / Structures (u8 operand)
    pub const LD_LOC: Self = Self(0x40);
    pub const ST_LOC: Self = Self(0x41);
    pub const LD_CST: Self = Self(0x42);
    pub const MK_PRD: Self = Self(0x43);
    pub const LD_FLD: Self = Self(0x44);
    pub const MK_VAR: Self = Self(0x45);
    pub const LD_PAY: Self = Self(0x46);
    pub const CMP_TAG: Self = Self(0x47);
    pub const EFF_PSH: Self = Self(0x48);
    pub const EFF_POP: Self = Self(0x49);
    /// Indirect (dynamic) call through a closure or fn value. u8 = arg count.
    pub const INV_DYN: Self = Self(0x4A);
    /// Store into an object field. u8 = field index. Pops [obj, value].
    pub const ST_FLD: Self = Self(0x4B);

    // §9  Wide locals (u16 operand)
    pub const LD_LOC_W: Self = Self(0x80);
    pub const ST_LOC_W: Self = Self(0x81);
    pub const LD_CST_W: Self = Self(0x82);
    pub const MK_VAR_W: Self = Self(0x83);
    pub const CMP_TAG_W: Self = Self(0x84);

    // §10  Invocation (u32 operand)
    pub const INV: Self = Self(0xC0);
    pub const INV_EFF: Self = Self(0xC1);
    pub const INV_TAL: Self = Self(0xC2);
    pub const INV_TAL_EFF: Self = Self(0xC3);

    // §11  Globals / Allocation (u32 operand)
    pub const LD_GLB: Self = Self(0xC4);
    pub const ST_GLB: Self = Self(0xC5);
    pub const MK_ARR: Self = Self(0xC6);
    pub const ALC_REF: Self = Self(0xC7);
    pub const ALC_ARN: Self = Self(0xC8);

    // §12  Effects (u32 operand)
    pub const EFF_DO: Self = Self(0xC9);
    pub const EFF_RES: Self = Self(0xCA);

    // §13  Concurrency (u32 operand)
    pub const TSK_SPN: Self = Self(0xCB);
    pub const TSK_CHS: Self = Self(0xCC);
    pub const TSK_CHR: Self = Self(0xCD);
    pub const TSK_CMK: Self = Self(0xCE);

    // §14  FFI (u32 operand)
    pub const INV_FFI: Self = Self(0xCF);

    // §15  Wide Jumps (u32 operand)
    pub const JMP_W: Self = Self(0xD0);
    pub const JMP_T_W: Self = Self(0xD1);
    pub const JMP_F_W: Self = Self(0xD2);

    // §16  Type operations (u32 operand)
    /// Pop value, push bool: true iff value's runtime type matches the given `type_id`.
    pub const TYP_CHK: Self = Self(0xD3);

    // §17  Closures
    /// Allocate a closure object. u32 = `fn_id`. Pops N upvalues (N = fn's `upvalue_count`), pushes REF.
    pub const MK_CLO: Self = Self(0xD4);
    /// Load an upvalue from the current closure. u8 = upvalue index.
    pub const LD_UPV: Self = Self(0x4C);

    /// Human-readable name for this opcode, if known.
    #[must_use]
    pub fn name(self) -> Option<&'static str> {
        OPCODE_NAMES[usize::from(self.0)]
    }
}

const OPCODE_NAMES: [Option<&str>; 256] = {
    let mut t: [Option<&str>; 256] = [None; 256];
    // §0 Control / Stack
    t[0x00] = Some("nop");
    t[0x01] = Some("hlt");
    t[0x02] = Some("ret");
    t[0x03] = Some("ret.u");
    t[0x04] = Some("unr");
    t[0x05] = Some("brk");
    t[0x06] = Some("dup");
    t[0x07] = Some("pop");
    t[0x08] = Some("swp");
    // §1 Structural (no operand)
    t[0x09] = Some("ld.tag");
    t[0x0A] = Some("ld.len");
    t[0x0B] = Some("ld.idx");
    t[0x0C] = Some("st.idx");
    t[0x0D] = Some("fre");
    t[0x0E] = Some("eff.res.c");
    t[0x0F] = Some("eff.abt");
    // §2 Integer Arithmetic
    t[0x10] = Some("i.add");
    t[0x11] = Some("i.add.un");
    t[0x12] = Some("i.sub");
    t[0x13] = Some("i.sub.un");
    t[0x14] = Some("i.mul");
    t[0x15] = Some("i.mul.un");
    t[0x16] = Some("i.div");
    t[0x17] = Some("i.div.un");
    t[0x18] = Some("i.rem");
    t[0x19] = Some("i.rem.un");
    t[0x1A] = Some("i.neg");
    t[0x1B] = Some("tsk.awt");
    // §3 Float Arithmetic
    t[0x20] = Some("f.add");
    t[0x21] = Some("f.sub");
    t[0x22] = Some("f.mul");
    t[0x23] = Some("f.div");
    t[0x24] = Some("f.rem");
    t[0x25] = Some("f.neg");
    // §4 Bitwise / Logical
    t[0x26] = Some("b.and");
    t[0x27] = Some("b.or");
    t[0x28] = Some("b.xor");
    t[0x29] = Some("b.not");
    t[0x2A] = Some("b.shl");
    t[0x2B] = Some("b.shr");
    t[0x2C] = Some("b.shr.un");
    // §5 Comparison
    t[0x2D] = Some("cmp.eq");
    t[0x2E] = Some("cmp.ne");
    t[0x2F] = Some("cmp.lt");
    t[0x30] = Some("cmp.lt.un");
    t[0x31] = Some("cmp.le");
    t[0x32] = Some("cmp.le.un");
    t[0x33] = Some("cmp.gt");
    t[0x34] = Some("cmp.gt.un");
    t[0x35] = Some("cmp.ge");
    t[0x36] = Some("cmp.ge.un");
    // §6 Float Comparison
    t[0x37] = Some("cmp.f.eq");
    t[0x38] = Some("cmp.f.ne");
    t[0x39] = Some("cmp.f.lt");
    t[0x3A] = Some("cmp.f.le");
    t[0x3B] = Some("cmp.f.gt");
    t[0x3C] = Some("cmp.f.ge");
    // §7 Conversion
    t[0x3D] = Some("cnv.itf");
    t[0x3E] = Some("cnv.fti");
    t[0x3F] = Some("cnv.trm");
    // §8 Locals / Constants / Structures (u8 operand)
    t[0x40] = Some("ld.loc");
    t[0x41] = Some("st.loc");
    t[0x42] = Some("ld.cst");
    t[0x43] = Some("mk.prd");
    t[0x44] = Some("ld.fld");
    t[0x45] = Some("mk.var");
    t[0x46] = Some("ld.pay");
    t[0x47] = Some("cmp.tag");
    t[0x48] = Some("eff.psh");
    t[0x49] = Some("eff.pop");
    t[0x4A] = Some("inv.dyn");
    t[0x4B] = Some("st.fld");
    t[0x4C] = Some("ld.upv");
    // §9 Wide locals (u16 operand)
    t[0x80] = Some("ld.loc.w");
    t[0x81] = Some("st.loc.w");
    t[0x82] = Some("ld.cst.w");
    t[0x83] = Some("mk.var.w");
    t[0x84] = Some("cmp.tag.w");
    // §10 Invocation (u32 operand)
    t[0xC0] = Some("inv");
    t[0xC1] = Some("inv.eff");
    t[0xC2] = Some("inv.tal");
    t[0xC3] = Some("inv.tal.eff");
    // §11 Globals / Allocation (u32 operand)
    t[0xC4] = Some("ld.glb");
    t[0xC5] = Some("st.glb");
    t[0xC6] = Some("mk.arr");
    t[0xC7] = Some("alc.ref");
    t[0xC8] = Some("alc.arn");
    // §12 Effects (u32 operand)
    t[0xC9] = Some("eff.do");
    t[0xCA] = Some("eff.res");
    // §13 Concurrency (u32 operand)
    t[0xCB] = Some("tsk.spn");
    t[0xCC] = Some("tsk.chs");
    t[0xCD] = Some("tsk.chr");
    t[0xCE] = Some("tsk.cmk");
    // §14 FFI (u32 operand)
    t[0xCF] = Some("inv.ffi");
    // §15 Wide Jumps (u32 operand)
    t[0xD0] = Some("jmp.w");
    t[0xD1] = Some("jmp.t.w");
    t[0xD2] = Some("jmp.f.w");
    // §16 Type operations (u32 operand)
    t[0xD3] = Some("type.chk");
    // §17 Closures (u32 operand)
    t[0xD4] = Some("mk.clo");
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
/// `0x00..=0x3F` → 1 byte, `0x40..=0x7F` → 2 bytes,
/// `0x80..=0xBF` → 3 bytes, `0xC0..=0xFF` → 5 bytes.
#[must_use]
pub const fn instr_len(op: u8) -> usize {
    match op >> 6 {
        0 => 1,
        1 => 2,
        2 => 3,
        _ => 5, // 3 → 5
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

#[cfg(test)]
mod tests;
