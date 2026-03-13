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
    /// Pop value, push bool: true iff value's runtime type matches the given type_id.
    pub const TYPE_CHK: Self = Self(0xD3);
}

impl fmt::Display for Opcode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let name = match *self {
            Self::NOP => "nop",
            Self::HLT => "hlt",
            Self::RET => "ret",
            Self::RET_U => "ret.u",
            Self::UNR => "unr",
            Self::BRK => "brk",
            Self::DUP => "dup",
            Self::POP => "pop",
            Self::SWP => "swp",
            Self::LD_TAG => "ld.tag",
            Self::LD_LEN => "ld.len",
            Self::LD_IDX => "ld.idx",
            Self::ST_IDX => "st.idx",
            Self::FRE => "fre",
            Self::EFF_RES_C => "eff.res.c",
            Self::EFF_ABT => "eff.abt",
            Self::I_ADD => "i.add",
            Self::I_ADD_UN => "i.add.un",
            Self::I_SUB => "i.sub",
            Self::I_SUB_UN => "i.sub.un",
            Self::I_MUL => "i.mul",
            Self::I_MUL_UN => "i.mul.un",
            Self::I_DIV => "i.div",
            Self::I_DIV_UN => "i.div.un",
            Self::I_REM => "i.rem",
            Self::I_REM_UN => "i.rem.un",
            Self::I_NEG => "i.neg",
            Self::TSK_AWT => "tsk.awt",
            Self::F_ADD => "f.add",
            Self::F_SUB => "f.sub",
            Self::F_MUL => "f.mul",
            Self::F_DIV => "f.div",
            Self::F_REM => "f.rem",
            Self::F_NEG => "f.neg",
            Self::B_AND => "b.and",
            Self::B_OR => "b.or",
            Self::B_XOR => "b.xor",
            Self::B_NOT => "b.not",
            Self::B_SHL => "b.shl",
            Self::B_SHR => "b.shr",
            Self::B_SHR_UN => "b.shr.un",
            Self::CMP_EQ => "cmp.eq",
            Self::CMP_NE => "cmp.ne",
            Self::CMP_LT => "cmp.lt",
            Self::CMP_LT_UN => "cmp.lt.un",
            Self::CMP_LE => "cmp.le",
            Self::CMP_LE_UN => "cmp.le.un",
            Self::CMP_GT => "cmp.gt",
            Self::CMP_GT_UN => "cmp.gt.un",
            Self::CMP_GE => "cmp.ge",
            Self::CMP_GE_UN => "cmp.ge.un",
            Self::CMP_F_EQ => "cmp.f.eq",
            Self::CMP_F_NE => "cmp.f.ne",
            Self::CMP_F_LT => "cmp.f.lt",
            Self::CMP_F_LE => "cmp.f.le",
            Self::CMP_F_GT => "cmp.f.gt",
            Self::CMP_F_GE => "cmp.f.ge",
            Self::CNV_ITF => "cnv.itf",
            Self::CNV_FTI => "cnv.fti",
            Self::CNV_TRM => "cnv.trm",
            Self::LD_LOC => "ld.loc",
            Self::ST_LOC => "st.loc",
            Self::LD_CST => "ld.cst",
            Self::MK_PRD => "mk.prd",
            Self::LD_FLD => "ld.fld",
            Self::MK_VAR => "mk.var",
            Self::LD_PAY => "ld.pay",
            Self::CMP_TAG => "cmp.tag",
            Self::EFF_PSH => "eff.psh",
            Self::EFF_POP => "eff.pop",
            Self::INV_DYN => "inv.dyn",
            Self::ST_FLD => "st.fld",
            Self::LD_LOC_W => "ld.loc.w",
            Self::ST_LOC_W => "st.loc.w",
            Self::LD_CST_W => "ld.cst.w",
            Self::MK_VAR_W => "mk.var.w",
            Self::CMP_TAG_W => "cmp.tag.w",
            Self::INV => "inv",
            Self::INV_EFF => "inv.eff",
            Self::INV_TAL => "inv.tal",
            Self::INV_TAL_EFF => "inv.tal.eff",
            Self::LD_GLB => "ld.glb",
            Self::ST_GLB => "st.glb",
            Self::MK_ARR => "mk.arr",
            Self::ALC_REF => "alc.ref",
            Self::ALC_ARN => "alc.arn",
            Self::EFF_DO => "eff.do",
            Self::EFF_RES => "eff.res",
            Self::TSK_SPN => "tsk.spn",
            Self::TSK_CHS => "tsk.chs",
            Self::TSK_CHR => "tsk.chr",
            Self::TSK_CMK => "tsk.cmk",
            Self::INV_FFI => "inv.ffi",
            Self::JMP_W => "jmp.w",
            Self::JMP_T_W => "jmp.t.w",
            Self::JMP_F_W => "jmp.f.w",
            Self::TYPE_CHK => "type.chk",
            _ => return write!(f, "0x{:02X}", self.0),
        };
        f.write_str(name)
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
