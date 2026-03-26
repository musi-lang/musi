/// All 78 active opcodes in the SEAM bytecode instruction set.
///
/// Each variant carries its encoding as a `u8` discriminant via `#[repr(u8)]`.
/// Gaps in the opcode space are reserved for future use.
#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Opcode {
    // -- Data Movement (0x00-0x0E) --
    LdLoc = 0x00,
    LdConst = 0x01,
    LdGlob = 0x02,
    LdUpv = 0x03,
    LdUnit = 0x04,
    LdTru = 0x05,
    LdFls = 0x06,
    LdNil = 0x07,
    LdOne = 0x08,
    LdSmi = 0x09,
    StLoc = 0x0A,
    StGlob = 0x0B,
    StUpv = 0x0C,
    LdLocW = 0x0D,
    StLocW = 0x0E,

    // -- Stack (0x10-0x13) --
    Pop = 0x10,
    Dup = 0x11,
    Swap = 0x12,
    Rot = 0x13,

    // -- Scalar Arithmetic (0x14-0x1E) --
    IAdd = 0x14,
    ISub = 0x15,
    IMul = 0x16,
    IDiv = 0x17,
    IRem = 0x18,
    INeg = 0x19,
    FAdd = 0x1A,
    FSub = 0x1B,
    FMul = 0x1C,
    FDiv = 0x1D,
    FNeg = 0x1E,

    // -- Bitwise / Logic (0x1F-0x22) --
    And = 0x1F,
    Or = 0x20,
    Not = 0x21,
    Xor = 0x22,

    // -- Bit Shifts (0x23-0x24) --
    Shl = 0x23,
    Shr = 0x24,

    // -- Comparison (0x25-0x2A) --
    CmpEq = 0x25,
    CmpNeq = 0x26,
    CmpLt = 0x27,
    CmpGt = 0x28,
    CmpLeq = 0x29,
    CmpGeq = 0x2A,

    // -- Branch (0x2B-0x2F) --
    BrTrue = 0x2B,
    BrFalse = 0x2C,
    BrJmp = 0x2D,
    BrTbl = 0x2E,
    BrBack = 0x2F,

    // -- Call / Return / Closure (0x31-0x34) --
    Call = 0x31,
    CallTail = 0x32,
    Ret = 0x33,
    ClsNew = 0x34,
    // 0x35 reserved (formerly ClsUpv — upvalues now captured inline by ClsNew)

    // -- Array / Memory (0x37-0x41) --
    ArrNew = 0x37,
    ArrGet = 0x38,
    ArrSet = 0x39,
    ArrLen = 0x3A,
    ArrSlice = 0x3B,
    // 0x3C-0x3E reserved (formerly ArrMap, ArrFold, ArrZip -- now emitter-lowered)
    ArrFill = 0x3F,
    ArrCopy = 0x40,
    ArrCaten = 0x41,

    // -- Array Fast Paths (0x42-0x45) --
    ArrGetI = 0x42,
    ArrSetI = 0x43,
    ArrTag = 0x44,
    ArrNewT = 0x45,

    // -- Type Operations (0x4A-0x4C) --
    TyChk = 0x4A,
    TyCast = 0x4B,
    TyTag = 0x4C,

    // -- Effect Operations (0x4E-0x51) --
    EffPush = 0x4E,
    EffPop = 0x4F,
    EffNeed = 0x50,
    EffCont = 0x51,

    // -- Type Class Dispatch (0x52-0x53) --
    TyclDict = 0x52,
    TyclCall = 0x53,

    // -- GC / Foreign / Misc (0x56-0x5B) --
    GcPin = 0x56,
    GcUnpin = 0x57,
    FfiCall = 0x58,
    Nop = 0x59,
    Panic = 0x5A,
    Halt = 0x5B,
}

/// Total number of active opcodes in the instruction set.
pub const OPCODE_COUNT: usize = 78;

/// Lookup table for `from_byte`: maps every `u8` to `Some(Opcode)` or `None`.
const BYTE_TO_OPCODE: [Option<Opcode>; 256] = {
    let mut table: [Option<Opcode>; 256] = [None; 256];
    table[0x00] = Some(Opcode::LdLoc);
    table[0x01] = Some(Opcode::LdConst);
    table[0x02] = Some(Opcode::LdGlob);
    table[0x03] = Some(Opcode::LdUpv);
    table[0x04] = Some(Opcode::LdUnit);
    table[0x05] = Some(Opcode::LdTru);
    table[0x06] = Some(Opcode::LdFls);
    table[0x07] = Some(Opcode::LdNil);
    table[0x08] = Some(Opcode::LdOne);
    table[0x09] = Some(Opcode::LdSmi);
    table[0x0A] = Some(Opcode::StLoc);
    table[0x0B] = Some(Opcode::StGlob);
    table[0x0C] = Some(Opcode::StUpv);
    table[0x0D] = Some(Opcode::LdLocW);
    table[0x0E] = Some(Opcode::StLocW);
    table[0x10] = Some(Opcode::Pop);
    table[0x11] = Some(Opcode::Dup);
    table[0x12] = Some(Opcode::Swap);
    table[0x13] = Some(Opcode::Rot);
    table[0x14] = Some(Opcode::IAdd);
    table[0x15] = Some(Opcode::ISub);
    table[0x16] = Some(Opcode::IMul);
    table[0x17] = Some(Opcode::IDiv);
    table[0x18] = Some(Opcode::IRem);
    table[0x19] = Some(Opcode::INeg);
    table[0x1A] = Some(Opcode::FAdd);
    table[0x1B] = Some(Opcode::FSub);
    table[0x1C] = Some(Opcode::FMul);
    table[0x1D] = Some(Opcode::FDiv);
    table[0x1E] = Some(Opcode::FNeg);
    table[0x1F] = Some(Opcode::And);
    table[0x20] = Some(Opcode::Or);
    table[0x21] = Some(Opcode::Not);
    table[0x22] = Some(Opcode::Xor);
    table[0x23] = Some(Opcode::Shl);
    table[0x24] = Some(Opcode::Shr);
    table[0x25] = Some(Opcode::CmpEq);
    table[0x26] = Some(Opcode::CmpNeq);
    table[0x27] = Some(Opcode::CmpLt);
    table[0x28] = Some(Opcode::CmpGt);
    table[0x29] = Some(Opcode::CmpLeq);
    table[0x2A] = Some(Opcode::CmpGeq);
    table[0x2B] = Some(Opcode::BrTrue);
    table[0x2C] = Some(Opcode::BrFalse);
    table[0x2D] = Some(Opcode::BrJmp);
    table[0x2E] = Some(Opcode::BrTbl);
    table[0x2F] = Some(Opcode::BrBack);
    table[0x31] = Some(Opcode::Call);
    table[0x32] = Some(Opcode::CallTail);
    table[0x33] = Some(Opcode::Ret);
    table[0x34] = Some(Opcode::ClsNew);
    // 0x35 reserved
    table[0x37] = Some(Opcode::ArrNew);
    table[0x38] = Some(Opcode::ArrGet);
    table[0x39] = Some(Opcode::ArrSet);
    table[0x3A] = Some(Opcode::ArrLen);
    table[0x3B] = Some(Opcode::ArrSlice);
    // 0x3C-0x3E reserved
    table[0x3F] = Some(Opcode::ArrFill);
    table[0x40] = Some(Opcode::ArrCopy);
    table[0x41] = Some(Opcode::ArrCaten);
    table[0x42] = Some(Opcode::ArrGetI);
    table[0x43] = Some(Opcode::ArrSetI);
    table[0x44] = Some(Opcode::ArrTag);
    table[0x45] = Some(Opcode::ArrNewT);
    table[0x4A] = Some(Opcode::TyChk);
    table[0x4B] = Some(Opcode::TyCast);
    table[0x4C] = Some(Opcode::TyTag);
    table[0x4E] = Some(Opcode::EffPush);
    table[0x4F] = Some(Opcode::EffPop);
    table[0x50] = Some(Opcode::EffNeed);
    table[0x51] = Some(Opcode::EffCont);
    table[0x52] = Some(Opcode::TyclDict);
    table[0x53] = Some(Opcode::TyclCall);
    table[0x56] = Some(Opcode::GcPin);
    table[0x57] = Some(Opcode::GcUnpin);
    table[0x58] = Some(Opcode::FfiCall);
    table[0x59] = Some(Opcode::Nop);
    table[0x5A] = Some(Opcode::Panic);
    table[0x5B] = Some(Opcode::Halt);
    table
};

impl Opcode {
    /// Decode a byte into an opcode. Returns `None` for reserved/unused values.
    #[must_use]
    pub const fn from_byte(byte: u8) -> Option<Self> {
        // u8 -> usize indexing: lossless widening, safe in const context
        #[allow(clippy::as_conversions)]
        BYTE_TO_OPCODE[byte as usize]
    }

    /// The dot-separated mnemonic as used in assembly and diagnostics.
    #[must_use]
    pub const fn mnemonic(self) -> &'static str {
        match self {
            Self::LdLoc => "ld.loc",
            Self::LdConst => "ld.const",
            Self::LdGlob => "ld.glob",
            Self::LdUpv => "ld.upv",
            Self::LdUnit => "ld.unit",
            Self::LdTru => "ld.tru",
            Self::LdFls => "ld.fls",
            Self::LdNil => "ld.nil",
            Self::LdOne => "ld.one",
            Self::LdSmi => "ld.smi",
            Self::StLoc => "st.loc",
            Self::StGlob => "st.glob",
            Self::StUpv => "st.upv",
            Self::LdLocW => "ld.loc.w",
            Self::StLocW => "st.loc.w",

            Self::Pop => "pop",
            Self::Dup => "dup",
            Self::Swap => "swap",
            Self::Rot => "rot",

            Self::IAdd => "i.add",
            Self::ISub => "i.sub",
            Self::IMul => "i.mul",
            Self::IDiv => "i.div",
            Self::IRem => "i.rem",
            Self::INeg => "i.neg",
            Self::FAdd => "f.add",
            Self::FSub => "f.sub",
            Self::FMul => "f.mul",
            Self::FDiv => "f.div",
            Self::FNeg => "f.neg",

            Self::And => "and",
            Self::Or => "or",
            Self::Not => "not",
            Self::Xor => "xor",

            Self::Shl => "shl",
            Self::Shr => "shr",

            Self::CmpEq => "cmp.eq",
            Self::CmpNeq => "cmp.neq",
            Self::CmpLt => "cmp.lt",
            Self::CmpGt => "cmp.gt",
            Self::CmpLeq => "cmp.leq",
            Self::CmpGeq => "cmp.geq",

            Self::BrTrue => "br.true",
            Self::BrFalse => "br.false",
            Self::BrJmp => "br.jmp",
            Self::BrTbl => "br.tbl",
            Self::BrBack => "br.back",

            Self::Call => "call",
            Self::CallTail => "call.tail",
            Self::Ret => "ret",
            Self::ClsNew => "cls.new",

            Self::ArrNew => "arr.new",
            Self::ArrGet => "arr.get",
            Self::ArrSet => "arr.set",
            Self::ArrLen => "arr.len",
            Self::ArrSlice => "arr.slice",
            Self::ArrFill => "arr.fill",
            Self::ArrCopy => "arr.copy",
            Self::ArrCaten => "arr.caten",

            Self::ArrGetI => "arr.get.i",
            Self::ArrSetI => "arr.set.i",
            Self::ArrTag => "arr.tag",
            Self::ArrNewT => "arr.new.t",

            Self::TyChk => "ty.chk",
            Self::TyCast => "ty.cast",
            Self::TyTag => "ty.tag",

            Self::EffPush => "eff.push",
            Self::EffPop => "eff.pop",
            Self::EffNeed => "eff.need",
            Self::EffCont => "eff.cont",

            Self::TyclDict => "tycl.dict",
            Self::TyclCall => "tycl.call",

            Self::GcPin => "gc.pin",
            Self::GcUnpin => "gc.unpin",
            Self::FfiCall => "ffi.call",
            Self::Nop => "nop",
            Self::Panic => "panic",
            Self::Halt => "halt",
        }
    }

    /// Reverse lookup: parse a mnemonic string into an opcode.
    #[must_use]
    pub fn from_mnemonic(s: &str) -> Option<Self> {
        ALL_OPCODES.iter().find(|op| op.mnemonic() == s).copied()
    }
}

/// All 78 active opcodes in declaration order.
pub const ALL_OPCODES: [Opcode; OPCODE_COUNT] = [
    // Data Movement
    Opcode::LdLoc,
    Opcode::LdConst,
    Opcode::LdGlob,
    Opcode::LdUpv,
    Opcode::LdUnit,
    Opcode::LdTru,
    Opcode::LdFls,
    Opcode::LdNil,
    Opcode::LdOne,
    Opcode::LdSmi,
    Opcode::StLoc,
    Opcode::StGlob,
    Opcode::StUpv,
    Opcode::LdLocW,
    Opcode::StLocW,
    // Stack
    Opcode::Pop,
    Opcode::Dup,
    Opcode::Swap,
    Opcode::Rot,
    // Scalar Arithmetic
    Opcode::IAdd,
    Opcode::ISub,
    Opcode::IMul,
    Opcode::IDiv,
    Opcode::IRem,
    Opcode::INeg,
    Opcode::FAdd,
    Opcode::FSub,
    Opcode::FMul,
    Opcode::FDiv,
    Opcode::FNeg,
    // Bitwise / Logic
    Opcode::And,
    Opcode::Or,
    Opcode::Not,
    Opcode::Xor,
    // Bit Shifts
    Opcode::Shl,
    Opcode::Shr,
    // Comparison
    Opcode::CmpEq,
    Opcode::CmpNeq,
    Opcode::CmpLt,
    Opcode::CmpGt,
    Opcode::CmpLeq,
    Opcode::CmpGeq,
    // Branch
    Opcode::BrTrue,
    Opcode::BrFalse,
    Opcode::BrJmp,
    Opcode::BrTbl,
    Opcode::BrBack,
    // Call / Return / Closure
    Opcode::Call,
    Opcode::CallTail,
    Opcode::Ret,
    Opcode::ClsNew,
    // Array / Memory
    Opcode::ArrNew,
    Opcode::ArrGet,
    Opcode::ArrSet,
    Opcode::ArrLen,
    Opcode::ArrSlice,
    Opcode::ArrFill,
    Opcode::ArrCopy,
    Opcode::ArrCaten,
    // Array Fast Paths
    Opcode::ArrGetI,
    Opcode::ArrSetI,
    Opcode::ArrTag,
    Opcode::ArrNewT,
    // Type Operations
    Opcode::TyChk,
    Opcode::TyCast,
    Opcode::TyTag,
    // Effect Operations
    Opcode::EffPush,
    Opcode::EffPop,
    Opcode::EffNeed,
    Opcode::EffCont,
    // Class Dispatch
    Opcode::TyclDict,
    Opcode::TyclCall,
    // GC / Foreign / Misc
    Opcode::GcPin,
    Opcode::GcUnpin,
    Opcode::FfiCall,
    Opcode::Nop,
    Opcode::Panic,
    Opcode::Halt,
];

// as_conversions: tests cast #[repr(u8)] Opcode to u8 -- always lossless
#[cfg(test)]
#[allow(clippy::unwrap_used, clippy::panic, clippy::as_conversions)]
mod tests;
