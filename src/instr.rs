use std::fmt;

#[derive(Debug, Clone, PartialEq)]
pub enum Instr {
    // Base
    Nop,
    Br(i32),
    BrTrue(i32),
    BrFalse(i32),
    Beq(i32),
    Bge(i32),
    Bgt(i32),
    Ble(i32),
    Blt(i32),
    Bne(i32),
    Leave(i32),
    Ret,
    Throw,
    Rethrow,
    Try,
    Defer,

    // Stack Operations
    Dup,
    Pop,

    // Load Constants
    LdNull,
    LdCI4(i32),
    LdCI8(i64),
    LdCN8(i8),
    LdCN16(i16),
    LdCN32(i32),
    LdCN64(i64),
    LdCB32(f32),
    LdCB64(f64),
    LdCD32(f32),
    LdCD64(f64),
    LdCStr(u32),

    // Load/Store Variables
    LdLoc(usize),
    StLoc(usize),
    LdLocA(usize),
    LdArg(usize),
    StArg(usize),
    LdArgA(usize),

    // Object Operations
    NewObj(u32),
    Call(String),
    CallVirt(String),
    CallI,
    LdFld(u32),
    StFld(u32),
    LdFldA(u32),
    LdSFld(u32),
    StSFld(u32),
    LdSFldA(u32),
    NewArr(u32),
    LdElem,
    StElem,
    LdElemA,
    LdLen,

    // Type Operations
    LdType(u32),
    IsInst(u32),
    CastClass(u32),
    Box(u32),
    UnboxAny(u32),
    ConvI8,
    ConvI16,
    ConvI32,
    ConvI64,
    ConvI128,
    ConvN8,
    ConvN16,
    ConvN32,
    ConvN64,
    ConvN128,
    ConvB32,
    ConvB64,
    ConvB128,
    ConvD32,
    ConvD64,
    ConvD128,

    // Arithmetic Operations
    Add,
    AddOvf,
    AddOvfUn,
    Sub,
    SubOvf,
    SubOvfUn,
    Mul,
    MulOvf,
    MulOvfUn,
    Div,
    DivUn,
    Rem,
    RemUn,
    Mod,
    Neg,

    // Logical/Bitwise Operations
    And,
    Or,
    Xor,
    Not,
    Shl,
    Shr,
    ShrUn,

    // Comparison Operations
    Ceq,
    Cgt,
    CgtUn,
    Clt,
    CltUn,

    // Memory Management
    RefInc,
    RefDec,
    Pin,
    Unpin,

    // Dynamic Operations
    LdFldDyn(String),
    StFldDyn(String),
    LdFldADyn(String),
    CallDyn(String),
    CallVirtDyn(String),
}

impl Instr {
    pub fn size(&self) -> usize {
        match self {
            // 1-byte instrs
            Self::Nop
            | Self::Ret
            | Self::Throw
            | Self::Rethrow
            | Self::Try
            | Self::Defer
            | Self::Dup
            | Self::Pop
            | Self::LdNull
            | Self::Add
            | Self::AddOvf
            | Self::AddOvfUn
            | Self::Sub
            | Self::SubOvf
            | Self::SubOvfUn
            | Self::Mul
            | Self::MulOvf
            | Self::MulOvfUn
            | Self::Div
            | Self::DivUn
            | Self::Mod
            | Self::Rem
            | Self::RemUn
            | Self::Neg
            | Self::And
            | Self::Or
            | Self::Xor
            | Self::Not
            | Self::Shl
            | Self::Shr
            | Self::ShrUn
            | Self::Ceq
            | Self::Cgt
            | Self::CgtUn
            | Self::Clt
            | Self::CltUn
            | Self::RefInc
            | Self::RefDec
            | Self::Pin
            | Self::Unpin
            | Self::CallI
            | Self::ConvI8
            | Self::ConvI16
            | Self::ConvI32
            | Self::ConvI64
            | Self::ConvI128
            | Self::ConvN8
            | Self::ConvN16
            | Self::ConvN32
            | Self::ConvN64
            | Self::ConvN128
            | Self::ConvB32
            | Self::ConvB64
            | Self::ConvB128
            | Self::ConvD32
            | Self::ConvD64
            | Self::ConvD128
            | Self::LdElem
            | Self::StElem
            | Self::LdElemA
            | Self::LdLen => 1,

            // Branch instructions (1 byte + 4 byte offset)
            Self::Br(_)
            | Self::BrTrue(_)
            | Self::BrFalse(_)
            | Self::Beq(_)
            | Self::Bge(_)
            | Self::Bgt(_)
            | Self::Ble(_)
            | Self::Blt(_)
            | Self::Bne(_)
            | Self::Leave(_) => 5,

            // Integer/Natural constants (1 byte + var size)
            Self::LdCN8(_) => 2,
            Self::LdCN16(_) => 3,
            Self::LdCN32(_) | Self::LdCI4(_) => 5,
            Self::LdCN64(_) | Self::LdCI8(_) => 9,

            // Binary/Decimal float constants
            Self::LdCB32(_) | Self::LdCD32(_) => 5,
            Self::LdCB64(_) | Self::LdCD64(_) => 9,

            // String constants
            Self::LdCStr(_) => 5,

            // Variable access (1 byte + 1 byte index)
            Self::LdLoc(_)
            | Self::StLoc(_)
            | Self::LdLocA(_)
            | Self::LdArg(_)
            | Self::StArg(_)
            | Self::LdArgA(_) => 2,

            // Object operations (1 byte + 4 byte type ID)
            Self::NewObj(_)
            | Self::LdFld(_)
            | Self::StFld(_)
            | Self::LdFldA(_)
            | Self::LdSFld(_)
            | Self::StSFld(_)
            | Self::LdSFldA(_)
            | Self::NewArr(_)
            | Self::LdType(_)
            | Self::IsInst(_)
            | Self::CastClass(_)
            | Self::Box(_)
            | Self::UnboxAny(_) => 5,

            // Str-based operations (1 byte + null-terminated str)
            Self::Call(_)
            | Self::CallVirt(_)
            | Self::LdFldDyn(_)
            | Self::StFldDyn(_)
            | Self::LdFldADyn(_)
            | Self::CallDyn(_)
            | Self::CallVirtDyn(_) => {
                // size depends on str length + null term
                if let Self::Call(s)
                | Self::CallVirt(s)
                | Self::LdFldDyn(s)
                | Self::StFldDyn(s)
                | Self::LdFldADyn(s)
                | Self::CallDyn(s)
                | Self::CallVirtDyn(s) = self
                {
                    1 + s.len() + 1 // opcode + str + null term
                } else {
                    unreachable!()
                }
            }
        }
    }

    pub fn mnemonic(&self) -> &'static str {
        match self {
            // Base Instructions
            Self::Nop => "nop",
            Self::Br(_) => "br",
            Self::BrTrue(_) => "brtrue",
            Self::BrFalse(_) => "brfalse",
            Self::Beq(_) => "beq",
            Self::Bge(_) => "bge",
            Self::Bgt(_) => "bgt",
            Self::Ble(_) => "ble",
            Self::Blt(_) => "blt",
            Self::Bne(_) => "bne",
            Self::Leave(_) => "leave",
            Self::Ret => "ret",
            Self::Throw => "throw",
            Self::Rethrow => "rethrow",
            Self::Try => "try",
            Self::Defer => "defer",

            // Stack Operations
            Self::Dup => "dup",
            Self::Pop => "pop",

            // Load Constants
            Self::LdNull => "ldnull",
            Self::LdCI4(_) => "ldc.i4",
            Self::LdCI8(_) => "ldc.i8",
            Self::LdCN8(_) => "ldc.n8",
            Self::LdCN16(_) => "ldc.n16",
            Self::LdCN32(_) => "ldc.n32",
            Self::LdCN64(_) => "ldc.n64",
            Self::LdCB32(_) => "ldc.b32",
            Self::LdCB64(_) => "ldc.b64",
            Self::LdCD32(_) => "ldc.d32",
            Self::LdCD64(_) => "ldc.d64",
            Self::LdCStr(_) => "ldstr",

            // Load/Store Variables
            Self::LdLoc(_) => "ldloc",
            Self::StLoc(_) => "stloc",
            Self::LdLocA(_) => "ldloca",
            Self::LdArg(_) => "ldarg",
            Self::StArg(_) => "starg",
            Self::LdArgA(_) => "ldarga",

            // Object Operations
            Self::NewObj(_) => "newobj",
            Self::Call(_) => "call",
            Self::CallVirt(_) => "callvirt",
            Self::CallI => "calli",
            Self::LdFld(_) => "ldfld",
            Self::StFld(_) => "stfld",
            Self::LdFldA(_) => "ldflda",
            Self::LdSFld(_) => "ldsfld",
            Self::StSFld(_) => "stsfld",
            Self::LdSFldA(_) => "ldsflda",
            Self::NewArr(_) => "newarr",
            Self::LdElem => "ldelem",
            Self::StElem => "stelem",
            Self::LdElemA => "ldelema",
            Self::LdLen => "ldlen",

            // Type Operations
            Self::LdType(_) => "ldtype",
            Self::IsInst(_) => "isinst",
            Self::CastClass(_) => "castclass",
            Self::Box(_) => "box",
            Self::UnboxAny(_) => "unbox.any",
            Self::ConvI8 => "conv.i8",
            Self::ConvI16 => "conv.i16",
            Self::ConvI32 => "conv.i32",
            Self::ConvI64 => "conv.i64",
            Self::ConvI128 => "conv.i128",
            Self::ConvN8 => "conv.n8",
            Self::ConvN16 => "conv.n16",
            Self::ConvN32 => "conv.n32",
            Self::ConvN64 => "conv.n64",
            Self::ConvN128 => "conv.n128",
            Self::ConvB32 => "conv.b32",
            Self::ConvB64 => "conv.b64",
            Self::ConvB128 => "conv.b128",
            Self::ConvD32 => "conv.d32",
            Self::ConvD64 => "conv.d64",
            Self::ConvD128 => "conv.d128",

            // Arithmetic Operations
            Self::Add => "add",
            Self::AddOvf => "add.ovf",
            Self::AddOvfUn => "add.ovf.un",
            Self::Sub => "sub",
            Self::SubOvf => "sub.ovf",
            Self::SubOvfUn => "sub.ovf.un",
            Self::Mul => "mul",
            Self::MulOvf => "mul.ovf",
            Self::MulOvfUn => "mul.ovf.un",
            Self::Div => "div",
            Self::DivUn => "div.un",
            Self::Rem => "rem",
            Self::RemUn => "rem.un",
            Self::Mod => "mod",
            Self::Neg => "neg",

            // Logical/Bitwise Operations
            Self::And => "and",
            Self::Or => "or",
            Self::Xor => "xor",
            Self::Not => "not",
            Self::Shl => "shl",
            Self::Shr => "shr",
            Self::ShrUn => "shr.un",

            // Comparison Operations
            Self::Ceq => "ceq",
            Self::Cgt => "cgt",
            Self::CgtUn => "cgt.un",
            Self::Clt => "clt",
            Self::CltUn => "clt.un",

            // Memory Management
            Self::RefInc => "refinc",
            Self::RefDec => "refdec",
            Self::Pin => "pin",
            Self::Unpin => "unpin",

            // Dynamic Operations
            Self::LdFldDyn(_) => "ldfld.dyn",
            Self::StFldDyn(_) => "stfld.dyn",
            Self::LdFldADyn(_) => "ldflda.dyn",
            Self::CallDyn(_) => "call.dyn",
            Self::CallVirtDyn(_) => "callvirt.dyn",
        }
    }

    pub fn opcode(&self) -> u8 {
        match self {
            // Base Instructions - CIL compat!!!
            Self::Nop => 0x00,
            Self::Br(_) => 0x38,
            Self::BrTrue(_) => 0x39,
            Self::BrFalse(_) => 0x3A,
            Self::Beq(_) => 0x3B,
            Self::Bge(_) => 0x3C,
            Self::Bgt(_) => 0x3D,
            Self::Ble(_) => 0x3E,
            Self::Blt(_) => 0x3F,
            Self::Bne(_) => 0x40,
            Self::Leave(_) => 0xC7,
            Self::Ret => 0x2A,
            Self::Throw => 0x7A,
            Self::Rethrow => 0x7B,
            Self::Try => 0xC8,
            Self::Defer => 0xC9,

            // Stack Operations
            Self::Dup => 0x25,
            Self::Pop => 0x26,

            // Load Constants
            Self::LdNull => 0x14,
            Self::LdCI4(_) => 0x20,
            Self::LdCI8(_) => 0x21,
            Self::LdCN8(_) => 0x22,
            Self::LdCN16(_) => 0x23,
            Self::LdCN32(_) => 0x24,
            Self::LdCN64(_) => 0x2F,
            Self::LdCB32(_) => 0x30,
            Self::LdCB64(_) => 0x31,
            Self::LdCD32(_) => 0x32,
            Self::LdCD64(_) => 0x33,
            Self::LdCStr(_) => 0x72,

            // Load/Store Variables
            Self::LdLoc(_) => 0x0E,
            Self::StLoc(_) => 0x0F,
            Self::LdLocA(_) => 0x10,
            Self::LdArg(_) => 0x02,
            Self::StArg(_) => 0x03,
            Self::LdArgA(_) => 0x04,

            // Object Operations
            Self::NewObj(_) => 0x73,
            Self::Call(_) | Self::CallVirt(_) | Self::CallDyn(_) | Self::CallVirtDyn(_) => 0x28,
            Self::CallI => 0x29,
            Self::LdFld(_) | Self::LdFldDyn(_) => 0x7B,
            Self::StFld(_) | Self::StFldDyn(_) => 0x7D,
            Self::LdFldA(_) | Self::LdFldADyn(_) => 0x7C,
            Self::LdSFld(_) => 0x80,
            Self::StSFld(_) => 0x81,
            Self::LdSFldA(_) => 0x82,
            Self::NewArr(_) => 0x8D,
            Self::LdElem => 0xA3,
            Self::StElem => 0xA4,
            Self::LdElemA => 0x8F,
            Self::LdLen => 0x8E,

            // Type Operations
            Self::LdType(_) => 0xD1,
            Self::IsInst(_) => 0x75,
            Self::CastClass(_) => 0x74,
            Self::Box(_) => 0x8C,
            Self::UnboxAny(_) => 0xA5,
            Self::ConvI8 => 0x67,
            Self::ConvI16 => 0x68,
            Self::ConvI32 => 0x69,
            Self::ConvI64 => 0x6A,
            Self::ConvI128 => 0xD0,
            Self::ConvN8 => 0xD2,
            Self::ConvN16 => 0xD3,
            Self::ConvN32 => 0xD4,
            Self::ConvN64 => 0xD5,
            Self::ConvN128 => 0xD6,
            Self::ConvB32 => 0xB6,
            Self::ConvB64 => 0xB7,
            Self::ConvB128 => 0xD7,
            Self::ConvD32 => 0xD8,
            Self::ConvD64 => 0xD9,
            Self::ConvD128 => 0xDA,

            // Arithmetic Operations
            Self::Add => 0x58,
            Self::AddOvf => 0xD6,
            Self::AddOvfUn => 0xD7,
            Self::Sub => 0x59,
            Self::SubOvf => 0xD8,
            Self::SubOvfUn => 0xD9,
            Self::Mul => 0x5A,
            Self::MulOvf => 0xDA,
            Self::MulOvfUn => 0xDB,
            Self::Div => 0x5B,
            Self::DivUn => 0x5C,
            Self::Rem => 0x5D,
            Self::RemUn => 0x5E,
            Self::Mod => 0xDC,
            Self::Neg => 0x65,

            // Logical/Bitwise Operations
            Self::And => 0x5F,
            Self::Or => 0x60,
            Self::Xor => 0x61,
            Self::Not => 0x66,
            Self::Shl => 0x62,
            Self::Shr => 0x63,
            Self::ShrUn => 0x64,

            // Comparison Operations
            Self::Ceq => 0xFE,
            Self::Cgt => 0xC2,
            Self::CgtUn => 0xC3,
            Self::Clt => 0xC4,
            Self::CltUn => 0xC5,

            // Memory Management
            Self::Pin => 0xDF,
            Self::Unpin => 0xE0,
            Self::RefInc => 0xA0,
            Self::RefDec => 0xA1,
        }
    }
}

impl fmt::Display for Instr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            //  with args
            Self::Br(offset) => write!(f, "{} {offset}", self.mnemonic()),
            Self::BrTrue(offset) => write!(f, "{} {offset}", self.mnemonic()),
            Self::BrFalse(offset) => write!(f, "{} {offset}", self.mnemonic()),
            Self::Beq(offset) => write!(f, "{} {offset}", self.mnemonic()),
            Self::Bge(offset) => write!(f, "{} {offset}", self.mnemonic()),
            Self::Bgt(offset) => write!(f, "{} {offset}", self.mnemonic()),
            Self::Ble(offset) => write!(f, "{} {offset}", self.mnemonic()),
            Self::Blt(offset) => write!(f, "{} {offset}", self.mnemonic()),
            Self::Bne(offset) => write!(f, "{} {offset}", self.mnemonic()),
            Self::Leave(offset) => write!(f, "{} {offset}", self.mnemonic()),

            Self::LdCI4(n) => write!(f, "{} {n}", self.mnemonic()),
            Self::LdCI8(n) => write!(f, "{} {n}", self.mnemonic()),
            Self::LdCN8(n) => write!(f, "{} {n}", self.mnemonic()),
            Self::LdCN16(n) => write!(f, "{} {n}", self.mnemonic()),
            Self::LdCN32(n) => write!(f, "{} {n}", self.mnemonic()),
            Self::LdCN64(n) => write!(f, "{} {n}", self.mnemonic()),
            Self::LdCB32(val) => write!(f, "{} {val}", self.mnemonic()),
            Self::LdCB64(val) => write!(f, "{} {val}", self.mnemonic()),
            Self::LdCD32(val) => write!(f, "{} {val}", self.mnemonic()),
            Self::LdCD64(val) => write!(f, "{} {val}", self.mnemonic()),
            Self::LdCStr(idx) => write!(f, "{} {idx}", self.mnemonic()),

            Self::LdLoc(idx) => write!(f, "{} {idx}", self.mnemonic()),
            Self::StLoc(idx) => write!(f, "{} {idx}", self.mnemonic()),
            Self::LdLocA(idx) => write!(f, "{} {idx}", self.mnemonic()),
            Self::LdArg(idx) => write!(f, "{} {idx}", self.mnemonic()),
            Self::StArg(idx) => write!(f, "{} {idx}", self.mnemonic()),
            Self::LdArgA(idx) => write!(f, "{} {idx}", self.mnemonic()),

            Self::NewObj(ctor) => write!(f, "{} {ctor}", self.mnemonic()),
            Self::Call(name)
            | Self::CallVirt(name)
            | Self::CallDyn(name)
            | Self::CallVirtDyn(name) => write!(f, "{} {name}", self.mnemonic()),
            Self::LdFld(idx) => write!(f, "{} {idx}", self.mnemonic()),
            Self::StFld(idx) => write!(f, "{} {idx}", self.mnemonic()),
            Self::LdFldA(idx) => write!(f, "{} {idx}", self.mnemonic()),
            Self::LdSFld(idx) => write!(f, "{} {idx}", self.mnemonic()),
            Self::StSFld(idx) => write!(f, "{} {idx}", self.mnemonic()),
            Self::LdSFldA(idx) => write!(f, "{} {idx}", self.mnemonic()),
            Self::NewArr(ty_id) => write!(f, "{} {ty_id}", self.mnemonic()),

            Self::LdType(ty_id) => write!(f, "{} {ty_id}", self.mnemonic()),
            Self::IsInst(ty_id) => write!(f, "{} {ty_id}", self.mnemonic()),
            Self::CastClass(ty_id) => write!(f, "{} {ty_id}", self.mnemonic()),
            Self::Box(ty_id) => write!(f, "{} {ty_id}", self.mnemonic()),
            Self::UnboxAny(ty_id) => write!(f, "{} {ty_id}", self.mnemonic()),

            Self::LdFldDyn(name) => write!(f, "{} \"{name}\"", self.mnemonic()),
            Self::StFldDyn(name) => write!(f, "{} \"{name}\"", self.mnemonic()),
            Self::LdFldADyn(name) => write!(f, "{} \"{name}\"", self.mnemonic()),

            // without args
            _ => write!(f, "{}", self.mnemonic()),
        }
    }
}
