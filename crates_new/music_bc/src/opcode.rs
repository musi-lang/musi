use crate::instruction::OperandShape;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum OpcodeFamily {
    LoadStore,
    Scalar,
    LogicCompare,
    Branch,
    CallClosure,
    Sequence,
    Data,
    Ty,
    Eff,
    Ffi,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Opcode {
    LdLoc,
    StLoc,
    LdConst,
    LdSmi,
    LdStr,
    IAdd,
    CmpEq,
    Br,
    BrFalse,
    BrTbl,
    Call,
    CallTail,
    Ret,
    ClsNew,
    SeqNew,
    DataNew,
    DataTag,
    TyChk,
    TyCast,
    TyId,
    HdlPush,
    HdlPop,
    EffInvk,
    EffResume,
    FfiCall,
}

impl Opcode {
    #[must_use]
    pub const fn family(self) -> OpcodeFamily {
        match self {
            Self::LdLoc | Self::StLoc | Self::LdConst | Self::LdSmi | Self::LdStr => {
                OpcodeFamily::LoadStore
            }
            Self::IAdd => OpcodeFamily::Scalar,
            Self::CmpEq => OpcodeFamily::LogicCompare,
            Self::Br | Self::BrFalse | Self::BrTbl => OpcodeFamily::Branch,
            Self::Call | Self::CallTail | Self::Ret | Self::ClsNew => OpcodeFamily::CallClosure,
            Self::SeqNew => OpcodeFamily::Sequence,
            Self::DataNew | Self::DataTag => OpcodeFamily::Data,
            Self::TyChk | Self::TyCast | Self::TyId => OpcodeFamily::Ty,
            Self::HdlPush | Self::HdlPop | Self::EffInvk | Self::EffResume => OpcodeFamily::Eff,
            Self::FfiCall => OpcodeFamily::Ffi,
        }
    }

    #[must_use]
    pub const fn mnemonic(self) -> &'static str {
        match self {
            Self::LdLoc => "ld.loc",
            Self::StLoc => "st.loc",
            Self::LdConst => "ld.const",
            Self::LdSmi => "ld.smi",
            Self::LdStr => "ld.str",
            Self::IAdd => "i.add",
            Self::CmpEq => "cmp.eq",
            Self::Br => "br",
            Self::BrFalse => "br.false",
            Self::BrTbl => "br.tbl",
            Self::Call => "call",
            Self::CallTail => "call.tail",
            Self::Ret => "ret",
            Self::ClsNew => "cls.new",
            Self::SeqNew => "seq.new",
            Self::DataNew => "data.new",
            Self::DataTag => "data.tag",
            Self::TyChk => "ty.chk",
            Self::TyCast => "ty.cast",
            Self::TyId => "ty.id",
            Self::HdlPush => "hdl.push",
            Self::HdlPop => "hdl.pop",
            Self::EffInvk => "eff.invk",
            Self::EffResume => "eff.resume",
            Self::FfiCall => "ffi.call",
        }
    }

    #[must_use]
    pub const fn operand_shape(self) -> OperandShape {
        match self {
            Self::LdLoc | Self::StLoc => OperandShape::Local,
            Self::LdConst => OperandShape::Constant,
            Self::LdSmi => OperandShape::I16,
            Self::LdStr => OperandShape::String,
            Self::IAdd | Self::CmpEq | Self::Ret | Self::HdlPop | Self::EffResume => {
                OperandShape::None
            }
            Self::Br | Self::BrFalse => OperandShape::Label,
            Self::BrTbl => OperandShape::BranchTable,
            Self::Call | Self::CallTail | Self::ClsNew => OperandShape::Method,
            Self::SeqNew => OperandShape::TypeLen,
            Self::DataNew | Self::DataTag | Self::TyChk | Self::TyCast | Self::TyId => {
                OperandShape::Type
            }
            Self::HdlPush | Self::EffInvk => OperandShape::Effect,
            Self::FfiCall => OperandShape::Foreign,
        }
    }

    #[must_use]
    pub const fn wire_code(self) -> u16 {
        match self {
            Self::LdLoc => 0x0001,
            Self::StLoc => 0x0002,
            Self::LdConst => 0x0003,
            Self::LdSmi => 0x0004,
            Self::LdStr => 0x0005,
            Self::IAdd => 0x0101,
            Self::CmpEq => 0x0201,
            Self::Br => 0x0301,
            Self::BrFalse => 0x0302,
            Self::BrTbl => 0x0303,
            Self::Call => 0x0401,
            Self::CallTail => 0x0402,
            Self::Ret => 0x0403,
            Self::ClsNew => 0x0404,
            Self::SeqNew => 0x0501,
            Self::DataNew => 0x0601,
            Self::DataTag => 0x0602,
            Self::TyChk => 0x0701,
            Self::TyCast => 0x0702,
            Self::TyId => 0x0703,
            Self::HdlPush => 0x0801,
            Self::HdlPop => 0x0802,
            Self::EffInvk => 0x0803,
            Self::EffResume => 0x0804,
            Self::FfiCall => 0x0901,
        }
    }

    #[must_use]
    pub fn from_mnemonic(text: &str) -> Option<Self> {
        Some(match text {
            "ld.loc" => Self::LdLoc,
            "st.loc" => Self::StLoc,
            "ld.const" => Self::LdConst,
            "ld.smi" => Self::LdSmi,
            "ld.str" => Self::LdStr,
            "i.add" => Self::IAdd,
            "cmp.eq" => Self::CmpEq,
            "br" => Self::Br,
            "br.false" => Self::BrFalse,
            "br.tbl" => Self::BrTbl,
            "call" => Self::Call,
            "call.tail" => Self::CallTail,
            "ret" => Self::Ret,
            "cls.new" => Self::ClsNew,
            "seq.new" => Self::SeqNew,
            "data.new" => Self::DataNew,
            "data.tag" => Self::DataTag,
            "ty.chk" => Self::TyChk,
            "ty.cast" => Self::TyCast,
            "ty.id" => Self::TyId,
            "hdl.push" => Self::HdlPush,
            "hdl.pop" => Self::HdlPop,
            "eff.invk" => Self::EffInvk,
            "eff.resume" => Self::EffResume,
            "ffi.call" => Self::FfiCall,
            _ => return None,
        })
    }

    #[must_use]
    pub const fn from_wire_code(code: u16) -> Option<Self> {
        Some(match code {
            0x0001 => Self::LdLoc,
            0x0002 => Self::StLoc,
            0x0003 => Self::LdConst,
            0x0004 => Self::LdSmi,
            0x0005 => Self::LdStr,
            0x0101 => Self::IAdd,
            0x0201 => Self::CmpEq,
            0x0301 => Self::Br,
            0x0302 => Self::BrFalse,
            0x0303 => Self::BrTbl,
            0x0401 => Self::Call,
            0x0402 => Self::CallTail,
            0x0403 => Self::Ret,
            0x0404 => Self::ClsNew,
            0x0501 => Self::SeqNew,
            0x0601 => Self::DataNew,
            0x0602 => Self::DataTag,
            0x0701 => Self::TyChk,
            0x0702 => Self::TyCast,
            0x0703 => Self::TyId,
            0x0801 => Self::HdlPush,
            0x0802 => Self::HdlPop,
            0x0803 => Self::EffInvk,
            0x0804 => Self::EffResume,
            0x0901 => Self::FfiCall,
            _ => return None,
        })
    }
}
