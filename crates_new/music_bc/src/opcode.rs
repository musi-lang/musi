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
    LdGlob,
    StGlob,
    LdConst,
    LdSmi,
    LdStr,
    IAdd,
    ISub,
    IMul,
    IDiv,
    IRem,
    CmpEq,
    CmpNe,
    CmpLt,
    CmpGt,
    CmpLe,
    CmpGe,
    Br,
    BrFalse,
    BrTbl,
    Call,
    CallSeq,
    CallCls,
    CallClsSeq,
    CallTail,
    Ret,
    ClsNew,
    SeqNew,
    SeqGet,
    SeqSet,
    SeqCat,
    DataNew,
    DataTag,
    DataGet,
    DataSet,
    TyChk,
    TyCast,
    TyId,
    HdlPush,
    HdlPop,
    EffInvk,
    EffInvkSeq,
    EffResume,
    FfiCall,
    FfiCallSeq,
}

impl Opcode {
    #[must_use]
    pub const fn family(self) -> OpcodeFamily {
        match self {
            Self::LdLoc
            | Self::StLoc
            | Self::LdGlob
            | Self::StGlob
            | Self::LdConst
            | Self::LdSmi
            | Self::LdStr => OpcodeFamily::LoadStore,
            Self::IAdd | Self::ISub | Self::IMul | Self::IDiv | Self::IRem => OpcodeFamily::Scalar,
            Self::CmpEq | Self::CmpNe | Self::CmpLt | Self::CmpGt | Self::CmpLe | Self::CmpGe => {
                OpcodeFamily::LogicCompare
            }
            Self::Br | Self::BrFalse | Self::BrTbl => OpcodeFamily::Branch,
            Self::Call
            | Self::CallSeq
            | Self::CallCls
            | Self::CallClsSeq
            | Self::CallTail
            | Self::Ret
            | Self::ClsNew => OpcodeFamily::CallClosure,
            Self::SeqNew | Self::SeqGet | Self::SeqSet | Self::SeqCat => OpcodeFamily::Sequence,
            Self::DataNew | Self::DataTag | Self::DataGet | Self::DataSet => OpcodeFamily::Data,
            Self::TyChk | Self::TyCast | Self::TyId => OpcodeFamily::Ty,
            Self::HdlPush
            | Self::HdlPop
            | Self::EffInvk
            | Self::EffInvkSeq
            | Self::EffResume => OpcodeFamily::Eff,
            Self::FfiCall | Self::FfiCallSeq => OpcodeFamily::Ffi,
        }
    }

    #[must_use]
    pub const fn mnemonic(self) -> &'static str {
        match self {
            Self::LdLoc => "ld.loc",
            Self::StLoc => "st.loc",
            Self::LdGlob => "ld.glob",
            Self::StGlob => "st.glob",
            Self::LdConst => "ld.const",
            Self::LdSmi => "ld.smi",
            Self::LdStr => "ld.str",
            Self::IAdd => "i.add",
            Self::ISub => "i.sub",
            Self::IMul => "i.mul",
            Self::IDiv => "i.div",
            Self::IRem => "i.rem",
            Self::CmpEq => "cmp.eq",
            Self::CmpNe => "cmp.ne",
            Self::CmpLt => "cmp.lt",
            Self::CmpGt => "cmp.gt",
            Self::CmpLe => "cmp.le",
            Self::CmpGe => "cmp.ge",
            Self::Br => "br",
            Self::BrFalse => "br.false",
            Self::BrTbl => "br.tbl",
            Self::Call => "call",
            Self::CallSeq => "call.seq",
            Self::CallCls => "call.cls",
            Self::CallClsSeq => "callcls.seq",
            Self::CallTail => "call.tail",
            Self::Ret => "ret",
            Self::ClsNew => "cls.new",
            Self::SeqNew => "seq.new",
            Self::SeqGet => "seq.get",
            Self::SeqSet => "seq.set",
            Self::SeqCat => "seq.cat",
            Self::DataNew => "data.new",
            Self::DataTag => "data.tag",
            Self::DataGet => "data.get",
            Self::DataSet => "data.set",
            Self::TyChk => "ty.chk",
            Self::TyCast => "ty.cast",
            Self::TyId => "ty.id",
            Self::HdlPush => "hdl.push",
            Self::HdlPop => "hdl.pop",
            Self::EffInvk => "eff.invk",
            Self::EffInvkSeq => "eff.invk.seq",
            Self::EffResume => "eff.resume",
            Self::FfiCall => "ffi.call",
            Self::FfiCallSeq => "ffi.call.seq",
        }
    }

    #[must_use]
    pub const fn operand_shape(self) -> OperandShape {
        match self {
            Self::LdLoc | Self::StLoc => OperandShape::Local,
            Self::LdGlob | Self::StGlob => OperandShape::Global,
            Self::LdConst => OperandShape::Constant,
            Self::LdSmi => OperandShape::I16,
            Self::LdStr => OperandShape::String,
            Self::IAdd
            | Self::ISub
            | Self::IMul
            | Self::IDiv
            | Self::IRem
            | Self::CmpEq
            | Self::CmpNe
            | Self::CmpLt
            | Self::CmpGt
            | Self::CmpLe
            | Self::CmpGe
            | Self::Ret
            | Self::SeqGet
            | Self::SeqSet
            | Self::SeqCat
            | Self::HdlPop
            | Self::EffResume
            | Self::CallCls
            | Self::CallClsSeq
            | Self::DataGet
            | Self::DataSet => OperandShape::None,
            Self::Br | Self::BrFalse => OperandShape::Label,
            Self::BrTbl => OperandShape::BranchTable,
            Self::Call | Self::CallSeq | Self::CallTail => OperandShape::Method,
            Self::ClsNew => OperandShape::WideMethodCaptures,
            Self::SeqNew | Self::DataNew => OperandShape::TypeLen,
            Self::DataTag | Self::TyChk | Self::TyCast | Self::TyId => OperandShape::Type,
            Self::HdlPush => OperandShape::EffectId,
            Self::EffInvk | Self::EffInvkSeq => OperandShape::Effect,
            Self::FfiCall | Self::FfiCallSeq => OperandShape::Foreign,
        }
    }

    #[must_use]
    pub const fn wire_code(self) -> u16 {
        match self {
            Self::LdLoc => 0x0001,
            Self::StLoc => 0x0002,
            Self::LdGlob => 0x0003,
            Self::StGlob => 0x0004,
            Self::LdConst => 0x0005,
            Self::LdSmi => 0x0006,
            Self::LdStr => 0x0007,
            Self::IAdd => 0x0101,
            Self::ISub => 0x0102,
            Self::IMul => 0x0103,
            Self::IDiv => 0x0104,
            Self::IRem => 0x0105,
            Self::CmpEq => 0x0201,
            Self::CmpNe => 0x0202,
            Self::CmpLt => 0x0203,
            Self::CmpGt => 0x0204,
            Self::CmpLe => 0x0205,
            Self::CmpGe => 0x0206,
            Self::Br => 0x0301,
            Self::BrFalse => 0x0302,
            Self::BrTbl => 0x0303,
            Self::Call => 0x0401,
            Self::CallCls => 0x0402,
            Self::CallTail => 0x0403,
            Self::Ret => 0x0404,
            Self::ClsNew => 0x0405,
            Self::CallSeq => 0x0406,
            Self::CallClsSeq => 0x0407,
            Self::SeqNew => 0x0501,
            Self::SeqGet => 0x0502,
            Self::SeqSet => 0x0503,
            Self::SeqCat => 0x0504,
            Self::DataNew => 0x0601,
            Self::DataTag => 0x0602,
            Self::DataGet => 0x0603,
            Self::DataSet => 0x0604,
            Self::TyChk => 0x0701,
            Self::TyCast => 0x0702,
            Self::TyId => 0x0703,
            Self::HdlPush => 0x0801,
            Self::HdlPop => 0x0802,
            Self::EffInvk => 0x0803,
            Self::EffResume => 0x0804,
            Self::EffInvkSeq => 0x0805,
            Self::FfiCall => 0x0901,
            Self::FfiCallSeq => 0x0902,
        }
    }

    #[must_use]
    pub fn from_mnemonic(text: &str) -> Option<Self> {
        Some(match text {
            "ld.loc" => Self::LdLoc,
            "st.loc" => Self::StLoc,
            "ld.glob" => Self::LdGlob,
            "st.glob" => Self::StGlob,
            "ld.const" => Self::LdConst,
            "ld.smi" => Self::LdSmi,
            "ld.str" => Self::LdStr,
            "i.add" => Self::IAdd,
            "i.sub" => Self::ISub,
            "i.mul" => Self::IMul,
            "i.div" => Self::IDiv,
            "i.rem" => Self::IRem,
            "cmp.eq" => Self::CmpEq,
            "cmp.ne" => Self::CmpNe,
            "cmp.lt" => Self::CmpLt,
            "cmp.gt" => Self::CmpGt,
            "cmp.le" => Self::CmpLe,
            "cmp.ge" => Self::CmpGe,
            "br" => Self::Br,
            "br.false" => Self::BrFalse,
            "br.tbl" => Self::BrTbl,
            "call" => Self::Call,
            "call.seq" => Self::CallSeq,
            "call.cls" => Self::CallCls,
            "callcls.seq" => Self::CallClsSeq,
            "call.tail" => Self::CallTail,
            "ret" => Self::Ret,
            "cls.new" => Self::ClsNew,
            "seq.new" => Self::SeqNew,
            "seq.get" => Self::SeqGet,
            "seq.set" => Self::SeqSet,
            "seq.cat" => Self::SeqCat,
            "data.new" => Self::DataNew,
            "data.tag" => Self::DataTag,
            "data.get" => Self::DataGet,
            "data.set" => Self::DataSet,
            "ty.chk" => Self::TyChk,
            "ty.cast" => Self::TyCast,
            "ty.id" => Self::TyId,
            "hdl.push" => Self::HdlPush,
            "hdl.pop" => Self::HdlPop,
            "eff.invk" => Self::EffInvk,
            "eff.invk.seq" => Self::EffInvkSeq,
            "eff.resume" => Self::EffResume,
            "ffi.call" => Self::FfiCall,
            "ffi.call.seq" => Self::FfiCallSeq,
            _ => return None,
        })
    }

    #[must_use]
    pub const fn from_wire_code(code: u16) -> Option<Self> {
        Some(match code {
            0x0001 => Self::LdLoc,
            0x0002 => Self::StLoc,
            0x0003 => Self::LdGlob,
            0x0004 => Self::StGlob,
            0x0005 => Self::LdConst,
            0x0006 => Self::LdSmi,
            0x0007 => Self::LdStr,
            0x0101 => Self::IAdd,
            0x0102 => Self::ISub,
            0x0103 => Self::IMul,
            0x0104 => Self::IDiv,
            0x0105 => Self::IRem,
            0x0201 => Self::CmpEq,
            0x0202 => Self::CmpNe,
            0x0203 => Self::CmpLt,
            0x0204 => Self::CmpGt,
            0x0205 => Self::CmpLe,
            0x0206 => Self::CmpGe,
            0x0301 => Self::Br,
            0x0302 => Self::BrFalse,
            0x0303 => Self::BrTbl,
            0x0401 => Self::Call,
            0x0402 => Self::CallCls,
            0x0403 => Self::CallTail,
            0x0404 => Self::Ret,
            0x0405 => Self::ClsNew,
            0x0406 => Self::CallSeq,
            0x0407 => Self::CallClsSeq,
            0x0501 => Self::SeqNew,
            0x0502 => Self::SeqGet,
            0x0503 => Self::SeqSet,
            0x0504 => Self::SeqCat,
            0x0601 => Self::DataNew,
            0x0602 => Self::DataTag,
            0x0603 => Self::DataGet,
            0x0604 => Self::DataSet,
            0x0701 => Self::TyChk,
            0x0702 => Self::TyCast,
            0x0703 => Self::TyId,
            0x0801 => Self::HdlPush,
            0x0802 => Self::HdlPop,
            0x0803 => Self::EffInvk,
            0x0804 => Self::EffResume,
            0x0805 => Self::EffInvkSeq,
            0x0901 => Self::FfiCall,
            0x0902 => Self::FfiCallSeq,
            _ => return None,
        })
    }
}
