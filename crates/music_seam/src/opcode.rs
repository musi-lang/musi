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
    Module,
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
    FAdd,
    FSub,
    FMul,
    FDiv,
    FRem,
    StrCat,
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
    SeqGetN,
    SeqSet,
    SeqSetN,
    SeqCat,
    RangeNew,
    RangeContains,
    RangeMaterialize,
    SeqHas,
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
    ModLoad,
    ModGet,
}

impl Opcode {
    #[must_use]
    pub fn family(self) -> OpcodeFamily {
        self.info().family
    }

    #[must_use]
    pub fn mnemonic(self) -> &'static str {
        self.info().mnemonic
    }

    #[must_use]
    pub fn operand_shape(self) -> OperandShape {
        self.info().operand_shape
    }

    #[must_use]
    pub fn wire_code(self) -> u16 {
        self.info().wire_code
    }

    #[must_use]
    pub fn from_mnemonic(text: &str) -> Option<Self> {
        let mut index = 0;
        while index < OPCODE_INFOS.len() {
            let info = &OPCODE_INFOS[index];
            if info.mnemonic == text {
                return Some(info.opcode);
            }
            index += 1;
        }
        None
    }

    #[must_use]
    pub fn from_wire_code(code: u16) -> Option<Self> {
        let mut index = 0;
        while index < OPCODE_INFOS.len() {
            let info = &OPCODE_INFOS[index];
            if info.wire_code == code {
                return Some(info.opcode);
            }
            index += 1;
        }
        None
    }

    fn info(self) -> &'static OpcodeInfo {
        let mut index = 0;
        while index < OPCODE_INFOS.len() {
            let info = &OPCODE_INFOS[index];
            if info.opcode == self {
                return info;
            }
            index += 1;
        }
        &OPCODE_INFOS[0]
    }
}

struct OpcodeInfo {
    opcode: Opcode,
    family: OpcodeFamily,
    mnemonic: &'static str,
    operand_shape: OperandShape,
    wire_code: u16,
}

const OPCODE_INFOS: &[OpcodeInfo] = &[
    OpcodeInfo {
        opcode: Opcode::LdLoc,
        family: OpcodeFamily::LoadStore,
        mnemonic: "ld.loc",
        operand_shape: OperandShape::Local,
        wire_code: 0x0001,
    },
    OpcodeInfo {
        opcode: Opcode::StLoc,
        family: OpcodeFamily::LoadStore,
        mnemonic: "st.loc",
        operand_shape: OperandShape::Local,
        wire_code: 0x0002,
    },
    OpcodeInfo {
        opcode: Opcode::LdGlob,
        family: OpcodeFamily::LoadStore,
        mnemonic: "ld.glob",
        operand_shape: OperandShape::Global,
        wire_code: 0x0003,
    },
    OpcodeInfo {
        opcode: Opcode::StGlob,
        family: OpcodeFamily::LoadStore,
        mnemonic: "st.glob",
        operand_shape: OperandShape::Global,
        wire_code: 0x0004,
    },
    OpcodeInfo {
        opcode: Opcode::LdConst,
        family: OpcodeFamily::LoadStore,
        mnemonic: "ld.const",
        operand_shape: OperandShape::Constant,
        wire_code: 0x0005,
    },
    OpcodeInfo {
        opcode: Opcode::LdSmi,
        family: OpcodeFamily::LoadStore,
        mnemonic: "ld.smi",
        operand_shape: OperandShape::I16,
        wire_code: 0x0006,
    },
    OpcodeInfo {
        opcode: Opcode::LdStr,
        family: OpcodeFamily::LoadStore,
        mnemonic: "ld.str",
        operand_shape: OperandShape::String,
        wire_code: 0x0007,
    },
    OpcodeInfo {
        opcode: Opcode::IAdd,
        family: OpcodeFamily::Scalar,
        mnemonic: "i.add",
        operand_shape: OperandShape::None,
        wire_code: 0x0101,
    },
    OpcodeInfo {
        opcode: Opcode::ISub,
        family: OpcodeFamily::Scalar,
        mnemonic: "i.sub",
        operand_shape: OperandShape::None,
        wire_code: 0x0102,
    },
    OpcodeInfo {
        opcode: Opcode::IMul,
        family: OpcodeFamily::Scalar,
        mnemonic: "i.mul",
        operand_shape: OperandShape::None,
        wire_code: 0x0103,
    },
    OpcodeInfo {
        opcode: Opcode::IDiv,
        family: OpcodeFamily::Scalar,
        mnemonic: "i.div",
        operand_shape: OperandShape::None,
        wire_code: 0x0104,
    },
    OpcodeInfo {
        opcode: Opcode::IRem,
        family: OpcodeFamily::Scalar,
        mnemonic: "i.rem",
        operand_shape: OperandShape::None,
        wire_code: 0x0105,
    },
    OpcodeInfo {
        opcode: Opcode::FAdd,
        family: OpcodeFamily::Scalar,
        mnemonic: "f.add",
        operand_shape: OperandShape::None,
        wire_code: 0x0106,
    },
    OpcodeInfo {
        opcode: Opcode::FSub,
        family: OpcodeFamily::Scalar,
        mnemonic: "f.sub",
        operand_shape: OperandShape::None,
        wire_code: 0x0107,
    },
    OpcodeInfo {
        opcode: Opcode::FMul,
        family: OpcodeFamily::Scalar,
        mnemonic: "f.mul",
        operand_shape: OperandShape::None,
        wire_code: 0x0108,
    },
    OpcodeInfo {
        opcode: Opcode::FDiv,
        family: OpcodeFamily::Scalar,
        mnemonic: "f.div",
        operand_shape: OperandShape::None,
        wire_code: 0x0109,
    },
    OpcodeInfo {
        opcode: Opcode::FRem,
        family: OpcodeFamily::Scalar,
        mnemonic: "f.rem",
        operand_shape: OperandShape::None,
        wire_code: 0x010A,
    },
    OpcodeInfo {
        opcode: Opcode::StrCat,
        family: OpcodeFamily::Scalar,
        mnemonic: "str.cat",
        operand_shape: OperandShape::None,
        wire_code: 0x010B,
    },
    OpcodeInfo {
        opcode: Opcode::CmpEq,
        family: OpcodeFamily::LogicCompare,
        mnemonic: "cmp.eq",
        operand_shape: OperandShape::None,
        wire_code: 0x0201,
    },
    OpcodeInfo {
        opcode: Opcode::CmpNe,
        family: OpcodeFamily::LogicCompare,
        mnemonic: "cmp.ne",
        operand_shape: OperandShape::None,
        wire_code: 0x0202,
    },
    OpcodeInfo {
        opcode: Opcode::CmpLt,
        family: OpcodeFamily::LogicCompare,
        mnemonic: "cmp.lt",
        operand_shape: OperandShape::None,
        wire_code: 0x0203,
    },
    OpcodeInfo {
        opcode: Opcode::CmpGt,
        family: OpcodeFamily::LogicCompare,
        mnemonic: "cmp.gt",
        operand_shape: OperandShape::None,
        wire_code: 0x0204,
    },
    OpcodeInfo {
        opcode: Opcode::CmpLe,
        family: OpcodeFamily::LogicCompare,
        mnemonic: "cmp.le",
        operand_shape: OperandShape::None,
        wire_code: 0x0205,
    },
    OpcodeInfo {
        opcode: Opcode::CmpGe,
        family: OpcodeFamily::LogicCompare,
        mnemonic: "cmp.ge",
        operand_shape: OperandShape::None,
        wire_code: 0x0206,
    },
    OpcodeInfo {
        opcode: Opcode::Br,
        family: OpcodeFamily::Branch,
        mnemonic: "br",
        operand_shape: OperandShape::Label,
        wire_code: 0x0301,
    },
    OpcodeInfo {
        opcode: Opcode::BrFalse,
        family: OpcodeFamily::Branch,
        mnemonic: "br.false",
        operand_shape: OperandShape::Label,
        wire_code: 0x0302,
    },
    OpcodeInfo {
        opcode: Opcode::BrTbl,
        family: OpcodeFamily::Branch,
        mnemonic: "br.tbl",
        operand_shape: OperandShape::BranchTable,
        wire_code: 0x0303,
    },
    OpcodeInfo {
        opcode: Opcode::Call,
        family: OpcodeFamily::CallClosure,
        mnemonic: "call",
        operand_shape: OperandShape::Method,
        wire_code: 0x0401,
    },
    OpcodeInfo {
        opcode: Opcode::CallSeq,
        family: OpcodeFamily::CallClosure,
        mnemonic: "call.seq",
        operand_shape: OperandShape::Method,
        wire_code: 0x0406,
    },
    OpcodeInfo {
        opcode: Opcode::CallCls,
        family: OpcodeFamily::CallClosure,
        mnemonic: "call.cls",
        operand_shape: OperandShape::None,
        wire_code: 0x0402,
    },
    OpcodeInfo {
        opcode: Opcode::CallClsSeq,
        family: OpcodeFamily::CallClosure,
        mnemonic: "callcls.seq",
        operand_shape: OperandShape::None,
        wire_code: 0x0407,
    },
    OpcodeInfo {
        opcode: Opcode::CallTail,
        family: OpcodeFamily::CallClosure,
        mnemonic: "call.tail",
        operand_shape: OperandShape::Method,
        wire_code: 0x0403,
    },
    OpcodeInfo {
        opcode: Opcode::Ret,
        family: OpcodeFamily::CallClosure,
        mnemonic: "ret",
        operand_shape: OperandShape::None,
        wire_code: 0x0404,
    },
    OpcodeInfo {
        opcode: Opcode::ClsNew,
        family: OpcodeFamily::CallClosure,
        mnemonic: "cls.new",
        operand_shape: OperandShape::WideMethodCaptures,
        wire_code: 0x0405,
    },
    OpcodeInfo {
        opcode: Opcode::SeqNew,
        family: OpcodeFamily::Sequence,
        mnemonic: "seq.new",
        operand_shape: OperandShape::TypeLen,
        wire_code: 0x0501,
    },
    OpcodeInfo {
        opcode: Opcode::SeqGet,
        family: OpcodeFamily::Sequence,
        mnemonic: "seq.get",
        operand_shape: OperandShape::None,
        wire_code: 0x0502,
    },
    OpcodeInfo {
        opcode: Opcode::SeqGetN,
        family: OpcodeFamily::Sequence,
        mnemonic: "seq.getn",
        operand_shape: OperandShape::I16,
        wire_code: 0x0503,
    },
    OpcodeInfo {
        opcode: Opcode::SeqSet,
        family: OpcodeFamily::Sequence,
        mnemonic: "seq.set",
        operand_shape: OperandShape::None,
        wire_code: 0x0504,
    },
    OpcodeInfo {
        opcode: Opcode::SeqSetN,
        family: OpcodeFamily::Sequence,
        mnemonic: "seq.setn",
        operand_shape: OperandShape::I16,
        wire_code: 0x0505,
    },
    OpcodeInfo {
        opcode: Opcode::SeqCat,
        family: OpcodeFamily::Sequence,
        mnemonic: "seq.cat",
        operand_shape: OperandShape::None,
        wire_code: 0x0506,
    },
    OpcodeInfo {
        opcode: Opcode::RangeNew,
        family: OpcodeFamily::Sequence,
        mnemonic: "range.new",
        operand_shape: OperandShape::TypeLen,
        wire_code: 0x0507,
    },
    OpcodeInfo {
        opcode: Opcode::RangeContains,
        family: OpcodeFamily::Sequence,
        mnemonic: "range.contains",
        operand_shape: OperandShape::None,
        wire_code: 0x0508,
    },
    OpcodeInfo {
        opcode: Opcode::RangeMaterialize,
        family: OpcodeFamily::Sequence,
        mnemonic: "range.materialize",
        operand_shape: OperandShape::None,
        wire_code: 0x0509,
    },
    OpcodeInfo {
        opcode: Opcode::SeqHas,
        family: OpcodeFamily::Sequence,
        mnemonic: "seq.has",
        operand_shape: OperandShape::None,
        wire_code: 0x050A,
    },
    OpcodeInfo {
        opcode: Opcode::DataNew,
        family: OpcodeFamily::Data,
        mnemonic: "data.new",
        operand_shape: OperandShape::TypeLen,
        wire_code: 0x0601,
    },
    OpcodeInfo {
        opcode: Opcode::DataTag,
        family: OpcodeFamily::Data,
        mnemonic: "data.tag",
        operand_shape: OperandShape::Type,
        wire_code: 0x0602,
    },
    OpcodeInfo {
        opcode: Opcode::DataGet,
        family: OpcodeFamily::Data,
        mnemonic: "data.get",
        operand_shape: OperandShape::None,
        wire_code: 0x0603,
    },
    OpcodeInfo {
        opcode: Opcode::DataSet,
        family: OpcodeFamily::Data,
        mnemonic: "data.set",
        operand_shape: OperandShape::None,
        wire_code: 0x0604,
    },
    OpcodeInfo {
        opcode: Opcode::TyChk,
        family: OpcodeFamily::Ty,
        mnemonic: "ty.chk",
        operand_shape: OperandShape::Type,
        wire_code: 0x0701,
    },
    OpcodeInfo {
        opcode: Opcode::TyCast,
        family: OpcodeFamily::Ty,
        mnemonic: "ty.cast",
        operand_shape: OperandShape::Type,
        wire_code: 0x0702,
    },
    OpcodeInfo {
        opcode: Opcode::TyId,
        family: OpcodeFamily::Ty,
        mnemonic: "ty.id",
        operand_shape: OperandShape::Type,
        wire_code: 0x0703,
    },
    OpcodeInfo {
        opcode: Opcode::HdlPush,
        family: OpcodeFamily::Eff,
        mnemonic: "hdl.push",
        operand_shape: OperandShape::EffectId,
        wire_code: 0x0801,
    },
    OpcodeInfo {
        opcode: Opcode::HdlPop,
        family: OpcodeFamily::Eff,
        mnemonic: "hdl.pop",
        operand_shape: OperandShape::None,
        wire_code: 0x0802,
    },
    OpcodeInfo {
        opcode: Opcode::EffInvk,
        family: OpcodeFamily::Eff,
        mnemonic: "eff.invk",
        operand_shape: OperandShape::Effect,
        wire_code: 0x0803,
    },
    OpcodeInfo {
        opcode: Opcode::EffInvkSeq,
        family: OpcodeFamily::Eff,
        mnemonic: "eff.invk.seq",
        operand_shape: OperandShape::Effect,
        wire_code: 0x0805,
    },
    OpcodeInfo {
        opcode: Opcode::EffResume,
        family: OpcodeFamily::Eff,
        mnemonic: "eff.resume",
        operand_shape: OperandShape::None,
        wire_code: 0x0804,
    },
    OpcodeInfo {
        opcode: Opcode::FfiCall,
        family: OpcodeFamily::Ffi,
        mnemonic: "ffi.call",
        operand_shape: OperandShape::Foreign,
        wire_code: 0x0901,
    },
    OpcodeInfo {
        opcode: Opcode::FfiCallSeq,
        family: OpcodeFamily::Ffi,
        mnemonic: "ffi.call.seq",
        operand_shape: OperandShape::Foreign,
        wire_code: 0x0902,
    },
    OpcodeInfo {
        opcode: Opcode::ModLoad,
        family: OpcodeFamily::Module,
        mnemonic: "mod.load",
        operand_shape: OperandShape::None,
        wire_code: 0x0A01,
    },
    OpcodeInfo {
        opcode: Opcode::ModGet,
        family: OpcodeFamily::Module,
        mnemonic: "mod.get",
        operand_shape: OperandShape::String,
        wire_code: 0x0A02,
    },
];
