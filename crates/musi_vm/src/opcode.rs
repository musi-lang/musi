use music_seam::Opcode;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum VmOpcodeFamily {
    LoadStore,
    Scalar,
    Branch,
    Call,
    Sequence,
    Data,
    Types,
    Effects,
    Host,
}

#[must_use]
pub const fn classify_opcode(opcode: Opcode) -> VmOpcodeFamily {
    match opcode {
        Opcode::LdLoc
        | Opcode::StLoc
        | Opcode::LdGlob
        | Opcode::StGlob
        | Opcode::LdConst
        | Opcode::LdSmi
        | Opcode::LdStr => VmOpcodeFamily::LoadStore,
        Opcode::IAdd
        | Opcode::ISub
        | Opcode::IMul
        | Opcode::IDiv
        | Opcode::IRem
        | Opcode::FAdd
        | Opcode::FSub
        | Opcode::FMul
        | Opcode::FDiv
        | Opcode::FRem
        | Opcode::StrCat
        | Opcode::CmpEq
        | Opcode::CmpNe
        | Opcode::CmpLt
        | Opcode::CmpGt
        | Opcode::CmpLe
        | Opcode::CmpGe => VmOpcodeFamily::Scalar,
        Opcode::Br | Opcode::BrFalse | Opcode::BrTbl => VmOpcodeFamily::Branch,
        Opcode::Call
        | Opcode::CallSeq
        | Opcode::CallCls
        | Opcode::CallClsSeq
        | Opcode::CallTail
        | Opcode::Ret
        | Opcode::ClsNew => VmOpcodeFamily::Call,
        Opcode::SeqNew
        | Opcode::SeqGet
        | Opcode::SeqGetN
        | Opcode::SeqSet
        | Opcode::SeqSetN
        | Opcode::SeqCat
        | Opcode::RangeNew
        | Opcode::RangeContains
        | Opcode::RangeMaterialize
        | Opcode::SeqHas => VmOpcodeFamily::Sequence,
        Opcode::DataNew | Opcode::DataTag | Opcode::DataGet | Opcode::DataSet => {
            VmOpcodeFamily::Data
        }
        Opcode::TyId | Opcode::TyApply | Opcode::TyChk | Opcode::TyCast => VmOpcodeFamily::Types,
        Opcode::HdlPush
        | Opcode::HdlPop
        | Opcode::EffInvk
        | Opcode::EffInvkSeq
        | Opcode::EffResume => VmOpcodeFamily::Effects,
        Opcode::FfiCall
        | Opcode::FfiCallSeq
        | Opcode::FfiRef
        | Opcode::ModLoad
        | Opcode::ModGet => VmOpcodeFamily::Host,
    }
}
