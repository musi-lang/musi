#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum OpcodeFamily {
    Core,
    Storage,
    Scalar,
    Branch,
    Call,
    Object,
    Type,
    Module,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum OpcodeVisibility {
    Public,
    Internal,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Opcode {
    LdC,
    LdCI4,
    LdStr,
    LdLoc,
    StLoc,
    LdGlob,
    StGlob,
    LdFld,
    StFld,
    Add,
    Sub,
    Mul,
    DivS,
    RemS,
    And,
    Or,
    Xor,
    Not,
    Ceq,
    Cne,
    CltS,
    CgtS,
    CleS,
    CgeS,
    Br,
    BrZ,
    BrTbl,
    Ret,
    Call,
    CallInd,
    CallFfi,
    TailCall,
    NewFn,
    LdFfi,
    NewObj,
    NewArr,
    LdElem,
    StElem,
    LdLen,
    LdType,
    IsInst,
    Cast,
    LdModDyn,
    LdExpDyn,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum OpcodeWire {
    Core(u8),
    Extended(u16),
}
