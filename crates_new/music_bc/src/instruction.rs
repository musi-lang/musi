use music_arena::Idx;

use crate::artifact::StringRecord;
use crate::descriptor::{
    ConstantDescriptor, EffectDescriptor, ForeignDescriptor, GlobalDescriptor, MethodDescriptor,
    TypeDescriptor,
};
use crate::opcode::Opcode;

pub type LabelId = u16;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum OperandShape {
    None,
    I16,
    Local,
    String,
    Type,
    Constant,
    Global,
    Method,
    Foreign,
    Effect,
    Label,
    TypeLen,
    BranchTable,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Label {
    pub id: LabelId,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Operand {
    None,
    I16(i16),
    Local(u16),
    String(Idx<StringRecord>),
    Type(Idx<TypeDescriptor>),
    Constant(Idx<ConstantDescriptor>),
    Global(Idx<GlobalDescriptor>),
    Method(Idx<MethodDescriptor>),
    Foreign(Idx<ForeignDescriptor>),
    Effect {
        effect: Idx<EffectDescriptor>,
        op: u16,
    },
    Label(LabelId),
    TypeLen {
        ty: Idx<TypeDescriptor>,
        len: u16,
    },
    BranchTable(Box<[LabelId]>),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Instruction {
    pub opcode: Opcode,
    pub operand: Operand,
}

impl Instruction {
    #[must_use]
    pub const fn new(opcode: Opcode, operand: Operand) -> Self {
        Self { opcode, operand }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum CodeEntry {
    Label(Label),
    Instruction(Instruction),
}
