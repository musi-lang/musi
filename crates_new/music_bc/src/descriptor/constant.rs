use music_arena::Idx;

use crate::artifact::StringRecord;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ConstantValue {
    Int(i64),
    Bool(bool),
    String(Idx<StringRecord>),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ConstantDescriptor {
    pub name: Idx<StringRecord>,
    pub value: ConstantValue,
}
