use music_arena::Idx;

use crate::artifact::StringRecord;

#[derive(Debug, Clone)]
pub enum ConstantValue {
    Int(i64),
    Float(f64),
    Bool(bool),
    String(Idx<StringRecord>),
}

impl PartialEq for ConstantValue {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Int(left), Self::Int(right)) => left == right,
            (Self::Float(left), Self::Float(right)) => left.to_bits() == right.to_bits(),
            (Self::Bool(left), Self::Bool(right)) => left == right,
            (Self::String(left), Self::String(right)) => left == right,
            _ => false,
        }
    }
}

impl Eq for ConstantValue {}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ConstantDescriptor {
    pub name: Idx<StringRecord>,
    pub value: ConstantValue,
}
