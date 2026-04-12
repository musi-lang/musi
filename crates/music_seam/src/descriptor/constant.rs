use music_term::SyntaxShape;

use crate::artifact::StringId;

#[derive(Debug, Clone)]
pub enum ConstantValue {
    Int(i64),
    Float(f64),
    Bool(bool),
    String(StringId),
    Syntax { shape: SyntaxShape, text: StringId },
}

impl PartialEq for ConstantValue {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Int(left), Self::Int(right)) => left == right,
            (Self::Float(left), Self::Float(right)) => left.to_bits() == right.to_bits(),
            (Self::Bool(left), Self::Bool(right)) => left == right,
            (Self::String(left), Self::String(right)) => left == right,
            (
                Self::Syntax {
                    shape: left_shape,
                    text: left_text,
                },
                Self::Syntax {
                    shape: right_shape,
                    text: right_text,
                },
            ) => left_shape == right_shape && left_text == right_text,
            _ => false,
        }
    }
}

impl Eq for ConstantValue {}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ConstantDescriptor {
    pub name: StringId,
    pub value: ConstantValue,
}

impl ConstantDescriptor {
    #[must_use]
    pub const fn new(name: StringId, value: ConstantValue) -> Self {
        Self { name, value }
    }
}
