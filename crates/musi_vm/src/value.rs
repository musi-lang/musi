//! The runtime value type for the Musi VM.

use core::fmt;
use std::rc::Rc;

/// A Musi runtime value.
#[derive(Debug, PartialEq)]
pub enum Value {
    Int(i64),
    Float(f64),
    String(Rc<str>),
    Unit,
    /// A first-class function reference (index into the module function table).
    Function(u16),
    /// A heap-allocated object (record or choice variant).
    /// Layout: for records, fields are in declaration order.
    /// For choices: field[0] = discriminant (Int), field[1..] = payload.
    /// `type_tag` identifies the user-defined type (0 = anonymous/Bool/internal).
    Object { type_tag: u16, fields: Rc<Vec<Self>> },
}

impl Clone for Value {
    fn clone(&self) -> Self {
        match self {
            Self::Int(v) => Self::Int(*v),
            Self::Float(v) => Self::Float(*v),
            Self::String(s) => Self::String(Rc::clone(s)),
            Self::Unit => Self::Unit,
            Self::Function(idx) => Self::Function(*idx),
            Self::Object { type_tag, fields } => Self::Object { type_tag: *type_tag, fields: Rc::clone(fields) },
        }
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Int(v) => write!(f, "{v}"),
            Self::Float(v) => write!(f, "{v}"),
            Self::String(s) => write!(f, "{s}"),
            Self::Unit => write!(f, "()"),
            Self::Function(idx) => write!(f, "<fn {idx}>"),
            Self::Object { .. } => write!(f, "<object>"),
        }
    }
}

#[cfg(test)]
mod tests;
