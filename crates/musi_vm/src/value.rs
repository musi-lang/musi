//! The runtime value type for the Musi VM.

use core::fmt;
use std::rc::Rc;

/// A Musi runtime value.
///
/// `Clone` is implemented manually so that the `Rc<str>` variant uses
/// [`Rc::clone`] (the associated-function form) rather than method syntax,
/// satisfying `clippy::clone_on_ref_ptr`.
#[derive(Debug, PartialEq)]
pub enum Value {
    /// A 64-bit signed integer.
    Int(i64),
    /// A 64-bit IEEE 754 float.
    Float(f64),
    /// A boolean.
    Bool(bool),
    /// A reference-counted, immutable UTF-8 string.
    String(Rc<str>),
    /// The unit value -- the type with exactly one inhabitant.
    Unit,
}

impl Clone for Value {
    fn clone(&self) -> Self {
        match self {
            Self::Int(v) => Self::Int(*v),
            Self::Float(v) => Self::Float(*v),
            Self::Bool(v) => Self::Bool(*v),
            Self::String(s) => Self::String(Rc::clone(s)),
            Self::Unit => Self::Unit,
        }
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Int(v) => write!(f, "{v}"),
            Self::Float(v) => write!(f, "{v}"),
            Self::Bool(v) => write!(f, "{v}"),
            Self::String(s) => write!(f, "{s}"),
            Self::Unit => write!(f, "()"),
        }
    }
}

#[cfg(test)]
mod tests;
