//! Attributes and attribute values.

use music_shared::{Span, Symbol};

use crate::lit::Lit;

/// An attribute annotation (e.g. `@inline`).
#[derive(Debug, Clone, PartialEq)]
pub struct Attr {
    pub name: Symbol,
    pub value: Option<AttrValue>,
    pub span: Span,
}

/// The value part of an attribute.
#[derive(Debug, Clone, PartialEq)]
pub enum AttrValue {
    Lit { lit: Lit, span: Span },
    Tuple { lits: Vec<Lit>, span: Span },
    Named { fields: Vec<AttrField>, span: Span },
}

/// A named field in an attribute (e.g. `key := "value"`).
#[derive(Debug, Clone, PartialEq)]
pub struct AttrField {
    pub name: Symbol,
    pub value: Lit,
    pub span: Span,
}
