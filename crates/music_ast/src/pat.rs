//! Pattern nodes.

#[cfg(test)]
mod tests;

use music_shared::{Idx, Span, Symbol};

use crate::PatList;
use crate::expr::BindKind;
use crate::lit::Lit;

/// A pattern node. Recursive children use arena indices.
#[derive(Debug, Clone, PartialEq)]
pub enum Pat {
    Wild {
        span: Span,
    },
    Lit {
        lit: Lit,
        span: Span,
    },
    Bind {
        kind: BindKind,
        name: Symbol,
        inner: Option<Idx<Self>>,
        span: Span,
    },
    Variant {
        name: Symbol,
        args: PatList,
        span: Span,
    },
    Record {
        fields: Vec<PatRecField>,
        span: Span,
    },
    Tuple {
        elems: PatList,
        span: Span,
    },
    Array {
        elems: PatList,
        span: Span,
    },
    Or {
        left: Idx<Self>,
        right: Idx<Self>,
        span: Span,
    },
    Error {
        span: Span,
    },
}

/// A field in a record pattern.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PatRecField {
    pub kind: BindKind,
    pub name: Symbol,
    pub pat: Option<Idx<Pat>>,
    pub span: Span,
}
