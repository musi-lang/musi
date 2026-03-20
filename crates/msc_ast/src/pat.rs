//! Pattern nodes.

#[cfg(test)]
mod tests;

use msc_shared::{Span, Symbol};

use crate::expr::BindKind;
use crate::lit::Lit;
use crate::{PatIdx, PatList};

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
        inner: Option<PatIdx>,
        span: Span,
    },
    Variant {
        name: Symbol,
        args: PatList,
        span: Span,
    },
    Record {
        ty_name: Option<Symbol>,
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
        left: PatIdx,
        right: PatIdx,
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
    pub pat: Option<PatIdx>,
    pub span: Span,
}
