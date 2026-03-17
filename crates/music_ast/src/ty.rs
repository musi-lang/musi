//! Type nodes.

#[cfg(test)]
mod tests;

use music_shared::{Span, Symbol};

use crate::expr::Arrow;
use crate::{TyIdx, TyList};

/// A type node. Recursive children use arena indices.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Ty {
    Var {
        name: Symbol,
        span: Span,
    },
    Named {
        name: Symbol,
        args: TyList,
        span: Span,
    },
    Qualified {
        module: Symbol,
        name: Symbol,
        args: TyList,
        span: Span,
    },
    Option {
        inner: TyIdx,
        span: Span,
    },
    Fn {
        params: TyList,
        ret: TyIdx,
        arrow: Arrow,
        effects: Option<EffectSet>,
        span: Span,
    },
    Product {
        fields: TyList,
        span: Span,
    },
    Sum {
        variants: TyList,
        span: Span,
    },
    Array {
        len: Option<u32>,
        elem: TyIdx,
        span: Span,
    },
    Error {
        span: Span,
    },
}

/// A set of effects on a function type.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct EffectSet {
    pub effects: Vec<EffectItem>,
    pub span: Span,
}

/// An individual effect in an effect set.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum EffectItem {
    Named {
        name: Symbol,
        arg: Option<TyIdx>,
        span: Span,
    },
    Var {
        name: Symbol,
        span: Span,
    },
}

/// A type parameter (e.g. `'T`).
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct TyParam {
    pub name: Symbol,
    pub span: Span,
}

/// A named type with optional type arguments (used in constraints).
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TyNamedRef {
    pub name: Symbol,
    pub args: TyList,
    pub span: Span,
}

/// A constraint on a type parameter (e.g. `'T <: Eq`).
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Constraint {
    pub param: Symbol,
    pub rel: Rel,
    pub bound: TyNamedRef,
    pub span: Span,
}

/// Constraint relation direction.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Rel {
    Sub,
    Super,
}
