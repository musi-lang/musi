//! Type parameter declarations and constraints.

use msc_shared::{Span, Symbol};

use crate::ExprIdx;

/// A type parameter (e.g. `'T`).
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct TyParam {
    pub name: Symbol,
    pub span: Span,
}

/// A constraint on a type parameter (e.g. `'T <: Eq`).
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Constraint {
    pub param: Symbol,
    pub rel: Rel,
    /// The bound type expression (e.g. `Eq` or `Functor of 'F`).
    pub bound: ExprIdx,
    pub span: Span,
}

/// Constraint relation direction.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Rel {
    Sub,
    Super,
    Member,
}
