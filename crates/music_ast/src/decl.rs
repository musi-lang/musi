//! Declaration-level nodes: class members, function signatures, exports.

use music_shared::{Idx, Span, Symbol};

use crate::expr::{Expr, Param};
use crate::ty::Ty;

/// A member of a class or given declaration.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ClassMember {
    Fn {
        sig: FnSig,
        default: Option<Idx<Expr>>,
        span: Span,
    },
    Law {
        name: Symbol,
        body: Idx<Expr>,
        span: Span,
    },
}

/// A function signature (name, params, optional return type).
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FnSig {
    pub name: Symbol,
    pub params: Vec<Param>,
    pub ty: Option<Idx<Ty>>,
    pub span: Span,
}

/// An item in an export list.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ExportItem {
    pub name: Symbol,
    pub alias: Option<Symbol>,
    pub span: Span,
}
