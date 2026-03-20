//! Declaration-level nodes: class members, function signatures, exports, foreign.

use msc_shared::{Span, Symbol};

use crate::ExprIdx;
use crate::attr::Attr;
use crate::expr::Param;
use crate::ty_param::{Constraint, TyParam};

/// A member of a class or given declaration.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ClassMember {
    Fn {
        sig: FnSig,
        default: Option<ExprIdx>,
        span: Span,
    },
    Law {
        name: Symbol,
        params: Vec<Param>,
        body: ExprIdx,
        span: Span,
    },
}

/// A function signature (name, params, optional return type).
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FnSig {
    pub name: Symbol,
    pub params: Vec<Param>,
    pub ret: Option<ExprIdx>,
    pub span: Span,
}

/// An operation in an effect definition.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct EffectOp {
    pub fatal: bool,
    pub name: Symbol,
    pub params: Vec<Param>,
    pub ret: Option<ExprIdx>,
    pub span: Span,
}

/// A declaration inside a `foreign "C" (...)` block.
#[derive(Debug, Clone, PartialEq)]
pub enum ForeignDecl {
    /// A foreign function binding: `let name ['T] [as "ext_name"] [where ...] : ty`.
    Fn {
        attrs: Vec<Attr>,
        name: Symbol,
        params: Vec<TyParam>,
        constraints: Vec<Constraint>,
        ext_name: Option<Symbol>,
        ty: ExprIdx,
        span: Span,
    },
    /// An opaque type declaration: `let NAME;` (no type annotation).
    OpaqueType { name: Symbol, span: Span },
}
