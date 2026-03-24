use core::error;
use core::fmt;

use music_found::{Span, Symbol};
use thiserror::Error;

use crate::types::{SemaTypeId, TyVarId};

/// A type-checking error with source location and optional context.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SemaError {
    pub kind: SemaErrorKind,
    pub span: Span,
    pub context: Option<&'static str>,
}

/// Classification of type-checking errors.
#[derive(Debug, Clone, PartialEq, Eq, Error)]
pub enum SemaErrorKind {
    #[error("type mismatch: expected {expected}, found {found}")]
    TypeMismatch {
        expected: SemaTypeId,
        found: SemaTypeId,
    },

    #[error("cannot unify {left} with {right}")]
    CannotUnify { left: SemaTypeId, right: SemaTypeId },

    #[error("undefined field {field}")]
    UndefinedField { field: Symbol },

    #[error("missing type annotation")]
    MissingAnnotation,

    #[error("non-exhaustive match")]
    NonExhaustiveMatch,

    #[error("purity violation: unhandled effect {effect}")]
    PurityViolation { effect: Symbol },

    #[error("missing handler for effect operation {op}")]
    MissingHandler { op: Symbol },

    #[error("arity mismatch: expected {expected} arguments, found {found}")]
    ArityMismatch { expected: usize, found: usize },

    #[error("no instance found for class {class}")]
    NoInstance { class: Symbol },

    #[error("mutability violation")]
    MutabilityViolation,

    #[error("invalid assignment target")]
    InvalidAssignTarget,

    #[error("expression is not callable")]
    NotCallable,

    #[error("expression is not indexable")]
    NotIndexable,

    #[error("constraint {constraint} not satisfied")]
    ConstraintNotSatisfied { constraint: Symbol },

    #[error("infinite type")]
    InfiniteType,

    #[error("occurs check: variable {var} occurs in its own binding")]
    OccursCheck { var: TyVarId },
}

impl SemaError {
    /// The source span where this error occurred.
    #[must_use]
    pub const fn span(&self) -> Span {
        self.span
    }
}

impl fmt::Display for SemaError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let Some(ctx) = self.context {
            write!(f, "{} (in {})", self.kind, ctx)
        } else {
            write!(f, "{}", self.kind)
        }
    }
}

impl error::Error for SemaError {
    fn source(&self) -> Option<&(dyn error::Error + 'static)> {
        Some(&self.kind)
    }
}

#[cfg(test)]
#[allow(clippy::unwrap_used, clippy::panic)]
mod tests;
