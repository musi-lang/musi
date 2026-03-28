use core::error;
use core::fmt;

use music_shared::diag::{Diag, DiagCode};
use music_shared::{Interner, SourceId};
use music_shared::{Span, Symbol};
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
    #[error("expected type '{expected}', found '{found}'")]
    TypeMismatch {
        expected: SemaTypeId,
        found: SemaTypeId,
    },

    #[error("cannot unify type '{left}' with '{right}'")]
    CannotUnify { left: SemaTypeId, right: SemaTypeId },

    #[error("undefined field '{field}'")]
    UndefinedField { field: Symbol },

    #[error("missing type annotation")]
    MissingAnnotation,

    #[error("non-exhaustive match")]
    NonExhaustiveMatch,

    #[error("unhandled effect '{effect}'")]
    PurityViolation { effect: Symbol },

    #[error("missing handler for effect operation '{op}'")]
    MissingHandler { op: Symbol },

    #[error("missing return handler")]
    MissingReturnHandler,

    #[error("duplicate handler for '{op}'")]
    DuplicateHandler { op: Symbol },

    #[error("unknown handler operation '{op}'")]
    UnknownHandlerOp { op: Symbol },

    #[error("expected {expected} argument(s), found {found}")]
    ArityMismatch { expected: usize, found: usize },

    #[error("no instance found for class '{class}'")]
    NoInstance { class: Symbol },

    #[error("mutability violation")]
    MutabilityViolation,

    #[error("invalid assignment target")]
    InvalidAssignTarget,

    #[error("expression is not callable")]
    NotCallable,

    #[error("expression is not indexable")]
    NotIndexable,

    #[error("constraint '{constraint}' not satisfied")]
    ConstraintNotSatisfied { constraint: Symbol },

    #[error("infinite type")]
    InfiniteType,

    #[error("type variable '{var}' occurs in its own binding")]
    OccursCheck { var: TyVarId },

    #[error("'export' is only allowed at top level")]
    ExportNotTopLevel,

    #[error("'opaque' requires 'export'")]
    OpaqueWithoutExport,

    #[error("'foreign' declaration is only allowed at top level")]
    ForeignNotTopLevel,

    #[error("splice outside 'quote'")]
    SpliceOutsideQuote,

    #[error("unreachable pattern after wildcard")]
    UnreachablePattern,

    #[error("unreachable code after diverging expression")]
    UnreachableCode,

    #[error("unused binding '{name}'")]
    UnusedBinding { name: Symbol },

    #[error("unused parameter '{name}'")]
    UnusedParameter { name: Symbol },

    #[error("'or'-pattern alternatives bind different names")]
    OrPatternMismatch,

    #[error("duplicate instance of '{class}' for '{ty}'")]
    DuplicateInstance { class: Symbol, ty: Symbol },

    #[error("resume in handler for operation '{op}' which returns 'Empty'")]
    ResumeOnNever { op: Symbol },

    #[error("type '{type_name}' is not FFI-compatible")]
    IncompatibleFfiType { type_name: String },
}

impl SemaError {
    /// The source span where this error occurred.
    #[must_use]
    pub const fn span(&self) -> Span {
        self.span
    }

    #[must_use]
    pub fn diagnostic(&self, interner: &Interner, source_id: SourceId) -> Diag {
        let (code, message, hint): (DiagCode, String, Option<String>) = match &self.kind {
            SemaErrorKind::TypeMismatch { expected, found } => (
                DiagCode::new(4001),
                format!("type mismatch; expected {expected}, found {found}"),
                None,
            ),
            SemaErrorKind::CannotUnify { left, right } => (
                DiagCode::new(4002),
                format!("cannot unify {left} with {right}"),
                None,
            ),
            SemaErrorKind::UndefinedField { field } => (
                DiagCode::new(4003),
                format!("undefined field '{}'", interner.resolve(*field)),
                None,
            ),
            SemaErrorKind::MissingAnnotation => (
                DiagCode::new(4004),
                String::from("missing type annotation"),
                None,
            ),
            SemaErrorKind::NonExhaustiveMatch => (
                DiagCode::new(4005),
                String::from("non-exhaustive match"),
                None,
            ),
            SemaErrorKind::PurityViolation { effect } => (
                DiagCode::new(4006),
                format!("unhandled effect '{}'", interner.resolve(*effect)),
                None,
            ),
            SemaErrorKind::MissingHandler { op } => (
                DiagCode::new(4007),
                format!(
                    "missing handler for effect operation '{}'",
                    interner.resolve(*op)
                ),
                None,
            ),
            SemaErrorKind::MissingReturnHandler => (
                DiagCode::new(4029),
                String::from("missing return handler clause"),
                None,
            ),
            SemaErrorKind::DuplicateHandler { op } => (
                DiagCode::new(4030),
                format!("duplicate handler clause for '{}'", interner.resolve(*op)),
                None,
            ),
            SemaErrorKind::UnknownHandlerOp { op } => (
                DiagCode::new(4031),
                format!(
                    "'{}' is not operation of handled effect",
                    interner.resolve(*op)
                ),
                None,
            ),
            SemaErrorKind::ArityMismatch { expected, found } => (
                DiagCode::new(4008),
                format!("expected {expected} argument(s), found {found}"),
                None,
            ),
            SemaErrorKind::NoInstance { class } => (
                DiagCode::new(4009),
                format!("no instance found for class '{}'", interner.resolve(*class)),
                None,
            ),
            SemaErrorKind::MutabilityViolation => (
                DiagCode::new(4010),
                String::from("cannot mutate immutable binding"),
                None,
            ),
            SemaErrorKind::InvalidAssignTarget => (
                DiagCode::new(4011),
                String::from("invalid assignment target"),
                None,
            ),
            SemaErrorKind::NotCallable => (
                DiagCode::new(4012),
                String::from("expression is not callable"),
                None,
            ),
            SemaErrorKind::NotIndexable => (
                DiagCode::new(4013),
                String::from("expression is not indexable"),
                None,
            ),
            SemaErrorKind::ConstraintNotSatisfied { constraint } => (
                DiagCode::new(4014),
                format!(
                    "constraint '{}' is not satisfied",
                    interner.resolve(*constraint)
                ),
                None,
            ),
            SemaErrorKind::InfiniteType => {
                (DiagCode::new(4015), String::from("infinite type"), None)
            }
            SemaErrorKind::OccursCheck { var } => (
                DiagCode::new(4016),
                format!("type variable {var} occurs in its own binding"),
                None,
            ),
            SemaErrorKind::ExportNotTopLevel => (
                DiagCode::new(4017),
                String::from("export is only allowed at top level"),
                None,
            ),
            SemaErrorKind::OpaqueWithoutExport => (
                DiagCode::new(4018),
                String::from("opaque binding must also be exported"),
                None,
            ),
            SemaErrorKind::ForeignNotTopLevel => (
                DiagCode::new(4019),
                String::from("foreign declaration is only allowed at top level"),
                None,
            ),
            SemaErrorKind::SpliceOutsideQuote => (
                DiagCode::new(4020),
                String::from("splice is only allowed inside quote"),
                None,
            ),
            SemaErrorKind::UnreachablePattern => (
                DiagCode::new(4021),
                String::from("pattern is unreachable after wildcard"),
                None,
            ),
            SemaErrorKind::UnreachableCode => (
                DiagCode::new(4022),
                String::from("code is unreachable after diverging expression"),
                None,
            ),
            SemaErrorKind::UnusedBinding { name } => (
                DiagCode::new(4023),
                format!("unused binding '{}'", interner.resolve(*name)),
                None,
            ),
            SemaErrorKind::UnusedParameter { name } => (
                DiagCode::new(4024),
                format!("unused parameter '{}'", interner.resolve(*name)),
                None,
            ),
            SemaErrorKind::OrPatternMismatch => (
                DiagCode::new(4025),
                String::from("or-pattern alternatives bind different names"),
                None,
            ),
            SemaErrorKind::DuplicateInstance { class, ty } => (
                DiagCode::new(4026),
                format!(
                    "duplicate instance of '{}' for '{}'",
                    interner.resolve(*class),
                    interner.resolve(*ty)
                ),
                None,
            ),
            SemaErrorKind::ResumeOnNever { op } => (
                DiagCode::new(4027),
                format!(
                    "cannot resume operation '{}' because it returns Empty",
                    interner.resolve(*op)
                ),
                None,
            ),
            SemaErrorKind::IncompatibleFfiType { type_name } => (
                DiagCode::new(4028),
                format!("type '{type_name}' is not FFI-compatible"),
                None,
            ),
        };

        let mut diag = Diag::error(message)
            .with_code(code)
            .with_label(self.span, source_id, "");
        if let Some(hint) = hint {
            diag = diag.with_hint(hint);
        }
        if let Some(ctx) = self.context {
            diag = diag.with_note(format!("in {ctx}"));
        }
        diag
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
