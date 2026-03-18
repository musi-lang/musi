//! Structured semantic analysis errors.

use msc_shared::{IntoDiagnostic, Severity};

#[derive(Debug, thiserror::Error)]
pub enum SemaError {
    #[error("undefined name `{name}`")]
    UndefinedName { name: Box<str> },
    #[error("duplicate definition of `{name}`")]
    DuplicateDefinition { name: Box<str> },
    #[error("expected `{expected}` but found `{found}`")]
    TypeMismatch { expected: Box<str>, found: Box<str> },
    #[error("infinite type (occurs check failed)")]
    InfiniteType,
    #[error("expected {expected} argument(s) but found {found}")]
    ArityMismatch { expected: usize, found: usize },
    #[error("cannot call value of type `{ty}`")]
    NotCallable { ty: Box<str> },
    #[error("no field `{field}` on type `{ty}`")]
    NoSuchField { field: Box<str>, ty: Box<str> },
    #[error("no instance of `{class}` for type `{ty}`")]
    NoInstance { class: Box<str>, ty: Box<str> },
    #[error("effectful call in pure function")]
    EffectInPureContext,
    #[error("undeclared effect `{effect}`")]
    UndeclaredEffect { effect: Box<str> },
    #[error("unused binding `{name}`")]
    UnusedBinding { name: Box<str> },
    #[error("unused parameter `{name}`")]
    UnusedParameter { name: Box<str> },
    #[error("unused type `{name}`")]
    UnusedType { name: Box<str> },
    #[error("unused class `{name}`")]
    UnusedClass { name: Box<str> },
    #[error("unused effect `{name}`")]
    UnusedEffect { name: Box<str> },
    #[error("unused import `{name}`")]
    UnusedImport { name: Box<str> },
    #[error("non-exhaustive match; missing case `{missing}`")]
    NonExhaustiveMatch { missing: Box<str> },
    #[error("instance of `{class}` is missing method `{method}`")]
    MissingInstanceMethod { class: Box<str>, method: Box<str> },
    #[error("handler for `{effect}` is missing operation `{op}`")]
    MissingHandlerOp { effect: Box<str>, op: Box<str> },
    #[error("unsafe cast from `{from}` to `{to}`; types are incompatible")]
    UnsafeCast { from: Box<str>, to: Box<str> },
    #[error("`{name}` is abstract and cannot be constructed directly")]
    AbstractConstruct { name: Box<str> },
    #[error("`{name}` is deprecated: {message}")]
    Deprecated { name: Box<str>, message: Box<str> },
    #[error("unknown attribute `{name}`")]
    UnknownAttribute { name: Box<str> },
    #[error("cannot index into type `{ty}`")]
    NotIndexable { ty: Box<str> },
    #[error("`resume` used outside of a handler body")]
    ResumeOutsideHandler,
    #[error("no instance of `{delegate}` found to delegate `{class}` via")]
    NoDelegateInstance { class: Box<str>, delegate: Box<str> },
}

impl IntoDiagnostic for SemaError {
    fn severity(&self) -> Severity {
        match self {
            Self::UnusedBinding { .. }
            | Self::UnusedParameter { .. }
            | Self::UnusedType { .. }
            | Self::UnusedClass { .. }
            | Self::UnusedEffect { .. }
            | Self::UnusedImport { .. }
            | Self::NonExhaustiveMatch { .. }
            | Self::MissingHandlerOp { .. }
            | Self::UnsafeCast { .. }
            | Self::Deprecated { .. }
            | Self::UnknownAttribute { .. } => Severity::Warning,
            _ => Severity::Error,
        }
    }
}
