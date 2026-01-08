use musi_core::{MusiError, Span, Name};

use crate::TyKind;

#[must_use]
pub fn type_mismatch(expected: &TyKind, found: &TyKind, span: Span) -> MusiError {
    MusiError::new(
        format!("expected type `{expected:?}`, found type `{found:?}`"),
        span,
    )
}

#[must_use]
pub fn unknown_field(field: Name, record_ty: &TyKind, span: Span) -> MusiError {
    MusiError::new(
        format!("unknown field `{:?}` on type `{record_ty:?}`", field.id),
        span,
    )
}

#[must_use]
pub fn arity_mismatch(expected: usize, found: usize, span: Span) -> MusiError {
    MusiError::new(
        format!("expected {expected} argument(s), found {found}"),
        span,
    )
}

#[must_use]
pub fn break_outside_loop(span: Span) -> MusiError {
    MusiError::new("'break' outside of loop", span)
}

#[must_use]
pub fn cycle_outside_loop(span: Span) -> MusiError {
    MusiError::new("'cycle' outside of loop", span)
}

#[must_use]
pub fn return_outside_fn(span: Span) -> MusiError {
    MusiError::new("'return' outside of function", span)
}

#[must_use]
pub fn duplicate_field(field: Name, span: Span) -> MusiError {
    MusiError::new(format!("duplicate field `{:?}`", field.id), span)
}

#[must_use]
pub fn undefined_variable(sym: Name, span: Span) -> MusiError {
    MusiError::new(format!("undefined variable `{:?}`", sym.id), span)
}

#[must_use]
pub fn pattern_arity_mismatch(expected: usize, found: usize, span: Span) -> MusiError {
    MusiError::new(
        format!("pattern expected {expected} element(s), found {found}"),
        span,
    )
}

#[must_use]
pub fn field_on_non_record(span: Span) -> MusiError {
    MusiError::new("field access on non-record type", span)
}

#[must_use]
pub fn empty_match(span: Span) -> MusiError {
    MusiError::new("'match' expression has no cases", span)
}
