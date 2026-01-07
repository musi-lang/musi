use musi_ast::{ExprKind, PatKind};
use musi_core::{Interner, MusiError, Span, Symbol};

#[must_use]
pub fn undefined_identifier(name: Symbol, interner: &Interner) -> MusiError {
    let resolved = interner.resolve(name.id);
    MusiError::new(format!("undefined identifier `{resolved}`"), name.span)
}

#[must_use]
pub fn unsupported_expr(kind: &ExprKind, span: Span) -> MusiError {
    MusiError::new(format!("unsupported expression: {kind:?}"), span)
}

#[must_use]
pub fn unsupported_pattern(kind: &PatKind, span: Span) -> MusiError {
    MusiError::new(format!("unsupported pattern: {kind:?}"), span)
}
