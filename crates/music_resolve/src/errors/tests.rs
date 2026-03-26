use music_shared::{Span, Symbol};

use crate::errors::{ResolveError, ResolveErrorKind};

fn dummy_symbol() -> Symbol {
    // Symbol(0) - the raw constructor is not public, but from_raw on Idx
    // is. Symbol only exposes raw(). We need an interner to create real ones.
    // For Display tests we only care that the format includes the symbol repr.
    use music_shared::Interner;
    let mut interner = Interner::new();
    interner.intern("foo")
}

#[test]
fn display_undefined_name() {
    let sym = dummy_symbol();
    let err = ResolveError {
        kind: ResolveErrorKind::UndefinedName(sym),
        span: Span::new(0, 3),
    };
    let msg = err.to_string();
    assert!(msg.contains("undefined name"), "got: {msg}");
}

#[test]
fn display_undefined_type() {
    let sym = dummy_symbol();
    let err = ResolveError {
        kind: ResolveErrorKind::UndefinedType(sym),
        span: Span::new(10, 15),
    };
    let msg = err.to_string();
    assert!(msg.contains("undefined type"), "got: {msg}");
}

#[test]
fn display_undefined_variant() {
    let sym = dummy_symbol();
    let err = ResolveError {
        kind: ResolveErrorKind::UndefinedVariant(sym),
        span: Span::new(0, 4),
    };
    let msg = err.to_string();
    assert!(msg.contains("undefined variant"), "got: {msg}");
}

#[test]
fn display_duplicate_definition() {
    let sym = dummy_symbol();
    let err = ResolveError {
        kind: ResolveErrorKind::DuplicateDefinition {
            name: sym,
            original: Span::new(0, 3),
        },
        span: Span::new(10, 13),
    };
    let msg = err.to_string();
    assert!(msg.contains("duplicate definition"), "got: {msg}");
    assert!(msg.contains("originally defined at"), "got: {msg}");
}

#[test]
fn display_not_yet_defined() {
    let sym = dummy_symbol();
    let err = ResolveError {
        kind: ResolveErrorKind::NotYetDefined(sym),
        span: Span::new(5, 8),
    };
    let msg = err.to_string();
    assert!(msg.contains("before its definition"), "got: {msg}");
}

#[test]
fn span_accessor() {
    let sym = dummy_symbol();
    let err = ResolveError {
        kind: ResolveErrorKind::UndefinedName(sym),
        span: Span::new(42, 50),
    };
    assert_eq!(err.span(), Span::new(42, 50));
}
