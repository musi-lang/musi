use crate::{Interner, Span};

use super::*;

#[test]
fn new_sets_name_and_span() {
    let mut interner = Interner::new();
    let sym = interner.intern("x");
    let span = Span::new(5, 6);
    let ident = Ident::new(sym, span);
    assert_eq!(ident.name, sym);
    assert_eq!(ident.span, span);
}

#[test]
fn dummy_uses_dummy_span() {
    let mut interner = Interner::new();
    let sym = interner.intern("generated");
    let ident = Ident::dummy(sym);
    assert_eq!(ident.name, sym);
    assert_eq!(ident.span, Span::DUMMY);
}

#[test]
fn equality_compares_both_fields() {
    let mut interner = Interner::new();
    let a = interner.intern("x");
    let b = interner.intern("y");

    let id1 = Ident::new(a, Span::new(0, 1));
    let id2 = Ident::new(a, Span::new(0, 1));
    let id3 = Ident::new(b, Span::new(0, 1));
    let id4 = Ident::new(a, Span::new(5, 6));

    assert_eq!(id1, id2);
    assert_ne!(id1, id3);
    assert_ne!(id1, id4);
}
