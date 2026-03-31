use music_basic::Span;

use crate::{Ident, Interner};

#[test]
fn test_new_stores_name_and_span() {
    let mut interner = Interner::new();
    let symbol = interner.intern("alpha");
    let ident = Ident::new(symbol, Span::new(2, 7));

    assert_eq!(ident.name, symbol);
    assert_eq!(ident.span, Span::new(2, 7));
}

#[test]
fn test_dummy_uses_dummy_span() {
    let mut interner = Interner::new();
    let symbol = interner.intern("generated");
    let ident = Ident::dummy(symbol);

    assert_eq!(ident.name, symbol);
    assert_eq!(ident.span, Span::DUMMY);
}
