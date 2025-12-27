use crate::symbol::{SymbolKind, SymbolTable};
use crate::ty_repr::TyRepr;
use musi_ast::Ident;
use musi_basic::span::Span;

#[test]
fn scope_push_pop() {
    let mut symbols = SymbolTable::new();
    assert_eq!(symbols.scope_depth(), 1);

    let _ = symbols.push_scope();
    assert_eq!(symbols.scope_depth(), 2);

    symbols.pop_scope();
    assert_eq!(symbols.scope_depth(), 1);
}

#[test]
fn define_lookup() {
    let mut symbols = SymbolTable::new();
    let name: Ident = 0;
    let ty = TyRepr::unit();
    let span = Span::new(0, 0);

    let id = symbols
        .define(name, SymbolKind::Local, ty.clone(), span, false)
        .expect("define failed");
    let lookup = symbols.lookup(name).expect("lookup failed");

    assert_eq!(id, lookup);
    let sym = symbols.get(id).expect("get failed");
    assert_eq!(sym.name, name);
}

#[test]
fn shadowing_same_scope_error() {
    let mut symbols = SymbolTable::new();
    let name: Ident = 0;
    let ty = TyRepr::unit();
    let span = Span::new(0, 0);

    let _ = symbols
        .define(name, SymbolKind::Local, ty.clone(), span, false)
        .unwrap();
    assert!(
        symbols
            .define(name, SymbolKind::Local, ty, span, false)
            .is_err()
    );
}

#[test]
fn shadowing_nested_scope_ok() {
    let mut symbols = SymbolTable::new();
    let name: Ident = 0;
    let ty = TyRepr::unit();
    let span = Span::new(0, 0);

    let id1 = symbols
        .define(name, SymbolKind::Local, ty.clone(), span, false)
        .unwrap();
    let _ = symbols.push_scope();
    let id2 = symbols
        .define(name, SymbolKind::Local, ty.clone(), span, false)
        .unwrap();

    assert_ne!(id1, id2);
    assert_eq!(symbols.lookup(name), Some(id2));

    symbols.pop_scope();
    assert_eq!(symbols.lookup(name), Some(id1));
}

#[test]
fn lookup_in_parent_scope() {
    let mut symbols = SymbolTable::new();
    let name: Ident = 0;
    let ty = TyRepr::unit();
    let span = Span::new(0, 0);

    let id = symbols
        .define(name, SymbolKind::Local, ty, span, false)
        .unwrap();
    let _ = symbols.push_scope();

    assert_eq!(symbols.lookup(name), Some(id));
}
