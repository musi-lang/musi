//! Tests for the definition table.

use music_shared::{Interner, Span};

use crate::def::{DefId, DefKind, DefTable};

#[test]
fn test_def_table_alloc_returns_sequential_ids() {
    let mut interner = Interner::new();
    let mut defs = DefTable::new();

    let sym_a = interner.intern("a");
    let sym_b = interner.intern("b");

    let id_a = defs.alloc(sym_a, DefKind::Let, Span::new(0, 1));
    let id_b = defs.alloc(sym_b, DefKind::Fn, Span::new(4, 1));

    assert_eq!(id_a, DefId(0));
    assert_eq!(id_b, DefId(1));
    assert_eq!(defs.len(), 2);
}

#[test]
fn test_def_table_get_returns_correct_info() {
    let mut interner = Interner::new();
    let mut defs = DefTable::new();

    let sym = interner.intern("foo");
    let span = Span::new(10, 3);
    let id = defs.alloc(sym, DefKind::Fn, span);

    let info = defs.get(id);
    assert_eq!(info.name, sym);
    assert_eq!(info.kind, DefKind::Fn);
    assert_eq!(info.span, span);
    assert_eq!(info.use_count, 0);
    assert!(info.ty_info.ty.is_none());
    assert!(info.parent.is_none());
}

#[test]
fn test_def_table_get_mut_allows_modification() {
    let mut interner = Interner::new();
    let mut defs = DefTable::new();

    let sym = interner.intern("x");
    let id = defs.alloc(sym, DefKind::Let, Span::new(0, 1));

    defs.get_mut(id).use_count = 5;
    assert_eq!(defs.get(id).use_count, 5);
}

#[test]
fn test_def_table_empty_by_default() {
    let defs = DefTable::new();
    assert!(defs.is_empty());
    assert_eq!(defs.len(), 0);
}
