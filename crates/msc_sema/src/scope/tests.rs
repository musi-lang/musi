//! Tests for the scope tree.

use msc_shared::Interner;

use crate::def::DefId;
use crate::scope::ScopeTree;

#[test]
fn test_scope_define_and_lookup() {
    let mut interner = Interner::new();
    let mut scopes = ScopeTree::new();

    let root = scopes.push_root();
    let sym = interner.intern("x");
    let def = DefId(0);

    let prev = scopes.define(root, sym, def);
    assert!(prev.is_none());
    assert_eq!(scopes.lookup(root, sym), Some(def));
}

#[test]
fn test_scope_child_inherits_parent() {
    let mut interner = Interner::new();
    let mut scopes = ScopeTree::new();

    let root = scopes.push_root();
    let sym = interner.intern("x");
    let def = DefId(0);
    let _prev = scopes.define(root, sym, def);

    let child = scopes.push_child(root);
    assert_eq!(scopes.lookup(child, sym), Some(def));
}

#[test]
fn test_scope_child_shadows_parent() {
    let mut interner = Interner::new();
    let mut scopes = ScopeTree::new();

    let root = scopes.push_root();
    let sym = interner.intern("x");
    let def1 = DefId(0);
    let def2 = DefId(1);

    let _prev = scopes.define(root, sym, def1);
    let child = scopes.push_child(root);
    let _prev = scopes.define(child, sym, def2);

    assert_eq!(scopes.lookup(child, sym), Some(def2));
    assert_eq!(scopes.lookup(root, sym), Some(def1));
}

#[test]
fn test_scope_lookup_unbound_returns_none() {
    let mut interner = Interner::new();
    let mut scopes = ScopeTree::new();

    let root = scopes.push_root();
    let sym = interner.intern("missing");
    assert_eq!(scopes.lookup(root, sym), None);
}

#[test]
fn test_scope_lookup_local_ignores_parent() {
    let mut interner = Interner::new();
    let mut scopes = ScopeTree::new();

    let root = scopes.push_root();
    let sym = interner.intern("x");
    let def = DefId(0);
    let _prev = scopes.define(root, sym, def);

    let child = scopes.push_child(root);
    assert_eq!(scopes.lookup_local(child, sym), None);
    assert_eq!(scopes.lookup(child, sym), Some(def));
}

#[test]
fn test_scope_define_duplicate_returns_previous() {
    let mut interner = Interner::new();
    let mut scopes = ScopeTree::new();

    let root = scopes.push_root();
    let sym = interner.intern("x");
    let def1 = DefId(0);
    let def2 = DefId(1);

    let prev1 = scopes.define(root, sym, def1);
    assert!(prev1.is_none());

    let prev2 = scopes.define(root, sym, def2);
    assert_eq!(prev2, Some(def1));
}

#[test]
fn test_scope_visible_names() {
    let mut interner = Interner::new();
    let mut scopes = ScopeTree::new();

    let root = scopes.push_root();
    let sym_a = interner.intern("a");
    let sym_b = interner.intern("b");
    let _prev = scopes.define(root, sym_a, DefId(0));

    let child = scopes.push_child(root);
    let _prev = scopes.define(child, sym_b, DefId(1));

    let visible = scopes.visible_names(child);
    assert!(visible.contains(&sym_a));
    assert!(visible.contains(&sym_b));
    assert_eq!(visible.len(), 2);
}
