use music_arena::Arena;
use music_shared::{Interner, Span};

use crate::def::{DefInfo, DefKind, Visibility};
use crate::scope::{ScopeArena, ScopeKind};

fn make_def(defs: &mut Arena<DefInfo>, interner: &mut Interner, name: &str) -> super::DefId {
    let sym = interner.intern(name);
    defs.alloc(DefInfo {
        name: sym,
        span: Span::DUMMY,
        kind: DefKind::Value,
        vis: Visibility::Private,
        scope: 0,
    })
}

#[test]
fn bind_and_lookup() {
    let mut interner = Interner::new();
    let mut defs = Arena::new();
    let def_id = make_def(&mut defs, &mut interner, "x");
    let sym = interner.intern("x");

    let mut arena = ScopeArena::new();
    let scope = arena.push(ScopeKind::Module, None);
    let _prev = arena.get_mut(scope).bind(sym, def_id);

    assert_eq!(arena.get(scope).lookup(sym), Some(def_id));
}

#[test]
fn lookup_missing_returns_none() {
    let mut interner = Interner::new();
    let missing = interner.intern("missing");

    let mut arena = ScopeArena::new();
    let scope = arena.push(ScopeKind::Module, None);

    assert_eq!(arena.get(scope).lookup(missing), None);
}

#[test]
fn resolve_walks_parent_chain() {
    let mut interner = Interner::new();
    let mut defs = Arena::new();
    let def_x = make_def(&mut defs, &mut interner, "x");
    let sym_x = interner.intern("x");

    let mut arena = ScopeArena::new();
    let parent = arena.push(ScopeKind::Module, None);
    let _prev = arena.get_mut(parent).bind(sym_x, def_x);

    let child = arena.push(ScopeKind::Block, Some(parent));

    // Child can see parent's binding
    assert_eq!(arena.resolve(child, sym_x), Some(def_x));
}

#[test]
fn shadowing_prefers_child() {
    let mut interner = Interner::new();
    let mut defs = Arena::new();
    let def_parent = make_def(&mut defs, &mut interner, "x");
    let def_child = make_def(&mut defs, &mut interner, "x");
    let sym_x = interner.intern("x");

    let mut arena = ScopeArena::new();
    let parent = arena.push(ScopeKind::Module, None);
    let _prev = arena.get_mut(parent).bind(sym_x, def_parent);

    let child = arena.push(ScopeKind::Block, Some(parent));
    let _prev = arena.get_mut(child).bind(sym_x, def_child);

    assert_eq!(arena.resolve(child, sym_x), Some(def_child));
    // Parent still sees its own
    assert_eq!(arena.resolve(parent, sym_x), Some(def_parent));
}

#[test]
fn resolve_returns_none_when_not_found() {
    let mut interner = Interner::new();
    let missing = interner.intern("ghost");

    let mut arena = ScopeArena::new();
    let parent = arena.push(ScopeKind::Module, None);
    let child = arena.push(ScopeKind::Lambda, Some(parent));

    assert_eq!(arena.resolve(child, missing), None);
}

#[test]
fn scope_kind_preserved() {
    let mut arena = ScopeArena::new();
    let id = arena.push(ScopeKind::Comprehension, None);
    assert_eq!(arena.get(id).kind, ScopeKind::Comprehension);
}

#[test]
fn default_creates_empty_arena() {
    let arena = ScopeArena::default();
    // No scopes yet; cannot call get without pushing first
    let _ = arena; // just verify it compiles and does not panic
}
