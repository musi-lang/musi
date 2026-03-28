use music_builtins::types::BuiltinType;
use music_shared::{Interner, Span};

use crate::def::{DefInfo, DefKind, Visibility};

#[test]
fn def_info_construction() {
    let mut interner = Interner::new();
    let name = interner.intern("x");
    let def = DefInfo {
        name,
        span: Span::new(0, 1),
        kind: DefKind::Value,
        vis: Visibility::Private,
        scope: 0,
        module_name: None,
    };
    assert_eq!(def.name, name);
    assert_eq!(def.span, Span::new(0, 1));
    assert_eq!(def.kind, DefKind::Value);
    assert_eq!(def.vis, Visibility::Private);
}

#[test]
fn def_kind_builtin_carries_type() {
    let kind = DefKind::Builtin(BuiltinType::Int);
    assert!(matches!(kind, DefKind::Builtin(BuiltinType::Int)));
}

#[test]
fn def_kind_variants_distinct() {
    assert_ne!(DefKind::Value, DefKind::Function);
    assert_ne!(DefKind::Type, DefKind::TypeClass);
    assert_ne!(DefKind::Effect, DefKind::TypeParam);
    assert_ne!(DefKind::Variant, DefKind::Field);
    assert_ne!(DefKind::Method, DefKind::Law);
    assert_ne!(DefKind::Import, DefKind::Value);
}

#[test]
fn visibility_default_is_private() {
    assert_eq!(Visibility::default(), Visibility::Private);
}

#[test]
fn visibility_variants_distinct() {
    assert_ne!(Visibility::Private, Visibility::Exported);
    assert_ne!(Visibility::Exported, Visibility::Opaque);
    assert_ne!(Visibility::Private, Visibility::Opaque);
}
