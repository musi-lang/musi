use std::collections::HashSet;

use super::*;

#[test]
fn all_has_ten_entries() {
    assert_eq!(BuiltinType::ALL.len(), 10);
}

#[test]
fn name_returns_correct_string() {
    assert_eq!(BuiltinType::Type.name(), "Type");
    assert_eq!(BuiltinType::Any.name(), "Any");
    assert_eq!(BuiltinType::Unknown.name(), "Unknown");
    assert_eq!(BuiltinType::Never.name(), "Never");
    assert_eq!(BuiltinType::Unit.name(), "Unit");
    assert_eq!(BuiltinType::Bool.name(), "Bool");
    assert_eq!(BuiltinType::Int.name(), "Int");
    assert_eq!(BuiltinType::Float.name(), "Float");
    assert_eq!(BuiltinType::String.name(), "String");
    assert_eq!(BuiltinType::Rune.name(), "Rune");
}

#[test]
fn display_matches_name() {
    for ty in BuiltinType::ALL {
        assert_eq!(ty.to_string(), ty.name());
    }
}

#[test]
fn all_entries_are_unique() {
    let mut seen = HashSet::new();
    for ty in BuiltinType::ALL {
        assert!(seen.insert(ty), "duplicate entry: {ty}");
    }
}
