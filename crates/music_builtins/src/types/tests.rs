use std::collections::HashSet;

use super::*;

#[test]
fn all_has_expected_entries() {
    assert_eq!(BuiltinType::ALL.len(), 23);
}

#[test]
fn name_returns_correct_string_for_intrinsics() {
    assert_eq!(BuiltinType::Type.name(), "Type");
    assert_eq!(BuiltinType::Any.name(), "Any");
    assert_eq!(BuiltinType::Unknown.name(), "Unknown");
    assert_eq!(BuiltinType::Never.name(), "Never");
}

#[test]
fn name_returns_correct_string_for_primitives() {
    assert_eq!(BuiltinType::Unit.name(), "Unit");
    assert_eq!(BuiltinType::Bool.name(), "Bool");
    assert_eq!(BuiltinType::Int.name(), "Int");
    assert_eq!(BuiltinType::Nat.name(), "Nat");
    assert_eq!(BuiltinType::Float.name(), "Float");
    assert_eq!(BuiltinType::Rune.name(), "Rune");
}

#[test]
fn name_returns_correct_string_for_collections() {
    assert_eq!(BuiltinType::String.name(), "String");
    assert_eq!(BuiltinType::Array.name(), "Array");
    assert_eq!(BuiltinType::List.name(), "List");
}

#[test]
fn name_returns_correct_string_for_sized_integers() {
    assert_eq!(BuiltinType::Int8.name(), "Int8");
    assert_eq!(BuiltinType::Int16.name(), "Int16");
    assert_eq!(BuiltinType::Int32.name(), "Int32");
    assert_eq!(BuiltinType::Int64.name(), "Int64");
}

#[test]
fn name_returns_correct_string_for_sized_naturals() {
    assert_eq!(BuiltinType::Nat8.name(), "Nat8");
    assert_eq!(BuiltinType::Nat16.name(), "Nat16");
    assert_eq!(BuiltinType::Nat32.name(), "Nat32");
    assert_eq!(BuiltinType::Nat64.name(), "Nat64");
}

#[test]
fn name_returns_correct_string_for_sized_floats() {
    assert_eq!(BuiltinType::Float32.name(), "Float32");
    assert_eq!(BuiltinType::Float64.name(), "Float64");
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

#[test]
fn type_ids_are_unique() {
    let mut seen = HashSet::new();
    for ty in BuiltinType::ALL {
        assert!(seen.insert(ty.type_id()), "duplicate type_id for: {ty}");
    }
}

#[test]
fn intrinsic_type_ids_in_high_range() {
    assert_eq!(BuiltinType::Type.type_id(), 0xFFF0);
    assert_eq!(BuiltinType::Any.type_id(), 0xFFF1);
    assert_eq!(BuiltinType::Unknown.type_id(), 0xFFF2);
    assert_eq!(BuiltinType::Never.type_id(), 0xFFF3);
    assert_eq!(BuiltinType::Unit.type_id(), 0xFFF4);
    assert_eq!(BuiltinType::Bool.type_id(), 0xFFF5);
    assert_eq!(BuiltinType::Int.type_id(), 0xFFF6);
    assert_eq!(BuiltinType::Float.type_id(), 0xFFF7);
    assert_eq!(BuiltinType::String.type_id(), 0xFFF8);
}

#[test]
fn nan_box_tag_smi_for_integers() {
    assert_eq!(BuiltinType::Int.nan_box_tag(), Some(0b001));
    assert_eq!(BuiltinType::Nat.nan_box_tag(), Some(0b001));
    assert_eq!(BuiltinType::Int8.nan_box_tag(), Some(0b001));
    assert_eq!(BuiltinType::Int64.nan_box_tag(), Some(0b001));
    assert_eq!(BuiltinType::Nat8.nan_box_tag(), Some(0b001));
    assert_eq!(BuiltinType::Nat64.nan_box_tag(), Some(0b001));
}

#[test]
fn nan_box_tag_bool_unit_rune() {
    assert_eq!(BuiltinType::Bool.nan_box_tag(), Some(0b010));
    assert_eq!(BuiltinType::Unit.nan_box_tag(), Some(0b011));
    assert_eq!(BuiltinType::Rune.nan_box_tag(), Some(0b101));
}

#[test]
fn nan_box_tag_none_for_floats() {
    assert_eq!(BuiltinType::Float.nan_box_tag(), None);
    assert_eq!(BuiltinType::Float32.nan_box_tag(), None);
    assert_eq!(BuiltinType::Float64.nan_box_tag(), None);
}

#[test]
fn nan_box_tag_ptr_for_heap_types() {
    assert_eq!(BuiltinType::String.nan_box_tag(), Some(0b000));
    assert_eq!(BuiltinType::Array.nan_box_tag(), Some(0b000));
    assert_eq!(BuiltinType::List.nan_box_tag(), Some(0b000));
    assert_eq!(BuiltinType::Type.nan_box_tag(), Some(0b000));
}
