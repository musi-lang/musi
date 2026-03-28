use std::path::PathBuf;

use super::resolve_module;

#[test]
fn resolve_known_intrinsic_module() {
    let resolved = resolve_module("musi:test").expect("musi:test path");
    assert!(resolved.ends_with(PathBuf::from("modules/test.ms")));
}

#[test]
fn reject_unknown_intrinsic_module() {
    assert!(resolve_module("musi:missing").is_none());
    assert!(resolve_module("musi:").is_none());
    assert!(resolve_module("@std/testing").is_none());
}
