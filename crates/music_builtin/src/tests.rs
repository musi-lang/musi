use std::collections::BTreeSet;

use super::{
    JitLowering, all_builtin_intrinsics, all_builtin_types, all_foundation_modules,
    all_std_package_files, builtin_intrinsic_by_symbol,
};

#[test]
fn test_builtin_types_have_unique_names() {
    let mut names = BTreeSet::new();
    for def in all_builtin_types() {
        assert!(
            names.insert(def.name),
            "duplicate builtin type {}",
            def.name
        );
    }
}

#[test]
fn test_builtin_intrinsics_have_unique_symbols() {
    let mut symbols = BTreeSet::new();
    for def in all_builtin_intrinsics() {
        assert!(
            symbols.insert(def.symbol),
            "duplicate builtin intrinsic symbol {}",
            def.symbol
        );
    }
}

#[test]
fn test_builtin_intrinsics_have_jit_contracts() {
    for def in all_builtin_intrinsics() {
        match def.jit {
            JitLowering::CraneliftOpcode(opcode) => assert!(!opcode.is_empty()),
            JitLowering::CraneliftTrap(reason)
            | JitLowering::RuntimeCall(reason)
            | JitLowering::UnsupportedForJit(reason) => assert!(!reason.is_empty()),
            JitLowering::VmOnly => {}
        }
    }
}

#[test]
fn test_pointer_specialized_symbols_map_to_base_intrinsic() {
    assert_eq!(
        builtin_intrinsic_by_symbol("ffi.ptr.read.i64").map(|def| def.symbol),
        Some("ffi.ptr.read")
    );
    assert_eq!(
        builtin_intrinsic_by_symbol("ffi.ptr.write.f64").map(|def| def.symbol),
        Some("ffi.ptr.write")
    );
}

#[test]
fn test_foundation_module_specs_are_unique() {
    let mut specs = BTreeSet::new();
    for def in all_foundation_modules() {
        assert!(
            specs.insert(def.spec),
            "duplicate foundation spec {}",
            def.spec
        );
    }
}

#[test]
fn test_std_package_file_paths_are_unique() {
    let mut paths = BTreeSet::new();
    for def in all_std_package_files() {
        assert!(paths.insert(def.path), "duplicate std file {}", def.path);
    }
}
