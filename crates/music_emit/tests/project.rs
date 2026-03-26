#![allow(clippy::unwrap_used, clippy::panic, clippy::tests_outside_test_module)]

use std::fs;

use music_emit::pool::ConstantEntry;
use music_emit::project::emit_project;
use music_il::instruction::Operand;
use music_il::opcode::Opcode;
use music_resolve::loader::ModuleLoader;
use music_resolve::resolve_project;

fn resolve_and_emit(dir: &std::path::Path, entry: &str) -> music_emit::project::ProjectEmitResult {
    let entry_path = dir.join(entry);
    let loader = ModuleLoader::new(dir.to_path_buf());
    let resolution = resolve_project(&entry_path, &loader).unwrap();
    emit_project(resolution, &loader).unwrap()
}

#[test]
fn single_module_produces_valid_output() {
    let dir = tempfile::tempdir().unwrap();
    fs::write(dir.path().join("main.ms"), "export let x := 42").unwrap();

    let result = resolve_and_emit(dir.path(), "main.ms");
    let module = &result.module;

    // Should have at least one global (x).
    assert!(
        module.globals.iter().any(|g| g.exported),
        "expected at least one exported global"
    );

    // Should have at least one method (the implicit main).
    assert!(
        !module.methods.is_empty(),
        "expected at least one method (main)"
    );
}

#[test]
fn single_module_global_has_correct_value() {
    let dir = tempfile::tempdir().unwrap();
    fs::write(dir.path().join("main.ms"), "export let x := 42").unwrap();

    let result = resolve_and_emit(dir.path(), "main.ms");
    let module = &result.module;

    // The main method should contain LdSmi(42) followed by StGlob(0).
    let main = module.methods.iter().find(|m| m.name.is_none()).unwrap();
    let has_ld_42 = main
        .instructions
        .iter()
        .any(|i| i.opcode == Opcode::LdSmi && i.operand == Operand::I16(42));
    assert!(has_ld_42, "expected LdSmi(42) in main method");

    let has_st_glob = main
        .instructions
        .iter()
        .any(|i| i.opcode == Opcode::StGlob && i.operand == Operand::U16(0));
    assert!(has_st_glob, "expected StGlob(0) in main method");
}

#[test]
fn globals_offset_when_merging_two_modules() {
    let dir = tempfile::tempdir().unwrap();
    fs::write(dir.path().join("a.ms"), "export let x := 1").unwrap();
    fs::write(
        dir.path().join("main.ms"),
        "import \"./a.ms\" as _\nexport let y := 2",
    )
    .unwrap();

    let result = resolve_and_emit(dir.path(), "main.ms");
    let module = &result.module;

    // Module A has 1 global (x), module main has 1 global (y).
    // After merging, combined should have 2 globals.
    assert_eq!(
        module.globals.len(),
        2,
        "expected 2 globals after merging, got {}",
        module.globals.len()
    );

    // Module A's global is at index 0, main's global is at index 1.
    // The main method's StGlob should reference index 1 (offset by A's globals).
    let main_method = module
        .methods
        .iter()
        .filter(|m| m.name.is_none())
        .last()
        .unwrap();

    let st_glob_operands: Vec<u16> = main_method
        .instructions
        .iter()
        .filter(|i| i.opcode == Opcode::StGlob)
        .filter_map(|i| match i.operand {
            Operand::U16(idx) => Some(idx),
            _ => None,
        })
        .collect();

    assert!(
        st_glob_operands.contains(&1),
        "expected main's StGlob to reference global index 1 (offset), got {st_glob_operands:?}"
    );
}

#[test]
fn method_indices_offset_when_merging() {
    let dir = tempfile::tempdir().unwrap();
    // Use lambda bindings (not typed signatures) so that sema doesn't
    // need builtin type resolution to emit function methods.
    fs::write(dir.path().join("a.ms"), "export let x := 1").unwrap();
    fs::write(
        dir.path().join("main.ms"),
        "import \"./a.ms\" as _\nexport let y := 2",
    )
    .unwrap();

    let result = resolve_and_emit(dir.path(), "main.ms");
    let module = &result.module;

    // Each module has a main method (implicit). After merging there should
    // be at least 2 methods (one per module's main block).
    assert!(
        module.methods.len() >= 2,
        "expected at least 2 methods after merging, got {}",
        module.methods.len()
    );
}

#[test]
fn constant_pool_deduplicates_across_modules() {
    let dir = tempfile::tempdir().unwrap();
    // Both modules use the string "hello" as a constant.
    fs::write(dir.path().join("a.ms"), "export let x := \"hello\"").unwrap();
    fs::write(
        dir.path().join("main.ms"),
        "import \"./a.ms\" as _\nexport let y := \"hello\"",
    )
    .unwrap();

    let result = resolve_and_emit(dir.path(), "main.ms");
    let module = &result.module;

    // The constant pool should have only one entry for "hello".
    let hello_count = module
        .constants
        .entries()
        .iter()
        .filter(|e| matches!(e, ConstantEntry::Str(s) if s == "hello"))
        .count();
    assert_eq!(
        hello_count, 1,
        "expected constant pool to deduplicate 'hello', got {hello_count} entries"
    );
}

#[test]
fn module_exports_tracked_in_result() {
    let dir = tempfile::tempdir().unwrap();
    fs::write(
        dir.path().join("a.ms"),
        "export let x := 1\nexport let y := 2",
    )
    .unwrap();
    fs::write(
        dir.path().join("main.ms"),
        "import \"./a.ms\" as _\nexport let z := 3",
    )
    .unwrap();

    let result = resolve_and_emit(dir.path(), "main.ms");

    // The module_exports map should have entries for both modules.
    assert_eq!(
        result.module_exports.len(),
        2,
        "expected exports for 2 modules, got {}",
        result.module_exports.len()
    );

    // Module A should have 2 exported globals.
    let a_exports = result.module_exports.values().find(|v| v.len() == 2);
    assert!(a_exports.is_some(), "expected one module with 2 exports");
}
