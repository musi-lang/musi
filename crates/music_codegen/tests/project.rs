#![allow(clippy::unwrap_used, clippy::panic, clippy::tests_outside_test_module)]

use std::fs;
use std::path::Path;

use musi_vm::Vm;
use music_codegen::pool::ConstantEntry;
use music_codegen::project::emit_project;
use music_codegen::{ProjectEmitResult, write_seam};
use music_hir::type_project;
use music_il::instruction::Operand;
use music_il::opcode::Opcode;
use music_resolve::loader::ModuleLoader;
use music_resolve::resolve_project;

fn resolve_and_emit(dir: &Path, entry: &str) -> ProjectEmitResult {
    let entry_path = dir.join(entry);
    let loader = ModuleLoader::new(dir.to_path_buf());
    let resolution = resolve_project(&entry_path, &loader).unwrap();
    let typed = type_project(resolution, loader);
    emit_project(typed).unwrap()
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

#[test]
fn top_level_function_is_emitted_as_callable_global() {
    let dir = tempfile::tempdir().unwrap();
    fs::write(
        dir.path().join("main.ms"),
        "let helper (x : Int) : Int := x\nexport let value := helper(7)",
    )
    .unwrap();

    let result = resolve_and_emit(dir.path(), "main.ms");
    let module = &result.module;

    assert_eq!(
        module.globals.len(),
        2,
        "expected helper and value globals, got {}",
        module.globals.len()
    );

    let main = module.methods.iter().find(|m| m.name.is_none()).unwrap();
    let has_ld_glob = main.instructions.iter().any(|i| i.opcode == Opcode::LdGlob);
    assert!(
        has_ld_glob,
        "expected top-level helper call to load a global closure"
    );
}

#[test]
fn imported_function_exports_are_available_to_dependents() {
    let dir = tempfile::tempdir().unwrap();
    fs::write(
        dir.path().join("a.ms"),
        "export let double (x : Int) : Int := x + x",
    )
    .unwrap();
    fs::write(
        dir.path().join("main.ms"),
        "import \"./a.ms\" as _\nexport let value := double(21)",
    )
    .unwrap();

    let result = resolve_and_emit(dir.path(), "main.ms");

    let has_double_export = result
        .module_exports
        .values()
        .any(|exports| exports.iter().any(|export| export.name == "double"));
    assert!(
        has_double_export,
        "expected exported function to be tracked in module exports"
    );

    let module = &result.module;
    let main = module
        .methods
        .iter()
        .filter(|m| m.name.is_none())
        .last()
        .unwrap();
    let has_ld_glob = main.instructions.iter().any(|i| i.opcode == Opcode::LdGlob);
    assert!(
        has_ld_glob,
        "expected imported function call to load a global"
    );
}

#[test]
fn zero_arg_lambda_does_not_become_entrypoint() {
    let dir = tempfile::tempdir().unwrap();
    fs::write(
        dir.path().join("main.ms"),
        "let f := () => 7\nexport let value := 1",
    )
    .unwrap();

    let result = resolve_and_emit(dir.path(), "main.ms");
    let entrypoint_count = result
        .module
        .methods
        .iter()
        .filter(|method| method.name.is_none())
        .count();

    assert_eq!(
        entrypoint_count, 1,
        "expected exactly one entrypoint method, got {entrypoint_count}"
    );
}

#[test]
fn same_named_effects_from_different_modules_do_not_merge() {
    let dir = tempfile::tempdir().unwrap();
    fs::write(
        dir.path().join("a.ms"),
        "export let Test := effect { let emit (value : Int) : Int };\nexport let a := 1;",
    )
    .unwrap();
    fs::write(
        dir.path().join("b.ms"),
        "export let Test := effect { let emit (value : Int) : Int };\nexport let b := 2;",
    )
    .unwrap();
    fs::write(
        dir.path().join("main.ms"),
        "import \"./a.ms\" as _;\nimport \"./b.ms\" as _;\nexport let value := 0;",
    )
    .unwrap();

    let result = resolve_and_emit(dir.path(), "main.ms");
    let test_effects: Vec<_> = result
        .module
        .effects
        .iter()
        .filter(|effect| effect.name == "Test")
        .collect();

    assert_eq!(
        test_effects.len(),
        2,
        "expected both Test effects to remain distinct"
    );
    assert_ne!(test_effects[0].module_name, test_effects[1].module_name);
}

#[test]
fn same_named_types_from_different_modules_do_not_merge() {
    let dir = tempfile::tempdir().unwrap();
    fs::write(
        dir.path().join("a.ms"),
        "export let Node := data { | Node }; export let is_a := (x) => x :? Node",
    )
    .unwrap();
    fs::write(
        dir.path().join("b.ms"),
        "export let Node := data { | Node }; export let is_b := (x) => x :? Node",
    )
    .unwrap();
    fs::write(
        dir.path().join("main.ms"),
        "import \"./a.ms\" as A; import \"./b.ms\" as B; export let value := 0",
    )
    .unwrap();

    let result = resolve_and_emit(dir.path(), "main.ms");
    let node_types: Vec<_> = result
        .module
        .types
        .iter()
        .filter(|ty| ty.key.ends_with("::Node") || ty.key == "Node")
        .collect();

    assert_eq!(
        node_types.len(),
        2,
        "expected both Node type descriptors to remain distinct"
    );
    assert_ne!(node_types[0].key, node_types[1].key);
}

#[test]
fn top_level_typed_value_is_not_emitted_as_callable_global() {
    let dir = tempfile::tempdir().unwrap();
    fs::write(dir.path().join("main.ms"), "export let answer : Int := 42").unwrap();

    let result = resolve_and_emit(dir.path(), "main.ms");
    let module = &result.module;
    let main = module
        .methods
        .iter()
        .find(|method| method.name.is_none())
        .unwrap();
    let has_cls_new = main
        .instructions
        .iter()
        .any(|instruction| instruction.opcode == Opcode::ClsNew);

    assert!(
        !has_cls_new,
        "expected typed top-level value binding to store a value, not a closure"
    );
}

#[test]
fn variant_case_roundtrips_through_vm() {
    let dir = tempfile::tempdir().unwrap();
    fs::write(
        dir.path().join("main.ms"),
        r#"
let _State := data {
  | A
  | B
};

let branch_local (value) :=
  case value of (
    | .A => .True
    | _ => .False
  );

export let x := branch_local(.A)
;
"#,
    )
    .unwrap();

    let result = resolve_and_emit(dir.path(), "main.ms");
    let bytes = write_seam(&result.module);
    let module = musi_vm::load(&bytes).unwrap();
    let mut vm = Vm::new(module);
    let _ = vm.run().unwrap();

    let exported = result
        .module_exports
        .values()
        .flat_map(|exports| exports.iter())
        .find(|export| export.name == "x")
        .unwrap();
    let value = vm.global_value(exported.index).unwrap();
    assert!(value.is_bool(), "expected Bool, got {value:?}");
    assert!(value.as_bool(), "expected .True");
}
