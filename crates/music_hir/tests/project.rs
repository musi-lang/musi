#![allow(clippy::panic, clippy::unwrap_used, clippy::tests_outside_test_module)]

use std::fs;

use music_hir::type_project;
use music_resolve::loader::ModuleLoader;
use music_resolve::resolve_project;

#[test]
fn typed_project_contains_typed_modules_for_project_order() {
    let dir = tempfile::tempdir().unwrap();
    fs::write(dir.path().join("dep.ms"), "export let value := 1;").unwrap();
    fs::write(
        dir.path().join("main.ms"),
        "import \"./dep.ms\" as _;\nexport let result := value;",
    )
    .unwrap();

    let entry = dir.path().join("main.ms");
    let loader = ModuleLoader::new(dir.path().to_path_buf());
    let resolved = resolve_project(&entry, &loader).unwrap();
    let typed = type_project(resolved, loader);

    assert_eq!(typed.modules.len(), 2, "expected both modules to be typed");
    assert_eq!(
        typed.order.len(),
        2,
        "expected dependency order to remain intact"
    );
    assert!(
        typed.order.iter().all(|module_id| typed.modules.contains_key(module_id)),
        "expected every ordered module to have typed state"
    );
    assert!(
        typed.modules.values().all(|module| !module.has_errors),
        "expected error-free typed modules"
    );
}

#[test]
fn typed_project_preserves_sema_diagnostics() {
    let dir = tempfile::tempdir().unwrap();
    fs::write(dir.path().join("main.ms"), "export let x : Int := \"nope\";").unwrap();

    let entry = dir.path().join("main.ms");
    let loader = ModuleLoader::new(dir.path().to_path_buf());
    let resolved = resolve_project(&entry, &loader).unwrap();
    let typed = type_project(resolved, loader);
    let module = typed.modules.values().next().unwrap();

    assert!(module.has_errors, "expected sema errors to be preserved");
    assert!(
        !module.diagnostics.is_empty(),
        "expected sema diagnostics on typed module"
    );
}

#[test]
fn user_module_rejects_reserved_compiler_attributes() {
    let dir = tempfile::tempdir().unwrap();
    fs::write(
        dir.path().join("main.ms"),
        "@musi.lang(name := \"Fake\") let Fake := data { | Fake };",
    )
    .unwrap();

    let entry = dir.path().join("main.ms");
    let loader = ModuleLoader::new(dir.path().to_path_buf());
    let resolved = resolve_project(&entry, &loader).unwrap();
    let typed = type_project(resolved, loader);
    let module = typed.modules.values().next().unwrap();

    assert!(module.has_errors, "expected reserved compiler attr to fail");
    assert!(
        module
            .diagnostics
            .iter()
            .any(|diag| diag.code.map(|code| code.raw()) == Some(2502)),
        "expected reserved compiler attr diagnostic, got {:?}",
        module
            .diagnostics
            .iter()
            .map(|diag| diag.message.as_str())
            .collect::<Vec<_>>()
    );
}

#[test]
fn diag_allow_suppresses_matching_code() {
    let dir = tempfile::tempdir().unwrap();
    fs::write(dir.path().join("main.ms"), "@diag.allow(ms4023) let x := 1;").unwrap();

    let entry = dir.path().join("main.ms");
    let loader = ModuleLoader::new(dir.path().to_path_buf());
    let resolved = resolve_project(&entry, &loader).unwrap();
    let typed = type_project(resolved, loader);
    let module = typed.modules.values().next().unwrap();

    assert!(
        module
            .diagnostics
            .iter()
            .all(|diag| diag.code.map(|code| code.raw()) != Some(4023)),
        "expected ms4023 to be suppressed, got {:?}",
        module
            .diagnostics
            .iter()
            .map(|diag| (diag.code.map(|code| code.raw()), diag.message.as_str()))
            .collect::<Vec<_>>()
    );
}

#[test]
fn diag_warn_demotes_matching_error_code() {
    let dir = tempfile::tempdir().unwrap();
    fs::write(
        dir.path().join("main.ms"),
        "@diag.warn(ms2502) @musi.lang(name := \"Fake\") let Fake := data { | Fake };",
    )
    .unwrap();

    let entry = dir.path().join("main.ms");
    let loader = ModuleLoader::new(dir.path().to_path_buf());
    let resolved = resolve_project(&entry, &loader).unwrap();
    let typed = type_project(resolved, loader);
    let module = typed.modules.values().next().unwrap();

    assert!(
        module
            .diagnostics
            .iter()
            .any(|diag| diag.code.map(|code| code.raw()) == Some(2502)
                && matches!(diag.level, music_shared::diag::DiagLevel::Warning)),
        "expected ms2502 to be demoted to warning"
    );
}

#[test]
fn diag_expect_consumes_matching_diagnostic() {
    let dir = tempfile::tempdir().unwrap();
    fs::write(
        dir.path().join("main.ms"),
        "@diag.expect(ms2502) @musi.lang(name := \"Fake\") let Fake := data { | Fake };",
    )
    .unwrap();

    let entry = dir.path().join("main.ms");
    let loader = ModuleLoader::new(dir.path().to_path_buf());
    let resolved = resolve_project(&entry, &loader).unwrap();
    let typed = type_project(resolved, loader);
    let module = typed.modules.values().next().unwrap();

    assert!(
        module
            .diagnostics
            .iter()
            .all(|diag| diag.code.map(|code| code.raw()) != Some(2502)),
        "expected ms2502 to be consumed by @diag.expect"
    );
    assert!(
        module
            .diagnostics
            .iter()
            .all(|diag| diag.code.map(|code| code.raw()) != Some(2510)),
        "expected no unmet expectation diagnostic"
    );
}

#[test]
fn diag_expect_reports_unmet_code() {
    let dir = tempfile::tempdir().unwrap();
    fs::write(dir.path().join("main.ms"), "@diag.expect(ms2502) let x := 1;").unwrap();

    let entry = dir.path().join("main.ms");
    let loader = ModuleLoader::new(dir.path().to_path_buf());
    let resolved = resolve_project(&entry, &loader).unwrap();
    let typed = type_project(resolved, loader);
    let module = typed.modules.values().next().unwrap();

    assert!(
        module
            .diagnostics
            .iter()
            .any(|diag| diag.code.map(|code| code.raw()) == Some(2510)),
        "expected unmet expectation diagnostic"
    );
}

#[test]
fn legacy_public_attr_is_rejected() {
    let dir = tempfile::tempdir().unwrap();
    fs::write(dir.path().join("main.ms"), "@diagnostic.allow(ms2502) let x := 1;").unwrap();

    let entry = dir.path().join("main.ms");
    let loader = ModuleLoader::new(dir.path().to_path_buf());
    let resolved = resolve_project(&entry, &loader).unwrap();
    let typed = type_project(resolved, loader);
    let module = typed.modules.values().next().unwrap();

    assert!(
        module
            .diagnostics
            .iter()
            .any(|diag| diag.code.map(|code| code.raw()) == Some(2501)),
        "expected legacy attribute diagnostic"
    );
}
