#![allow(unused_imports)]

use std::fs::{self, DirEntry};
use std::path::Path;

use music_module::{ImportMap, ModuleKey};
use music_session::{Session, SessionOptions};

use crate::{
    extend_import_map, module_source, register_modules, resolve_public_spec, resolve_spec,
};

fn compile_main_entry_with_source(source: &str) {
    let mut options = SessionOptions::default();
    extend_import_map(&mut options.import_map);
    let mut session = Session::new(options);
    register_modules(&mut session).unwrap();
    session
        .set_module_text(&ModuleKey::new("main"), source)
        .unwrap();
    let output = session.compile_entry(&ModuleKey::new("main")).unwrap();
    assert!(!output.bytes.is_empty());
}

fn collect_plain_foundation_doc_issues(root: &Path, dir: &Path, issues: &mut Vec<String>) {
    let mut entries = fs::read_dir(dir)
        .expect("foundation modules dir should be readable")
        .collect::<Result<Vec<_>, _>>()
        .expect("foundation module dir entries should be readable");
    entries.sort_by_key(DirEntry::path);
    for entry in entries {
        let path = entry.path();
        if path.extension().is_none_or(|ext| ext != "ms") {
            continue;
        }
        collect_plain_foundation_doc_issues_in_file(root, &path, issues);
    }
}

fn collect_plain_foundation_doc_issues_in_file(root: &Path, path: &Path, issues: &mut Vec<String>) {
    let banned = [
        "alias",
        "typedef",
        "wrapper",
        "helper namespace",
        "helpers surface",
        "surface",
        "core operation",
        "implementation",
        "internal",
        "intrinsic",
    ];
    let text = fs::read_to_string(path).expect("foundation module should be readable");
    for (index, line) in text.lines().enumerate() {
        let trimmed = line.trim_start();
        if !(trimmed.starts_with("---") || trimmed.starts_with("--!")) {
            continue;
        }
        let lower = trimmed.to_lowercase();
        if banned.iter().any(|word| lower.contains(word)) {
            let relative = path
                .strip_prefix(root)
                .expect("foundation module path should be under root");
            issues.push(format!("{}:{}: {}", relative.display(), index + 1, trimmed));
        }
    }
}

fn collect_missing_public_foundation_export_docs(
    root: &Path,
    public_files: &[&str],
    missing: &mut Vec<String>,
) {
    for public_file in public_files {
        let path = root.join(public_file);
        let text = fs::read_to_string(&path).expect("public foundation module should be readable");
        let lines = text.lines().collect::<Vec<_>>();
        for (index, line) in lines.iter().enumerate() {
            if !line.trim_start().starts_with("export ") {
                continue;
            }
            let has_doc = lines[..index]
                .iter()
                .rev()
                .find(|line| !line.trim().is_empty())
                .is_some_and(|line| line.trim_start().starts_with("---"));
            if !has_doc {
                missing.push(format!("{public_file}:{}", index + 1));
            }
        }
    }
}

mod success {
    use music_builtin::all_foundation_modules;

    use crate::registry::{self, registered_specs};

    use super::*;

    #[test]
    fn extend_import_map_registers_foundation_specs() {
        let mut import_map = ImportMap::default();
        extend_import_map(&mut import_map);

        assert_eq!(
            import_map.imports.get("musi:test").map(String::as_str),
            Some("musi:test")
        );
        assert_eq!(
            import_map.imports.get("musi:core").map(String::as_str),
            Some("musi:core")
        );
        assert_eq!(import_map.imports.get("musi:intrinsics"), None);
        assert_eq!(
            import_map.imports.get("musi:env").map(String::as_str),
            Some("musi:env")
        );
        assert_eq!(
            import_map.imports.get("musi:process").map(String::as_str),
            Some("musi:process")
        );
        assert_eq!(
            import_map.imports.get("musi:syntax").map(String::as_str),
            Some("musi:syntax")
        );
    }

    #[test]
    fn foundation_registry_matches_builtin_catalog() {
        let registry_specs = registered_specs();
        let catalog_specs = all_foundation_modules()
            .iter()
            .map(|module| module.spec)
            .collect::<Vec<_>>();

        assert_eq!(registry_specs, catalog_specs);
    }

    #[test]
    fn resolve_spec_maps_known_specs() {
        assert_eq!(resolve_spec("musi:core"), Some(ModuleKey::new("musi:core")));
        assert_eq!(resolve_spec("musi:intrinsics"), None);
        assert_eq!(resolve_spec("musi:env"), Some(ModuleKey::new("musi:env")));
        assert_eq!(resolve_spec("musi:test"), Some(ModuleKey::new("musi:test")));
        assert_eq!(
            resolve_spec("musi:syntax"),
            Some(ModuleKey::new("musi:syntax"))
        );
        assert_eq!(resolve_spec("musi:log"), None);
        assert_eq!(resolve_spec("musi:missing"), None);
    }

    #[test]
    fn resolve_public_spec_exposes_native_api_roots() {
        for public_spec in ["musi:core", "musi:ffi", "musi:test", "musi:syntax"] {
            assert_eq!(
                resolve_public_spec(public_spec),
                Some(ModuleKey::new(public_spec))
            );
        }
        for host_spec in ["musi:env", "musi:io", "musi:fs", "musi:text", "musi:json"] {
            assert_eq!(
                resolve_public_spec(host_spec),
                None,
                "host spec should stay behind @std: {host_spec}"
            );
        }
    }

    #[test]
    fn module_source_maps_known_specs() {
        assert!(module_source("musi:core").is_some());
        assert!(module_source("musi:intrinsics").is_some());
        assert!(module_source("musi:env").is_some());
        assert!(module_source("musi:test").is_some());
        assert!(module_source("musi:syntax").is_some());
        assert_eq!(module_source("musi:log"), None);
        assert_eq!(module_source("musi:missing"), None);
        assert!(
            module_source("musi:core")
                .unwrap()
                .contains("export hidden let Rangeable [T] := shape")
        );
        assert!(
            module_source("musi:core")
                .unwrap()
                .contains("export hidden let Maybe [T] := data")
        );
        for helper in [
            "export let Some [T]",
            "export let None [T]",
            "export let isSome [T]",
            "export let isNone [T]",
            "export let unwrapOr [T]",
        ] {
            assert!(
                !module_source("musi:core").unwrap().contains(helper),
                "musi:core should leave Maybe helpers to @std/maybe: {helper}"
            );
        }
        assert!(
            module_source("musi:env")
                .unwrap()
                .contains("let Musi__get (name : String) : String;")
        );
        assert!(
            module_source("musi:process")
                .unwrap()
                .contains("let Musi__argCount () : Int;")
        );
        assert!(
            module_source("musi:test")
                .unwrap()
                .contains("export hidden let Sample [T] := shape")
        );
        assert!(
            module_source("musi:test")
                .unwrap()
                .contains("export hidden let SampleList [T] := data")
        );
        assert!(
            module_source("musi:test")
                .unwrap()
                .contains("export hidden let SampleCase [T] := data")
        );
    }

    #[test]
    fn public_foundation_modules_are_native_or_compiler_owned() {
        for module in all_foundation_modules() {
            let source = module_source(module.spec).expect("foundation source should exist");
            if module.hidden || module.spec == "musi:core" || module.spec == "musi:ffi" {
                continue;
            }

            assert!(
                source.contains("@foreign(abi := .musi)"),
                "{} should stay a native host module or move to @std",
                module.spec
            );
        }
    }

    #[test]
    fn foundation_docs_use_plain_public_wording() {
        let repo_root = Path::new(env!("CARGO_MANIFEST_DIR"))
            .join("../..")
            .canonicalize()
            .expect("repo root should resolve");
        let modules_root = repo_root.join("crates/musi_foundation/modules");
        let mut issues = Vec::<String>::new();
        collect_plain_foundation_doc_issues(&modules_root, &modules_root, &mut issues);

        assert!(
            issues.is_empty(),
            "foundation docs use implementation-framed wording:\n{}",
            issues.join("\n")
        );
    }

    #[test]
    fn public_foundation_exports_have_doc_comments() {
        let repo_root = Path::new(env!("CARGO_MANIFEST_DIR"))
            .join("../..")
            .canonicalize()
            .expect("repo root should resolve");
        let modules_root = repo_root.join("crates/musi_foundation/modules");
        let mut missing = Vec::<String>::new();
        collect_missing_public_foundation_export_docs(
            &modules_root,
            &["core.ms", "ffi.ms", "syntax.ms", "test.ms"],
            &mut missing,
        );

        assert!(
            missing.is_empty(),
            "public foundation exports missing doc comments:\n{}",
            missing.join("\n")
        );
    }

    #[test]
    fn foundation_host_sources_use_private_filenames() {
        let repo_root = Path::new(env!("CARGO_MANIFEST_DIR"))
            .join("../..")
            .canonicalize()
            .expect("repo root should resolve");
        let modules_root = repo_root.join("crates/musi_foundation/modules");
        let public_sources = ["core.ms", "ffi.ms", "syntax.ms", "test.ms"];

        for entry in fs::read_dir(&modules_root).expect("foundation modules dir should be readable")
        {
            let entry = entry.expect("foundation modules dir entry should be readable");
            let path = entry.path();
            if path.extension().and_then(|ext| ext.to_str()) != Some("ms") {
                continue;
            }
            let file_name = path
                .file_name()
                .and_then(|name| name.to_str())
                .expect("foundation module file name should be utf-8");
            if public_sources.contains(&file_name) {
                continue;
            }
            assert!(
                file_name.starts_with('_'),
                "foundation host source should use private filename: {file_name}"
            );
        }
    }

    #[test]
    fn foundation_rust_api_uses_spec_names() {
        let repo_root = Path::new(env!("CARGO_MANIFEST_DIR"))
            .join("../..")
            .canonicalize()
            .expect("repo root should resolve");
        let lib = fs::read_to_string(repo_root.join("crates/musi_foundation/src/lib.rs"))
            .expect("foundation lib should be readable");

        for old_name in ["json_host", "encoding_host", "crypto_host", "uuid_host"] {
            assert!(
                !lib.contains(old_name),
                "foundation Rust API should use spec names, found {old_name}"
            );
        }
    }

    #[test]
    fn register_modules_installs_foundation_modules() {
        compile_main_entry_with_source(
            r#"
let Core := import "musi:core";
let Intrinsics := import "musi:test";
export let result : Int := 1;
"#,
        );
    }

    #[test]
    fn register_modules_installs_syntax_root() {
        compile_main_entry_with_source(
            r#"
let Core := import "musi:core";
let Syntax := import "musi:syntax";
export let result (body : Syntax, result : Type) : Any := Syntax.eval(body, result);
"#,
        );
    }

    #[test]
    fn register_modules_installs_time_root() {
        compile_main_entry_with_source(
            r#"
let Time := import "musi:time";
export let result () : Int := Time.nowUnixMs();
"#,
        );
    }
}

mod failure {
    use super::*;

    #[test]
    fn unknown_foundation_spec_is_not_registered() {
        assert_eq!(resolve_spec("musi:missing"), None);
        assert_eq!(module_source("musi:missing"), None);
    }
}
