#![allow(unused_imports)]

use music_module::{ImportMap, ModuleKey};
use music_session::{Session, SessionOptions};

use crate::{extend_import_map, module_source, register_modules, resolve_spec};

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

mod success {
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
    fn resolve_spec_maps_known_specs() {
        assert_eq!(resolve_spec("musi:core"), Some(ModuleKey::new("musi:core")));
        assert_eq!(resolve_spec("musi:intrinsics"), None);
        assert_eq!(resolve_spec("musi:env"), Some(ModuleKey::new("musi:env")));
        assert_eq!(resolve_spec("musi:test"), Some(ModuleKey::new("musi:test")));
        assert_eq!(
            resolve_spec("musi:syntax"),
            Some(ModuleKey::new("musi:syntax"))
        );
        assert_eq!(resolve_spec("musi:missing"), None);
    }

    #[test]
    fn module_source_maps_known_specs() {
        assert!(module_source("musi:core").is_some());
        assert!(module_source("musi:intrinsics").is_some());
        assert!(module_source("musi:env").is_some());
        assert!(module_source("musi:test").is_some());
        assert!(module_source("musi:syntax").is_some());
        assert_eq!(module_source("musi:missing"), None);
        assert!(
            module_source("musi:core")
                .unwrap()
                .contains("export opaque let Rangeable [T] := shape")
        );
        assert!(
            module_source("musi:core")
                .unwrap()
                .contains("export opaque let Option [T] := data")
        );
        assert!(
            module_source("musi:env")
                .unwrap()
                .contains("export opaque let Env := effect")
        );
        assert!(
            module_source("musi:process")
                .unwrap()
                .contains("let argCount () : Int;")
        );
        assert!(
            module_source("musi:test")
                .unwrap()
                .contains("export opaque let Sample [T] := shape")
        );
        assert!(
            module_source("musi:test")
                .unwrap()
                .contains("export opaque let SampleList [T] := data")
        );
        assert!(
            module_source("musi:test")
                .unwrap()
                .contains("export opaque let SampleCase [T] := data")
        );
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
