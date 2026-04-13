use music_module::{ImportMap, ModuleKey};
use music_session::{Session, SessionOptions};

use crate::{
    core, extend_import_map, intrinsics, module_source, register_modules, resolve_spec, syntax,
    test,
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

#[test]
fn extend_import_map_registers_foundation_specs() {
    let mut import_map = ImportMap::default();
    extend_import_map(&mut import_map);

    assert_eq!(
        import_map.imports.get(test::SPEC).map(String::as_str),
        Some(test::SPEC)
    );
    assert_eq!(
        import_map.imports.get(core::SPEC).map(String::as_str),
        Some(core::SPEC)
    );
    assert_eq!(
        import_map.imports.get(intrinsics::SPEC).map(String::as_str),
        Some(intrinsics::SPEC)
    );
    assert_eq!(
        import_map.imports.get(syntax::SPEC).map(String::as_str),
        Some(syntax::SPEC)
    );
}

#[test]
fn resolve_spec_maps_known_specs() {
    assert_eq!(resolve_spec(core::SPEC), Some(ModuleKey::new(core::SPEC)));
    assert_eq!(
        resolve_spec(intrinsics::SPEC),
        Some(ModuleKey::new(intrinsics::SPEC))
    );
    assert_eq!(resolve_spec(test::SPEC), Some(ModuleKey::new(test::SPEC)));
    assert_eq!(
        resolve_spec(syntax::SPEC),
        Some(ModuleKey::new(syntax::SPEC))
    );
    assert_eq!(resolve_spec("musi:missing"), None);
}

#[test]
fn module_source_maps_known_specs() {
    assert_eq!(module_source(core::SPEC), Some(core::MODULE));
    assert_eq!(module_source(intrinsics::SPEC), Some(intrinsics::MODULE));
    assert_eq!(module_source(test::SPEC), Some(test::MODULE));
    assert_eq!(module_source(syntax::SPEC), Some(syntax::MODULE));
    assert_eq!(module_source("musi:missing"), None);
    assert!(core::MODULE.contains("export opaque let Rangeable[T] := class"));
    assert!(core::MODULE.contains("export opaque let Option[T] := data"));
    assert!(test::MODULE.contains("export opaque let Sample[T] := class"));
    assert!(test::MODULE.contains("export opaque let SampleList[T] := data"));
    assert!(test::MODULE.contains("export opaque let SampleCase[T] := data"));
}

#[test]
fn register_modules_installs_foundation_modules() {
    compile_main_entry_with_source(
        r#"
let Core := import "musi:core";
let Intrinsics := import "musi:test";
export let answer : Int := 1;
"#,
    );
}

#[test]
fn register_modules_installs_syntax_root() {
    compile_main_entry_with_source(
        r#"
let Core := import "musi:core";
let Syntax := import "musi:syntax";
export let answer (body : Syntax, result : Type) : Any := Syntax.eval(body, result);
"#,
    );
}
