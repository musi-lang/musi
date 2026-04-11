use music_module::{ImportMap, ModuleKey};
use music_session::{Session, SessionOptions};

use crate::{extend_import_map, module_source, register_modules, resolve_spec, syntax, test};

#[test]
fn extend_import_map_registers_foundation_specs() {
    let mut import_map = ImportMap::default();
    extend_import_map(&mut import_map);

    assert_eq!(
        import_map.imports.get(test::SPEC).map(String::as_str),
        Some(test::SPEC)
    );
    assert_eq!(
        import_map.imports.get(syntax::SPEC).map(String::as_str),
        Some(syntax::SPEC)
    );
}

#[test]
fn resolve_spec_maps_known_specs() {
    assert_eq!(resolve_spec(test::SPEC), Some(ModuleKey::new(test::SPEC)));
    assert_eq!(
        resolve_spec(syntax::SPEC),
        Some(ModuleKey::new(syntax::SPEC))
    );
    assert_eq!(resolve_spec("musi:missing"), None);
}

#[test]
fn module_source_maps_known_specs() {
    assert_eq!(module_source(test::SPEC), Some(test::MODULE));
    assert_eq!(module_source(syntax::SPEC), Some(syntax::MODULE));
    assert_eq!(module_source("musi:missing"), None);
}

#[test]
fn register_modules_installs_foundation_modules() {
    let mut options = SessionOptions::default();
    extend_import_map(&mut options.import_map);
    let mut session = Session::new(options);
    register_modules(&mut session).unwrap();
    session
        .set_module_text(
            &ModuleKey::new("main"),
            r#"
let Intrinsics := import "musi:test";
export let answer : Int := 1;
"#,
        )
        .unwrap();

    let output = session.compile_entry(&ModuleKey::new("main")).unwrap();
    assert!(!output.bytes.is_empty());
}

#[test]
fn register_modules_installs_syntax_root() {
    let mut options = SessionOptions::default();
    extend_import_map(&mut options.import_map);
    let mut session = Session::new(options);
    register_modules(&mut session).unwrap();
    session
        .set_module_text(
            &ModuleKey::new("main"),
            r#"
let Syntax := import "musi:syntax";
export let answer (body : Syntax, result : Type) : Any := Syntax.eval(body, result);
"#,
        )
        .unwrap();

    let output = session.compile_entry(&ModuleKey::new("main")).unwrap();
    assert!(!output.bytes.is_empty());
}
