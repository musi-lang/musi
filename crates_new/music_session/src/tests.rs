use music_module::{ImportMap, ModuleKey};

use crate::{Session, SessionOptions};

fn session() -> Session {
    let mut import_map = ImportMap::default();
    let _ = import_map.imports.insert("dep".into(), "dep".into());
    Session::new(SessionOptions {
        import_map,
        ..SessionOptions::default()
    })
}

#[test]
fn compiles_module_to_artifact_bytes_and_text() {
    let mut session = session();
    session
        .set_module_text(&ModuleKey::new("main"), "export let answer : Int := 42;")
        .unwrap();

    let output = session.compile_module(&ModuleKey::new("main")).unwrap();

    assert!(output.artifact.validate().is_ok());
    assert!(!output.bytes.is_empty());
    assert!(output.text.contains(".global @main::answer export"));
}

#[test]
fn compiles_reachable_entry_graph() {
    let mut session = session();
    session
        .set_module_text(&ModuleKey::new("dep"), "export let base : Int := 41;")
        .unwrap();
    session
        .set_module_text(
            &ModuleKey::new("main"),
            "import \"dep\"; export let answer : Int := 42;",
        )
        .unwrap();

    let output = session.compile_entry(&ModuleKey::new("main")).unwrap();

    assert!(output.artifact.validate().is_ok());
    assert!(output.text.contains(".global @dep::base export"));
    assert!(output.text.contains(".global @main::answer export"));
}

#[test]
fn reuses_caches_and_invalidates_dependents_on_edit() {
    let mut session = session();
    session
        .set_module_text(&ModuleKey::new("dep"), "export let base : Int := 41;")
        .unwrap();
    session
        .set_module_text(
            &ModuleKey::new("main"),
            "import \"dep\"; export let answer : Int := 42;",
        )
        .unwrap();

    let _ = session.compile_entry(&ModuleKey::new("main")).unwrap();
    let first_stats = session.stats().clone();
    let _ = session.compile_entry(&ModuleKey::new("main")).unwrap();
    assert_eq!(session.stats(), &first_stats);

    session
        .set_module_text(&ModuleKey::new("dep"), "export let base : Int := 99;")
        .unwrap();
    let _ = session.compile_entry(&ModuleKey::new("main")).unwrap();
    assert!(session.stats().resolve_runs > first_stats.resolve_runs);
    assert!(session.stats().emit_runs > first_stats.emit_runs);
}
