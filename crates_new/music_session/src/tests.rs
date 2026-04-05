use std::collections::BTreeSet;

use music_module::{ImportMap, ModuleKey};
use music_sema::TargetInfo;

use crate::{Session, SessionOptions};

fn session() -> Session {
    let mut import_map = ImportMap::default();
    let _ = import_map.imports.insert("dep".into(), "dep".into());
    Session::new(SessionOptions {
        import_map,
        ..SessionOptions::default()
    })
}

fn session_with_target(target: TargetInfo) -> Session {
    let mut options = SessionOptions::default();
    let mut import_map = ImportMap::default();
    let _ = import_map.imports.insert("dep".into(), "dep".into());
    options.import_map = import_map;
    options.target = Some(target);
    Session::new(options)
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
    assert!(output.text.contains(".global $main::answer export"));
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
    assert!(output.text.contains(".global $dep::base export"));
    assert!(output.text.contains(".global $main::answer export"));
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

#[test]
fn resolve_reuses_cached_parse_product() {
    let mut session = session();
    session
        .set_module_text(&ModuleKey::new("main"), "export let answer : Int := 42;")
        .unwrap();

    let _ = session.parse_module(&ModuleKey::new("main")).unwrap();
    let after_parse = session.stats().clone();
    let _ = session.resolve_module(&ModuleKey::new("main")).unwrap();

    assert_eq!(session.stats().parse_runs, after_parse.parse_runs);
    assert!(session.stats().resolve_runs > after_parse.resolve_runs);
}

#[test]
fn compiles_imported_generic_callable_calls() {
    let mut session = session();
    session
        .set_module_text(&ModuleKey::new("dep"), "export let id[T] (x : T) : T := x;")
        .unwrap();
    session
        .set_module_text(
            &ModuleKey::new("main"),
            r#"
            let dep := import "dep";
            export let answer () : Int := dep.id[Int](42);
        "#,
        )
        .unwrap();

    let output = session.compile_entry(&ModuleKey::new("main")).unwrap();

    assert!(output.artifact.validate().is_ok());
    assert!(output.text.contains("$dep::id"));
    assert!(output.text.contains("$main::answer"));
}

#[test]
fn compiles_imported_globals_and_local_assignment() {
    let mut session = session();
    session
        .set_module_text(&ModuleKey::new("dep"), "export let base : Int := 41;")
        .unwrap();
    session
        .set_module_text(
            &ModuleKey::new("main"),
            r#"
            let dep := import "dep";
            export let answer () : Int := (
              let mut local := dep.base;
              local <- local + 1;
              local
            );
        "#,
        )
        .unwrap();

    let output = session.compile_entry(&ModuleKey::new("main")).unwrap();

    assert!(output.artifact.validate().is_ok());
    assert!(output.text.contains("ld.glob $dep::base"));
    assert!(output.text.contains("$main::answer"));
}

#[test]
fn compiles_closures_and_higher_order_calls() {
    let mut session = session();
    session
        .set_module_text(
            &ModuleKey::new("main"),
            r"
            let apply (f : Int -> Int, x : Int) : Int := f(x);
            export let answer (x : Int) : Int := (
              let base : Int := 41;
              let add_base (y : Int) : Int := y + base;
              apply(add_base, x)
            );
        ",
        )
        .unwrap();

    let output = session.compile_entry(&ModuleKey::new("main")).unwrap();

    assert!(output.artifact.validate().is_ok());
    assert!(output.text.contains("call.cls"));
    assert!(output.text.contains("cls.new"));
}

#[test]
fn compiles_case_tuple_and_array_patterns() {
    let mut session = session();
    session
        .set_module_text(
            &ModuleKey::new("main"),
            r"
            export let answer () : Int := (
              let pair := (1, 2);
              let items := [3, 4];
              let p : Int := case pair of (| (1, b) => b | _ => 0);
              let q : Int := case items of (| [3, b] => b | _ => 0);
              p + q
            );
        ",
        )
        .unwrap();

    let output = session.compile_entry(&ModuleKey::new("main")).unwrap();

    assert!(output.artifact.validate().is_ok());
    assert!(output.text.contains("seq.get"));
    assert!(output.text.contains("br.false"));
}

#[test]
fn compiles_records_with_projection_and_update() {
    let mut session = session();
    session
        .set_module_text(
            &ModuleKey::new("main"),
            r"
            export let answer () : Int := (
              let r := { y := 2, x := 1 };
              let a : Int := r.x;
              let s := r.{ x := 3 };
              a + s.x
            );
        ",
        )
        .unwrap();

    let output = session.compile_entry(&ModuleKey::new("main")).unwrap();

    assert!(output.artifact.validate().is_ok());
    assert!(output.text.contains("data.get"));
    assert!(output.text.contains("data.new"));
    assert!(output.text.contains(".type $\"{ x: Int; y: Int }\""));
}

#[test]
fn compiles_record_destructuring_let_patterns() {
    let mut session = session();
    session
        .set_module_text(
            &ModuleKey::new("main"),
            r"
            export let answer () : Int := (
              let r := { y := 2, x := 1 };
              let {x, y} := r;
              x + y
            );
        ",
        )
        .unwrap();

    let output = session.compile_entry(&ModuleKey::new("main")).unwrap();

    assert!(output.artifact.validate().is_ok());
    assert!(output.text.contains("data.get"));
}

#[test]
fn compiles_tuple_and_array_destructuring_let_patterns() {
    let mut session = session();
    session
        .set_module_text(
            &ModuleKey::new("main"),
            r"
            export let answer () : Int := (
              let pair := (1, 2);
              let items := [3, 4];
              let (a, b) := pair;
              let [c, d] := items;
              a + b + c + d
            );
        ",
        )
        .unwrap();

    let output = session.compile_entry(&ModuleKey::new("main")).unwrap();

    assert!(output.artifact.validate().is_ok());
    assert!(output.text.contains("seq.get"));
}

#[test]
fn compiles_variants_with_case_patterns() {
    let mut session = session();
    session
        .set_module_text(
            &ModuleKey::new("main"),
            r"
            let Maybe := data { | Some : Int | None };
            export let answer () : Int := (
              let x : Maybe := .Some(1);
              case x of (
              | .Some(y) => y
              | .None => 0
              )
            );
        ",
        )
        .unwrap();

    let output = session.compile_entry(&ModuleKey::new("main")).unwrap();

    assert!(output.artifact.validate().is_ok());
    assert!(output.text.contains("data.tag"));
    assert!(output.text.contains("br.tbl"));
    assert!(output.text.contains("data.get"));
    assert!(output.text.contains("data.new"));
    assert!(output.text.contains(".type $main::Maybe"));
}

#[test]
fn compiles_variants_without_type_context_when_tag_unique() {
    let mut session = session();
    session
        .set_module_text(
            &ModuleKey::new("main"),
            r"
            let Maybe := data { | Some : Int | None };
            export let answer () : Int := (
              let x := .Some(1);
              case x of (
              | .Some(y) => y
              | .None => 0
              )
            );
        ",
        )
        .unwrap();

    let output = session.compile_entry(&ModuleKey::new("main")).unwrap();

    assert!(output.artifact.validate().is_ok());
    assert!(output.text.contains("data.tag"));
    assert!(output.text.contains("br.tbl"));
    assert!(output.text.contains("data.get"));
    assert!(output.text.contains("data.new"));
}

#[test]
fn compiles_effects_with_perform_handle_resume() {
    let mut session = session();
    session
        .set_module_text(
            &ModuleKey::new("main"),
            r#"
            let Console := effect { let readln () : String; };
            export let answer () : String :=
              handle perform Console.readln() with Console of (
              | value => value
              | readln(k) => resume "ok"
              );
        "#,
        )
        .unwrap();

    let output = session.compile_entry(&ModuleKey::new("main")).unwrap();

    assert!(output.artifact.validate().is_ok());
    assert!(output.text.contains("hdl.push"));
    assert!(output.text.contains("hdl.pop"));
    assert!(output.text.contains("eff.invk"));
    assert!(output.text.contains("eff.resume"));
}

#[test]
fn compiles_exported_foreign_declarations_into_artifact() {
    let mut session = session();
    session
        .set_module_text(
            &ModuleKey::new("main"),
            r#"
            export foreign "c" (
              let puts (msg : CString) : Int;
            );
            export let answer : Int := 1;
        "#,
        )
        .unwrap();

    let output = session.compile_module(&ModuleKey::new("main")).unwrap();

    assert!(output.artifact.validate().is_ok());
    assert!(output
        .text
        .contains(".foreign $main::puts abi \"c\" symbol \"puts\" export"));
}

#[test]
fn lowers_link_attrs_into_foreign_descriptors() {
    let mut session = session();
    session
        .set_module_text(
            &ModuleKey::new("main"),
            r#"
            @link(name := "m")
            foreign "c" (
              let sin (x : Float) : Float;
            );
        "#,
        )
        .unwrap();

    let output = session.compile_module(&ModuleKey::new("main")).unwrap();
    assert!(output.artifact.validate().is_ok());
    assert!(output
        .text
        .contains(".foreign $main::sin abi \"c\" symbol \"sin\" link \"m\""));
}

#[test]
fn skips_gated_foreign_declarations_for_target() {
    let mut session = session_with_target(TargetInfo {
        os: Some("linux".into()),
        arch: Some("x86_64".into()),
        env: None,
        abi: None,
        vendor: None,
        features: BTreeSet::default(),
    });
    session
        .set_module_text(
            &ModuleKey::new("main"),
            r#"
            @when(os := "linux")
            foreign let clock_gettime (id : Int, out : CPtr) : Int;

            @when(os := "windows")
            foreign let QueryPerformanceCounter (out : CPtr) : Int;
        "#,
        )
        .unwrap();

    let output = session.compile_module(&ModuleKey::new("main")).unwrap();
    assert!(output.artifact.validate().is_ok());
    assert!(output.text.contains("clock_gettime"));
    assert!(!output.text.contains("QueryPerformanceCounter"));
}
