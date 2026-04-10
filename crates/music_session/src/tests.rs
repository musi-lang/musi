use std::collections::BTreeSet;

use music_base::diag::Diag;
use music_bc::Artifact;
use music_emit::{EmitDiagKind, emit_diag_kind};
use music_ir::{IrDiagKind, ir_diag_kind};
use music_module::{ImportMap, ModuleKey};
use music_resolve::{ResolveDiagKind, resolve_diag_kind};
use music_sema::{SemaDiagKind, TargetInfo, sema_diag_kind};
use music_syntax::{ParseErrorKind, TokenKind};

use crate::{Session, SessionError, SessionOptions};

fn meta_records(artifact: &Artifact) -> Vec<(String, String, Vec<String>)> {
    artifact
        .meta
        .as_slice()
        .iter()
        .map(|record| {
            (
                artifact.string_text(record.target).to_owned(),
                artifact.string_text(record.key).to_owned(),
                record
                    .values
                    .iter()
                    .map(|value| artifact.string_text(*value).to_owned())
                    .collect::<Vec<_>>(),
            )
        })
        .collect::<Vec<_>>()
}

fn meta_has_exact(
    meta: &[(String, String, Vec<String>)],
    target: &str,
    key: &str,
    values: &[&str],
) -> bool {
    meta.iter().any(|(t, k, v)| {
        t == target
            && k == key
            && v.len() == values.len()
            && v.iter()
                .map(String::as_str)
                .zip(values.iter().copied())
                .all(|(left, right)| left == right)
    })
}

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
fn parse_failures_expose_typed_syntax_errors_and_diags() {
    let mut session = session();
    session
        .set_module_text(&ModuleKey::new("main"), "let x := 1")
        .unwrap();

    let err = session.parse_module(&ModuleKey::new("main")).unwrap_err();
    let SessionError::Parse { syntax, .. } = err else {
        panic!("parse error expected");
    };

    assert!(syntax.lex_errors().is_empty());
    assert_eq!(syntax.parse_errors().len(), 1);
    assert!(matches!(
        syntax.parse_errors()[0].kind,
        ParseErrorKind::ExpectedToken {
            expected: TokenKind::Semicolon,
            ..
        }
    ));
    assert_eq!(syntax.diags().len(), 1);
    assert!(!syntax.diags()[0].labels().is_empty());
}

#[test]
fn compile_module_propagates_parse_failures() {
    let mut session = session();
    session
        .set_module_text(&ModuleKey::new("main"), "let x := 1")
        .unwrap();

    let err = session.compile_module(&ModuleKey::new("main")).unwrap_err();
    let SessionError::Parse { syntax, .. } = err else {
        panic!("parse error expected");
    };

    assert!(syntax.lex_errors().is_empty());
    assert_eq!(syntax.parse_errors().len(), 1);
    assert_eq!(syntax.diags().len(), 1);
}

#[test]
fn compile_entry_propagates_parse_failures() {
    let mut session = session();
    session
        .set_module_text(&ModuleKey::new("main"), "let x := 1")
        .unwrap();

    let err = session.compile_entry(&ModuleKey::new("main")).unwrap_err();
    let SessionError::Parse { syntax, .. } = err else {
        panic!("parse error expected");
    };

    assert!(syntax.lex_errors().is_empty());
    assert_eq!(syntax.parse_errors().len(), 1);
    assert_eq!(syntax.diags().len(), 1);
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
              let local := mut dep.base;
              local := local + 1;
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
fn compiles_dynamic_import_multi_index_and_quote() {
    let mut session = session();
    session
        .set_module_text(
            &ModuleKey::new("main"),
            r"
            export let touch (name : String, grid : mut Array[Int, 2, 2]) : Int := (
              let loaded := import name;
              grid.[0, 1] := 7;
              grid.[0, 1]
            );
            export let quoted : Syntax := quote (#(1 + 2));
        ",
        )
        .unwrap();

    let output = session.compile_entry(&ModuleKey::new("main")).unwrap();

    assert!(output.artifact.validate().is_ok());
    assert!(output.text.contains("mod.load"));
    assert!(output.text.contains("seq.getn"));
    assert!(output.text.contains("seq.setn"));
    assert!(output.text.contains("quote (#(1 + 2))"));
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
fn resolve_failures_surface_session_resolve_error() {
    let mut session = session();
    session
        .set_module_text(
            &ModuleKey::new("main"),
            "import \"missing\"; export let answer : Int := 42;",
        )
        .unwrap();

    let err = session.resolve_module(&ModuleKey::new("main")).unwrap_err();
    let SessionError::Resolve { diags, .. } = err else {
        panic!("resolve error expected");
    };

    assert_eq!(diags.len(), 1);
    assert_eq!(
        resolve_diag_kind(&diags[0]),
        Some(ResolveDiagKind::ImportResolveFailed)
    );
    assert!(!diags[0].labels().is_empty());
}

#[test]
fn sema_failures_surface_session_sema_error() {
    let mut session = session();
    session
        .set_module_text(
            &ModuleKey::new("main"),
            "export let answer : Int := \"no\";",
        )
        .unwrap();

    let err = session.check_module(&ModuleKey::new("main")).unwrap_err();
    let SessionError::Sema { diags, .. } = err else {
        panic!("sema error expected");
    };

    assert!(!diags.is_empty());
    assert!(
        diags
            .iter()
            .any(|diag| sema_diag_kind(diag) == Some(SemaDiagKind::TypeMismatch))
    );
    assert!(diags.iter().any(|diag| !diag.labels().is_empty()));
}

#[test]
fn lower_module_propagates_ir_failure_with_typed_kind() {
    let mut session = session();
    session
        .set_module_text(&ModuleKey::new("main"), "export let answer : Int := 42;")
        .unwrap();
    session.inject_ir_failure_for_tests(
        vec![
            Diag::error(IrDiagKind::LoweringRequiresSemaCleanModule.message())
                .with_code(IrDiagKind::LoweringRequiresSemaCleanModule.code()),
        ]
        .into_boxed_slice(),
    );

    let err = session.lower_module(&ModuleKey::new("main")).unwrap_err();
    let SessionError::Ir { diags, .. } = err else {
        panic!("ir error expected");
    };

    assert_eq!(diags.len(), 1);
    assert_eq!(
        ir_diag_kind(&diags[0]),
        Some(IrDiagKind::LoweringRequiresSemaCleanModule)
    );
}

#[test]
fn compile_module_propagates_emit_failure_with_typed_kind() {
    let mut session = session();
    session
        .set_module_text(&ModuleKey::new("main"), "export let answer : Int := 42;")
        .unwrap();
    session.inject_emit_failure_for_tests(
        vec![
            Diag::error(EmitDiagKind::UnknownTypeValue.message())
                .with_code(EmitDiagKind::UnknownTypeValue.code()),
        ]
        .into_boxed_slice(),
    );

    let err = session.compile_module(&ModuleKey::new("main")).unwrap_err();
    let SessionError::Emit { diags, .. } = err else {
        panic!("emit error expected");
    };

    assert_eq!(diags.len(), 1);
    assert_eq!(
        emit_diag_kind(&diags[0]),
        Some(EmitDiagKind::UnknownTypeValue)
    );
}

#[test]
fn compile_entry_propagates_emit_failure_with_typed_kind() {
    let mut session = session();
    session
        .set_module_text(&ModuleKey::new("main"), "export let answer : Int := 42;")
        .unwrap();
    session.inject_emit_failure_for_tests(
        vec![
            Diag::error(EmitDiagKind::UnknownTypeValue.message())
                .with_code(EmitDiagKind::UnknownTypeValue.code()),
        ]
        .into_boxed_slice(),
    );

    let err = session.compile_entry(&ModuleKey::new("main")).unwrap_err();
    let SessionError::Emit { diags, .. } = err else {
        panic!("emit error expected");
    };

    assert_eq!(diags.len(), 1);
    assert_eq!(
        emit_diag_kind(&diags[0]),
        Some(EmitDiagKind::UnknownTypeValue)
    );
}

#[test]
fn compiles_local_recursive_callable_let() {
    let mut session = session();
    session
        .set_module_text(
            &ModuleKey::new("main"),
            r"
            export let answer (n : Int) : Int := (
              let rec loop (x : Int) : Int := case x of (| 0 => 0 | _ => loop(x - 1));
              loop(n)
            );
        ",
        )
        .unwrap();

    let output = session.compile_entry(&ModuleKey::new("main")).unwrap();

    assert!(output.artifact.validate().is_ok());
    assert!(output.text.contains("loop"));
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
fn compiles_record_field_assignment() {
    let mut session = session();
    session
        .set_module_text(
            &ModuleKey::new("main"),
            r"
            export let answer () : Int := (
              let r := mut { x := 1, y := 2 };
              r.x := 3;
              r.x
            );
        ",
        )
        .unwrap();

    let output = session.compile_entry(&ModuleKey::new("main")).unwrap();

    assert!(output.artifact.validate().is_ok());
    assert!(output.text.contains("data.set"), "{}", output.text);
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
fn compiles_capturing_recursion_record_patterns_and_type_values() {
    let mut session = session();
    session
        .set_module_text(
            &ModuleKey::new("main"),
            r"
            export let answer (n : Int) : Int := (
              let base := 1;
              let rec loop (x : Int) : Int := case x of (| 0 => base | _ => loop(x - 1));
              let point := { x := 1, y := 2 };
              let picked : Int := case point of (| { x } => x | _ => 0);
              picked + loop(n)
            );
        ",
        )
        .unwrap();

    let output = session.compile_entry(&ModuleKey::new("main")).unwrap();

    assert!(output.artifact.validate().is_ok());
    assert!(output.text.contains("data.get"));
    assert!(output.text.contains("call.cls"));
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
    assert!(output.text.contains(
        ".foreign $main::puts param $CString result $Int abi \"c\" symbol \"puts\" export"
    ));
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
    assert!(
        output.text.contains(
            ".foreign $main::sin param $Float result $Float abi \"c\" symbol \"sin\" link \"m\""
        ),
        "{}",
        output.text
    );
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
    assert!(output.text.contains("clock_gettime"), "{}", output.text);
    assert!(!output.text.contains("QueryPerformanceCounter"));
}

#[test]
fn emits_meta_records_for_laws_and_attrs() {
    let mut session = session();
    session
        .set_module_text(
            &ModuleKey::new("main"),
            r#"
            foreign let musi_true () : Bool;

            @foo.bar(baz := "qux")
            export let answer : Int := 42;

            @musi.codegen(mode := "test")
            export let meaning : Int := 1;

            export let Eq[T] := class {
              let (=) (a : T, b : T) : Bool;
              law reflexive (x : T) := musi_true();
            };

            export let Console := effect {
              let readln () : String;
              law total () := musi_true();
            };
        "#,
        )
        .unwrap();

    let output = session.compile_module(&ModuleKey::new("main")).unwrap();
    assert!(output.artifact.validate().is_ok());

    let meta = meta_records(&output.artifact);

    assert!(
        meta.iter().any(|(target, key, values)| {
            target == "main::Eq" && key == "class.laws" && values == &vec!["reflexive".to_owned()]
        }),
        "{meta:?}"
    );
    assert!(
        meta.iter().any(|(target, key, values)| {
            target == "main::Console" && key == "effect.laws" && values == &vec!["total".to_owned()]
        }),
        "{meta:?}"
    );
    assert!(
        meta.iter().any(|(target, key, values)| {
            target == "main::answer"
                && key == "inert.attr"
                && values == &vec!["@foo.bar(baz := \"qux\")".to_owned()]
        }),
        "{meta:?}"
    );
    assert!(
        meta.iter().any(|(target, key, values)| {
            target == "main::meaning"
                && key == "musi.attr"
                && values == &vec!["@musi.codegen(mode := \"test\")".to_owned()]
        }),
        "{meta:?}"
    );
}

#[test]
fn emits_meta_records_for_exported_signatures() {
    let mut session = session();
    session
        .set_module_text(
            &ModuleKey::new("main"),
            r"
            let Option[T] := data { | Some : Int | None };

            let Eq[T] := class { };
            let eqInt := instance Eq[Int] { };

            let Console := effect { let readln () : String; };

            export let f[T] (x : T) where T : Eq with { Console } : T := x;
            export let sumId (x : Int + String) : Int + String := x;
            export let tupId (x : (Int, String)) : (Int, String) := x;
            export let arrId (x : Array[Int, 2]) : Array[Int, 2] := x;
            export let mutArrId (x : mut Array[Int, 2]) : mut Array[Int, 2] := x;
            export let noneInt () : Option[Int] := .None;
        ",
        )
        .unwrap();

    let output = session.compile_module(&ModuleKey::new("main")).unwrap();
    assert!(output.artifact.validate().is_ok());

    let meta = meta_records(&output.artifact);

    assert!(
        meta_has_exact(&meta, "main::f", "value.type_params", &["T"]),
        "{meta:?}"
    );
    assert!(
        meta_has_exact(&meta, "main::f", "value.constraints", &["T : Eq"]),
        "{meta:?}"
    );
    assert!(
        meta_has_exact(&meta, "main::f", "value.effects", &["with { Console }"]),
        "{meta:?}"
    );
    assert!(
        meta.iter().any(|(target, key, values)| {
            target == "main::sumId"
                && key == "value.ty"
                && values
                    .first()
                    .is_some_and(|value| value.contains("Int + String"))
        }),
        "{meta:?}"
    );
    assert!(
        meta.iter().any(|(target, key, values)| {
            target == "main::tupId"
                && key == "value.ty"
                && values
                    .first()
                    .is_some_and(|value| value.contains("(Int, String)"))
        }),
        "{meta:?}"
    );
    assert!(
        meta.iter().any(|(target, key, values)| {
            target == "main::arrId"
                && key == "value.ty"
                && values
                    .first()
                    .is_some_and(|value| value.contains("Array[Int, 2]"))
        }),
        "{meta:?}"
    );
    assert!(
        meta.iter().any(|(target, key, values)| {
            target == "main::mutArrId"
                && key == "value.ty"
                && values
                    .first()
                    .is_some_and(|value| value.contains("mut Array[Int, 2]"))
        }),
        "{meta:?}"
    );
    assert!(
        meta.iter().any(|(target, key, values)| {
            target == "main::noneInt"
                && key == "value.ty"
                && values
                    .first()
                    .is_some_and(|value| value.contains("Option[Int]"))
        }),
        "{meta:?}"
    );
}
