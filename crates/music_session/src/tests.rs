use music_base::diag::Diag;
use music_emit::{EmitDiagKind, emit_diag_kind};
use music_ir::{IrDiagKind, ir_diag_kind};
use music_module::{ImportMap, ModuleKey};
use music_resolve::{ResolveDiagKind, resolve_diag_kind};
use music_seam::Artifact;
use music_seam::descriptor::ConstantValue;
use music_sema::{SemaDiagKind, TargetInfo, sema_diag_kind};
use music_syntax::{ParseErrorKind, TokenKind};

use crate::{CompiledOutput, Session, SessionError, SessionOptions, SessionSyntaxErrors};

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
    Session::new(SessionOptions::new().with_import_map(import_map))
}

fn session_with_target(target: TargetInfo) -> Session {
    let mut import_map = ImportMap::default();
    let _ = import_map.imports.insert("dep".into(), "dep".into());
    Session::new(
        SessionOptions::new()
            .with_import_map(import_map)
            .with_target(target),
    )
}

fn main_key() -> ModuleKey {
    ModuleKey::new("main")
}

fn set_main_text(session: &mut Session, text: &str) {
    session.set_module_text(&main_key(), text).unwrap();
}

fn compile_main_module(session: &mut Session) -> CompiledOutput {
    session.compile_module(&main_key()).unwrap()
}

fn compile_main_entry(session: &mut Session) -> CompiledOutput {
    session.compile_entry(&main_key()).unwrap()
}

fn compile_main_module_with_source(source: &str) -> CompiledOutput {
    let mut session = session();
    set_main_text(&mut session, source);
    compile_main_module(&mut session)
}

fn assert_output_contains(output: &CompiledOutput, needles: &[&str]) {
    for needle in needles {
        assert!(
            output.text.contains(needle),
            "missing `{needle}` in:\n{}",
            output.text
        );
    }
}

fn assert_main_module_compiles_with(source: &str, needles: &[&str]) -> CompiledOutput {
    let output = compile_main_module_with_source(source);
    assert!(output.artifact.validate().is_ok());
    assert_output_contains(&output, needles);
    output
}

fn compile_main_entry_with_dep(dep_source: &str, main_source: &str) -> CompiledOutput {
    let mut session = session();
    session
        .set_module_text(&ModuleKey::new("dep"), dep_source)
        .unwrap();
    set_main_text(&mut session, main_source);
    let output = compile_main_entry(&mut session);
    assert!(output.artifact.validate().is_ok());
    output
}

fn parse_failure_syntax(err: SessionError) -> SessionSyntaxErrors {
    let SessionError::ModuleParseFailed { syntax, .. } = err else {
        panic!("parse error expected");
    };
    syntax
}

fn assert_parse_failure_via_compile<F>(run: F)
where
    F: FnOnce(&mut Session, &ModuleKey) -> Result<CompiledOutput, SessionError>,
{
    let mut session = session();
    set_main_text(&mut session, "let x := 1");
    let syntax = parse_failure_syntax(run(&mut session, &main_key()).unwrap_err());
    assert!(syntax.lex_errors().is_empty());
    assert_eq!(syntax.parse_errors().len(), 1);
    assert_eq!(syntax.diags().len(), 1);
}

macro_rules! assert_main_entry_compiles_with {
    ($source:expr, $needles:expr $(,)?) => {{
        let mut session = session();
        set_main_text(&mut session, $source);
        let output = compile_main_entry(&mut session);
        assert!(output.artifact.validate().is_ok());
        assert_output_contains(&output, $needles);
        output
    }};
}

macro_rules! assert_emit_failure_with_unknown_type_value {
    ($run:expr $(,)?) => {{
        let mut session = session();
        set_main_text(&mut session, "export let answer : Int := 42;");
        session.inject_emit_failure_for_tests(
            vec![
                Diag::error(EmitDiagKind::UnknownTypeValue.message())
                    .with_code(EmitDiagKind::UnknownTypeValue.code()),
            ]
            .into_boxed_slice(),
        );

        let err = $run(&mut session, &main_key()).unwrap_err();
        let SessionError::ModuleEmissionFailed { diags, .. } = err else {
            panic!("emit error expected");
        };

        assert_eq!(diags.len(), 1);
        assert_eq!(
            emit_diag_kind(&diags[0]),
            Some(EmitDiagKind::UnknownTypeValue)
        );
    }};
}

#[test]
fn compiles_module_to_artifact_bytes_and_text() {
    let output = assert_main_module_compiles_with(
        "export let answer : Int := 42;",
        &[".global $main::answer export"],
    );
    assert!(!output.bytes.is_empty());
}

#[test]
fn compiles_piped_calls_as_normal_calls() {
    let output = assert_main_module_compiles_with(
        "export let add (left : Int, right : Int) : Int := left + right; export let answer : Int := 1 |> add(2);",
        &[".global $main::answer export"],
    );
    assert!(output.artifact.validate().is_ok());
}

#[test]
fn parse_failures_expose_typed_syntax_errors_and_diags() {
    let mut session = session();
    set_main_text(&mut session, "let x := 1");

    let syntax = parse_failure_syntax(session.parse_module(&main_key()).unwrap_err());

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
    assert_parse_failure_via_compile(Session::compile_module);
}

#[test]
fn compile_entry_propagates_parse_failures() {
    assert_parse_failure_via_compile(Session::compile_entry);
}

#[test]
fn compiles_reachable_entry_graph() {
    let output = compile_main_entry_with_dep(
        "export let base : Int := 41;",
        "import \"dep\"; export let answer : Int := 42;",
    );
    assert_output_contains(
        &output,
        &[".global $dep::base export", ".global $main::answer export"],
    );
}

#[test]
fn reuses_caches_and_invalidates_dependents_on_edit() {
    let mut session = session();
    session
        .set_module_text(&ModuleKey::new("dep"), "export let base : Int := 41;")
        .unwrap();
    set_main_text(
        &mut session,
        "import \"dep\"; export let answer : Int := 42;",
    );

    let _ = compile_main_entry(&mut session);
    let first_stats = session.stats().clone();
    let _ = compile_main_entry(&mut session);
    assert_eq!(session.stats(), &first_stats);

    session
        .set_module_text(&ModuleKey::new("dep"), "export let base : Int := 99;")
        .unwrap();
    let _ = compile_main_entry(&mut session);
    assert!(session.stats().resolve_runs > first_stats.resolve_runs);
    assert!(session.stats().emit_runs > first_stats.emit_runs);
}

#[test]
fn resolve_reuses_cached_parse_product() {
    let mut session = session();
    set_main_text(&mut session, "export let answer : Int := 42;");

    let _ = session.parse_module(&main_key()).unwrap();
    let after_parse = session.stats().clone();
    let _ = session.resolve_module(&main_key()).unwrap();

    assert_eq!(session.stats().parse_runs, after_parse.parse_runs);
    assert!(session.stats().resolve_runs > after_parse.resolve_runs);
}

#[test]
fn compiles_imported_generic_callable_calls() {
    let output = compile_main_entry_with_dep(
        "export let id[T] (x : T) : T := x;",
        r#"
            let dep := import "dep";
            export let answer () : Int := dep.id[Int](42);
        "#,
    );
    assert_output_contains(&output, &["$dep::id", "$main::answer"]);
}

#[test]
fn compiles_first_class_generic_values_in_records() {
    let output = compile_main_entry_with_dep(
        "export let id[T] (x : T) : T := x;",
        r#"
            let dep := import "dep";
            let tools := { id := dep.id };
            export let answer () : Int := tools.id[Int](42);
        "#,
    );
    assert_output_contains(&output, &["ty.apply", "$main::answer"]);
}

#[test]
fn compiles_imported_globals_and_local_assignment() {
    let output = compile_main_entry_with_dep(
        "export let base : Int := 41;",
        r#"
            let dep := import "dep";
            export let answer () : Int := (
              let local := mut dep.base;
              local := local + 1;
              local
            );
        "#,
    );
    assert_output_contains(&output, &["ld.glob $dep::base", "$main::answer"]);
}

#[test]
fn compiles_dynamic_import_multi_index_and_quote() {
    let output = assert_main_entry_compiles_with!(
        r"
            export let touch (name : String, grid : mut [2][2]Int) : Int := (
              let loaded := import name;
              grid.[0, 1] := 7;
              grid.[0, 1]
            );
            export let quoted : Syntax := quote (#(1 + 2));
        ",
        &[
            "mod.load",
            "seq.getn",
            "seq.setn",
            "syntax expr \"#(1 + 2)\""
        ],
    );
    assert!(output.artifact.constants.iter().any(|(_, constant)| {
        matches!(
            constant.value,
            ConstantValue::Syntax { text, .. }
                if output.artifact.string_text(text).contains("#(1 + 2)")
        )
    }));
}

#[test]
fn compiles_closures_and_higher_order_calls() {
    let _ = assert_main_entry_compiles_with!(
        r"
            let apply (f : Int -> Int, x : Int) : Int := f(x);
            export let answer (x : Int) : Int := (
              let base : Int := 41;
              let add_base (y : Int) : Int := y + base;
              apply(add_base, x)
            );
        ",
        &["call.cls", "cls.new"],
    );
}

#[test]
fn compiles_named_call_arguments_and_named_requests() {
    let _ = assert_main_module_compiles_with(
        r#"
        export let Console := effect {
          let readLine (prompt : String) : String;
        };

        let render (port : Int, secure : Bool) : Int := port;
        export let read () : String using { Console } := request Console.readLine(prompt := ">");
        export let main () : Int := render(secure := 0 = 0, port := 8080);
        "#,
        &["call $main::render", "eff.invk $main::Console $readLine"],
    );
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
    let SessionError::ModuleResolveFailed { diags, .. } = err else {
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
    let SessionError::ModuleSemanticCheckFailed { diags, .. } = err else {
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
    set_main_text(&mut session, "export let answer : Int := 42;");
    session.inject_ir_failure_for_tests(
        vec![
            Diag::error(IrDiagKind::LoweringRequiresSemaCleanModule.message())
                .with_code(IrDiagKind::LoweringRequiresSemaCleanModule.code()),
        ]
        .into_boxed_slice(),
    );

    let err = session.lower_module(&main_key()).unwrap_err();
    let SessionError::ModuleLoweringFailed { diags, .. } = err else {
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
    assert_emit_failure_with_unknown_type_value!(
        |session: &mut Session, key: &ModuleKey| session.compile_module(key)
    );
}

#[test]
fn compile_entry_propagates_emit_failure_with_typed_kind() {
    assert_emit_failure_with_unknown_type_value!(
        |session: &mut Session, key: &ModuleKey| session.compile_entry(key)
    );
}

#[test]
fn compiles_local_recursive_callable_let() {
    let _ = assert_main_entry_compiles_with!(
        r"
            export let answer (n : Int) : Int := (
              let rec loop (x : Int) : Int := match x (| 0 => 0 | _ => loop(x - 1));
              loop(n)
            );
        ",
        &["loop"],
    );
}

#[test]
fn compiles_case_tuple_and_array_patterns() {
    let _ = assert_main_entry_compiles_with!(
        r"
            export let answer () : Int := (
              let pair := (1, 2);
              let items := [3, 4];
              let p : Int := match pair (| (1, b) => b | _ => 0);
              let q : Int := match items (| [3, b] => b | _ => 0);
              p + q
            );
        ",
        &["seq.get", "br.false"],
    );
}

#[test]
fn compiles_records_with_projection_and_update() {
    let _ = assert_main_entry_compiles_with!(
        r"
            export let answer () : Int := (
              let r := { y := 2, x := 1 };
              let a : Int := r.x;
              let s := { ...r, x := 3 };
              a + s.x
            );
        ",
        &["data.get", "data.new", ".type $\"{ x: Int; y: Int }\""],
    );
}

#[test]
fn compiles_record_field_assignment() {
    let output = assert_main_entry_compiles_with!(
        r"
            export let answer () : Int := (
              let r := mut { x := 1, y := 2 };
              r.x := 3;
              r.x
            );
        ",
        &["data.set"],
    );
    assert!(output.text.contains("data.set"), "{}", output.text);
}

#[test]
fn compiles_record_destructuring_let_patterns() {
    let _ = assert_main_entry_compiles_with!(
        r"
            export let answer () : Int := (
              let r := { y := 2, x := 1 };
              let {x, y} := r;
              x + y
            );
        ",
        &["data.get"],
    );
}

#[test]
fn compiles_tuple_and_array_destructuring_let_patterns() {
    let _ = assert_main_entry_compiles_with!(
        r"
            export let answer () : Int := (
              let pair := (1, 2);
              let items := [3, 4];
              let (a, b) := pair;
              let [c, d] := items;
              a + b + c + d
            );
        ",
        &["seq.get"],
    );
}

#[test]
fn compiles_capturing_recursion_record_patterns_and_type_values() {
    let _ = assert_main_entry_compiles_with!(
        r"
            export let answer (n : Int) : Int := (
              let base := 1;
              let rec loop (x : Int) : Int := match x (| 0 => base | _ => loop(x - 1));
              let point := { x := 1, y := 2 };
              let picked : Int := match point (| { x } => x | _ => 0);
              picked + loop(n)
            );
        ",
        &["data.get", "call.cls"],
    );
}

#[test]
fn compiles_variants_with_case_patterns() {
    let _ = assert_main_entry_compiles_with!(
        r"
            let Maybe := data { | Some(Int) | None };
            export let answer () : Int := (
              let x : Maybe := .Some(1);
              match x (
              | .Some(y) => y
              | .None => 0
              )
            );
        ",
        &[
            "data.tag",
            "br.tbl",
            "data.get",
            "data.new",
            ".type $main::Maybe"
        ],
    );
}

#[test]
fn compiles_variants_without_type_context_when_tag_unique() {
    let _ = assert_main_entry_compiles_with!(
        r"
            let Maybe := data { | Some(Int) | None };
            export let answer () : Int := (
              let x := .Some(1);
              match x (
              | .Some(y) => y
              | .None => 0
              )
            );
        ",
        &["data.tag", "br.tbl", "data.get", "data.new"],
    );
}

#[test]
fn compiles_effects_with_perform_handle_resume() {
    let _ = assert_main_entry_compiles_with!(
        r#"
            let Console := effect { let readLine () : String; };
            export let answer () : String :=
              handle request Console.readLine() using Console {
                value => value;
                readLine(k) => resume "ok";
              };
        "#,
        &["hdl.push", "hdl.pop", "eff.invk", "eff.resume"],
    );
}

#[test]
fn compiles_exported_foreign_declarations_into_artifact() {
    let _ = assert_main_module_compiles_with(
        r#"
            export foreign "c" (
              let puts (msg : CString) : Int;
            );
            export let answer : Int := 1;
        "#,
        &[".foreign $main::puts param $CString result $Int abi \"c\" symbol \"puts\" export"],
    );
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
    let mut session = session_with_target(TargetInfo::new().with_os("linux").with_arch("x86_64"));
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
              law reflexive (x : T) := unsafe { musi_true(); };
            };

            export let Console := effect {
              let readLine () : String;
              law total () := unsafe { musi_true(); };
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
        !meta.iter().any(|(target, key, values)| {
            target == "main::meaning"
                && key == "musi.attr"
                && values == &vec!["@musi.codegen(mode := \"test\")".to_owned()]
        }),
        "{meta:?}"
    );
}

#[test]
fn synthesizes_law_suite_modules_for_law_bearing_exports() {
    let mut session = session();
    session
        .set_module_text(
            &ModuleKey::new("main"),
            r"
            foreign let musi_true () : Bool;

            export let Eq[T] := class {
              let (=) (a : T, b : T) : Bool;
              law reflexive (x : T) := unsafe { musi_true(); };
            };

            export let Console := effect {
              let readLine () : String;
              law total () := unsafe { musi_true(); };
            };
        ",
        )
        .unwrap();

    let suites = session.law_suite_modules().unwrap();
    assert_eq!(suites.len(), 1);

    let suite = &suites[0];
    assert_eq!(suite.source_module_key, ModuleKey::new("main"));
    assert_eq!(suite.suite_module_key, ModuleKey::new("main::__laws"));
    assert_eq!(suite.export_name.as_ref(), "__laws_test");
    assert_eq!(suite.law_count, 1);
    let suite_source = session
        .module_text(&suite.suite_module_key)
        .expect("suite source should be materialized in session");
    assert!(
        suite_source.contains("foreign let musi_true () : Bool;"),
        "{suite_source}"
    );
    assert!(
        suite_source.contains("suiteStart(\"main laws\")"),
        "{suite_source}"
    );
    assert!(
        suite_source.contains("__musi_law_test.testCase(\"Console.total\""),
        "{suite_source}"
    );
    assert!(
        suite_source.contains("unsafe { musi_true(); }"),
        "{suite_source}"
    );
    assert!(!suite_source.contains(".True)"), "{suite_source}");
}

#[test]
fn synthesizes_class_laws_for_reachable_monomorphic_instances() {
    let mut session = session();
    session
        .set_module_text(
            &ModuleKey::new("main"),
            r"
            foreign let musi_true () : Bool;

            export let IntEq := class {
              let eq (a : Int, b : Int) : Bool;
              law reflexive (x : Int) := eq(x, x);
            };

            let eqInt := instance IntEq {
              let eq (a : Int, b : Int) : Bool := unsafe { musi_true(); };
            };
        ",
        )
        .unwrap();

    let suites = session.law_suite_modules().unwrap();
    assert_eq!(suites.len(), 1);

    let suite = &suites[0];
    assert_eq!(suite.export_name.as_ref(), "__laws_test");
    assert_eq!(suite.law_count, 5);

    let suite_source = session
        .module_text(&suite.suite_module_key)
        .expect("suite source should be materialized in session");
    assert!(
        suite_source.contains("__musi_law_test.testCase(\"IntEq.reflexive[-2]\""),
        "{suite_source}"
    );
    assert!(
        suite_source.contains("let eq (a : Int, b : Int) : Bool := unsafe { musi_true(); };"),
        "{suite_source}"
    );
    assert!(suite_source.contains("eq(x, x)"), "{suite_source}");
}

#[test]
fn rejects_polymorphic_instances_for_class_law_suites() {
    let mut session = session();
    session
        .set_module_text(
            &ModuleKey::new("main"),
            r"
            foreign let musi_true () : Bool;

            export let Eq[T] := class {
              let eq (a : T, b : T) : Bool;
              law reflexive (x : T) := eq(x, x);
            };

            instance[T] Eq[T] {
              let eq (a : T, b : T) : Bool := unsafe { musi_true(); };
            };
        ",
        )
        .unwrap();

    let err = session.law_suite_modules().unwrap_err();
    let SessionError::LawSuiteSynthesisFailed { reason, .. } = err else {
        panic!("expected law suite synthesis failure");
    };
    assert!(reason.contains("remains polymorphic"), "{reason}");
}

#[test]
fn synthesizes_law_suites_for_concrete_constrained_instances() {
    let mut session = session();
    session
        .set_module_text(
            &ModuleKey::new("main"),
            r"
            export let Mark[T] := class { };

            let markInt := instance Mark[Int] { };

            let requireMark (x : Int) : Int where Int : Mark := x;

            export let Foo := class {
              let useMark (x : Int) : Int;
              law reflexive (x : Int) := useMark(x) = x;
            };

            let fooInt := instance Foo where Int : Mark {
              let useMark (x : Int) : Int := (
                let helper (y : Int) : Int := requireMark(y);
                helper(x)
              );
            };
        ",
        )
        .unwrap();

    let suites = session.law_suite_modules().unwrap();
    assert_eq!(suites.len(), 1);
    assert_eq!(suites[0].law_count, 5);
}

#[test]
fn emits_meta_records_for_exported_signatures() {
    let mut session = session();
    session
        .set_module_text(
            &ModuleKey::new("main"),
            r"
            let Option[T] := data { | Some(Int) | None };

            let Eq[T] := class { };
            let eqInt := instance Eq[Int] { };

            let Console := effect { let readLine () : String; };

            export let f (x : Int) : Int where Int : Eq using { Console } := x;
            export let sumId (x : Int + String) : Int + String := x;
            export let tupId (x : (Int, String)) : (Int, String) := x;
            export let arrId (x : [2]Int) : [2]Int := x;
            export let mutArrId (x : mut [2]Int) : mut [2]Int := x;
            export let noneInt () : Option[Int] := .None;
        ",
        )
        .unwrap();

    let output = session.compile_module(&ModuleKey::new("main")).unwrap();
    assert!(output.artifact.validate().is_ok());

    let meta = meta_records(&output.artifact);

    assert!(
        meta_has_exact(&meta, "main::f", "value.constraints", &["Int : Eq"]),
        "{meta:?}"
    );
    assert!(
        meta_has_exact(&meta, "main::f", "value.effects", &["using { Console }"]),
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
                && values.first().is_some_and(|value| value.contains("[2]Int"))
        }),
        "{meta:?}"
    );
    assert!(
        meta.iter().any(|(target, key, values)| {
            target == "main::mutArrId"
                && key == "value.ty"
                && values
                    .first()
                    .is_some_and(|value| value.contains("mut [2]Int"))
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
