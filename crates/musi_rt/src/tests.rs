use musi_native::{NativeHost, NativeTestCaseResult, NativeTestReport};
use musi_vm::{EffectCall, ForeignCall, Value, VmError, VmErrorKind, VmHost, VmResult};
use music_module::ImportMap;
use music_session::SessionOptions;
use music_term::{SyntaxShape, SyntaxTerm, SyntaxTermError};

use crate::{Runtime, RuntimeErrorKind, RuntimeOptions};

#[derive(Default)]
struct TestHost;

impl VmHost for TestHost {
    fn call_foreign(&mut self, foreign: &ForeignCall, _args: &[Value]) -> VmResult<Value> {
        Err(VmError::new(VmErrorKind::ForeignCallRejected {
            foreign: foreign.name().into(),
        }))
    }

    fn handle_effect(&mut self, effect: &EffectCall, _args: &[Value]) -> VmResult<Value> {
        Err(VmError::new(VmErrorKind::EffectRejected {
            effect: effect.effect_name().into(),
            op: Some(effect.op_name().into()),
            reason: "test host rejected effect call".into(),
        }))
    }
}

fn expr_syntax(text: &str) -> Value {
    Value::syntax(SyntaxTerm::parse(SyntaxShape::Expr, text).unwrap())
}

fn module_syntax(text: &str) -> Value {
    Value::syntax(SyntaxTerm::parse(SyntaxShape::Module, text).unwrap())
}

fn register_runtime_module(runtime: &mut Runtime, spec: &str, text: &str) {
    runtime.register_module_text(spec, text).unwrap();
}

fn run_main_answer(main_source: &str) -> Value {
    let mut runtime = Runtime::new(NativeHost::new(), RuntimeOptions::default());
    runtime.register_module_text("main", main_source).unwrap();
    runtime.load_root("main").unwrap();
    runtime.call_export("answer", &[]).unwrap()
}

#[test]
fn loads_root_and_calls_export() {
    let mut runtime = Runtime::new(NativeHost::new(), RuntimeOptions::default());
    runtime
        .register_module_text("main", "export let answer () : Int := 42;")
        .unwrap();
    runtime.load_root("main").unwrap();

    let value = runtime.call_export("answer", &[]).unwrap();
    assert_eq!(value, Value::Int(42));
}

#[test]
fn loads_dynamic_module_from_registered_text() {
    let mut runtime = Runtime::new(NativeHost::new(), RuntimeOptions::default());
    runtime
        .register_module_text("main", "export let root () : Int := 0;")
        .unwrap();
    runtime
        .register_module_text(
            "dep",
            "export let answer () : Int := 42; export let base : Int := 41;",
        )
        .unwrap();
    runtime.load_root("main").unwrap();

    let module = runtime.load_module("dep").unwrap();
    let value = runtime.call_module_export(&module, "answer", &[]).unwrap();

    assert_eq!(value, Value::Int(42));
}

#[test]
fn rejects_opaque_exports_through_runtime_api() {
    let mut runtime = Runtime::new(NativeHost::new(), RuntimeOptions::default());
    runtime
        .register_module_text(
            "main",
            "export opaque let secret : Int := 7; export let root () : Int := 0;",
        )
        .unwrap();
    runtime
        .register_module_text(
            "dep",
            "export opaque let hidden : Int := 41; export let answer () : Int := 42;",
        )
        .unwrap();
    runtime.load_root("main").unwrap();

    let err = runtime.lookup_export("secret").unwrap_err();
    assert!(matches!(
        err.kind(),
        RuntimeErrorKind::VmExecutionFailed(VmError { .. })
    ));

    let module = runtime.load_module("dep").unwrap();
    let err = runtime
        .call_module_export(&module, "hidden", &[])
        .unwrap_err();
    assert!(matches!(
        err.kind(),
        RuntimeErrorKind::VmExecutionFailed(VmError { .. })
    ));
}

#[test]
fn evaluates_expression_syntax_through_runtime_service() {
    let mut runtime = Runtime::new(NativeHost::new(), RuntimeOptions::default());
    runtime
        .register_module_text("main", "export let root () : Int := 0;")
        .unwrap();
    runtime.load_root("main").unwrap();

    let value = runtime.eval_expr_syntax(&expr_syntax("42"), "Int").unwrap();
    assert_eq!(value, Value::Int(42));
}

#[test]
fn loads_module_syntax_through_runtime_service() {
    let mut runtime = Runtime::new(NativeHost::new(), RuntimeOptions::default());
    runtime
        .register_module_text("main", "export let root () : Int := 0;")
        .unwrap();
    runtime.load_root("main").unwrap();

    let module = runtime
        .load_module_syntax(
            "generated",
            &module_syntax("export let answer () : Int := 42;"),
        )
        .unwrap();
    let value = runtime.call_module_export(&module, "answer", &[]).unwrap();

    assert_eq!(value, Value::Int(42));
}

#[test]
fn evaluates_expression_through_musi_syntax_root() {
    let value = run_main_answer(
        r#"
            let Syntax := import "musi:syntax";
            export let answer () : Int := Syntax.eval(quote (40 + 2), Int) :?> Int;
            "#,
    );
    assert_eq!(value, Value::Int(42));
}

#[test]
fn registers_module_syntax_through_musi_syntax_root() {
    let value = run_main_answer(
        r#"
            let Syntax := import "musi:syntax";
            export let answer () : Int := (
              let name : String := "generated";
              Syntax.register_module(name, quote {
                export let answer : Int := 42;
              });
              let loaded := import name;
              loaded.answer :?> Int
            );
            "#,
    );
    assert_eq!(value, Value::Int(42));
}

#[test]
fn routes_foreign_calls_through_registered_handlers() {
    let mut host = NativeHost::new();
    host.register_foreign_handler("main::puts", |_foreign, args| {
        assert_eq!(args, &[Value::Int(42)]);
        Ok(Value::Int(7))
    });
    let mut runtime = Runtime::new(host, RuntimeOptions::default());
    runtime
        .register_module_text(
            "main",
            r#"
            foreign "c" (
              let puts (value : Int) : Int;
            );
            export let answer () : Int := puts(42);
        "#,
        )
        .unwrap();
    runtime.load_root("main").unwrap();

    let value = runtime.call_export("answer", &[]).unwrap();
    assert_eq!(value, Value::Int(7));
}

#[test]
fn routes_effect_calls_through_registered_handlers() {
    let mut host = NativeHost::new();
    host.register_effect_handler("main::Console", "readln", |_effect, args| {
        assert_eq!(args, &[Value::string(">")]);
        Ok(Value::Int(42))
    });
    let mut runtime = Runtime::new(host, RuntimeOptions::default());
    runtime
        .register_module_text(
            "main",
            r#"
            let Console := effect { let readln (prompt : String) : Int; };
            export let answer () : Int := perform Console.readln(">");
        "#,
        )
        .unwrap();
    runtime.load_root("main").unwrap();

    let value = runtime.call_export("answer", &[]).unwrap();
    assert_eq!(value, Value::Int(42));
}

#[test]
fn rejects_invalid_syntax_value() {
    let mut runtime = Runtime::new(NativeHost::new(), RuntimeOptions::default());
    runtime
        .register_module_text("main", "export let root () : Int := 0;")
        .unwrap();
    runtime.load_root("main").unwrap();

    let err = runtime.eval_expr_syntax(&Value::Int(1), "Int").unwrap_err();
    assert!(matches!(
        err.kind(),
        RuntimeErrorKind::InvalidSyntaxValue { .. }
    ));
}

#[test]
fn reports_parse_failure_for_expression_syntax() {
    let err = SyntaxTerm::parse(SyntaxShape::Expr, "(").unwrap_err();
    assert_eq!(err, SyntaxTermError::FragmentParseFailed);
}

#[test]
fn reports_parse_failure_for_module_syntax() {
    let err = SyntaxTerm::parse(SyntaxShape::Module, "export let := ;").unwrap_err();
    assert_eq!(err, SyntaxTermError::FragmentParseFailed);
}

#[test]
fn reports_missing_root_source() {
    let mut runtime = Runtime::new(NativeHost::new(), RuntimeOptions::default());
    let err = runtime.load_root("missing").unwrap_err();
    assert!(matches!(
        err.kind(),
        RuntimeErrorKind::ModuleSourceMissing { .. }
    ));
}

#[test]
fn custom_host_still_handles_unregistered_edges() {
    let mut runtime = Runtime::new(
        NativeHost::with_fallback(TestHost),
        RuntimeOptions::default(),
    );
    runtime
        .register_module_text(
            "main",
            r#"
            foreign "c" (
              let puts (value : Int) : Int;
            );
            export let answer () : Int := puts(1);
        "#,
        )
        .unwrap();
    runtime.load_root("main").unwrap();

    let err = runtime.call_export("answer", &[]).unwrap_err();
    assert!(matches!(
        err.kind(),
        RuntimeErrorKind::VmExecutionFailed(VmError { .. })
    ));
}

#[test]
fn runs_registered_test_module_and_collects_case_results() {
    let mut import_map = ImportMap::default();
    let _ = import_map.imports.insert("@std/".into(), "@std/".into());
    let mut runtime = Runtime::new(
        NativeHost::new(),
        RuntimeOptions::default().with_session(SessionOptions::new().with_import_map(import_map)),
    );
    runtime
        .register_module_text(
            "suite",
            r#"
let Intrinsics := import "musi:test";
let Test := Intrinsics.Test;

export let test () :=
    (
      perform Test.suiteStart("demo");
      perform Test.testCase("first", 1 = 1);
      perform Test.testCase("second", 1 = 2);
      perform Test.suiteEnd()
    );
"#,
        )
        .unwrap();

    let report = runtime.run_test_module("suite").unwrap();

    assert_eq!(
        report,
        NativeTestReport::new(
            "suite",
            vec![
                NativeTestCaseResult::new("demo".into(), "first".into(), true),
                NativeTestCaseResult::new("demo".into(), "second".into(), false),
            ]
            .into_boxed_slice(),
        )
    );
}

#[test]
fn runs_root_hub_std_test_module() {
    let mut import_map = ImportMap::default();
    let _ = import_map.imports.insert("@std/".into(), "@std/".into());
    let mut runtime = Runtime::new(
        NativeHost::new(),
        RuntimeOptions::default().with_session(SessionOptions::new().with_import_map(import_map)),
    );
    register_runtime_module(
        &mut runtime,
        "@std",
        r#"
export let Bytes := import "@std/bytes";
export let Math := import "@std/math";
export let Option := import "@std/option";
export let Testing := import "@std/testing";
"#,
    );
    register_runtime_module(
        &mut runtime,
        "@std/bytes",
        r"
export let equals (left : Array[Int], right : Array[Int]) : Bool := left = right;
",
    );
    register_runtime_module(
        &mut runtime,
        "@std/math",
        r"
export let clamp (value : Int, low : Int, high : Int) : Int :=
    case () of (
        | _ if value < low => low
        | _ if value > high => high
        | _ => value
    );
",
    );
    register_runtime_module(
        &mut runtime,
        "@std/option",
        r"
export let Option[T] := data {
    | Some : T
    | None
};

export let none [T] () : Option[T] := .None;

export let unwrap_or [T] (value : Option[T], fallback : T) : T :=
    case value of (
        | .Some(item) => item
        | .None => fallback
    );
",
    );
    register_runtime_module(
        &mut runtime,
        "@std/testing",
        r#"
let Intrinsics := import "musi:test";
let Test := Intrinsics.Test;

export let to_be (actual : Int, expected : Int) := actual = expected;
export let to_be_truthy (actual : Bool) := actual;

export let describe (name : String) :=
    perform Test.suiteStart(name);
export let end_describe () :=
    perform Test.suiteEnd();
export let it (name : String, passed : Bool) :=
    perform Test.testCase(name, passed);
"#,
    );
    register_runtime_module(
        &mut runtime,
        "suite",
        r#"
let Testing := import "@std/testing";
let Std := import "@std";

export let test () :=
    (
      let Bytes := Std.Bytes;
      let Math := Std.Math;
      let Option := Std.Option;
      Testing.describe("std root");
      Testing.it("bytes chain", Testing.to_be_truthy(Bytes.equals([1, 2], [1, 2])));
      Testing.it("math chain", Testing.to_be(Math.clamp(9, 0, 4), 4));
      Testing.it("option chain", Testing.to_be(Option.unwrap_or[Int](Option.none[Int](), 5), 5));
      Testing.end_describe()
    );
"#,
    );

    let report = runtime.run_test_module("suite").unwrap();

    assert_eq!(report.cases.len(), 3);
    assert!(report.cases.iter().all(|case| case.passed));
}
