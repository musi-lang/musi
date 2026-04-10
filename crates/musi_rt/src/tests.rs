use musi_vm::{Value, VmError, VmErrorKind, VmHost, VmResult};
use music_module::ImportMap;

use crate::{Runtime, RuntimeErrorKind, RuntimeOptions};

#[derive(Default)]
struct TestHost;

impl VmHost for TestHost {
    fn call_foreign(&mut self, foreign: &musi_vm::ForeignCall, _args: &[Value]) -> VmResult<Value> {
        Err(VmError::new(VmErrorKind::ForeignCallRejected {
            foreign: foreign.name().into(),
        }))
    }

    fn handle_effect(&mut self, effect: &musi_vm::EffectCall, _args: &[Value]) -> VmResult<Value> {
        Err(VmError::new(VmErrorKind::EffectRejected {
            effect: effect.effect_name().into(),
            op: Some(effect.op_name().into()),
            reason: "test host rejected effect call".into(),
        }))
    }
}

#[test]
fn loads_root_and_calls_export() {
    let mut runtime = Runtime::new(RuntimeOptions::default());
    runtime
        .register_module_text("main", "export let answer () : Int := 42;")
        .unwrap();
    runtime.load_root("main").unwrap();

    let value = runtime.call_export("answer", &[]).unwrap();
    assert_eq!(value, Value::Int(42));
}

#[test]
fn loads_dynamic_module_from_registered_text() {
    let mut runtime = Runtime::new(RuntimeOptions::default());
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
fn evaluates_expression_syntax_through_runtime_service() {
    let mut runtime = Runtime::new(RuntimeOptions::default());
    runtime
        .register_module_text("main", "export let root () : Int := 0;")
        .unwrap();
    runtime.load_root("main").unwrap();

    let value = runtime
        .eval_expr_syntax(&Value::string("42"), "Int")
        .unwrap();
    assert_eq!(value, Value::Int(42));
}

#[test]
fn loads_module_syntax_through_runtime_service() {
    let mut runtime = Runtime::new(RuntimeOptions::default());
    runtime
        .register_module_text("main", "export let root () : Int := 0;")
        .unwrap();
    runtime.load_root("main").unwrap();

    let module = runtime
        .load_module_syntax(
            "generated",
            &Value::string("export let answer () : Int := 42;"),
        )
        .unwrap();
    let value = runtime.call_module_export(&module, "answer", &[]).unwrap();

    assert_eq!(value, Value::Int(42));
}

#[test]
fn routes_foreign_calls_through_registered_handlers() {
    let mut runtime = Runtime::new(RuntimeOptions::default());
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
    runtime.register_foreign_handler("main::puts", |_foreign, args| {
        assert_eq!(args, &[Value::Int(42)]);
        Ok(Value::Int(7))
    });
    runtime.load_root("main").unwrap();

    let value = runtime.call_export("answer", &[]).unwrap();
    assert_eq!(value, Value::Int(7));
}

#[test]
fn routes_effect_calls_through_registered_handlers() {
    let mut runtime = Runtime::new(RuntimeOptions::default());
    runtime
        .register_module_text(
            "main",
            r#"
            let Console := effect { let readln (prompt : String) : Int; };
            export let answer () : Int := perform Console.readln(">");
        "#,
        )
        .unwrap();
    runtime.register_effect_handler("main::Console", "readln", |_effect, args| {
        assert_eq!(args, &[Value::string(">")]);
        Ok(Value::Int(42))
    });
    runtime.load_root("main").unwrap();

    let value = runtime.call_export("answer", &[]).unwrap();
    assert_eq!(value, Value::Int(42));
}

#[test]
fn rejects_invalid_syntax_value() {
    let mut runtime = Runtime::new(RuntimeOptions::default());
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
    let mut runtime = Runtime::new(RuntimeOptions::default());
    runtime
        .register_module_text("main", "export let root () : Int := 0;")
        .unwrap();
    runtime.load_root("main").unwrap();

    let err = runtime
        .eval_expr_syntax(&Value::string("("), "Int")
        .unwrap_err();
    assert!(matches!(
        err.kind(),
        RuntimeErrorKind::SessionParseFailed { .. }
    ));
}

#[test]
fn reports_parse_failure_for_module_syntax() {
    let mut runtime = Runtime::new(RuntimeOptions::default());
    runtime
        .register_module_text("main", "export let root () : Int := 0;")
        .unwrap();
    runtime.load_root("main").unwrap();

    let err = runtime
        .load_module_syntax("generated", &Value::string("export let := ;"))
        .unwrap_err();
    assert!(matches!(
        err.kind(),
        RuntimeErrorKind::SessionParseFailed { .. }
    ));
}

#[test]
fn reports_missing_root_source() {
    let mut runtime = Runtime::new(RuntimeOptions::default());
    let err = runtime.load_root("missing").unwrap_err();
    assert!(matches!(
        err.kind(),
        RuntimeErrorKind::ModuleSourceMissing { .. }
    ));
}

#[test]
fn custom_host_still_handles_unregistered_edges() {
    let mut runtime = Runtime::with_host(TestHost, RuntimeOptions::default());
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
    assert!(matches!(err.kind(), RuntimeErrorKind::Vm(VmError { .. })));
}

#[test]
fn runs_registered_test_module_and_collects_case_results() {
    let mut import_map = ImportMap::default();
    let _ = import_map.imports.insert("@std/".into(), "@std/".into());
    let mut runtime = Runtime::new(RuntimeOptions {
        session: music_session::SessionOptions {
            import_map,
            ..music_session::SessionOptions::default()
        },
        ..RuntimeOptions::default()
    });
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

    assert_eq!(report.cases.len(), 2);
    assert_eq!(report.cases[0].suite.as_ref(), "demo");
    assert_eq!(report.cases[0].name.as_ref(), "first");
    assert!(report.cases[0].passed);
    assert_eq!(report.cases[1].name.as_ref(), "second");
    assert!(!report.cases[1].passed);
}

#[test]
fn runs_root_hub_std_test_module() {
    let mut import_map = ImportMap::default();
    let _ = import_map.imports.insert("@std/".into(), "@std/".into());
    let mut runtime = Runtime::new(RuntimeOptions {
        session: music_session::SessionOptions {
            import_map,
            ..music_session::SessionOptions::default()
        },
        ..RuntimeOptions::default()
    });
    runtime
        .register_module_text(
            "@std",
            r#"
export let Bytes := import "@std/bytes";
export let Math := import "@std/math";
export let Option := import "@std/option";
export let Testing := import "@std/testing";
"#,
        )
        .unwrap();
    runtime
        .register_module_text(
            "@std/bytes",
            r#"
export let equals (left : Array[Int], right : Array[Int]) : Bool := left = right;
"#,
        )
        .unwrap();
    runtime
        .register_module_text(
            "@std/math",
            r#"
export let clamp (value : Int, low : Int, high : Int) : Int :=
    case () of (
        | _ if value < low => low
        | _ if value > high => high
        | _ => value
    );
"#,
        )
        .unwrap();
    runtime
        .register_module_text(
            "@std/option",
            r#"
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
"#,
        )
        .unwrap();
    runtime
        .register_module_text(
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
        )
        .unwrap();
    runtime
        .register_module_text(
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
        )
        .unwrap();

    let report = runtime.run_test_module("suite").unwrap();

    assert_eq!(report.cases.len(), 3);
    assert!(report.cases.iter().all(|case| case.passed));
}
