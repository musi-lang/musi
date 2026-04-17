use std::env::{remove_var, temp_dir, var, var_os};
use std::fs;
use std::time::{SystemTime, UNIX_EPOCH};

use musi_native::{NativeHost, NativeTestCaseResult, NativeTestReport};
use musi_vm::{EffectCall, ForeignCall, Value, VmError, VmErrorKind, VmHost, VmResult};
use music_module::ImportMap;
use music_session::SessionOptions;
use music_term::{SyntaxShape, SyntaxTerm, SyntaxTermError};

use crate::{Runtime, RuntimeErrorKind, RuntimeOptions, RuntimeSessionPhase};

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

fn unique_test_suffix() -> String {
    let nanos = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .map_or(0, |duration| duration.as_nanos());
    nanos.to_string()
}

fn temp_text_path() -> String {
    let mut path = temp_dir();
    path.push(format!("musi_rt_runtime_{}.txt", unique_test_suffix()));
    path.to_string_lossy().into_owned()
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
fn array_patterns_require_exact_length() {
    let mut runtime = Runtime::new(NativeHost::new(), RuntimeOptions::default());
    runtime
        .register_module_text(
            "main",
            r"
            export let emptyMismatch () : Int :=
              match [1] (
              | [] => 1
              | _ => 0
              );
            export let lengthMismatch () : Int :=
              match [1, 2, 3] (
              | [1, 2] => 1
              | _ => 0
              );
            export let emptyMatches () : Int :=
              match [] (
              | [] => 1
              | _ => 0
              );
        ",
        )
        .unwrap();
    runtime.load_root("main").unwrap();

    assert_eq!(
        runtime.call_export("emptyMismatch", &[]).unwrap(),
        Value::Int(0)
    );
    assert_eq!(
        runtime.call_export("lengthMismatch", &[]).unwrap(),
        Value::Int(0)
    );
    assert_eq!(
        runtime.call_export("emptyMatches", &[]).unwrap(),
        Value::Int(1)
    );
}

#[test]
fn runtime_array_spread_preserves_expression_type() {
    let mut runtime = Runtime::new(NativeHost::new(), RuntimeOptions::default());
    runtime
        .register_module_text(
            "main",
            r"
            export let prependMatches () : Int :=
              match () (
              | _ if [0, ...[1, 2]] = [0, 1, 2] => 1
              | _ => 0
              );
        ",
        )
        .unwrap();
    runtime.load_root("main").unwrap();

    assert_eq!(
        runtime.call_export("prependMatches", &[]).unwrap(),
        Value::Int(1)
    );
}

#[test]
fn rejects_opaque_exports_through_runtime_api() {
    let mut runtime = Runtime::new(NativeHost::new(), RuntimeOptions::default());
    runtime
        .register_module_text(
            "main",
            "export opaque let Secret := data { | Secret(Int) }; export let root () : Int := 0;",
        )
        .unwrap();
    runtime
        .register_module_text(
            "dep",
            "export opaque let Hidden := data { | Hidden(Int) }; export let answer () : Int := 42;",
        )
        .unwrap();
    runtime.load_root("main").unwrap();

    let err = runtime.lookup_export("Secret").unwrap_err();
    assert!(matches!(
        err.kind(),
        RuntimeErrorKind::VmExecutionFailed(VmError { .. })
    ));

    let module = runtime.load_module("dep").unwrap();
    let err = runtime
        .call_module_export(&module, "Hidden", &[])
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
            export let answer () : Int := unsafe { puts(42); };
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
    host.register_effect_handler("main::Console", "readLine", |_effect, args| {
        assert_eq!(args, &[Value::string(">")]);
        Ok(Value::Int(42))
    });
    let mut runtime = Runtime::new(host, RuntimeOptions::default());
    runtime
        .register_module_text(
            "main",
            r#"
            let Console := effect { let readLine (prompt : String) : Int; };
            export let answer () : Int := request Console.readLine(">");
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
fn collapses_session_failures_into_runtime_phase_error() {
    let mut runtime = Runtime::new(NativeHost::new(), RuntimeOptions::default());
    runtime
        .register_module_text("main", "export let broken := ")
        .unwrap();

    let err = runtime.load_root("main").unwrap_err();
    assert!(matches!(
        err.kind(),
        RuntimeErrorKind::SessionFailed {
            phase: RuntimeSessionPhase::Parse,
            ..
        }
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
            export let answer () : Int := unsafe { puts(1); };
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
    register_runtime_module(
        &mut runtime,
        "@std/prelude",
        r#"
let Core := import "musi:core";
export let Int := Core.Int;
export let Bool := Core.Bool;
export let String := Core.String;
export let Unit := Core.Unit;
"#,
    );
    runtime
        .register_module_text(
            "suite",
            r#"
let Intrinsics := import "musi:test";

export let test () :=
    (
      Intrinsics.suiteStart("demo");
      Intrinsics.testCase("first", 1 = 1);
      Intrinsics.testCase("second", 1 = 2);
      Intrinsics.suiteEnd()
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
fn handles_runtime_process_and_time_services() {
    let mut runtime = Runtime::new(NativeHost::new(), RuntimeOptions::default());
    runtime
        .register_module_text(
            "main",
            r#"
            let Runtime := import "musi:runtime";
            export let argCount () : Int := Runtime.processArgCount();
            export let cwd () : String := Runtime.processCwd();
            export let now () : Int := Runtime.timeNowUnixMs();
        "#,
        )
        .unwrap();
    runtime.load_root("main").unwrap();

    let Value::Int(arg_count) = runtime.call_export("argCount", &[]).unwrap() else {
        panic!("argCount should return Int");
    };
    assert!(arg_count >= 1);

    let Value::String(cwd) = runtime.call_export("cwd", &[]).unwrap() else {
        panic!("cwd should return String");
    };
    assert!(!cwd.is_empty());

    let Value::Int(now) = runtime.call_export("now", &[]).unwrap() else {
        panic!("now should return Int");
    };
    assert!(now > 0);
}

#[test]
fn handles_runtime_env_and_random_services() {
    let mut runtime = Runtime::new(NativeHost::new(), RuntimeOptions::default());
    runtime
        .register_module_text(
            "main",
            r#"
            let Runtime := import "musi:runtime";
            export let envGet (name : String) : String := Runtime.envGet(name);
            export let envHas (name : String) : Int := Runtime.envHas(name);
            export let random () : Int := Runtime.randomInt();
        "#,
        )
        .unwrap();
    runtime.load_root("main").unwrap();

    let missing_key = format!("MUSI_RT_MISSING_{}", unique_test_suffix());
    let has_value = runtime
        .call_export("envHas", &[Value::string(missing_key.clone())])
        .unwrap();
    let env_value = runtime
        .call_export("envGet", &[Value::string(missing_key)])
        .unwrap();
    assert_eq!(has_value, Value::Int(0));
    assert_eq!(env_value, Value::string(""));

    let first = runtime.call_export("random", &[]).unwrap();
    let second = runtime.call_export("random", &[]).unwrap();
    assert!(matches!(first, Value::Int(_)));
    assert!(matches!(second, Value::Int(_)));
    assert_ne!(first, second);
}

#[test]
fn supports_runtime_env_mutation_services() {
    let mut runtime = Runtime::new(NativeHost::new(), RuntimeOptions::default());
    runtime
        .register_module_text(
            "main",
            r#"
            let Runtime := import "musi:runtime";
            export let envSet (name : String, value : String) : Int := Runtime.envSet(name, value);
            export let envRemove (name : String) : Int := Runtime.envRemove(name);
        "#,
        )
        .unwrap();
    runtime.load_root("main").unwrap();

    let key = format!("MUSI_RT_TEST_{}", unique_test_suffix());
    #[allow(unsafe_code)]
    unsafe {
        remove_var(&key);
    }
    let set_value = runtime
        .call_export(
            "envSet",
            &[Value::string(key.clone()), Value::string("value")],
        )
        .unwrap();
    let has_value = runtime
        .call_export("envHas", &[Value::string(key.clone())])
        .unwrap();
    let get_value = runtime
        .call_export("envGet", &[Value::string(key.clone())])
        .unwrap();
    assert_eq!(var(&key).as_deref(), Ok("value"));
    let remove_value = runtime
        .call_export("envRemove", &[Value::string(key.clone())])
        .unwrap();
    let missing_value = runtime
        .call_export("envHas", &[Value::string(key.clone())])
        .unwrap();

    assert_eq!(set_value, Value::Int(1));
    assert_eq!(has_value, Value::Int(1));
    assert_eq!(get_value, Value::string("value"));
    assert_eq!(remove_value, Value::Int(1));
    assert_eq!(missing_value, Value::Int(0));
    assert_eq!(var_os(&key), None);
}

#[test]
fn rejects_unsupported_runtime_process_exit_service() {
    let mut runtime = Runtime::new(NativeHost::new(), RuntimeOptions::default());
    runtime
        .register_module_text(
            "main",
            r#"
            let Runtime := import "musi:runtime";
            export let quit (code : Int) : Unit := Runtime.processExit(code);
        "#,
        )
        .unwrap();
    runtime.load_root("main").unwrap();

    let error = runtime.call_export("quit", &[Value::Int(7)]).unwrap_err();
    assert!(matches!(
        error.kind(),
        RuntimeErrorKind::VmExecutionFailed(VmError { .. })
    ));
}

#[test]
fn handles_runtime_fs_and_log_services() {
    let mut runtime = Runtime::new(NativeHost::new(), RuntimeOptions::default());
    runtime
        .register_module_text(
            "main",
            r#"
            let Runtime := import "musi:runtime";
            export let roundtrip (path : String, text : String) : String := (
              Runtime.fsWriteText(path, text);
              Runtime.fsReadText(path)
            );
            export let logAndPrint () : Unit := (
              Runtime.logInfo("runtime-log");
              Runtime.ioPrint("runtime-print")
            );
        "#,
        )
        .unwrap();
    runtime.load_root("main").unwrap();

    let path = temp_text_path();
    let text = Value::string("runtime-file");
    let value = runtime
        .call_export("roundtrip", &[Value::string(path.clone()), text.clone()])
        .unwrap();
    assert_eq!(value, text);

    let unit = runtime.call_export("logAndPrint", &[]).unwrap();
    assert_eq!(unit, Value::Unit);

    drop(fs::remove_file(path));
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
        "@std/prelude",
        r#"
let Core := import "musi:core";
export let Int := Core.Int;
export let Bool := Core.Bool;
export let String := Core.String;
export let Unit := Core.Unit;
"#,
    );
    register_runtime_module(
        &mut runtime,
        "@std",
        r#"
	export let bytes := import "@std/bytes";
	export let math := import "@std/math";
	export let option := import "@std/option";
	export let testing := import "@std/testing";
"#,
    );
    register_runtime_module(
        &mut runtime,
        "@std/bytes",
        r"
export let equals (left : []Int, right : []Int) : Bool := left = right;
",
    );
    register_runtime_module(
        &mut runtime,
        "@std/math",
        r"
export let clamp (value : Int, low : Int, high : Int) : Int :=
    match () (
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
export opaque let Option[T] := data {
    | Some(T)
    | None
};

export let none [T] () : Option[T] := .None;

export let unwrapOr [T] (value : Option[T], fallback : T) : T :=
    match value (
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

export let toBe (actual : Int, expected : Int) := actual = expected;
export let toBeTruthy (actual : Bool) := actual;

export let describe (name : String) :=
    Intrinsics.suiteStart(name);
export let endDescribe () :=
    Intrinsics.suiteEnd();
export let it (name : String, passed : Bool) :=
    Intrinsics.testCase(name, passed);
"#,
    );
    register_runtime_module(
        &mut runtime,
        "suite",
        r#"
let Testing := import "@std/testing";
let Bytes := import "@std/bytes";
let Math := import "@std/math";
let Option := import "@std/option";

export let test () :=
    (
      Testing.describe("std root");
      Testing.it("bytes chain", Testing.toBeTruthy(Bytes.equals([1, 2], [1, 2])));
      Testing.it("math chain", Testing.toBe(Math.clamp(9, 0, 4), 4));
      Testing.it("option chain", Testing.toBe(Option.unwrapOr[Int](Option.none[Int](), 5), 5));
      Testing.endDescribe()
    );
"#,
    );

    let report = runtime.run_test_module("suite").unwrap();

    assert_eq!(report.cases.len(), 3);
    assert!(report.cases.iter().all(|case| case.passed));
}
