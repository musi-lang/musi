#![allow(unused_imports)]

use std::env::{remove_var, temp_dir, var, var_os};
use std::fs;
use std::time::{SystemTime, UNIX_EPOCH};

use musi_native::{NativeHost, NativeTestCaseResult, NativeTestReport};
use musi_vm::{
    EffectCall, ForeignCall, Value, ValueView, VmDiagKind, VmError, VmErrorKind, VmHost,
    VmHostCallContext, VmHostContext, VmResult,
};
use music_base::diag::DiagContext;
use music_module::ImportMap;
use music_session::SessionOptions;
use music_term::{SyntaxShape, SyntaxTerm, SyntaxTermError};

use crate::{Runtime, RuntimeErrorKind, RuntimeOptions, RuntimeOutputMode, RuntimeSessionPhase};

#[derive(Default)]
struct TestHost;

impl VmHost for TestHost {
    fn call_foreign(
        &mut self,
        _ctx: VmHostCallContext<'_, '_>,
        foreign: &ForeignCall,
        _args: &[Value],
    ) -> VmResult<Value> {
        Err(VmError::new(VmErrorKind::ForeignCallRejected {
            foreign: foreign.name().into(),
        }))
    }

    fn handle_effect(
        &mut self,
        _ctx: VmHostCallContext<'_, '_>,
        effect: &EffectCall,
        _args: &[Value],
    ) -> VmResult<Value> {
        Err(VmError::new(VmErrorKind::EffectRejected {
            effect: effect.effect_name().into(),
            op: Some(effect.op_name().into()),
            reason: VmDiagKind::EffectRejected
                .message_with(
                    &DiagContext::new()
                        .with("effect", effect.effect_name())
                        .with("op", effect.op_name())
                        .with("reason", "test host"),
                )
                .into(),
        }))
    }
}

fn expr_syntax(runtime: &mut Runtime, text: &str) -> Value {
    runtime
        .vm_mut()
        .unwrap()
        .alloc_syntax(SyntaxTerm::parse(SyntaxShape::Expr, text).unwrap())
        .unwrap()
}

fn module_syntax(runtime: &mut Runtime, text: &str) -> Value {
    runtime
        .vm_mut()
        .unwrap()
        .alloc_syntax(SyntaxTerm::parse(SyntaxShape::Module, text).unwrap())
        .unwrap()
}

fn runtime_string(runtime: &mut Runtime, text: impl Into<Box<str>>) -> Value {
    runtime.vm_mut().unwrap().alloc_string(text).unwrap()
}

fn assert_runtime_string(runtime: &Runtime, value: &Value, expected: &str) {
    let ValueView::String(text) = runtime.inspect(value).unwrap() else {
        panic!("expected string");
    };
    assert_eq!(text.as_str(), expected);
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

mod success {
    use super::*;

    #[test]
    fn loads_root_and_calls_export() {
        let mut runtime = Runtime::new(NativeHost::new(), RuntimeOptions::default());
        runtime
            .register_module_text("main", "export let result () : Int := 42;")
            .unwrap();
        runtime.load_root("main").unwrap();

        let value = runtime.call_export("result", &[]).unwrap();
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
                "export let result () : Int := 42; export let base : Int := 41;",
            )
            .unwrap();
        runtime.load_root("main").unwrap();

        let module = runtime.load_module("dep").unwrap();
        let value = runtime.call_module_export(&module, "result", &[]).unwrap();

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
    fn evaluates_expression_syntax_through_runtime_service() {
        let mut runtime = Runtime::new(NativeHost::new(), RuntimeOptions::default());
        runtime
            .register_module_text("main", "export let root () : Int := 0;")
            .unwrap();
        runtime.load_root("main").unwrap();

        let syntax = expr_syntax(&mut runtime, "42");
        let value = runtime.eval_expr_syntax(&syntax, "Int").unwrap();
        assert_eq!(value, Value::Int(42));
    }

    #[test]
    fn loads_module_syntax_through_runtime_service() {
        let mut runtime = Runtime::new(NativeHost::new(), RuntimeOptions::default());
        runtime
            .register_module_text("main", "export let root () : Int := 0;")
            .unwrap();
        runtime.load_root("main").unwrap();

        let syntax = module_syntax(&mut runtime, "export let result () : Int := 42;");
        let module = runtime.load_module_syntax("generated", &syntax).unwrap();
        let value = runtime.call_module_export(&module, "result", &[]).unwrap();

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
            native "c" (
              let puts (value : Int) : Int;
            );
            export let result () : Int := unsafe { puts(42); };
        "#,
            )
            .unwrap();
        runtime.load_root("main").unwrap();

        let value = runtime.call_export("result", &[]).unwrap();
        assert_eq!(value, Value::Int(7));
    }

    #[test]
    fn routes_effect_calls_through_registered_handlers() {
        let mut host = NativeHost::new();
        host.register_effect_handler_with_context(
            "main::Console",
            "readLine",
            |ctx, _effect, args| {
                let [prompt] = args else {
                    panic!("prompt expected");
                };
                let prompt = ctx.string(prompt).expect("prompt should be string");
                assert_eq!(prompt.as_str(), ">");
                Ok(Value::Int(42))
            },
        );
        let mut runtime = Runtime::new(host, RuntimeOptions::default());
        runtime
            .register_module_text(
                "main",
                r#"
            let Console := effect { let readLine (prompt : String) : Int; };
            export let result () : Int := ask Console.readLine(">");
        "#,
            )
            .unwrap();
        runtime.load_root("main").unwrap();

        let value = runtime.call_export("result", &[]).unwrap();
        assert_eq!(value, Value::Int(42));
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
            native "c" (
              let puts (value : Int) : Int;
            );
            export let result () : Int := unsafe { puts(1); };
        "#,
            )
            .unwrap();
        runtime.load_root("main").unwrap();

        let err = runtime.call_export("result", &[]).unwrap_err();
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
            RuntimeOptions::default()
                .with_session(SessionOptions::new().with_import_map(import_map)),
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
            let Process := import "musi:process";
            let Time := import "musi:time";
            export let argCount () : Int := Process.argCount();
            export let cwd () : String := Process.cwd();
            export let now () : Int := Time.nowUnixMs();
        "#,
            )
            .unwrap();
        runtime.load_root("main").unwrap();

        let Value::Int(arg_count) = runtime.call_export("argCount", &[]).unwrap() else {
            panic!("argCount should return Int");
        };
        assert!(arg_count >= 1);

        let cwd = runtime.call_export("cwd", &[]).unwrap();
        let ValueView::String(cwd) = runtime.inspect(&cwd).unwrap() else {
            panic!("cwd should return String");
        };
        assert!(!cwd.as_str().is_empty());

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
            let Env := import "musi:env";
            let Random := import "musi:random";
            export let envGet (name : String) : String := Env.get(name);
            export let envHas (name : String) : Int := Env.has(name);
            export let random () : Int := Random.int();
        "#,
            )
            .unwrap();
        runtime.load_root("main").unwrap();

        let missing_key = format!("MUSI_RT_MISSING_{}", unique_test_suffix());
        let missing_arg = runtime_string(&mut runtime, missing_key.clone());
        let has_value = runtime.call_export("envHas", &[missing_arg]).unwrap();
        let missing_arg = runtime_string(&mut runtime, missing_key);
        let env_value = runtime.call_export("envGet", &[missing_arg]).unwrap();
        assert_eq!(has_value, Value::Int(0));
        assert_runtime_string(&runtime, &env_value, "");

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
            let Env := import "musi:env";
            export let envGet (name : String) : String := Env.get(name);
            export let envHas (name : String) : Int := Env.has(name);
            export let envSet (name : String, value : String) : Int := Env.set(name, value);
            export let envRemove (name : String) : Int := Env.remove(name);
        "#,
            )
            .unwrap();
        runtime.load_root("main").unwrap();

        let key = format!("MUSI_RT_TEST_{}", unique_test_suffix());
        #[allow(unsafe_code)]
        unsafe {
            remove_var(&key);
        }
        let key_arg = runtime_string(&mut runtime, key.clone());
        let value_arg = runtime_string(&mut runtime, "value");
        let set_value = runtime
            .call_export("envSet", &[key_arg, value_arg])
            .unwrap();
        let key_arg = runtime_string(&mut runtime, key.clone());
        let has_value = runtime.call_export("envHas", &[key_arg]).unwrap();
        let key_arg = runtime_string(&mut runtime, key.clone());
        let get_value = runtime.call_export("envGet", &[key_arg]).unwrap();
        assert_eq!(var(&key).as_deref(), Ok("value"));
        let key_arg = runtime_string(&mut runtime, key.clone());
        let remove_value = runtime.call_export("envRemove", &[key_arg]).unwrap();
        let key_arg = runtime_string(&mut runtime, key.clone());
        let missing_value = runtime.call_export("envHas", &[key_arg]).unwrap();

        assert_eq!(set_value, Value::Int(1));
        assert_eq!(has_value, Value::Int(1));
        assert_runtime_string(&runtime, &get_value, "value");
        assert_eq!(remove_value, Value::Int(1));
        assert_eq!(missing_value, Value::Int(0));
        assert_eq!(var_os(&key), None);
    }

    #[test]
    fn handles_runtime_fs_and_log_services() {
        let mut runtime = Runtime::new(
            NativeHost::new(),
            RuntimeOptions::default().with_output(RuntimeOutputMode::Capture),
        );
        runtime
            .register_module_text(
                "main",
                r#"
            let Fs := import "musi:fs";
            let Io := import "musi:io";
            let Log := import "musi:log";
            export let roundtrip (path : String, text : String) : String := (
              Fs.writeText(path, text);
              Fs.readText(path)
            );
            export let logAndPrint () : Unit := (
              Log.info("runtime-log");
              Io.print("runtime-print")
            );
        "#,
            )
            .unwrap();
        runtime.load_root("main").unwrap();

        let path = temp_text_path();
        let text = runtime_string(&mut runtime, "runtime-file");
        let path_value = runtime_string(&mut runtime, path.clone());
        let value = runtime
            .call_export("roundtrip", &[path_value, text])
            .unwrap();
        assert_runtime_string(&runtime, &value, "runtime-file");

        let unit = runtime.call_export("logAndPrint", &[]).unwrap();
        assert_eq!(unit, Value::Unit);

        drop(fs::remove_file(path));
    }

    #[test]
    fn captures_runtime_output_during_tests() {
        let mut runtime = Runtime::new(
            NativeHost::new(),
            RuntimeOptions::default().with_output(RuntimeOutputMode::Capture),
        );
        runtime
            .register_module_text(
                "main",
                r#"
            let Io := import "musi:io";
            let Log := import "musi:log";
            export let test () : Unit := (
              Io.print("out");
              Io.printLine(" line");
              Io.printError("err");
              Io.printErrorLine(" line");
              Log.write(40, "boom")
            );
        "#,
            )
            .unwrap();

        let report = runtime.run_test_export("main", "test").unwrap();

        assert_eq!(report.stdout.as_ref(), "out line\n");
        assert_eq!(report.stderr.as_ref(), "err line\n[std:40] boom\n");
    }

    #[test]
    fn suppresses_runtime_output_during_tests() {
        let mut runtime = Runtime::new(
            NativeHost::new(),
            RuntimeOptions::default().with_output(RuntimeOutputMode::Suppress),
        );
        runtime
            .register_module_text(
                "main",
                r#"
            let Io := import "musi:io";
            let Log := import "musi:log";
            export let test () : Unit := (
              Io.printLine("hidden");
              Io.printErrorLine("hidden");
              Log.write(40, "hidden")
            );
        "#,
            )
            .unwrap();

        let report = runtime.run_test_export("main", "test").unwrap();

        assert_eq!(report.stdout.as_ref(), "");
        assert_eq!(report.stderr.as_ref(), "");
    }

    #[test]
    fn runs_root_hub_std_test_module() {
        let mut import_map = ImportMap::default();
        let _ = import_map.imports.insert("@std/".into(), "@std/".into());
        let mut runtime = Runtime::new(
            NativeHost::new(),
            RuntimeOptions::default()
                .with_session(SessionOptions::new().with_import_map(import_map)),
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
}

mod failure {
    use super::*;

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
            "export opaque let Hidden := data { | Hidden(Int) }; export let result () : Int := 42;",
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
            RuntimeErrorKind::MissingModuleSource { .. }
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
    fn rejects_unsupported_runtime_process_exit_service() {
        let mut runtime = Runtime::new(NativeHost::new(), RuntimeOptions::default());
        runtime
            .register_module_text(
                "main",
                r#"
            let Process := import "musi:process";
            export let quit (code : Int) : Unit := Process.exit(code);
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
}
