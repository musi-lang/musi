use musi_foundation::{register_modules, test};
use musi_vm::{
    EffectCall, ForeignCall, Program, RejectingLoader, Value, Vm, VmError, VmErrorKind, VmHost,
    VmOptions, VmResult,
};
use music_module::ModuleKey;
use music_session::{Session, SessionOptions};

use crate::{NativeHost, NativeTestCaseResult, NativeTestReport};

#[derive(Default)]
struct FallbackHost;

impl VmHost for FallbackHost {
    fn call_foreign(&mut self, foreign: &ForeignCall, _args: &[Value]) -> VmResult<Value> {
        if foreign.name() == "main::puts" {
            return Ok(Value::Int(11));
        }
        Err(VmError::new(VmErrorKind::ForeignCallRejected {
            foreign: foreign.name().into(),
        }))
    }

    fn handle_effect(&mut self, effect: &EffectCall, _args: &[Value]) -> VmResult<Value> {
        if effect.effect_name() == "main::Console" && effect.op_name() == "readln" {
            return Ok(Value::Int(9));
        }
        Err(VmError::new(VmErrorKind::EffectRejected {
            effect: effect.effect_name().into(),
            op: Some(effect.op_name().into()),
            reason: "fallback rejected effect".into(),
        }))
    }
}

fn compile_program(modules: &[(&str, &str)], entry: &str) -> Program {
    let mut session = Session::new(SessionOptions::default());
    register_modules(&mut session).expect("foundation modules should install");
    for &(name, source) in modules {
        session
            .set_module_text(&ModuleKey::new(name), source)
            .expect("module text should install");
    }
    let output = session
        .compile_entry(&ModuleKey::new(entry))
        .expect("session compile should succeed");
    Program::from_bytes(&output.bytes).expect("program load should succeed")
}

fn call_export_with_host(host: NativeHost, source: &str) -> VmResult<Value> {
    let program = compile_program(&[("main", source)], "main");
    let mut vm = Vm::new(program, RejectingLoader, host, VmOptions);
    vm.initialize().expect("vm init should succeed");
    vm.call_export("answer", &[])
}

#[test]
fn dispatches_registered_foreign_handler() {
    let mut host = NativeHost::new();
    host.register_foreign_handler("main::puts", |_foreign, args| {
        assert_eq!(args, &[Value::Int(42)]);
        Ok(Value::Int(7))
    });

    let value = call_export_with_host(
        host,
        r#"
        foreign "c" (
          let puts (value : Int) : Int;
        );
        export let answer () : Int := puts(42);
        "#,
    )
    .expect("registered foreign should succeed");

    assert_eq!(value, Value::Int(7));
}

#[test]
fn dispatches_registered_effect_handler() {
    let mut host = NativeHost::new();
    host.register_effect_handler("main::Console", "readln", |_effect, args| {
        assert_eq!(args, &[Value::string(">")]);
        Ok(Value::Int(5))
    });

    let value = call_export_with_host(
        host,
        r#"
        let Console := effect { let readln (prompt : String) : Int; };
        export let answer () : Int := perform Console.readln(">");
        "#,
    )
    .expect("registered effect should succeed");

    assert_eq!(value, Value::Int(5));
}

#[test]
fn registered_handlers_override_fallback() {
    let mut host = NativeHost::with_fallback(FallbackHost);
    host.register_foreign_handler("main::puts", |_foreign, _args| Ok(Value::Int(13)));

    let value = call_export_with_host(
        host,
        r#"
        foreign "c" (
          let puts (value : Int) : Int;
        );
        export let answer () : Int := puts(1);
        "#,
    )
    .expect("registered foreign should win");

    assert_eq!(value, Value::Int(13));
}

#[test]
fn falls_back_for_unregistered_edges() {
    let host = NativeHost::with_fallback(FallbackHost);

    let value = call_export_with_host(
        host,
        r#"
        foreign "c" (
          let puts (value : Int) : Int;
        );
        export let answer () : Int := puts(1);
        "#,
    )
    .expect("fallback should handle foreign");

    assert_eq!(value, Value::Int(11));
}

#[test]
fn rejects_unhandled_edges_without_fallback() {
    let err = call_export_with_host(
        NativeHost::new(),
        r#"
        foreign "c" (
          let puts (value : Int) : Int;
        );
        export let answer () : Int := puts(1);
        "#,
    )
    .expect_err("missing host edge should reject");

    assert!(matches!(
        err.kind(),
        VmErrorKind::ForeignCallRejected { .. }
    ));
}

#[test]
fn clones_share_registered_state() {
    let host = NativeHost::new();
    let mut clone = host.clone();
    clone.register_foreign_handler("main::puts", |_foreign, _args| Ok(Value::Int(23)));

    let value = call_export_with_host(
        host,
        r#"
        foreign "c" (
          let puts (value : Int) : Int;
        );
        export let answer () : Int := puts(1);
        "#,
    )
    .expect("shared state should be visible");

    assert_eq!(value, Value::Int(23));
}

#[test]
fn collects_test_effect_reports() {
    let mut host = NativeHost::new();
    host.begin_test_session();
    let source = format!(
        r#"
            let Intrinsics := import "{spec}";
            let Test := Intrinsics.Test;

            export let answer () :=
                (
                  perform Test.suiteStart("demo");
                  perform Test.testCase("first", 1 = 1);
                  perform Test.testCase("second", 1 = 2);
                  perform Test.suiteEnd()
                );
            "#,
        spec = test::SPEC,
    );

    let program = compile_program(&[("main", source.as_str())], "main");
    let mut vm = Vm::new(program, RejectingLoader, host.clone(), VmOptions);
    vm.initialize().expect("vm init should succeed");
    let _ = vm
        .call_export("answer", &[])
        .expect("test export should run");

    let report = host.finish_test_session("main");
    assert_eq!(
        report,
        NativeTestReport {
            module: "main".into(),
            cases: vec![
                NativeTestCaseResult {
                    suite: "demo".into(),
                    name: "first".into(),
                    passed: true,
                },
                NativeTestCaseResult {
                    suite: "demo".into(),
                    name: "second".into(),
                    passed: false,
                },
            ]
            .into_boxed_slice(),
        }
    );
}

#[test]
fn rejects_test_effect_without_active_session() {
    let source = format!(
        r#"
        let Intrinsics := import "{spec}";
        let Test := Intrinsics.Test;
        export let answer () := perform Test.testCase("first", 1 = 1);
        "#,
        spec = test::SPEC,
    );
    let err = call_export_with_host(NativeHost::new(), source.as_str())
        .expect_err("inactive test session should reject");

    assert!(matches!(err.kind(), VmErrorKind::EffectRejected { .. }));
}

#[cfg(not(any(target_os = "macos", target_os = "linux", target_os = "windows")))]
#[test]
fn unsupported_targets_reject_runtime_effects() {
    let err = call_export_with_host(
        NativeHost::new(),
        r#"
        let Console := effect { let readln (prompt : String) : Int; };
        export let answer () : Int := perform Console.readln(">");
        "#,
    )
    .expect_err("unsupported target should reject");

    assert!(matches!(err.kind(), VmErrorKind::EffectRejected { .. }));
}
