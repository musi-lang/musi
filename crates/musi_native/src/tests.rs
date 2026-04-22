#![allow(unused_imports)]
#![allow(unsafe_code)]

use musi_foundation::{register_modules, test};
use musi_vm::{
    EffectCall, ForeignCall, NativeFailureStage, Program, ProgramTypeAbiKind, RejectingLoader,
    Value, Vm, VmError, VmErrorKind, VmHost, VmHostCallContext, VmHostContext, VmOptions, VmResult,
};
use music_module::ModuleKey;
use music_session::{Session, SessionOptions};

use crate::platform::{NativeAbiCallSupport, NativeAbiTypePosition, PlatformHost};
use crate::{NativeHost, NativeTestCaseResult, NativeTestReport};

#[derive(Default)]
struct FallbackHost;

impl VmHost for FallbackHost {
    fn call_foreign(
        &mut self,
        _ctx: VmHostCallContext<'_, '_>,
        foreign: &ForeignCall,
        _args: &[Value],
    ) -> VmResult<Value> {
        if foreign.name() == "main::puts" {
            return Ok(Value::Int(11));
        }
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
        if effect.effect_name() == "main::Console" && effect.op_name() == "readLine" {
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
    vm.initialize()?;
    vm.call_export("result", &[])
}

const fn is_supported_target() -> bool {
    cfg!(any(
        target_os = "macos",
        target_os = "linux",
        target_os = "windows"
    ))
}

mod success {
    use super::*;

    #[test]
    fn dispatches_registered_foreign_handler() {
        let mut host = NativeHost::new();
        host.register_foreign_handler("main::puts", |foreign, args| {
            assert_eq!(args, &[Value::Int(42)]);
            assert!(foreign.param_data_layout(0).is_none());
            assert!(foreign.result_data_layout().is_none());
            Ok(Value::Int(7))
        });

        let value = call_export_with_host(
            host,
            r#"
        native "c" (
          let puts (value : Int) : Int;
        );
        export let result () : Int := unsafe { puts(42); };
        "#,
        )
        .expect("registered foreign should succeed");

        assert_eq!(value, Value::Int(7));
    }

    #[test]
    fn foreign_calls_expose_data_layout_descriptors() {
        let mut host = NativeHost::new();
        host.register_foreign_handler("main::inspect", |foreign, args| {
            assert_eq!(args.len(), 1);
            let Value::Data(_) = &args[0] else {
                panic!("expected data arg");
            };
            let platform = PlatformHost::new();
            if is_supported_target() {
                assert!(!platform.supports_native_abi_call(foreign));
                assert!(matches!(
                    platform.native_abi_support(foreign),
                    NativeAbiCallSupport::UnsupportedType {
                        position: NativeAbiTypePosition::Param(0),
                        kind: ProgramTypeAbiKind::Unsupported,
                        ..
                    }
                ));
                assert_eq!(
                    foreign
                        .param_data_layout(0)
                        .map(|layout| layout.name.as_ref()),
                    Some("main::Maybe")
                );
            } else {
                assert_eq!(
                    platform.native_abi_support(foreign),
                    NativeAbiCallSupport::UnsupportedTarget
                );
            }
            assert!(PlatformHost::foreign_uses_data_layout(foreign));
            let layout = foreign
                .param_data_layout(0)
                .expect("Maybe layout should be exposed");
            assert_eq!(layout.name.as_ref(), "main::Maybe");
            assert_eq!(layout.variant_count, 2);
            assert_eq!(layout.field_count, 1);
            assert!(!layout.is_single_variant_product());
            Ok(Value::Int(17))
        });

        let value = call_export_with_host(
            host,
            r#"
        let Maybe := data { | Some(Int) | None };
        native "c" (
          let inspect (value : Maybe) : Int;
        );
        export let result () : Int := unsafe { inspect(.Some(1)); };
        "#,
        )
        .expect("layout-aware foreign should succeed");

        assert_eq!(value, Value::Int(17));
    }

    #[test]
    fn native_abi_support_accepts_c_scalar_foreigns() {
        let mut host = NativeHost::new();
        host.register_foreign_handler("main::puts", |foreign, _args| {
            let platform = PlatformHost::new();
            if is_supported_target() {
                assert_eq!(
                    platform.native_abi_support(foreign),
                    NativeAbiCallSupport::MissingLink
                );
                assert!(!platform.supports_native_abi_call(foreign));
            } else {
                assert_eq!(
                    platform.native_abi_support(foreign),
                    NativeAbiCallSupport::UnsupportedTarget
                );
            }
            Ok(Value::Int(19))
        });

        let value = call_export_with_host(
            host,
            r#"
        native "c" (
          let puts (value : Int) : Int;
        );
        export let result () : Int := unsafe { puts(42); };
        "#,
        )
        .expect("scalar foreign should succeed");

        assert_eq!(value, Value::Int(19));
    }

    #[cfg(any(target_os = "macos", target_os = "linux", target_os = "windows"))]
    #[test]
    fn native_abi_support_link_smoke() {
        let source = r#"
        @link(name := "c", symbol := "strerror")
        native "c" let strerror (code : Int) : CString;
        @link(name := "c", symbol := "strlen")
        native "c" let strlen (value : CString) : Int;
        export let result () : Int := unsafe { strlen(strerror(2)); };
    "#;
        let value = call_export_with_host(NativeHost::default(), source)
            .expect("linked native call should succeed");
        let Value::Int(len) = value else {
            panic!("expected `Int` result");
        };
        assert!(len > 0);
    }

    #[cfg(target_os = "macos")]
    #[test]
    fn native_abi_cstring_results_roundtrip() {
        let source = r#"
        @link(name := "/usr/lib/libSystem.B.dylib", symbol := "getprogname")
        native "c" let musi_native_test_progname () : CString;
        @link(name := "/usr/lib/libSystem.B.dylib", symbol := "strchr")
        native "c" let strchr (value : CString, code : Int) : CString;
        export let result () : CString := unsafe { strchr(musi_native_test_progname(), 0); };
    "#;
        let program = compile_program(&[("main", source)], "main");
        let mut vm = Vm::new(program, RejectingLoader, NativeHost::default(), VmOptions);
        vm.initialize().expect("program should initialize");
        let value = vm
            .call_export("result", &[])
            .expect("cstring result should succeed");
        let musi_vm::ValueView::String(text) = vm.inspect(&value) else {
            panic!("expected string result");
        };
        assert_eq!(text.as_str(), "");
    }

    #[test]
    fn dispatches_registered_effect_handler() {
        let mut host = NativeHost::new();
        host.register_effect_handler_with_context(
            "main::Console",
            "readLine",
            |ctx, _effect, args| {
                let [prompt] = args else {
                    panic!("expected prompt");
                };
                let prompt = ctx.string(prompt).expect("prompt should be string");
                assert_eq!(prompt.as_str(), ">");
                Ok(Value::Int(5))
            },
        );

        let value = call_export_with_host(
            host,
            r#"
        let Console := effect { let readLine (prompt : String) : Int; };
        export let result () : Int := ask Console.readLine(">");
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
        native "c" (
          let puts (value : Int) : Int;
        );
        export let result () : Int := unsafe { puts(1); };
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
        native "c" (
          let puts (value : Int) : Int;
        );
        export let result () : Int := unsafe { puts(1); };
        "#,
        )
        .expect("fallback should handle foreign");

        assert_eq!(value, Value::Int(11));
    }

    #[test]
    fn clones_share_registered_state() {
        let host = NativeHost::new();
        let mut clone = host.clone();
        clone.register_foreign_handler("main::puts", |_foreign, _args| Ok(Value::Int(23)));

        let value = call_export_with_host(
            host,
            r#"
        native "c" (
          let puts (value : Int) : Int;
        );
        export let result () : Int := unsafe { puts(1); };
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
            let Test := import "{spec}";

            export let result () :=
                (
                  Test.suiteStart("demo");
                  Test.testCase("first", 1 = 1);
                  Test.testCase("second", 1 = 2);
                  Test.suiteEnd()
                );
            "#,
            spec = test::SPEC,
        );

        let program = compile_program(&[("main", source.as_str())], "main");
        let mut vm = Vm::new(program, RejectingLoader, host.clone(), VmOptions);
        vm.initialize().expect("vm init should succeed");
        let _ = vm
            .call_export("result", &[])
            .expect("test export should run");

        let report = host.finish_test_session("main");
        assert_eq!(
            report,
            NativeTestReport::new(
                "main",
                vec![
                    NativeTestCaseResult::new("demo".into(), "first".into(), true),
                    NativeTestCaseResult::new("demo".into(), "second".into(), false),
                ]
                .into_boxed_slice(),
            )
        );
    }
}

mod failure {
    use super::*;

    #[test]
    fn native_abi_support_rejects_non_c_abi() {
        let mut host = NativeHost::new();
        host.register_foreign_handler("main::puts", |foreign, _args| {
            let platform = PlatformHost::new();
            if is_supported_target() {
                assert_eq!(
                    platform.native_abi_support(foreign),
                    NativeAbiCallSupport::UnsupportedAbi {
                        abi: "system".into(),
                    }
                );
                assert!(!platform.supports_native_abi_call(foreign));
            }
            Ok(Value::Int(23))
        });

        let value = call_export_with_host(
            host,
            r#"
        native "system" (
          let puts (value : Int) : Int;
        );
        export let result () : Int := unsafe { puts(42); };
        "#,
        )
        .expect("non-c foreign should still dispatch through registered host");

        assert_eq!(value, Value::Int(23));
    }

    #[test]
    fn native_abi_symbol_failures_report_typed_errors() {
        let source = r#"
        @link(name := "c")
        native "c" let musi_native_test_missing_symbol (value : Int) : Int;
        export let result () : Int := unsafe { musi_native_test_missing_symbol(1); };
    "#;
        let err = call_export_with_host(NativeHost::default(), source)
            .expect_err("missing symbol should fail");

        assert!(matches!(
            err.kind(),
            VmErrorKind::NativeCallFailed {
                stage: NativeFailureStage::SymbolLoad,
                ..
            }
        ));
    }

    #[test]
    fn rejects_unhandled_edges_without_fallback() {
        let err = call_export_with_host(
            NativeHost::new(),
            r#"
        native "c" (
          let puts (value : Int) : Int;
        );
        export let result () : Int := unsafe { puts(1); };
        "#,
        )
        .expect_err("missing host edge should reject");

        assert!(matches!(
            err.kind(),
            VmErrorKind::ForeignCallRejected { .. }
        ));
    }

    #[test]
    fn rejects_test_effect_without_active_session() {
        let source = format!(
            r#"
        let Test := import "{spec}";
        export let result () := Test.testCase("first", 1 = 1);
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
        let Console := effect { let readLine (prompt : String) : Int; };
        export let result () : Int := ask Console.readLine(">");
        "#,
        )
        .expect_err("unsupported target should reject");

        assert!(matches!(err.kind(), VmErrorKind::EffectRejected { .. }));
    }
}
