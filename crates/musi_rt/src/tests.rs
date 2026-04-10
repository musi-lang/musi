use musi_vm::{EffectCall, ForeignCall, Value, VmError, VmErrorKind, VmHost, VmResult};

use crate::{Runtime, RuntimeOptions};

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

#[test]
fn loads_root_and_calls_export() {
    let mut runtime = Runtime::new(TestHost, RuntimeOptions::default());
    runtime
        .register_module_text("main", "export let answer () : Int := 42;")
        .unwrap();
    runtime.load_root("main").unwrap();

    let value = runtime.call_export("answer", &[]).unwrap();
    assert_eq!(value, Value::Int(42));
}

#[test]
fn loads_dynamic_module_from_registered_text() {
    let mut runtime = Runtime::new(TestHost, RuntimeOptions::default());
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
    let mut runtime = Runtime::new(TestHost, RuntimeOptions::default());
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
    let mut runtime = Runtime::new(TestHost, RuntimeOptions::default());
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
