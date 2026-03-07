use std::rc::Rc;

use musi_codegen::intrinsics::Intrinsic;
use musi_codegen::Module;

use super::*;
use crate::value::Value;
use crate::vm::Vm;

fn make_vm() -> Vm {
    Vm::new(Module::new(), crate::native_registry::NativeRegistry::empty())
}

#[test]
fn writeln_returns_unit() {
    let vm = make_vm();
    let result = dispatch(&vm, Intrinsic::Writeln, &[Value::String(Rc::from("test"))]);
    assert_eq!(result, Value::Unit);
}

#[test]
fn write_returns_unit() {
    let vm = make_vm();
    let result = dispatch(&vm, Intrinsic::Write, &[Value::String(Rc::from("test"))]);
    assert_eq!(result, Value::Unit);
}

#[test]
fn int_to_string_converts() {
    let vm = make_vm();
    let result = dispatch(&vm, Intrinsic::IntToString, &[Value::Int(42)]);
    assert_eq!(result, Value::String(Rc::from("42")));
}

#[test]
fn int_to_string_empty_for_non_int() {
    let vm = make_vm();
    let result = dispatch(&vm, Intrinsic::IntToString, &[]);
    assert_eq!(result, Value::String(Rc::from("")));
}

#[test]
fn string_concat_joins_strings() {
    let vm = make_vm();
    let result = dispatch(
        &vm,
        Intrinsic::StringConcat,
        &[Value::String(Rc::from("hello")), Value::String(Rc::from(" world"))],
    );
    assert_eq!(result, Value::String(Rc::from("hello world")));
}
