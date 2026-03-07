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

#[test]
fn string_slice_extracts_chars() {
    let vm = make_vm();
    let result = dispatch(
        &vm,
        Intrinsic::StringSlice,
        &[Value::String(Rc::from("abcdef")), Value::Int(1), Value::Int(4)],
    );
    assert_eq!(result, Value::String(Rc::from("bcd")));
}

#[test]
fn string_to_int_parses_valid() {
    let vm = make_vm();
    let result = dispatch(&vm, Intrinsic::StringToInt, &[Value::String(Rc::from("42"))]);
    let Value::Object { fields, .. } = result else { panic!("expected Object") };
    assert_eq!(fields[0], Value::Int(1));  // Some discriminant
    assert_eq!(fields[1], Value::Int(42));
}

#[test]
fn string_to_int_returns_none_for_bad_input() {
    let vm = make_vm();
    let result = dispatch(&vm, Intrinsic::StringToInt, &[Value::String(Rc::from("abc"))]);
    let Value::Object { fields, .. } = result else { panic!("expected Object") };
    assert_eq!(fields[0], Value::Int(0));  // None discriminant
}

#[test]
fn string_contains_finds_substring() {
    let vm = make_vm();
    let result = dispatch(
        &vm,
        Intrinsic::StringContains,
        &[Value::String(Rc::from("hello world")), Value::String(Rc::from("world"))],
    );
    let Value::Object { fields, .. } = result else { panic!("expected Object") };
    assert_eq!(fields[0], Value::Int(1));  // True
}

#[test]
fn float_sqrt_computes() {
    let vm = make_vm();
    let result = dispatch(&vm, Intrinsic::FloatSqrt, &[Value::Float(4.0)]);
    assert_eq!(result, Value::Float(2.0));
}

#[test]
fn float_floor_rounds_down() {
    let vm = make_vm();
    let result = dispatch(&vm, Intrinsic::FloatFloor, &[Value::Float(3.9)]);
    assert_eq!(result, Value::Float(3.0));
}

#[test]
fn float_ceil_rounds_up() {
    let vm = make_vm();
    let result = dispatch(&vm, Intrinsic::FloatCeil, &[Value::Float(3.1)]);
    assert_eq!(result, Value::Float(4.0));
}
