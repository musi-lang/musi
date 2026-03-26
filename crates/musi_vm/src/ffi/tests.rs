#![allow(clippy::unwrap_used, clippy::panic, clippy::as_conversions)]

use super::*;
use crate::errors::VmError;
use crate::heap::Heap;
use crate::value::Value;

// ── FfiRuntime: library loading ──────────────────────────────────────────────

#[test]
#[cfg(unix)]
fn load_process_itself() {
    let mut rt = FfiRuntime::new();
    rt.load_library("").unwrap();
}

#[test]
#[cfg(target_os = "macos")]
fn load_libsystem() {
    let mut rt = FfiRuntime::new();
    rt.load_library("libSystem.dylib").unwrap();
}

#[test]
fn unknown_library_errors() {
    let mut rt = FfiRuntime::new();
    let err = rt.load_library("libdefinitely_not_a_real_library_12345.so");
    assert!(matches!(err, Err(VmError::FfiLibraryNotFound(_))));
}

// ── FfiRuntime: symbol resolution ────────────────────────────────────────────

#[test]
#[cfg(unix)]
fn resolve_abs_from_process() {
    let mut rt = FfiRuntime::new();
    rt.load_library("").unwrap();
    let ptr = rt.resolve_symbol("", "abs").unwrap();
    assert!(!ptr.is_null());
}

#[test]
#[cfg(unix)]
fn unknown_symbol_errors() {
    let mut rt = FfiRuntime::new();
    rt.load_library("").unwrap();
    let err = rt.resolve_symbol("", "zzz_nonexistent_symbol_12345");
    assert!(matches!(err, Err(VmError::FfiSymbolNotFound(_))));
}

// ── FFI calls via execute_ffi_call ───────────────────────────────────────────

#[test]
#[cfg(unix)]
fn call_abs() {
    let mut rt = FfiRuntime::new();
    rt.load_library("").unwrap();
    let fn_ptr = rt.resolve_symbol("", "abs").unwrap();

    let mut heap = Heap::new();
    let mut args = vec![Value::from_int(-42)];
    let result =
        execute_ffi_call(fn_ptr, &[FfiType::Int], FfiType::Int, &mut args, &mut heap).unwrap();
    assert!(result.is_int());
    assert_eq!(result.as_int(), 42);
}

#[test]
#[cfg(unix)]
fn call_sin() {
    let mut rt = FfiRuntime::new();
    rt.load_library("").unwrap();
    let fn_ptr = rt.resolve_symbol("", "sin").unwrap();

    let mut heap = Heap::new();
    let mut args = vec![Value::from_float(0.0)];
    let result = execute_ffi_call(
        fn_ptr,
        &[FfiType::Float],
        FfiType::Float,
        &mut args,
        &mut heap,
    )
    .unwrap();
    assert!(result.is_float());
    let val = result.as_float();
    assert!(val.abs() < 1e-10, "sin(0.0) should be ~0.0, got {val}");
}

#[test]
#[cfg(unix)]
fn marshal_string_strlen() {
    let mut rt = FfiRuntime::new();
    rt.load_library("").unwrap();
    let fn_ptr = rt.resolve_symbol("", "strlen").unwrap();

    let mut heap = Heap::new();
    let str_idx = heap.alloc_string("hello".into());
    let mut args = vec![Value::from_ptr(str_idx)];
    let result =
        execute_ffi_call(fn_ptr, &[FfiType::Str], FfiType::Int, &mut args, &mut heap).unwrap();
    assert!(result.is_int());
    assert_eq!(result.as_int(), 5);
}

// ── Marshaling unit tests ────────────────────────────────────────────────────

#[test]
fn marshal_int_roundtrip() {
    let heap = Heap::new();
    let arg = value_to_ffi_arg(Value::from_int(99), FfiType::Int, &heap).unwrap();
    assert!(matches!(arg, FfiArg::Int(99)));
}

#[test]
fn marshal_float_roundtrip() {
    let heap = Heap::new();
    let arg = value_to_ffi_arg(Value::from_float(3.14), FfiType::Float, &heap).unwrap();
    match arg {
        FfiArg::Float(f) => assert!((f - 3.14).abs() < 1e-10),
        _ => panic!("expected Float"),
    }
}

#[test]
fn marshal_bool_roundtrip() {
    let heap = Heap::new();
    let arg = value_to_ffi_arg(Value::TRUE, FfiType::Bool, &heap).unwrap();
    assert!(matches!(arg, FfiArg::Bool(1)));
    let arg_false = value_to_ffi_arg(Value::FALSE, FfiType::Bool, &heap).unwrap();
    assert!(matches!(arg_false, FfiArg::Bool(0)));
}

#[test]
fn marshal_type_mismatch_errors() {
    let heap = Heap::new();
    let err = value_to_ffi_arg(Value::TRUE, FfiType::Int, &heap);
    assert!(matches!(err, Err(VmError::TypeError { .. })));
}

#[test]
fn ffi_result_void_returns_unit() {
    let mut heap = Heap::new();
    let v = ffi_result_to_value(FfiType::Void, 0, &mut heap);
    assert_eq!(v, Value::UNIT);
}

#[test]
fn ffi_result_int() {
    let mut heap = Heap::new();
    let v = ffi_result_to_value(FfiType::Int, 42u64, &mut heap);
    assert!(v.is_int());
    assert_eq!(v.as_int(), 42);
}

#[test]
fn ffi_result_bool_nonzero() {
    let mut heap = Heap::new();
    let v = ffi_result_to_value(FfiType::Bool, 1, &mut heap);
    assert!(v.is_bool());
    assert!(v.as_bool());
}

#[test]
fn ffi_result_bool_zero() {
    let mut heap = Heap::new();
    let v = ffi_result_to_value(FfiType::Bool, 0, &mut heap);
    assert!(v.is_bool());
    assert!(!v.as_bool());
}

#[test]
fn ffi_result_null_ptr_returns_unit() {
    let mut heap = Heap::new();
    let v = ffi_result_to_value(FfiType::Ptr, 0, &mut heap);
    assert_eq!(v, Value::UNIT);
}
