#![allow(clippy::unwrap_used, clippy::panic, clippy::as_conversions)]

use super::*;
use crate::errors::VmError;
use crate::heap::Heap;
use crate::value::Value;

#[cfg(target_os = "macos")]
const LIBC_NAME: &str = "libSystem.dylib";
#[cfg(target_os = "macos")]
const LIBM_NAME: &str = "libSystem.dylib";
#[cfg(all(unix, not(target_os = "macos")))]
const LIBC_NAME: &str = "libc.so.6";
#[cfg(all(unix, not(target_os = "macos")))]
const LIBM_NAME: &str = "libm.so.6";

// ── FfiRuntime: library loading ──────────────────────────────────────────────

#[test]
#[cfg(unix)]
fn builtin_host_symbols_do_not_require_library_load() {
    let mut rt = FfiRuntime::new();
    rt.load_library("").unwrap();
    assert!(
        !rt.resolve_symbol("", "musi_io_read_text")
            .unwrap()
            .is_null()
    );
}

#[test]
#[cfg(unix)]
fn load_native_library() {
    let mut rt = FfiRuntime::new();
    rt.load_library(LIBC_NAME).unwrap();
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
fn resolve_abs_from_native_library() {
    let mut rt = FfiRuntime::new();
    rt.load_library(LIBC_NAME).unwrap();
    let ptr = rt.resolve_symbol(LIBC_NAME, "abs").unwrap();
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

#[test]
#[cfg(unix)]
fn resolve_musi_io_read_text_from_builtin_host_symbols() {
    let mut rt = FfiRuntime::new();
    rt.load_library("").unwrap();
    let ptr = rt.resolve_symbol("", "musi_io_read_text").unwrap();
    assert!(!ptr.is_null());
}

// ── FFI calls via execute_ffi_call ───────────────────────────────────────────

#[test]
#[cfg(unix)]
fn call_abs() {
    let mut rt = FfiRuntime::new();
    rt.load_library(LIBC_NAME).unwrap();
    let fn_ptr = rt.resolve_symbol(LIBC_NAME, "abs").unwrap();

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
    rt.load_library(LIBM_NAME).unwrap();
    let fn_ptr = rt.resolve_symbol(LIBM_NAME, "sin").unwrap();

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
    rt.load_library(LIBC_NAME).unwrap();
    let fn_ptr = rt.resolve_symbol(LIBC_NAME, "strlen").unwrap();

    let mut heap = Heap::new();
    let str_idx = heap.alloc_string("hello".into());
    let mut args = vec![Value::from_ptr(str_idx)];
    let result =
        execute_ffi_call(fn_ptr, &[FfiType::Str], FfiType::Int, &mut args, &mut heap).unwrap();
    assert!(result.is_int());
    assert_eq!(result.as_int(), 5);
}

#[test]
#[cfg(unix)]
fn resolve_musi_io_symbols_from_builtin_host_symbols() {
    let mut rt = FfiRuntime::new();
    rt.load_library("").unwrap();
    assert!(
        !rt.resolve_symbol("", "musi_io_read_text")
            .unwrap()
            .is_null()
    );
    assert!(
        !rt.resolve_symbol("", "musi_io_write_text")
            .unwrap()
            .is_null()
    );
    assert!(
        !rt.resolve_symbol("", "musi_io_last_error")
            .unwrap()
            .is_null()
    );
}

#[test]
#[cfg(unix)]
fn host_text_io_roundtrip() {
    let dir = tempfile::tempdir().unwrap();
    let path = dir.path().join("note.txt");
    let path_str = path.to_string_lossy().into_owned();

    let mut rt = FfiRuntime::new();
    rt.load_library("").unwrap();
    let write_ptr = rt.resolve_symbol("", "musi_io_write_text").unwrap();
    let append_ptr = rt.resolve_symbol("", "musi_io_append_text").unwrap();
    let read_ptr = rt.resolve_symbol("", "musi_io_read_text").unwrap();

    let mut heap = Heap::new();
    let path_val = Value::from_ptr(heap.alloc_string(path_str));
    let hello_val = Value::from_ptr(heap.alloc_string("hello".into()));
    let world_val = Value::from_ptr(heap.alloc_string(" world".into()));

    let mut write_args = vec![path_val, hello_val];
    let write_ok = execute_ffi_call(
        write_ptr,
        &[FfiType::Str, FfiType::Str],
        FfiType::Bool,
        &mut write_args,
        &mut heap,
    )
    .unwrap();
    assert!(write_ok.as_bool());

    let mut append_args = vec![path_val, world_val];
    let append_ok = execute_ffi_call(
        append_ptr,
        &[FfiType::Str, FfiType::Str],
        FfiType::Bool,
        &mut append_args,
        &mut heap,
    )
    .unwrap();
    assert!(append_ok.as_bool());

    let mut read_args = vec![path_val];
    let read_result = execute_ffi_call(
        read_ptr,
        &[FfiType::Str],
        FfiType::Str,
        &mut read_args,
        &mut heap,
    )
    .unwrap();
    let read_idx = read_result.as_ptr_idx();
    let HeapObject::String(text) = heap.get(read_idx).unwrap() else {
        panic!("expected read_text to return a heap string");
    };
    assert_eq!(text, "hello world");
}

#[test]
#[cfg(unix)]
fn host_text_io_missing_file_sets_last_error() {
    let mut rt = FfiRuntime::new();
    rt.load_library("").unwrap();
    let read_ptr = rt.resolve_symbol("", "musi_io_read_text").unwrap();
    let error_ptr = rt.resolve_symbol("", "musi_io_last_error").unwrap();

    let mut heap = Heap::new();
    let path_val = Value::from_ptr(heap.alloc_string("/definitely/missing/file.ms".into()));

    let mut read_args = vec![path_val];
    let read_result = execute_ffi_call(
        read_ptr,
        &[FfiType::Str],
        FfiType::Str,
        &mut read_args,
        &mut heap,
    )
    .unwrap();
    assert_eq!(read_result, Value::UNIT);

    let mut error_args = vec![];
    let error = execute_ffi_call(error_ptr, &[], FfiType::Str, &mut error_args, &mut heap).unwrap();
    let error_idx = error.as_ptr_idx();
    let HeapObject::String(message) = heap.get(error_idx).unwrap() else {
        panic!("expected last_error to return a heap string");
    };
    assert!(
        !message.is_empty(),
        "expected missing-file read to populate last_error"
    );
}

#[test]
#[cfg(unix)]
fn call_musi_io_read_text() {
    let mut rt = FfiRuntime::new();
    rt.load_library("").unwrap();
    let fn_ptr = rt.resolve_symbol("", "musi_io_read_text").unwrap();

    let dir = tempfile::tempdir().unwrap();
    let path = dir.path().join("message.txt");
    std::fs::write(&path, "hello from ffi").unwrap();

    let mut heap = Heap::new();
    let path_idx = heap.alloc_string(path.to_string_lossy().into_owned());
    let mut args = vec![Value::from_ptr(path_idx)];
    let result =
        execute_ffi_call(fn_ptr, &[FfiType::Str], FfiType::Str, &mut args, &mut heap).unwrap();

    assert!(result.is_ptr());
    let heap_obj = heap.get(result.as_ptr_idx()).unwrap();
    let crate::heap::HeapObject::String(s) = heap_obj else {
        panic!("expected string result");
    };
    assert_eq!(s, "hello from ffi");
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
