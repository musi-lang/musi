use std::ffi::{CStr, CString};

use super::*;

#[test]
fn read_text_roundtrip() {
    let dir = tempfile::tempdir().unwrap();
    let path = dir.path().join("hello.txt");
    std::fs::write(&path, "hello").unwrap();

    let path_c = CString::new(path.to_string_lossy().as_bytes()).unwrap();
    let ptr = musi_io_read_text(path_c.as_ptr());
    assert!(!ptr.is_null());

    let value = unsafe { CStr::from_ptr(ptr) }.to_string_lossy().into_owned();
    assert_eq!(value, "hello");
}

#[test]
fn write_and_append_text_roundtrip() {
    let dir = tempfile::tempdir().unwrap();
    let path = dir.path().join("notes.txt");
    let path_c = CString::new(path.to_string_lossy().as_bytes()).unwrap();
    let hello = CString::new("hello").unwrap();
    let world = CString::new(" world").unwrap();

    assert!(musi_io_write_text(path_c.as_ptr(), hello.as_ptr()));
    assert!(musi_io_append_text(path_c.as_ptr(), world.as_ptr()));

    let contents = std::fs::read_to_string(path).unwrap();
    assert_eq!(contents, "hello world");
}

#[test]
fn read_text_failure_sets_last_error() {
    let path_c = CString::new("/definitely/not/real.txt").unwrap();
    let ptr = musi_io_read_text(path_c.as_ptr());
    assert!(ptr.is_null());

    let err_ptr = musi_io_last_error();
    assert!(!err_ptr.is_null());
    let err = unsafe { CStr::from_ptr(err_ptr) }.to_string_lossy().into_owned();
    assert!(!err.is_empty());
}
