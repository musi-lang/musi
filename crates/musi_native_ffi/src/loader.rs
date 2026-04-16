use std::ffi::{CStr, CString, c_char, c_int, c_void};
use std::ptr::null;

use musi_vm::{ForeignCall, VmResult};

use crate::{native_library_load_failed, native_symbol_load_failed};

const RTLD_NOW: c_int = 2;

#[cfg_attr(target_os = "linux", link(name = "dl"))]
unsafe extern "C" {
    fn dlopen(filename: *const c_char, flags: c_int) -> *mut c_void;
    fn dlsym(handle: *mut c_void, symbol: *const c_char) -> *mut c_void;
    fn dlerror() -> *const c_char;
}

pub fn resolve_symbol(foreign: &ForeignCall) -> VmResult<*mut c_void> {
    let symbol = CString::new(foreign.symbol()).map_err(|_| {
        native_symbol_load_failed(
            foreign,
            foreign.symbol().into(),
            "symbol contains interior NUL".into(),
        )
    })?;
    let handle = match foreign.link() {
        Some(link) => open_library(foreign, link)?,
        None => open_current_process(foreign)?,
    };
    let symbol_ptr = {
        // SAFETY: `handle` is a live loader handle and `symbol` is a NUL-terminated C string.
        unsafe { dlsym(handle, symbol.as_ptr()) }
    };
    if symbol_ptr.is_null() {
        return Err(native_symbol_load_failed(
            foreign,
            foreign.symbol().into(),
            dlerror_text(),
        ));
    }
    Ok(symbol_ptr)
}

fn open_current_process(foreign: &ForeignCall) -> VmResult<*mut c_void> {
    let handle = {
        // SAFETY: `dlopen(NULL, RTLD_NOW)` asks the loader for the current process symbol table.
        unsafe { dlopen(null(), RTLD_NOW) }
    };
    if handle.is_null() {
        return Err(native_library_load_failed(
            foreign,
            "<process>".into(),
            dlerror_text(),
        ));
    }
    Ok(handle)
}

fn open_library(foreign: &ForeignCall, link: &str) -> VmResult<*mut c_void> {
    if matches!(link, "self" | "process") {
        return open_current_process(foreign);
    }
    let candidates = library_candidates(link);
    for candidate in candidates {
        let library = CString::new(candidate.as_str()).map_err(|_| {
            native_library_load_failed(
                foreign,
                candidate.clone().into(),
                "library path contains interior NUL".into(),
            )
        })?;
        let handle = {
            // SAFETY: `library` is a valid NUL-terminated path string for the system loader.
            unsafe { dlopen(library.as_ptr(), RTLD_NOW) }
        };
        if !handle.is_null() {
            return Ok(handle);
        }
    }
    Err(native_library_load_failed(
        foreign,
        link.into(),
        dlerror_text(),
    ))
}

pub fn library_candidates(link: &str) -> Vec<String> {
    if link == "c" {
        return c_runtime_library_candidates();
    }
    let mut out = vec![link.to_owned()];
    if !link.contains('/') {
        out.push(format!("lib{link}.dylib"));
        out.push(format!("lib{link}.so"));
    }
    out
}

#[cfg(target_os = "macos")]
fn c_runtime_library_candidates() -> Vec<String> {
    vec![
        "libSystem.B.dylib".to_owned(),
        "libc.dylib".to_owned(),
        "libc.so".to_owned(),
    ]
}

#[cfg(target_os = "linux")]
fn c_runtime_library_candidates() -> Vec<String> {
    vec!["libc.so.6".to_owned(), "libc.so".to_owned()]
}

#[cfg(target_os = "windows")]
fn c_runtime_library_candidates() -> Vec<String> {
    vec!["ucrtbase.dll".to_owned(), "msvcrt.dll".to_owned()]
}

#[cfg(not(any(target_os = "macos", target_os = "linux", target_os = "windows")))]
fn c_runtime_library_candidates() -> Vec<String> {
    vec!["c".to_owned()]
}

fn dlerror_text() -> Box<str> {
    let error_ptr = {
        // SAFETY: reading the dynamic loader thread-local error string is side-effect free.
        unsafe { dlerror() }
    };
    if error_ptr.is_null() {
        return "unknown loader error".into();
    }
    let text = {
        // SAFETY: `dlerror` returned a valid NUL-terminated error string pointer.
        unsafe { CStr::from_ptr(error_ptr) }
    };
    text.to_string_lossy().into_owned().into_boxed_str()
}
