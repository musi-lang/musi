#![allow(unsafe_code)]

use std::cell::RefCell;
use std::ffi::{CStr, CString, c_char};
use std::fs;
use std::io::{self, Write};

thread_local! {
    static LAST_ERROR: RefCell<Option<CString>> = const { RefCell::new(None) };
    static LAST_STRING_RESULT: RefCell<Option<CString>> = const { RefCell::new(None) };
}

fn set_last_error(message: String) {
    let sanitized = sanitize_for_c(message);
    LAST_ERROR.with(|slot| {
        *slot.borrow_mut() =
            Some(CString::new(sanitized).expect("sanitized error contains no NUL"));
    });
}

fn clear_last_error() {
    LAST_ERROR.with(|slot| {
        *slot.borrow_mut() = None;
    });
}

fn last_error_ptr() -> *const c_char {
    LAST_ERROR.with(|slot| {
        slot.borrow()
            .as_ref()
            .map_or(std::ptr::null(), |message| message.as_ptr())
    })
}

fn store_result_string(value: String) -> *const c_char {
    let sanitized = sanitize_for_c(value);
    LAST_STRING_RESULT.with(|slot| {
        *slot.borrow_mut() =
            Some(CString::new(sanitized).expect("sanitized result contains no NUL"));
        slot.borrow()
            .as_ref()
            .map_or(std::ptr::null(), |message| message.as_ptr())
    })
}

fn sanitize_for_c(value: String) -> String {
    value.replace('\0', "\u{fffd}")
}

fn c_string_arg(ptr: *const c_char) -> Result<String, String> {
    if ptr.is_null() {
        return Err("null C string pointer".into());
    }

    let cstr = unsafe { CStr::from_ptr(ptr) };
    Ok(cstr.to_string_lossy().into_owned())
}

fn write_impl(message: *const c_char, newline: bool, stderr: bool) -> bool {
    match c_string_arg(message) {
        Ok(text) => {
            let result = if stderr {
                let mut handle = io::stderr().lock();
                if newline {
                    writeln!(handle, "{text}")
                } else {
                    write!(handle, "{text}")
                }
            } else {
                let mut handle = io::stdout().lock();
                if newline {
                    writeln!(handle, "{text}")
                } else {
                    write!(handle, "{text}")
                }
            };

            match result.and_then(|_| {
                if stderr {
                    io::stderr().lock().flush()
                } else {
                    io::stdout().lock().flush()
                }
            }) {
                Ok(()) => {
                    clear_last_error();
                    true
                }
                Err(err) => {
                    set_last_error(err.to_string());
                    false
                }
            }
        }
        Err(err) => {
            set_last_error(err);
            false
        }
    }
}

fn write_text_impl(path: *const c_char, contents: *const c_char, append: bool) -> bool {
    let path = match c_string_arg(path) {
        Ok(path) => path,
        Err(err) => {
            set_last_error(err);
            return false;
        }
    };
    let contents = match c_string_arg(contents) {
        Ok(contents) => contents,
        Err(err) => {
            set_last_error(err);
            return false;
        }
    };

    let result = if append {
        fs::OpenOptions::new()
            .create(true)
            .append(true)
            .open(&path)
            .and_then(|mut file| file.write_all(contents.as_bytes()))
    } else {
        fs::write(&path, contents)
    };

    match result {
        Ok(()) => {
            clear_last_error();
            true
        }
        Err(err) => {
            set_last_error(err.to_string());
            false
        }
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn musi_io_write(message: *const c_char) -> bool {
    write_impl(message, false, false)
}

#[unsafe(no_mangle)]
pub extern "C" fn musi_io_writeln(message: *const c_char) -> bool {
    write_impl(message, true, false)
}

#[unsafe(no_mangle)]
pub extern "C" fn musi_io_ewrite(message: *const c_char) -> bool {
    write_impl(message, false, true)
}

#[unsafe(no_mangle)]
pub extern "C" fn musi_io_ewriteln(message: *const c_char) -> bool {
    write_impl(message, true, true)
}

#[unsafe(no_mangle)]
pub extern "C" fn musi_io_read_text(path: *const c_char) -> *const c_char {
    let path = match c_string_arg(path) {
        Ok(path) => path,
        Err(err) => {
            set_last_error(err);
            return std::ptr::null();
        }
    };

    match fs::read_to_string(&path) {
        Ok(contents) => {
            clear_last_error();
            store_result_string(contents)
        }
        Err(err) => {
            set_last_error(err.to_string());
            std::ptr::null()
        }
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn musi_io_write_text(path: *const c_char, contents: *const c_char) -> bool {
    write_text_impl(path, contents, false)
}

#[unsafe(no_mangle)]
pub extern "C" fn musi_io_append_text(path: *const c_char, contents: *const c_char) -> bool {
    write_text_impl(path, contents, true)
}

#[unsafe(no_mangle)]
pub extern "C" fn musi_io_last_error() -> *const c_char {
    last_error_ptr()
}

#[cfg(test)]
#[allow(clippy::unwrap_used, clippy::panic, unsafe_code)]
mod tests;
