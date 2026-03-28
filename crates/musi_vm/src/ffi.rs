#![allow(
    unsafe_code,
    clippy::as_conversions,
    clippy::cast_possible_truncation,
    clippy::cast_possible_wrap,
    clippy::cast_sign_loss
)]

use std::collections::HashMap;
use std::ffi::{CStr, CString, c_char, c_int, c_void};

use libffi::middle::{Arg, Cif, CodePtr, Type};
use libloading::Library;
use music_il::format::FfiType;

use crate::errors::VmError;
use crate::heap::{Heap, HeapObject};
use crate::host_io::{
    musi_io_append_text, musi_io_ewrite, musi_io_ewriteln, musi_io_last_error, musi_io_read_text,
    musi_io_write, musi_io_write_text, musi_io_writeln,
};
use crate::value::Value;

/// Holds loaded shared libraries and resolves symbols at runtime.
pub struct FfiRuntime {
    libraries: HashMap<String, Library>,
}

impl Default for FfiRuntime {
    fn default() -> Self {
        Self::new()
    }
}

impl FfiRuntime {
    #[must_use]
    pub fn new() -> Self {
        Self {
            libraries: HashMap::new(),
        }
    }

    /// Load a shared library by name. Empty names are reserved for builtin host symbols.
    ///
    /// # Errors
    /// Returns [`VmError::FfiLibraryNotFound`] if the library cannot be loaded.
    pub fn load_library(&mut self, name: &str) -> Result<(), VmError> {
        if self.libraries.contains_key(name) {
            return Ok(());
        }
        if name.is_empty() {
            return Ok(());
        }

        let lib = unsafe {
            Library::new(name).map_err(|e| VmError::FfiLibraryNotFound(format!("{name}; {e}")))?
        };
        let _prev = self.libraries.insert(name.into(), lib);
        Ok(())
    }

    /// Resolve a symbol to a function pointer from a previously loaded library.
    ///
    /// # Errors
    /// Returns [`VmError::FfiSymbolNotFound`] if the symbol cannot be found.
    pub fn resolve_symbol(&self, lib_name: &str, symbol: &str) -> Result<*const (), VmError> {
        if lib_name.is_empty() {
            if let Some(ptr) = resolve_builtin_symbol(symbol) {
                return Ok(ptr);
            }
            return Err(VmError::FfiSymbolNotFound(symbol.into()));
        }

        let lib = self
            .libraries
            .get(lib_name)
            .ok_or_else(|| VmError::FfiLibraryNotFound(lib_name.into()))?;
        let sym_bytes = symbol.as_bytes();
        let func: libloading::Symbol<'_, *const ()> = unsafe {
            lib.get(sym_bytes)
                .map_err(|e| VmError::FfiSymbolNotFound(format!("{symbol}; {e}")))?
        };
        Ok(*func)
    }
}

fn resolve_builtin_symbol(symbol: &str) -> Option<*const ()> {
    match symbol {
        "musi_io_last_error" => Some(musi_io_last_error as *const ()),
        "musi_io_write" => Some(musi_io_write as *const ()),
        "musi_io_writeln" => Some(musi_io_writeln as *const ()),
        "musi_io_ewrite" => Some(musi_io_ewrite as *const ()),
        "musi_io_ewriteln" => Some(musi_io_ewriteln as *const ()),
        "musi_io_write_text" => Some(musi_io_write_text as *const ()),
        "musi_io_append_text" => Some(musi_io_append_text as *const ()),
        "musi_io_read_text" => Some(musi_io_read_text as *const ()),
        _ => None,
    }
}

fn ffi_type_to_libffi(ty: FfiType) -> Type {
    match ty {
        FfiType::Void => Type::void(),
        FfiType::Int => Type::i64(),
        FfiType::Float => Type::f64(),
        FfiType::Bool => Type::c_int(),
        FfiType::Ptr | FfiType::Str => Type::pointer(),
    }
}

/// Holds a C value marshaled from a Musi [`Value`].
///
/// The enum keeps the data alive so that [`Arg`] pointers into it remain valid
/// for the duration of the FFI call.
pub enum FfiArg {
    Int(i64),
    Float(f64),
    Bool(c_int),
    Ptr(*const c_void),
    /// Keeps the `CString` alive; the `*const c_char` points into it.
    Str(CString, *const c_char),
}

/// Marshal a Musi [`Value`] into a C-compatible [`FfiArg`].
///
/// # Errors
/// Returns [`VmError::TypeError`] if the value does not match the expected FFI type.
pub fn value_to_ffi_arg(value: Value, ty: FfiType, heap: &Heap) -> Result<FfiArg, VmError> {
    match ty {
        FfiType::Void => Ok(FfiArg::Int(0)),
        FfiType::Int => {
            if !value.is_int() {
                return Err(VmError::TypeError {
                    expected: "`Int` for FFI",
                    found: "non-`Int`",
                });
            }
            Ok(FfiArg::Int(value.as_int()))
        }
        FfiType::Float => {
            if !value.is_float() {
                return Err(VmError::TypeError {
                    expected: "`Float` for FFI",
                    found: "non-`Float`",
                });
            }
            Ok(FfiArg::Float(value.as_float()))
        }
        FfiType::Bool => {
            if !value.is_bool() {
                return Err(VmError::TypeError {
                    expected: "`Bool` for FFI",
                    found: "non-`Bool`",
                });
            }
            Ok(FfiArg::Bool(c_int::from(value.as_bool())))
        }
        FfiType::Ptr => {
            if !value.is_ptr() {
                return Err(VmError::TypeError {
                    expected: "`CPtr` for FFI",
                    found: "non-ptr",
                });
            }
            let idx = value.as_ptr_idx();
            let ptr = match heap.get(idx) {
                Some(HeapObject::CPtr(p)) => p.ptr.cast_const(),
                Some(_) => {
                    return Err(VmError::TypeError {
                        expected: "`CPtr`",
                        found: "non-CPtr heap object",
                    });
                }
                None => {
                    return Err(VmError::InvalidHeapIndex(idx));
                }
            };
            Ok(FfiArg::Ptr(ptr))
        }
        FfiType::Str => {
            if !value.is_ptr() {
                return Err(VmError::TypeError {
                    expected: "`String` for FFI",
                    found: "non-ptr",
                });
            }
            let idx = value.as_ptr_idx();
            let s = match heap.get(idx) {
                Some(HeapObject::String(s)) => s.data.clone(),
                Some(_) => {
                    return Err(VmError::TypeError {
                        expected: "String",
                        found: "non-String heap object",
                    });
                }
                None => {
                    return Err(VmError::InvalidHeapIndex(idx));
                }
            };
            let cstring = CString::new(s).map_err(|_| VmError::TypeError {
                expected: "null-free string for FFI",
                found: "string with interior NUL",
            })?;
            let ptr = cstring.as_ptr();
            Ok(FfiArg::Str(cstring, ptr))
        }
    }
}

/// Convert an [`FfiArg`] into a [`libffi::middle::Arg`].
#[must_use]
pub fn ffi_arg_to_libffi(ffi_arg: &FfiArg) -> Arg<'_> {
    match ffi_arg {
        FfiArg::Int(v) => Arg::new(v),
        FfiArg::Float(v) => Arg::new(v),
        FfiArg::Bool(v) => Arg::new(v),
        FfiArg::Ptr(v) => Arg::new(v),
        FfiArg::Str(_cs, ptr) => Arg::new(ptr),
    }
}

/// Marshal a C return value back into a Musi [`Value`].
#[must_use]
pub fn ffi_result_to_value(ty: FfiType, raw: u64, heap: &mut Heap) -> Value {
    match ty {
        FfiType::Void => Value::UNIT,
        FfiType::Int => Value::from_int(raw as i64),
        FfiType::Float => Value::from_float(f64::from_bits(raw)),
        FfiType::Bool => Value::from_bool(raw != 0),
        FfiType::Ptr => {
            let ptr = raw as *mut c_void;
            if ptr.is_null() {
                Value::UNIT
            } else {
                Value::from_ptr(heap.alloc_cptr(ptr))
            }
        }
        FfiType::Str => {
            let ptr = raw as *const c_char;
            if ptr.is_null() {
                Value::UNIT
            } else {
                let cstr = unsafe { CStr::from_ptr(ptr) };
                let s = cstr.to_string_lossy().into_owned();
                Value::from_ptr(heap.alloc_string(s))
            }
        }
    }
}

/// Execute an FFI call using libffi.
///
/// # Errors
/// Returns a [`VmError`] if arguments cannot be marshaled.
pub fn execute_ffi_call(
    fn_ptr: *const (),
    param_types: &[FfiType],
    return_type: FfiType,
    args: &mut [Value],
    heap: &mut Heap,
) -> Result<Value, VmError> {
    let arg_types: Vec<Type> = param_types.iter().map(|t| ffi_type_to_libffi(*t)).collect();
    let ret_type = ffi_type_to_libffi(return_type);
    let cif = Cif::new(arg_types, ret_type);

    let mut ffi_args: Vec<FfiArg> = Vec::with_capacity(args.len());
    for (value, ty) in args.iter().zip(param_types.iter()) {
        ffi_args.push(value_to_ffi_arg(*value, *ty, heap)?);
    }
    let libffi_args: Vec<Arg<'_>> = ffi_args.iter().map(ffi_arg_to_libffi).collect();

    let raw: u64 = unsafe { cif.call(CodePtr(fn_ptr.cast_mut().cast::<c_void>()), &libffi_args) };

    Ok(ffi_result_to_value(return_type, raw, heap))
}

#[cfg(test)]
mod tests;
