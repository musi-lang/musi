//! FFI calling convention: `dlopen` + `libffi` marshaling.
//!
//! Resolves foreign function entries from the loaded module into callable
//! function pointers, and marshals VM [`Value`]s to/from C types.
//!
//! # Safety
//!
//! All `unsafe` blocks in this module trust the bytecode's foreign pool
//! declarations to match actual C function signatures. The Musi `Unsafe`
//! effect gates user-level access to FFI.

use std::collections::HashMap;
use std::ffi::{CString, c_void};

use libffi::middle::{Arg, Cif, CodePtr, Type, arg};
use libloading::Library;
use musi_vm::{Heap, HostFunctions, LoadedForeignFn, Value, VmError};

struct ResolvedForeignFn {
    fn_ptr: CodePtr,
    cif: Cif,
}

struct FfiTable {
    entries: Vec<ResolvedForeignFn>,
    /// Kept alive so dlsym pointers remain valid.
    _libraries: Vec<Library>,
}

impl FfiTable {
    fn resolve(entries: &[LoadedForeignFn]) -> Result<Self, VmError> {
        let mut lib_cache: HashMap<Box<str>, usize> = HashMap::new();
        let mut libraries: Vec<Library> = vec![];
        let mut resolved: Vec<ResolvedForeignFn> = Vec::with_capacity(entries.len());

        for entry in entries {
            let lib_idx = if let Some(&idx) = lib_cache.get(&entry.lib_name) {
                idx
            } else {
                let lib = load_library(&entry.lib_name)?;
                let idx = libraries.len();
                libraries.push(lib);
                let _ = lib_cache.insert(entry.lib_name.clone(), idx);
                idx
            };

            let fn_ptr = resolve_symbol(&libraries[lib_idx], &entry.ext_name)?;

            let param_types: Vec<Type> = (0..entry.param_count).map(|_| Type::i64()).collect();
            let ret_type = Type::i64();

            let cif = Cif::new(param_types, ret_type);

            resolved.push(ResolvedForeignFn { fn_ptr, cif });
        }

        Ok(Self {
            entries: resolved,
            _libraries: libraries,
        })
    }

    fn call(&self, idx: u32, args: &[Value], heap: &Heap) -> Result<Value, VmError> {
        let i = usize::try_from(idx).map_err(|_| VmError::Malformed {
            desc: "foreign fn index overflows usize".into(),
        })?;
        let entry = self.entries.get(i).ok_or_else(|| VmError::Malformed {
            desc: format!("foreign fn index {idx} out of bounds").into_boxed_str(),
        })?;

        let (final_args, _keep_alive) = marshal_args(args, heap)?;

        // SAFETY: bytecode-declared signature
        let result: i64 = unsafe { entry.cif.call(entry.fn_ptr, &final_args) };

        Ok(Value::from_int(result))
    }
}

pub struct StdHost {
    ffi_table: FfiTable,
}

impl StdHost {
    /// # Errors
    ///
    /// Returns `VmError` if any library or symbol cannot be resolved.
    pub fn new(foreign_fns: &[LoadedForeignFn]) -> Result<Self, VmError> {
        let ffi_table = FfiTable::resolve(foreign_fns)?;
        Ok(Self { ffi_table })
    }
}

impl HostFunctions for StdHost {
    fn call_foreign(&mut self, idx: u32, args: &[Value], heap: &Heap) -> Result<Value, VmError> {
        self.ffi_table.call(idx, args, heap)
    }
}

fn resolve_symbol(lib: &Library, name: &str) -> Result<CodePtr, VmError> {
    // SAFETY: bytecode-declared symbol name
    let sym: libloading::Symbol<'_, *mut c_void> =
        unsafe { lib.get(name.as_bytes()) }.map_err(|e| VmError::Malformed {
            desc: format!("dlsym `{name}`: {e}").into_boxed_str(),
        })?;
    Ok(CodePtr(*sym))
}

/// Load a native library by name. Empty string = default C library (libc).
fn load_library(name: &str) -> Result<Library, VmError> {
    if name.is_empty() {
        // SAFETY: bytecode-declared library
        unsafe { Library::new(libloading::library_filename("c")) }.map_err(|e| VmError::Malformed {
            desc: format!("cannot open default C library: {e}").into_boxed_str(),
        })
    } else {
        // SAFETY: bytecode-declared library
        unsafe { Library::new(name) }.map_err(|e| VmError::Malformed {
            desc: format!("cannot open library `{name}`: {e}").into_boxed_str(),
        })
    }
}

/// Intermediate storage for a single marshaled argument.
enum MarshaledArg {
    Int(i64),
    Float(f64),
    Ptr { _cstr: CString, raw: *const i8 },
}

fn marshal_args(args: &[Value], heap: &Heap) -> Result<(Vec<Arg>, Vec<MarshaledArg>), VmError> {
    let mut storage: Vec<MarshaledArg> = Vec::with_capacity(args.len());

    for val in args {
        if val.is_float() {
            storage.push(MarshaledArg::Float(val.as_float()?));
        } else if val.is_unit() {
            storage.push(MarshaledArg::Int(0));
        } else if let Ok(n) = val.as_int() {
            storage.push(MarshaledArg::Int(n));
        } else if let Ok(b) = val.as_bool() {
            storage.push(MarshaledArg::Int(i64::from(i32::from(b))));
        } else if let Ok(ptr) = val.as_ref() {
            let obj = heap.get(ptr)?;
            if let Some(s) = &obj.string {
                let cstr = CString::new(s.as_bytes()).map_err(|_| VmError::Malformed {
                    desc: "string contains interior null byte for FFI".into(),
                })?;
                let raw = cstr.as_ptr();
                storage.push(MarshaledArg::Ptr { _cstr: cstr, raw });
            } else {
                storage.push(MarshaledArg::Int(0));
            }
        } else {
            storage.push(MarshaledArg::Int(i64::from_le_bytes(val.0.to_le_bytes())));
        }
    }

    let final_args: Vec<Arg> = storage
        .iter()
        .map(|m| match m {
            MarshaledArg::Int(n) => arg(n),
            MarshaledArg::Float(f) => arg(f),
            MarshaledArg::Ptr { raw, .. } => arg(raw),
        })
        .collect();

    Ok((final_args, storage))
}
