//! FFI calling convention: `dlopen` + `libffi` marshaling.
//!
//! Resolves foreign function entries from the loaded module into callable
//! function pointers, and marshals VM [`Value`]s to/from C types.

use std::collections::HashMap;
use std::ffi::{CString, c_void};

use libffi::middle::{Arg, Cif, CodePtr, Type, arg};
use libloading::Library;
use musi_vm::{Heap, HostFunctions, LoadedForeignFn, Value, VmError};

/// A resolved foreign function ready for calling.
struct ResolvedForeignFn {
    fn_ptr: CodePtr,
    cif: Cif,
}

/// Table of resolved foreign functions.
struct FfiTable {
    entries: Vec<ResolvedForeignFn>,
    /// Kept alive so dlsym pointers remain valid.
    _libraries: Vec<Library>,
}

impl FfiTable {
    /// Resolve all foreign function entries.
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

    /// Call a resolved foreign function by index.
    fn call(&self, idx: u32, args: &[Value], heap: &Heap) -> Result<Value, VmError> {
        let i = usize::try_from(idx).map_err(|_| VmError::Malformed {
            desc: "foreign fn index overflows usize".into(),
        })?;
        let entry = self.entries.get(i).ok_or_else(|| VmError::Malformed {
            desc: format!("foreign fn index {idx} out of bounds").into_boxed_str(),
        })?;

        let (final_args, _keep_alive) = marshal_args(args, heap)?;

        // SAFETY: we trust the bytecode's foreign pool declaration to
        // match the actual C function signature. The VM effect system
        // (Unsafe effect) gates access.
        let result: i64 = unsafe { entry.cif.call(entry.fn_ptr, &final_args) };

        Ok(Value::from_int(result))
    }
}

///// The standard host: implements `HostFunctions` via `libffi`.
pub struct StdHost {
    ffi_table: FfiTable,
}

impl StdHost {
    /// Create a new `StdHost`, resolving all foreign functions from the
    /// loaded module.
    ///
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

// ── Helpers ─────────────────────────────────────────────────────────────────

/// Look up a symbol in a loaded library, returning a `CodePtr`.
fn resolve_symbol(lib: &Library, name: &str) -> Result<CodePtr, VmError> {
    // SAFETY: looking up a symbol name from the bytecode's foreign pool.
    // The Musi Unsafe effect gates user-level access to FFI.
    let sym: libloading::Symbol<'_, *mut c_void> =
        unsafe { lib.get(name.as_bytes()) }.map_err(|e| VmError::Malformed {
            desc: format!("dlsym `{name}`: {e}").into_boxed_str(),
        })?;
    Ok(CodePtr(*sym))
}

/// Load a native library by name. Empty string = default C library (libc).
fn load_library(name: &str) -> Result<Library, VmError> {
    if name.is_empty() {
        // SAFETY: loading the default C library (libc).
        // The Musi Unsafe effect gates user-level access to FFI.
        unsafe { Library::new(libloading::library_filename("c")) }.map_err(|e| VmError::Malformed {
            desc: format!("cannot open default C library: {e}").into_boxed_str(),
        })
    } else {
        // SAFETY: loading a user-specified shared library.
        // The Musi Unsafe effect gates user-level access to FFI.
        unsafe { Library::new(name) }.map_err(|e| VmError::Malformed {
            desc: format!("cannot open library `{name}`: {e}").into_boxed_str(),
        })
    }
}

/// Marshal VM values into libffi `Arg` values.
///
/// Returns the arg vector and a `KeepAlive` struct that owns temporary
/// allocations (`CString`s, boxed ints/floats/ptrs) for the lifetime of the call.
fn marshal_args(args: &[Value], heap: &Heap) -> Result<(Vec<Arg>, KeepAlive), VmError> {
    let mut ka = KeepAlive {
        strings: vec![],
        ints: Vec::with_capacity(args.len()),
        floats: vec![],
        ptrs: vec![],
    };

    // First pass: collect all values into owned storage.
    for val in args {
        if val.is_float() {
            ka.floats.push(val.as_float()?);
        } else if val.is_unit() {
            ka.ints.push(0);
        } else if let Ok(n) = val.as_int() {
            ka.ints.push(n);
        } else if let Ok(b) = val.as_bool() {
            ka.ints.push(i64::from(i32::from(b)));
        } else if let Ok(ptr) = val.as_ref() {
            let obj = heap.get(ptr)?;
            if let Some(s) = &obj.string {
                let cstr = CString::new(s.as_bytes()).map_err(|_| VmError::Malformed {
                    desc: "string contains interior null byte for FFI".into(),
                })?;
                ka.ptrs.push(cstr.as_ptr());
                ka.strings.push(cstr);
            } else {
                ka.ints.push(0);
            }
        } else {
            ka.ints.push(i64::from_le_bytes(val.0.to_le_bytes()));
        }
    }

    // Second pass: build Arg references into the owned storage.
    let mut final_args: Vec<Arg> = Vec::with_capacity(args.len());
    let mut int_idx = 0usize;
    let mut float_idx = 0usize;
    let mut ptr_idx = 0usize;

    for val in args {
        if val.is_float() {
            final_args.push(arg(&ka.floats[float_idx]));
            float_idx += 1;
        } else if let Ok(heap_ptr) = val.as_ref() {
            let obj = heap.get(heap_ptr)?;
            if obj.string.is_some() {
                final_args.push(arg(&ka.ptrs[ptr_idx]));
                ptr_idx += 1;
            } else {
                final_args.push(arg(&ka.ints[int_idx]));
                int_idx += 1;
            }
        } else {
            final_args.push(arg(&ka.ints[int_idx]));
            int_idx += 1;
        }
    }

    Ok((final_args, ka))
}

/// Owns temporary allocations that must outlive the FFI call.
struct KeepAlive {
    strings: Vec<CString>,
    ints: Vec<i64>,
    floats: Vec<f64>,
    ptrs: Vec<*const i8>,
}
