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

use crate::core;

struct ResolvedForeignFn {
    fn_ptr: CodePtr,
    cif: Cif,
}

enum FfiEntry {
    Native(ResolvedForeignFn),
    Builtin(BuiltinFn),
}

type BuiltinFn = fn(&[Value], &mut Heap) -> Result<Value, VmError>;

struct FfiTable {
    entries: Vec<FfiEntry>,
    /// Kept alive so dlsym pointers remain valid.
    _libraries: Vec<Library>,
}

impl FfiTable {
    fn resolve(entries: &[LoadedForeignFn]) -> Result<Self, VmError> {
        let mut lib_cache: HashMap<Box<str>, usize> = HashMap::new();
        let mut libraries: Vec<Library> = vec![];
        let mut resolved: Vec<FfiEntry> = Vec::with_capacity(entries.len());

        for entry in entries {
            // Route musi_rt builtins via dedicated lookup.
            if entry.lib_name.as_ref() == "musi_rt" {
                if let Some(builtin) = lookup_musi_rt(&entry.ext_name) {
                    resolved.push(FfiEntry::Builtin(builtin));
                    continue;
                }
                return Err(VmError::Malformed {
                    desc: format!("unknown musi_rt builtin: {}", entry.ext_name).into_boxed_str(),
                });
            }

            // Check for built-in functions first.
            if let Some(builtin) = lookup_builtin(&entry.ext_name) {
                resolved.push(FfiEntry::Builtin(builtin));
                continue;
            }

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

            resolved.push(FfiEntry::Native(ResolvedForeignFn { fn_ptr, cif }));
        }

        Ok(Self {
            entries: resolved,
            _libraries: libraries,
        })
    }

    fn call(&self, idx: u32, args: &[Value], heap: &mut Heap) -> Result<Value, VmError> {
        let i = usize::try_from(idx).map_err(|_| VmError::Malformed {
            desc: "foreign fn index overflows usize".into(),
        })?;
        let entry = self.entries.get(i).ok_or_else(|| VmError::Malformed {
            desc: format!("foreign fn index {idx} out of bounds").into_boxed_str(),
        })?;

        match entry {
            FfiEntry::Builtin(f) => f(args, heap),
            FfiEntry::Native(native) => {
                let storage = marshal_args(args.to_vec(), heap)?;
                let final_args = build_args(&storage);

                // SAFETY: bytecode-declared signature
                let result: i64 = unsafe { native.cif.call(native.fn_ptr, &final_args) };

                Ok(Value::from_int(result))
            }
        }
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
    fn call_foreign(
        &mut self,
        idx: u32,
        args: &[Value],
        heap: &mut Heap,
    ) -> Result<Value, VmError> {
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

fn marshal_args(args: Vec<Value>, heap: &Heap) -> Result<Vec<MarshaledArg>, VmError> {
    let mut storage = Vec::with_capacity(args.len());
    for val in args {
        if val.is_float() {
            storage.push(MarshaledArg::Float(val.as_float().unwrap()));
        } else if val.is_unit() {
            storage.push(MarshaledArg::Int(0));
        } else if let Ok(n) = val.as_int() {
            storage.push(MarshaledArg::Int(n));
        } else if let Ok(n) = val.as_nat() {
            #[allow(clippy::as_conversions)]
            storage.push(MarshaledArg::Int(n as i64));
        } else if let Ok(b) = val.as_bool() {
            storage.push(MarshaledArg::Int(i64::from(i32::from(b))));
        } else if let Ok(c) = val.as_rune() {
            storage.push(MarshaledArg::Int(i64::from(u32::from(c))));
        } else if let Ok(ptr) = val.as_ref() {
            let obj = heap.get(ptr)?;
            // Wide int refs marshal as their i64 value.
            if let Some(n) = obj.wide_int {
                storage.push(MarshaledArg::Int(n));
            } else if let Some(s) = &obj.string {
                let cstr = CString::new(s.as_bytes()).map_err(|_| VmError::Malformed {
                    desc: "string contains interior null byte for FFI".into(),
                })?;
                let raw = cstr.as_ptr();
                storage.push(MarshaledArg::Ptr { _cstr: cstr, raw });
            } else {
                storage.push(MarshaledArg::Int(0));
            }
        } else if let Ok(id) = val.as_fn_id() {
            storage.push(MarshaledArg::Int(i64::from(id)));
        } else if let Ok(id) = val.as_task_id() {
            storage.push(MarshaledArg::Int(i64::from(id)));
        } else if let Ok(id) = val.as_chan_id() {
            storage.push(MarshaledArg::Int(i64::from(id)));
        } else {
            storage.push(MarshaledArg::Int(0));
        }
    }
    Ok(storage)
}

fn lookup_musi_rt(ext_name: &str) -> Option<BuiltinFn> {
    match ext_name {
        "show" => Some(builtin_show),
        "str_cat" => Some(builtin_str_cat),
        "write" => Some(builtin_write),
        "writeln" => Some(builtin_writeln),
        "int_to_float" => Some(int_to_float),
        "float_to_int" => Some(float_to_int),
        _ => core::lookup(ext_name),
    }
}

fn int_to_float(args: &[Value], _heap: &mut Heap) -> Result<Value, VmError> {
    let n = args
        .first()
        .copied()
        .ok_or_else(|| VmError::Malformed {
            desc: "int_to_float: expected 1 argument".into(),
        })?
        .as_int()?;
    Ok(Value::from_float(n as f64))
}

fn float_to_int(args: &[Value], _heap: &mut Heap) -> Result<Value, VmError> {
    let f = args
        .first()
        .copied()
        .ok_or_else(|| VmError::Malformed {
            desc: "float_to_int: expected 1 argument".into(),
        })?
        .as_float()?;
    Ok(Value::from_int(f as i64))
}

fn lookup_builtin(ext_name: &str) -> Option<BuiltinFn> {
    // Legacy unqualified names (existing builtins).
    match ext_name {
        "musi_show" => return Some(builtin_show),
        "musi_str_cat" => return Some(builtin_str_cat),
        "musi_writeln" => return Some(builtin_writeln),
        "musi_write" => return Some(builtin_write),
        _ => {}
    }
    // Module-qualified: "core::str_len", "core::arr_push", etc.
    if let Some(name) = ext_name.strip_prefix("core::") {
        return core::lookup(name);
    }
    None
}

fn builtin_writeln(args: &[Value], heap: &mut Heap) -> Result<Value, VmError> {
    let val = args.first().copied().ok_or_else(|| VmError::Malformed {
        desc: "musi_writeln: expected 1 argument".into(),
    })?;
    let s = value_to_string(val, heap);
    println!("{s}");
    Ok(Value::UNIT)
}

fn builtin_write(args: &[Value], heap: &mut Heap) -> Result<Value, VmError> {
    let val = args.first().copied().ok_or_else(|| VmError::Malformed {
        desc: "musi_write: expected 1 argument".into(),
    })?;
    let s = value_to_string(val, heap);
    print!("{s}");
    Ok(Value::UNIT)
}

fn builtin_show(args: &[Value], heap: &mut Heap) -> Result<Value, VmError> {
    let val = args.first().copied().ok_or_else(|| VmError::Malformed {
        desc: "musi_show: expected 1 argument".into(),
    })?;
    let s = value_to_string(val, heap);
    let ptr = heap.alloc_string(0, s.into_boxed_str());
    Ok(Value::from_ref(ptr))
}

fn builtin_str_cat(args: &[Value], heap: &mut Heap) -> Result<Value, VmError> {
    let a = args.first().copied().ok_or_else(|| VmError::Malformed {
        desc: "musi_str_cat: expected 2 arguments".into(),
    })?;
    let b = args.get(1).copied().ok_or_else(|| VmError::Malformed {
        desc: "musi_str_cat: expected 2 arguments".into(),
    })?;
    let sa = value_to_string(a, heap);
    let sb = value_to_string(b, heap);
    let result = format!("{sa}{sb}");
    let ptr = heap.alloc_string(0, result.into_boxed_str());
    Ok(Value::from_ref(ptr))
}

fn value_to_string(val: Value, heap: &Heap) -> String {
    if val.is_float() {
        return format!("{}", val.as_float().unwrap());
    }
    if val.is_unit() {
        return "()".to_owned();
    }
    match (
        val.as_bool(),
        val.as_int(),
        val.as_nat(),
        val.as_rune(),
        val.as_ref(),
        val.as_fn_id(),
        val.as_task_id(),
        val.as_chan_id(),
    ) {
        (Ok(b), _, _, _, _, _, _, _) => format!("{b}"),
        (_, Ok(n), _, _, _, _, _, _) => format!("{n}"),
        (_, _, Ok(n), _, _, _, _, _) => format!("{n}"),
        (_, _, _, Ok(c), _, _, _, _) => format!("{c}"),
        (_, _, _, _, Ok(ptr), _, _, _) => match heap.get(ptr) {
            Ok(obj) => {
                if let Some(n) = obj.wide_int {
                    format!("{n}")
                } else if let Some(s) = &obj.string {
                    s.to_string()
                } else {
                    format!("<ref:{ptr}>")
                }
            }
            Err(_) => format!("<ref:{ptr}>"),
        },
        (_, _, _, _, _, Ok(id), _, _) => format!("<fn:{id}>"),
        (_, _, _, _, _, _, Ok(id), _) => format!("<task:{id}>"),
        (_, _, _, _, _, _, _, Ok(id)) => format!("<chan:{id}>"),
        _ => format!("<unknown:{:#018x}>", val.0),
    }
}

fn build_args(storage: &[MarshaledArg]) -> Vec<Arg<'_>> {
    storage
        .iter()
        .map(|m| match m {
            MarshaledArg::Int(n) => arg(n),
            MarshaledArg::Float(f) => arg(f),
            MarshaledArg::Ptr { raw, .. } => arg(raw),
        })
        .collect()
}
