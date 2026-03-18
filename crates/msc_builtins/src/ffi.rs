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
use std::ffi::{CStr, CString, c_void};

use libffi::middle::{Arg, Cif, CodePtr, Type, arg};
use libloading::Library;
use msc_vm::{Heap, HeapPayload, HostFunctions, LoadedForeignFn, LoadedType, Value, VmError};

use crate::core;

// Type tags matching §11.3 of the Musi bytecode spec (mirrored from loader.rs).
const TAG_TY_UNIT: u8 = 0x01;
const TAG_TY_BOOL: u8 = 0x02;
const TAG_TY_I8: u8 = 0x03;
const TAG_TY_I16: u8 = 0x04;
const TAG_TY_I32: u8 = 0x05;
const TAG_TY_F32: u8 = 0x0B;
const TAG_TY_F64: u8 = 0x0C;
const TAG_TY_PTR: u8 = 0x0E;

struct ResolvedForeignFn {
    fn_ptr: CodePtr,
    cif: Cif,
    /// Tag of the declared return type — drives return-value unmarshaling.
    ret_tag: u8,
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

/// Map a type pool entry to its libffi `Type`.
///
/// Unknown or composite tags fall back to `i64` — a conservative ABI choice
/// for integer-class values on both x86-64 and ARM64.
fn type_to_ffi(types: &[LoadedType], type_id: u32) -> Type {
    let tag = types
        .get(usize::try_from(type_id).unwrap_or(usize::MAX))
        .map_or(0, |t| t.tag);
    match tag {
        TAG_TY_F32 | TAG_TY_F64 => Type::f64(),
        TAG_TY_UNIT => Type::void(),
        TAG_TY_BOOL | TAG_TY_I8 | TAG_TY_I16 | TAG_TY_I32 => Type::i32(),
        TAG_TY_PTR => Type::pointer(),
        // Int, Nat (i64/u64), Rune, Any, and composite types all use i64.
        _ => Type::i64(),
    }
}

impl FfiTable {
    fn resolve(entries: &[LoadedForeignFn], types: &[LoadedType]) -> Result<Self, VmError> {
        let mut lib_cache: HashMap<Box<str>, usize> = HashMap::new();
        let mut libraries: Vec<Library> = vec![];
        let mut resolved: Vec<FfiEntry> = Vec::with_capacity(entries.len());

        for entry in entries {
            // Route msc_rt builtins via dedicated lookup.
            if entry.lib_name.as_ref() == "msc_rt" {
                if let Some(builtin) = lookup_msc_rt(&entry.ext_name) {
                    resolved.push(FfiEntry::Builtin(builtin));
                    continue;
                }
                return Err(VmError::Malformed {
                    desc: format!("unknown msc_rt builtin: {}", entry.ext_name).into_boxed_str(),
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

            let param_types: Vec<Type> = entry
                .param_type_ids
                .iter()
                .map(|&tid| type_to_ffi(types, tid))
                .collect();
            let ret_tag = types
                .get(usize::try_from(entry.ret_type_id).unwrap_or(usize::MAX))
                .map_or(0, |t| t.tag);
            let ret_type = type_to_ffi(types, entry.ret_type_id);

            let cif = Cif::new(param_types, ret_type);

            resolved.push(FfiEntry::Native(ResolvedForeignFn {
                fn_ptr,
                cif,
                ret_tag,
            }));
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
                let value = unsafe { dispatch_native(native, &final_args, heap) };
                value
            }
        }
    }
}

/// Dispatch a native FFI call and unmarshal the return value based on `ret_tag`.
///
/// # Safety
///
/// Caller must ensure `native.fn_ptr` and `native.cif` match the actual C
/// function signature, as declared in the bytecode's foreign pool.
unsafe fn dispatch_native(
    native: &ResolvedForeignFn,
    args: &[Arg<'_>],
    heap: &mut Heap,
) -> Result<Value, VmError> {
    match native.ret_tag {
        TAG_TY_F32 | TAG_TY_F64 => {
            // SAFETY: CIF declares f64 return.
            let r: f64 = unsafe { native.cif.call(native.fn_ptr, args) };
            Ok(Value::from_float(r))
        }
        TAG_TY_UNIT => {
            // SAFETY: CIF declares void return.
            let (): () = unsafe { native.cif.call(native.fn_ptr, args) };
            Ok(Value::UNIT)
        }
        TAG_TY_BOOL | TAG_TY_I8 | TAG_TY_I16 | TAG_TY_I32 => {
            // SAFETY: CIF declares i32 return.
            let r: i32 = unsafe { native.cif.call(native.fn_ptr, args) };
            Ok(Value::from_int(i64::from(r)))
        }
        TAG_TY_PTR => {
            // SAFETY: CIF declares pointer return.
            let ptr: *const i8 = unsafe { native.cif.call(native.fn_ptr, args) };
            if ptr.is_null() {
                Ok(Value::UNIT)
            } else {
                // SAFETY: non-null pointer returned by C — trust the bytecode declaration.
                let s = unsafe { CStr::from_ptr(ptr) }
                    .to_string_lossy()
                    .into_owned()
                    .into_boxed_str();
                let heap_ptr = heap.alloc_string(0, s);
                Ok(Value::from_ref(heap_ptr))
            }
        }
        _ => {
            // SAFETY: CIF declares i64 return.
            let r: i64 = unsafe { native.cif.call(native.fn_ptr, args) };
            Ok(Value::from_int(r))
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
    pub fn new(foreign_fns: &[LoadedForeignFn], types: &[LoadedType]) -> Result<Self, VmError> {
        let ffi_table = FfiTable::resolve(foreign_fns, types)?;
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
            storage.push(MarshaledArg::Float(val.as_float()?));
        } else if val.is_unit() {
            storage.push(MarshaledArg::Int(0));
        } else if let Ok(n) = val.as_int() {
            storage.push(MarshaledArg::Int(n));
        } else if let Ok(n) = val.as_nat() {
            storage.push(MarshaledArg::Int(n.cast_signed()));
        } else if let Ok(b) = val.as_bool() {
            storage.push(MarshaledArg::Int(i64::from(i32::from(b))));
        } else if let Ok(c) = val.as_rune() {
            storage.push(MarshaledArg::Int(i64::from(u32::from(c))));
        } else if let Ok(ptr) = val.as_ref() {
            let obj = heap.get(ptr)?;
            match &obj.payload {
                HeapPayload::BoxedInt(n) => storage.push(MarshaledArg::Int(*n)),
                HeapPayload::BoxedNat(n) => storage.push(MarshaledArg::Int(*n)),
                HeapPayload::Str { data, .. } => {
                    let cstr = CString::new(data.as_bytes()).map_err(|_| VmError::Malformed {
                        desc: "string contains interior null byte for FFI".into(),
                    })?;
                    let raw = cstr.as_ptr();
                    storage.push(MarshaledArg::Ptr { _cstr: cstr, raw });
                }
                _ => storage.push(MarshaledArg::Int(0)),
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

fn lookup_msc_rt(ext_name: &str) -> Option<BuiltinFn> {
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
    // i64 -> f64: precision loss on values > 2^53 is intentional — this is a
    // best-effort numeric conversion for FFI callers, not a lossless cast.
    // `f64::from` is not available for i64; no safe alternative exists without
    // going through a lossy intermediate. Verified false positive.
    #[allow(clippy::cast_precision_loss, clippy::as_conversions)]
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
    // Truncate toward zero, clamp to i64 bounds, then convert. The f64 boundary
    // constants are precomputed to avoid inline `as` casts. Values outside
    // [i64::MIN, i64::MAX] or NaN saturate to the nearest bound.
    // i64::MIN is exactly representable in f64; i64::MAX rounds up slightly but
    // is safe for clamping since any f64 >= this constant must saturate to i64::MAX.
    const I64_MIN_F64: f64 = -9_223_372_036_854_775_808.0_f64;
    const I64_MAX_F64: f64 = 9_223_372_036_854_775_807.0_f64;
    if f.is_nan() || f < I64_MIN_F64 {
        return Ok(Value::from_int(i64::MIN));
    }
    if f > I64_MAX_F64 {
        return Ok(Value::from_int(i64::MAX));
    }
    #[allow(clippy::cast_possible_truncation, clippy::as_conversions)]
    // SAFETY: f is finite and clamped to [I64_MIN_F64, I64_MAX_F64], which fits
    // in i64 after truncation. The `as` cast is the only way to convert f64->i64.
    Ok(Value::from_int(f.trunc() as i64))
}

fn lookup_builtin(ext_name: &str) -> Option<BuiltinFn> {
    // Legacy unqualified names (existing builtins).
    match ext_name {
        "msc_show" => return Some(builtin_show),
        "msc_str_cat" => return Some(builtin_str_cat),
        "msc_writeln" => return Some(builtin_writeln),
        "msc_write" => return Some(builtin_write),
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
        desc: "msc_writeln: expected 1 argument".into(),
    })?;
    let s = value_to_string(val, heap);
    println!("{s}");
    Ok(Value::UNIT)
}

fn builtin_write(args: &[Value], heap: &mut Heap) -> Result<Value, VmError> {
    let val = args.first().copied().ok_or_else(|| VmError::Malformed {
        desc: "msc_write: expected 1 argument".into(),
    })?;
    let s = value_to_string(val, heap);
    print!("{s}");
    Ok(Value::UNIT)
}

fn builtin_show(args: &[Value], heap: &mut Heap) -> Result<Value, VmError> {
    let val = args.first().copied().ok_or_else(|| VmError::Malformed {
        desc: "msc_show: expected 1 argument".into(),
    })?;
    let s = value_to_string(val, heap);
    let ptr = heap.alloc_string(0, s.into_boxed_str());
    Ok(Value::from_ref(ptr))
}

fn builtin_str_cat(args: &[Value], heap: &mut Heap) -> Result<Value, VmError> {
    let a = args.first().copied().ok_or_else(|| VmError::Malformed {
        desc: "msc_str_cat: expected 2 arguments".into(),
    })?;
    let b = args.get(1).copied().ok_or_else(|| VmError::Malformed {
        desc: "msc_str_cat: expected 2 arguments".into(),
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
        (_, _, _, _, Ok(ptr), _, _, _) => heap.get(ptr).map_or_else(
            |_| format!("<ref:{ptr}>"),
            |obj| match &obj.payload {
                HeapPayload::BoxedInt(n) => format!("{n}"),
                HeapPayload::BoxedNat(n) => format!("{}", n.cast_unsigned()),
                HeapPayload::Str { data, .. } => data.as_ref().to_owned(),
                _ => format!("<ref:{ptr}>"),
            },
        ),
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
