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
use msc_bc::type_tag::*;
use msc_vm::{
    Heap, HeapPayload, HostFunctions, LoadedForeignFn, LoadedType, Value, VmError, VmResult,
};

use std::cell::RefCell;

use crate::core;

thread_local! {
    static TEST_FILTER: RefCell<String> = const { RefCell::new(String::new()) };
}

/// Set the test name filter for the current thread. Called by `musi test --name`.
pub fn set_test_filter(filter: &str) {
    TEST_FILTER.with(|f| filter.clone_into(&mut f.borrow_mut()));
}

fn builtin_test_filter(_args: &[Value], heap: &mut Heap) -> Value {
    let filter = TEST_FILTER.with(|f| f.borrow().clone());
    let ptr = heap.alloc_string(0, filter.into_boxed_str());
    Value::from_ref(ptr)
}

struct ResolvedForeignFn {
    fn_ptr: CodePtr,
    cif: Cif,
    /// Tag of the declared return type - drives return-value unmarshaling.
    ret_tag: u8,
    /// Per-parameter type tags for type-directed marshaling.
    param_tags: Vec<u8>,
    /// Per-parameter `type_ids` (indexes into the loaded type pool).
    param_type_ids: Vec<u32>,
    /// Return `type_id` (index into the loaded type pool).
    ret_type_id: u32,
}

enum FfiEntry {
    Native(ResolvedForeignFn),
    Builtin(BuiltinFn),
}

type BuiltinFn = fn(&[Value], &mut Heap) -> VmResult<Value>;

struct FfiTable {
    entries: Vec<FfiEntry>,
    /// Kept alive so dlsym pointers remain valid.
    _libraries: Vec<Library>,
    /// Copy of loaded types for struct marshal/unmarshal.
    types: Vec<LoadedType>,
}

/// Map a type pool entry to its libffi `Type`.
///
/// Unknown or composite tags fall back to `i64` - a conservative ABI choice
/// for integer-class values on both x86-64 and ARM64.
fn type_to_ffi(types: &[LoadedType], type_id: u32) -> Type {
    let idx = usize::try_from(type_id).unwrap_or(usize::MAX);
    let tag = types.get(idx).map_or(0, |t| t.tag);
    match tag {
        TAG_F32 | TAG_F64 => Type::f64(),
        TAG_UNIT => Type::void(),
        TAG_BOOL | TAG_I8 | TAG_I16 | TAG_I32 => Type::i32(),
        TAG_PTR => Type::pointer(),
        TAG_CSTRUCT => build_ffi_struct_type(types, idx),
        // Int, Nat (i64/u64), Rune, Any, and composite types all use i64.
        _ => Type::i64(),
    }
}

/// Extract the inner `type_id` from a `TAG_PTR` type entry (4 bytes of data).
fn ptr_inner_type_id(types: &[LoadedType], ptr_type_id: u32) -> Option<u32> {
    let idx = usize::try_from(ptr_type_id).ok()?;
    let entry = types.get(idx)?;
    if entry.tag != TAG_PTR || entry.data.len() < 4 {
        return None;
    }
    Some(u32::from_be_bytes([
        entry.data[0],
        entry.data[1],
        entry.data[2],
        entry.data[3],
    ]))
}

/// Build a libffi struct type from a CSTRUCT type entry.
fn build_ffi_struct_type(types: &[LoadedType], idx: usize) -> Type {
    let type_data = &types[idx].data;
    if type_data.len() < 2 {
        return Type::structure(vec![]);
    }
    let field_count = usize::from(u16::from_be_bytes([type_data[0], type_data[1]]));
    let mut field_types = Vec::with_capacity(field_count);
    for i in 0..field_count {
        let off = 2 + i * 8;
        if off + 4 > type_data.len() {
            break;
        }
        let field_type_id = u32::from_be_bytes([
            type_data[off],
            type_data[off + 1],
            type_data[off + 2],
            type_data[off + 3],
        ]);
        field_types.push(type_to_ffi(types, field_type_id));
    }
    Type::structure(field_types)
}

impl FfiTable {
    fn resolve(entries: &[LoadedForeignFn], types: &[LoadedType]) -> VmResult<Self> {
        let mut lib_cache: HashMap<Box<str>, usize> = HashMap::new();
        let mut libraries = vec![];
        let mut resolved = Vec::with_capacity(entries.len());

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
                let kind_str = match entry.link_kind_tag {
                    0x01 => "static",
                    0x02 => "framework",
                    _ => "", // 0x00 = dynamic (default)
                };
                let lib = load_library(&entry.lib_name, kind_str)?;
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
            let param_tags: Vec<u8> = entry
                .param_type_ids
                .iter()
                .map(|&tid| {
                    types
                        .get(usize::try_from(tid).unwrap_or(usize::MAX))
                        .map_or(0, |t| t.tag)
                })
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
                param_tags,
                param_type_ids: entry.param_type_ids.clone(),
                ret_type_id: entry.ret_type_id,
            }));
        }

        Ok(Self {
            entries: resolved,
            _libraries: libraries,
            types: types.to_vec(),
        })
    }

    fn call(&self, idx: u32, args: &[Value], heap: &mut Heap) -> VmResult<Value> {
        let i = usize::try_from(idx).map_err(|_| VmError::Malformed {
            desc: "foreign fn index overflows usize".into(),
        })?;
        let entry = self.entries.get(i).ok_or_else(|| VmError::Malformed {
            desc: format!("foreign fn index {idx} out of bounds").into_boxed_str(),
        })?;

        match entry {
            FfiEntry::Builtin(f) => f(args, heap),
            FfiEntry::Native(native) => {
                let storage = marshal_args_typed(
                    args,
                    &native.param_tags,
                    &native.param_type_ids,
                    &self.types,
                    heap,
                )?;
                let final_args = build_args(&storage);

                // SAFETY: bytecode-declared signature
                unsafe { dispatch_native(native, &final_args, &self.types, heap) }
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
    types: &[LoadedType],
    heap: &mut Heap,
) -> VmResult<Value> {
    match native.ret_tag {
        TAG_F32 | TAG_F64 => {
            // SAFETY: CIF declares f64 return.
            let r: f64 = unsafe { native.cif.call(native.fn_ptr, args) };
            Ok(Value::from_float(r))
        }
        TAG_UNIT => {
            // SAFETY: CIF declares void return.
            let (): () = unsafe { native.cif.call(native.fn_ptr, args) };
            Ok(Value::UNIT)
        }
        TAG_BOOL | TAG_I8 | TAG_I16 | TAG_I32 => {
            // SAFETY: CIF declares i32 return.
            let r: i32 = unsafe { native.cif.call(native.fn_ptr, args) };
            Ok(Value::from_int(i64::from(r)))
        }
        TAG_PTR => {
            // SAFETY: CIF declares pointer return.
            let ptr: *const i8 = unsafe { native.cif.call(native.fn_ptr, args) };
            if ptr.is_null() {
                Ok(Value::UNIT)
            } else {
                // SAFETY: non-null pointer returned by C - trust the bytecode declaration.
                let s = unsafe { CStr::from_ptr(ptr) }
                    .to_string_lossy()
                    .into_owned()
                    .into_boxed_str();
                let heap_ptr = heap.alloc_string(0, s);
                Ok(Value::from_ref(heap_ptr))
            }
        }
        TAG_CSTRUCT => {
            // SAFETY: CIF declares struct return - libffi returns bytes.
            let r: i64 = unsafe { native.cif.call(native.fn_ptr, args) };
            let buf = r.to_ne_bytes();
            unmarshal_struct_return(&buf, native.ret_type_id, types, heap)
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
    pub fn new(foreign_fns: &[LoadedForeignFn], types: &[LoadedType]) -> VmResult<Self> {
        let ffi_table = FfiTable::resolve(foreign_fns, types)?;
        Ok(Self { ffi_table })
    }
}

impl HostFunctions for StdHost {
    fn call_foreign(&mut self, idx: u32, args: &[Value], heap: &mut Heap) -> VmResult<Value> {
        self.ffi_table.call(idx, args, heap)
    }
}

fn resolve_symbol(lib: &Library, name: &str) -> VmResult<CodePtr> {
    // SAFETY: bytecode-declared symbol name
    let sym: libloading::Symbol<'_, *mut c_void> =
        unsafe { lib.get(name.as_bytes()) }.map_err(|e| VmError::Malformed {
            desc: format!("dlsym `{name}`: {e}").into_boxed_str(),
        })?;
    Ok(CodePtr(*sym))
}

/// Load a native library by name and kind hint.
///
/// `kind` controls how `name` is resolved to a file path:
/// - `"dylib"` or empty with non-empty name: use `libloading::library_filename`
///   (auto-prepends "lib" and appends ".dylib"/".so").
/// - `"framework"` (macOS only): open `/System/Library/Frameworks/{name}.framework/{name}`.
/// - `"raw"`: open `name` as-is with no transformation.
/// - empty name: open the default C library regardless of kind.
fn load_library(name: &str, kind: &str) -> VmResult<Library> {
    if name.is_empty() {
        // SAFETY: bytecode-declared library
        return unsafe { Library::new(libloading::library_filename("c")) }.map_err(|e| {
            VmError::Malformed {
                desc: format!("cannot open default C library: {e}").into_boxed_str(),
            }
        });
    }

    match kind {
        "raw" => {
            // SAFETY: bytecode-declared library path
            unsafe { Library::new(name) }.map_err(|e| VmError::Malformed {
                desc: format!("cannot open library (raw) `{name}`: {e}").into_boxed_str(),
            })
        }
        "framework" => {
            let path = format!("/System/Library/Frameworks/{name}.framework/{name}");
            // SAFETY: bytecode-declared framework path
            unsafe { Library::new(&path) }.map_err(|e| VmError::Malformed {
                desc: format!("cannot open framework `{name}` at `{path}`: {e}").into_boxed_str(),
            })
        }
        // "dylib" or empty: use the platform filename convention
        _ => {
            // SAFETY: bytecode-declared library
            unsafe { Library::new(libloading::library_filename(name)) }.map_err(|e| {
                VmError::Malformed {
                    desc: format!("cannot open library `{name}`: {e}").into_boxed_str(),
                }
            })
        }
    }
}

/// Intermediate storage for a single marshaled argument.
enum MarshaledArg {
    Int(i64),
    Float(f64),
    Ptr {
        _cstr: CString,
        raw: *const i8,
    },
    /// A C-layout struct buffer. `ptr` points into `_buf`.
    Struct {
        _buf: Vec<u8>,
        ptr: *const u8,
    },
}

/// Type-directed argument marshaling. Falls back to value-based marshaling
/// for non-struct parameters.
fn marshal_args_typed(
    args: &[Value],
    param_tags: &[u8],
    param_type_ids: &[u32],
    types: &[LoadedType],
    heap: &Heap,
) -> VmResult<Vec<MarshaledArg>> {
    let mut storage = Vec::with_capacity(args.len());
    for (i, &val) in args.iter().enumerate() {
        let tag = param_tags.get(i).copied().unwrap_or(0);
        if tag == TAG_CSTRUCT {
            let type_id = param_type_ids.get(i).copied().unwrap_or(0);
            storage.push(marshal_struct_arg(val, type_id, types, heap)?);
            continue;
        }
        // Ptr[CStruct]: marshal record to C buffer, pass pointer to it
        if tag == TAG_PTR {
            let type_id = param_type_ids.get(i).copied().unwrap_or(0);
            let inner_type_id = ptr_inner_type_id(types, type_id);
            if let Some(inner_id) = inner_type_id {
                let inner_tag = types
                    .get(usize::try_from(inner_id).unwrap_or(0))
                    .map_or(0, |t| t.tag);
                if inner_tag == TAG_CSTRUCT {
                    if val.is_unit() {
                        storage.push(MarshaledArg::Int(0)); // null pointer
                    } else {
                        // Marshal struct to buffer and pass pointer
                        let marshaled = marshal_struct_arg(val, inner_id, types, heap)?;
                        storage.push(marshaled);
                    }
                    continue;
                }
            }
        }
        storage.push(marshal_scalar_arg(val, heap)?);
    }
    Ok(storage)
}

fn marshal_scalar_arg(val: Value, heap: &Heap) -> VmResult<MarshaledArg> {
    if val.is_float() {
        return Ok(MarshaledArg::Float(val.as_float()?));
    }
    if val.is_unit() {
        return Ok(MarshaledArg::Int(0));
    }
    if let Ok(n) = val.as_int() {
        return Ok(MarshaledArg::Int(n));
    }
    if let Ok(n) = val.as_nat() {
        return Ok(MarshaledArg::Int(n.cast_signed()));
    }
    if let Ok(b) = val.as_bool() {
        return Ok(MarshaledArg::Int(i64::from(i32::from(b))));
    }
    if let Ok(c) = val.as_rune() {
        return Ok(MarshaledArg::Int(i64::from(u32::from(c))));
    }
    if let Ok(ptr) = val.as_ref() {
        let heap_obj = heap.get(ptr)?;
        return match &heap_obj.payload {
            HeapPayload::BoxedInt(n) | HeapPayload::BoxedNat(n) => Ok(MarshaledArg::Int(*n)),
            HeapPayload::Str { data, .. } => {
                let cstr = CString::new(data.as_bytes()).map_err(|_| VmError::Malformed {
                    desc: "string contains interior null byte for FFI".into(),
                })?;
                let raw = cstr.as_ptr();
                Ok(MarshaledArg::Ptr { _cstr: cstr, raw })
            }
            _ => Ok(MarshaledArg::Int(0)),
        };
    }
    if let Ok(id) = val.as_fn_id() {
        return Ok(MarshaledArg::Int(i64::from(id)));
    }
    if let Ok(id) = val.as_task_id() {
        return Ok(MarshaledArg::Int(i64::from(id)));
    }
    if let Ok(id) = val.as_chan_id() {
        return Ok(MarshaledArg::Int(i64::from(id)));
    }
    Ok(MarshaledArg::Int(0))
}

/// Marshal a Musi record value into a C-layout byte buffer.
fn marshal_struct_arg(
    val: Value,
    type_id: u32,
    types: &[LoadedType],
    heap: &Heap,
) -> VmResult<MarshaledArg> {
    let idx = usize::try_from(type_id).unwrap_or(0);
    let entry = types.get(idx).ok_or_else(|| VmError::Malformed {
        desc: "cstruct type_id out of bounds".into(),
    })?;
    let type_data = &entry.data;
    if type_data.len() < 8 {
        return Ok(MarshaledArg::Int(0));
    }
    let field_count = usize::from(u16::from_be_bytes([type_data[0], type_data[1]]));
    let total_size_off = 2 + field_count * 8;
    if type_data.len() < total_size_off + 6 {
        return Ok(MarshaledArg::Int(0));
    }
    let total_size = usize::try_from(u32::from_be_bytes([
        type_data[total_size_off],
        type_data[total_size_off + 1],
        type_data[total_size_off + 2],
        type_data[total_size_off + 3],
    ]))
    .unwrap_or(0);

    let mut buf = vec![0u8; total_size];

    let fields: &[Value] = if let Ok(ptr) = val.as_ref() {
        let heap_obj = heap.get(ptr)?;
        match &heap_obj.payload {
            HeapPayload::Record { fields, .. } => fields,
            _ => return Ok(MarshaledArg::Int(0)),
        }
    } else {
        return Ok(MarshaledArg::Int(0));
    };

    for i in 0..field_count {
        let off = 2 + i * 8;
        let field_type_id = u32::from_be_bytes([
            type_data[off],
            type_data[off + 1],
            type_data[off + 2],
            type_data[off + 3],
        ]);
        let field_size = usize::from(u16::from_be_bytes([type_data[off + 4], type_data[off + 5]]));
        let field_offset =
            usize::from(u16::from_be_bytes([type_data[off + 6], type_data[off + 7]]));

        let field_tag = types
            .get(usize::try_from(field_type_id).unwrap_or(0))
            .map_or(0, |t| t.tag);

        if let Some(&field_val) = fields.get(i) {
            write_scalar_to_buf(&mut buf, field_offset, field_size, field_tag, field_val);
        }
    }

    let ptr = buf.as_ptr();
    Ok(MarshaledArg::Struct { _buf: buf, ptr })
}

/// Write a scalar value into a byte buffer at the given offset.
fn write_scalar_to_buf(buf: &mut [u8], offset: usize, size: usize, tag: u8, val: Value) {
    let end = offset + size;
    if end > buf.len() {
        return;
    }
    match tag {
        TAG_I8 => {
            if let Ok(n) = val.as_int() {
                // Truncate to the low byte - C char/int8_t semantics.
                buf[offset] = n.to_ne_bytes()[0];
            }
        }
        TAG_I16 => {
            if let Ok(n) = val.as_int() {
                // Truncate to low 2 bytes - C int16_t semantics.
                buf[offset..end].copy_from_slice(&n.to_ne_bytes()[..2]);
            }
        }
        TAG_I32 | TAG_BOOL => {
            if let Ok(n) = val.as_int() {
                // Truncate to low 4 bytes - C int32_t semantics.
                buf[offset..end].copy_from_slice(&n.to_ne_bytes()[..4]);
            } else if let Ok(b) = val.as_bool() {
                buf[offset..end].copy_from_slice(&i32::from(b).to_ne_bytes());
            }
        }
        TAG_F32 => {
            if let Ok(f) = val.as_float() {
                // f64 -> f32: intentional precision loss for C float ABI.
                // No safe alternative exists for float narrowing.
                #[expect(
                    clippy::cast_possible_truncation,
                    clippy::as_conversions,
                    reason = "intentional f64->f32 narrowing for C float ABI; no safe alternative"
                )]
                buf[offset..end].copy_from_slice(&(f as f32).to_ne_bytes());
            }
        }
        TAG_F64 => {
            if let Ok(f) = val.as_float() {
                buf[offset..end].copy_from_slice(&f.to_ne_bytes());
            }
        }
        _ => {
            // Default: i64/u64/ptr - 8 bytes native endian
            if let Ok(n) = val.as_int() {
                buf[offset..offset + 8.min(size)].copy_from_slice(&n.to_ne_bytes()[..8.min(size)]);
            } else if let Ok(f) = val.as_float() {
                buf[offset..offset + 8.min(size)].copy_from_slice(&f.to_ne_bytes()[..8.min(size)]);
            }
        }
    }
}

/// Unmarshal a C struct return buffer into a Musi record on the heap.
fn unmarshal_struct_return(
    buf: &[u8],
    type_id: u32,
    types: &[LoadedType],
    heap: &mut Heap,
) -> VmResult<Value> {
    let idx = usize::try_from(type_id).unwrap_or(0);
    let entry = types.get(idx).ok_or_else(|| VmError::Malformed {
        desc: "cstruct ret type_id out of bounds".into(),
    })?;
    let type_data = &entry.data;
    if type_data.len() < 2 {
        return Ok(Value::UNIT);
    }
    let field_count = usize::from(u16::from_be_bytes([type_data[0], type_data[1]]));
    let mut fields = Vec::with_capacity(field_count);

    for i in 0..field_count {
        let off = 2 + i * 8;
        if off + 8 > type_data.len() {
            break;
        }
        let field_type_id = u32::from_be_bytes([
            type_data[off],
            type_data[off + 1],
            type_data[off + 2],
            type_data[off + 3],
        ]);
        let field_size = usize::from(u16::from_be_bytes([type_data[off + 4], type_data[off + 5]]));
        let field_offset =
            usize::from(u16::from_be_bytes([type_data[off + 6], type_data[off + 7]]));
        let field_tag = types
            .get(usize::try_from(field_type_id).unwrap_or(0))
            .map_or(0, |t| t.tag);
        fields.push(read_scalar_from_buf(
            buf,
            field_offset,
            field_size,
            field_tag,
        ));
    }

    let heap_ptr = heap.alloc_record(type_id, None, fields);
    Ok(Value::from_ref(heap_ptr))
}

/// Read a scalar value from a C-layout byte buffer at the given offset.
fn read_scalar_from_buf(buf: &[u8], offset: usize, size: usize, tag: u8) -> Value {
    let end = offset + size;
    if end > buf.len() {
        return Value::from_int(0);
    }
    let slice = &buf[offset..end];
    match tag {
        TAG_I8 => {
            let b = slice.first().copied().unwrap_or(0);
            Value::from_int(i64::from(b.cast_signed()))
        }
        TAG_I16 => {
            let bytes = <[u8; 2]>::try_from(slice).unwrap_or_default();
            Value::from_int(i64::from(i16::from_ne_bytes(bytes)))
        }
        TAG_I32 | TAG_BOOL => {
            let bytes = <[u8; 4]>::try_from(slice).unwrap_or_default();
            Value::from_int(i64::from(i32::from_ne_bytes(bytes)))
        }
        TAG_F32 => {
            let bytes = <[u8; 4]>::try_from(slice).unwrap_or_default();
            Value::from_float(f64::from(f32::from_ne_bytes(bytes)))
        }
        TAG_F64 => {
            let bytes = <[u8; 8]>::try_from(slice).unwrap_or_default();
            Value::from_float(f64::from_ne_bytes(bytes))
        }
        _ => {
            // Default: i64
            if size >= 8 {
                let bytes = <[u8; 8]>::try_from(slice).unwrap_or_default();
                Value::from_int(i64::from_ne_bytes(bytes))
            } else {
                Value::from_int(0)
            }
        }
    }
}

fn lookup_msc_rt(ext_name: &str) -> Option<BuiltinFn> {
    match ext_name {
        "show" => Some(builtin_show),
        "str_cat" => Some(builtin_str_cat),
        "write" => Some(builtin_write),
        "writeln" => Some(builtin_writeln),
        "int_to_float" => Some(int_to_float),
        "float_to_int" => Some(float_to_int),
        "__test_filter" => Some(|args, heap| Ok(builtin_test_filter(args, heap))),
        _ => core::lookup(ext_name),
    }
}

fn int_to_float(args: &[Value], _heap: &mut Heap) -> VmResult<Value> {
    let n = args
        .first()
        .copied()
        .ok_or_else(|| VmError::Malformed {
            desc: "int_to_float: expected 1 argument".into(),
        })?
        .as_int()?;
    // i64 -> f64: precision loss on values > 2^53 is intentional - this is a
    // best-effort numeric conversion for FFI callers, not a lossless cast.
    // `f64::from` is not available for i64; no safe alternative exists.
    #[expect(
        clippy::cast_precision_loss,
        clippy::as_conversions,
        reason = "i64->f64 precision loss is intentional; no safe alternative for this conversion"
    )]
    Ok(Value::from_float(n as f64))
}

// i64::MIN is exactly representable in f64; i64::MAX rounds up slightly but
// is safe for clamping since any f64 >= this constant saturates to i64::MAX.
const I64_MIN_F64: f64 = -9_223_372_036_854_775_808.0_f64;
const I64_MAX_F64: f64 = 9_223_372_036_854_775_807.0_f64;

fn float_to_int(args: &[Value], _heap: &mut Heap) -> VmResult<Value> {
    let f = args
        .first()
        .copied()
        .ok_or_else(|| VmError::Malformed {
            desc: "float_to_int: expected 1 argument".into(),
        })?
        .as_float()?;
    // Truncate toward zero, clamp to i64 bounds. Values outside
    // [i64::MIN, i64::MAX] or NaN saturate to the nearest bound.
    if f.is_nan() || f < I64_MIN_F64 {
        return Ok(Value::from_int(i64::MIN));
    }
    if f > I64_MAX_F64 {
        return Ok(Value::from_int(i64::MAX));
    }
    // f is finite and clamped to [I64_MIN_F64, I64_MAX_F64], which fits
    // in i64 after truncation. `as` is the only way to convert f64->i64.
    #[expect(
        clippy::cast_possible_truncation,
        clippy::as_conversions,
        reason = "f is clamped to i64 bounds above; `as` is the only way to convert f64->i64"
    )]
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

fn builtin_writeln(args: &[Value], heap: &mut Heap) -> VmResult<Value> {
    let val = args.first().copied().ok_or_else(|| VmError::Malformed {
        desc: "msc_writeln: expected 1 argument".into(),
    })?;
    let s = value_to_string(val, heap);
    println!("{s}");
    Ok(Value::UNIT)
}

fn builtin_write(args: &[Value], heap: &mut Heap) -> VmResult<Value> {
    let val = args.first().copied().ok_or_else(|| VmError::Malformed {
        desc: "msc_write: expected 1 argument".into(),
    })?;
    let s = value_to_string(val, heap);
    print!("{s}");
    Ok(Value::UNIT)
}

fn builtin_show(args: &[Value], heap: &mut Heap) -> VmResult<Value> {
    let val = args.first().copied().ok_or_else(|| VmError::Malformed {
        desc: "msc_show: expected 1 argument".into(),
    })?;
    let s = value_to_string(val, heap);
    let ptr = heap.alloc_string(0, s.into_boxed_str());
    Ok(Value::from_ref(ptr))
}

fn builtin_str_cat(args: &[Value], heap: &mut Heap) -> VmResult<Value> {
    let a = args.first().copied().ok_or_else(|| VmError::Malformed {
        desc: "msc_str_cat: expected 2 arguments".into(),
    })?;
    let b = args.get(1).copied().ok_or_else(|| VmError::Malformed {
        desc: "msc_str_cat: expected 2 arguments".into(),
    })?;
    let sa = value_to_string(a, heap);
    let sb = value_to_string(b, heap);
    let concatenated = format!("{sa}{sb}");
    let ptr = heap.alloc_string(0, concatenated.into_boxed_str());
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
            MarshaledArg::Struct { ptr, .. } => arg(ptr),
        })
        .collect()
}
