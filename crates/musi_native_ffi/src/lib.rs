#![allow(unsafe_code)]

use std::collections::BTreeSet;
use std::ffi::{CStr, CString, c_char, c_int, c_long, c_uint, c_void};
use std::mem::MaybeUninit;
use std::ptr::{from_mut, null, null_mut};

use musi_vm::{
    ForeignCall, NativeFailureStage, ProgramTypeAbiKind, Value, VmError, VmErrorKind, VmResult,
};
use music_seam::TypeId;

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq)]
pub struct NativeFfi;

type NativeErrorText = Box<str>;

impl NativeFfi {
    #[must_use]
    pub const fn new() -> Self {
        Self
    }
}

const FFI_OK: c_uint = 0;
const FFI_TYPE_STRUCT: u16 = 13;
const RTLD_NOW: c_int = 2;

#[repr(C)]
struct FfiType {
    size: usize,
    alignment: u16,
    type_code: u16,
    elements: *mut *mut Self,
}

#[repr(C)]
struct FfiCif {
    abi: c_uint,
    nargs: c_uint,
    arg_types: *mut *mut FfiType,
    rtype: *mut FfiType,
    bytes: c_uint,
    flags: c_uint,
}

#[link(name = "ffi")]
unsafe extern "C" {
    static mut ffi_type_void: FfiType;
    static mut ffi_type_uint8: FfiType;
    static mut ffi_type_sint64: FfiType;
    static mut ffi_type_double: FfiType;
    static mut ffi_type_pointer: FfiType;

    fn ffi_prep_cif(
        cif: *mut FfiCif,
        abi: c_uint,
        nargs: c_uint,
        rtype: *mut FfiType,
        atypes: *mut *mut FfiType,
    ) -> c_uint;
    fn ffi_get_struct_offsets(
        abi: c_uint,
        struct_type: *mut FfiType,
        offsets: *mut c_long,
    ) -> c_uint;
    fn ffi_call(cif: *mut FfiCif, fn_: *mut c_void, rvalue: *mut c_void, avalue: *mut *mut c_void);
}

#[cfg_attr(target_os = "linux", link(name = "dl"))]
unsafe extern "C" {
    fn dlopen(filename: *const c_char, flags: c_int) -> *mut c_void;
    fn dlsym(handle: *mut c_void, symbol: *const c_char) -> *mut c_void;
    fn dlerror() -> *const c_char;
}

#[derive(Debug)]
enum NativeAbiType {
    Unit,
    Bool { ty: TypeId },
    Int,
    Float,
    CString,
    CPtr,
    Transparent { ty: TypeId, inner: Box<Self> },
    ReprCProduct { ty: TypeId, fields: Box<[Self]> },
}

enum FfiTypeRef {
    Borrowed(*mut FfiType),
    Owned(Box<OwnedStructType>),
}

struct OwnedStructType {
    raw: FfiType,
    children: Vec<FfiTypeRef>,
    elements: Box<[*mut FfiType]>,
    offsets: Box<[usize]>,
}

enum ArgValue {
    Bool(u8),
    Int(i64),
    Float(f64),
    Pointer(*mut c_void),
    CString {
        storage: CString,
        pointer: *mut c_char,
    },
    Struct {
        bytes: Vec<u8>,
        strings: Vec<CString>,
    },
}

enum ResultValue {
    Unit,
    Bool(u8),
    Int(i64),
    Float(f64),
    Pointer(*mut c_void),
    Struct(Vec<u8>),
}

struct FieldWriteCtx<'a> {
    foreign: &'a ForeignCall,
    arg_index: usize,
    out: &'a mut [u8],
    strings: &'a mut Vec<CString>,
}

impl FfiTypeRef {
    fn as_mut_ptr(&mut self) -> *mut FfiType {
        match self {
            Self::Borrowed(ptr) => *ptr,
            Self::Owned(owned) => &raw mut owned.raw,
        }
    }

    fn struct_offsets(&self) -> Option<&[usize]> {
        match self {
            Self::Borrowed(_) => None,
            Self::Owned(owned) => Some(&owned.offsets),
        }
    }

    fn struct_size(&self) -> Option<usize> {
        match self {
            Self::Borrowed(_) => None,
            Self::Owned(owned) => Some(owned.raw.size),
        }
    }
}

impl ArgValue {
    const fn ffi_arg_ptr(&mut self) -> *mut c_void {
        match self {
            Self::Bool(value) => from_mut(value).cast(),
            Self::Int(value) => from_mut(value).cast(),
            Self::Float(value) => from_mut(value).cast(),
            Self::Pointer(value) => from_mut(value).cast(),
            Self::CString { pointer, storage } => {
                let _ = storage;
                from_mut(pointer).cast()
            }
            Self::Struct { bytes, strings } => {
                let _ = strings;
                bytes.as_mut_ptr().cast()
            }
        }
    }
}

impl ResultValue {
    const fn ffi_result_ptr(&mut self) -> *mut c_void {
        match self {
            Self::Unit => null_mut(),
            Self::Bool(value) => from_mut(value).cast(),
            Self::Int(value) => from_mut(value).cast(),
            Self::Float(value) => from_mut(value).cast(),
            Self::Pointer(value) => from_mut(value).cast(),
            Self::Struct(bytes) => bytes.as_mut_ptr().cast(),
        }
    }
}

/// # Errors
///
/// Returns [`VmError`] when symbol resolution, ABI preparation, argument marshalling, the native
/// call itself, or result unmarshalling fails.
pub fn call_foreign(foreign: &ForeignCall, args: &[Value]) -> VmResult<Value> {
    let mut signature = NativeAbiSignature::build(foreign)?;
    if args.len() != signature.arg_tys.len() {
        return Err(VmError::new(VmErrorKind::CallArityMismatch {
            callee: foreign.name().into(),
            expected: signature.arg_tys.len(),
            found: args.len(),
        }));
    }
    let symbol = resolve_symbol(foreign)?;
    let mut arg_values = signature
        .arg_tys
        .iter()
        .zip(args.iter())
        .enumerate()
        .map(|(index, (ty, value))| {
            marshal_arg_value(foreign, index, value, ty, &signature.arg_ffi[index])
        })
        .collect::<VmResult<Vec<_>>>()?;
    let mut ffi_args = arg_values
        .iter_mut()
        .map(ArgValue::ffi_arg_ptr)
        .collect::<Vec<_>>();
    let mut result_value = alloc_result_value(&signature.result_ty, &signature.result_ffi);
    let mut cif = MaybeUninit::<FfiCif>::uninit();
    let mut arg_type_ptrs = signature
        .arg_ffi
        .iter_mut()
        .map(FfiTypeRef::as_mut_ptr)
        .collect::<Vec<_>>();
    let result_type_ptr = signature.result_ffi.as_mut_ptr();
    let arg_count = c_uint::try_from(arg_type_ptrs.len())
        .map_err(|_| native_abi_unsupported(foreign, "native ffi arg count overflow".into()))?;
    let prep_status = {
        // SAFETY: `cif`, `result_type_ptr`, and the arg type array all outlive the call.
        unsafe {
            ffi_prep_cif(
                cif.as_mut_ptr(),
                default_ffi_abi(),
                arg_count,
                result_type_ptr,
                arg_type_ptrs.as_mut_ptr(),
            )
        }
    };
    if prep_status != FFI_OK {
        return Err(native_abi_unsupported(
            foreign,
            format!(
                "ffi_prep_cif failed with status `{prep_status}` for abi `{}`",
                default_ffi_abi()
            )
            .into(),
        ));
    }
    let mut cif = {
        // SAFETY: `ffi_prep_cif` initialized `cif` on the success path above.
        unsafe { cif.assume_init() }
    };
    {
        // SAFETY: `symbol` is a process-resolved C function pointer and all ffi buffers are valid
        // for the duration of the call.
        unsafe {
            ffi_call(
                &raw mut cif,
                symbol,
                result_value.ffi_result_ptr(),
                ffi_args.as_mut_ptr(),
            );
        }
    }
    unmarshal_result_value(
        foreign,
        &signature.result_ty,
        &signature.result_ffi,
        result_value,
    )
}

struct NativeAbiSignature {
    arg_tys: Vec<NativeAbiType>,
    arg_ffi: Vec<FfiTypeRef>,
    result_ty: NativeAbiType,
    result_ffi: FfiTypeRef,
}

impl NativeAbiSignature {
    fn build(foreign: &ForeignCall) -> VmResult<Self> {
        let mut seen = BTreeSet::<u32>::new();
        let arg_tys = foreign
            .param_tys()
            .iter()
            .copied()
            .map(|ty| native_abi_type(foreign, ty, &mut seen))
            .collect::<VmResult<Vec<_>>>()?;
        let result_ty = native_abi_type(foreign, foreign.result_ty(), &mut BTreeSet::new())?;
        let mut arg_ffi = arg_tys
            .iter()
            .map(build_ffi_type)
            .collect::<VmResult<Vec<_>>>()?;
        let result_ffi = build_ffi_type(&result_ty)?;
        for ffi in &mut arg_ffi {
            touch_ffi_type(ffi);
        }
        let mut result_ffi = result_ffi;
        touch_ffi_type(&mut result_ffi);
        Ok(Self {
            arg_tys,
            arg_ffi,
            result_ty,
            result_ffi,
        })
    }
}

fn native_abi_type(
    foreign: &ForeignCall,
    ty: TypeId,
    seen: &mut BTreeSet<u32>,
) -> VmResult<NativeAbiType> {
    if !seen.insert(ty.raw()) {
        return Err(native_abi_unsupported(
            foreign,
            format!(
                "native ABI type `{}` is recursive by value",
                foreign.type_name(ty)
            )
            .into(),
        ));
    }
    let abi_ty = match foreign.type_abi_kind(ty) {
        ProgramTypeAbiKind::Unit => NativeAbiType::Unit,
        ProgramTypeAbiKind::Bool => NativeAbiType::Bool { ty },
        ProgramTypeAbiKind::Int => NativeAbiType::Int,
        ProgramTypeAbiKind::Float => NativeAbiType::Float,
        ProgramTypeAbiKind::CString => NativeAbiType::CString,
        ProgramTypeAbiKind::CPtr => NativeAbiType::CPtr,
        ProgramTypeAbiKind::DataTransparent => {
            let layout = foreign.type_data_layout(ty).ok_or_else(|| {
                native_abi_unsupported(
                    foreign,
                    format!(
                        "transparent type `{}` is missing layout",
                        foreign.type_name(ty)
                    )
                    .into(),
                )
            })?;
            let variant = layout.single_variant().ok_or_else(|| {
                native_abi_unsupported(
                    foreign,
                    format!(
                        "transparent type `{}` is not a single-variant wrapper",
                        foreign.type_name(ty)
                    )
                    .into(),
                )
            })?;
            let Some(field_ty) = variant.field_tys.first().copied() else {
                return Err(native_abi_unsupported(
                    foreign,
                    format!(
                        "transparent type `{}` is missing a wrapped field",
                        foreign.type_name(ty)
                    )
                    .into(),
                ));
            };
            NativeAbiType::Transparent {
                ty,
                inner: Box::new(native_abi_type(foreign, field_ty, seen)?),
            }
        }
        ProgramTypeAbiKind::DataReprCProduct => {
            let layout = foreign.type_data_layout(ty).ok_or_else(|| {
                native_abi_unsupported(
                    foreign,
                    format!("repr(c) type `{}` is missing layout", foreign.type_name(ty)).into(),
                )
            })?;
            let variant = layout.single_variant().ok_or_else(|| {
                native_abi_unsupported(
                    foreign,
                    format!(
                        "repr(c) type `{}` is not a single-variant product",
                        foreign.type_name(ty)
                    )
                    .into(),
                )
            })?;
            let fields = variant
                .field_tys
                .iter()
                .copied()
                .map(|field_ty| native_abi_type(foreign, field_ty, seen))
                .collect::<VmResult<Vec<_>>>()?
                .into_boxed_slice();
            let _ = layout;
            NativeAbiType::ReprCProduct { ty, fields }
        }
        ProgramTypeAbiKind::Unsupported => {
            return Err(native_abi_unsupported(
                foreign,
                format!(
                    "type `{}` is not native-ABI compatible",
                    foreign.type_name(ty)
                )
                .into(),
            ));
        }
    };
    let _ = seen.remove(&ty.raw());
    Ok(abi_ty)
}

fn build_ffi_type(ty: &NativeAbiType) -> VmResult<FfiTypeRef> {
    match ty {
        NativeAbiType::Unit => Ok(FfiTypeRef::Borrowed(ffi_type_void_ptr())),
        NativeAbiType::Bool { .. } => Ok(FfiTypeRef::Borrowed(ffi_type_uint8_ptr())),
        NativeAbiType::Int => Ok(FfiTypeRef::Borrowed(ffi_type_sint64_ptr())),
        NativeAbiType::Float => Ok(FfiTypeRef::Borrowed(ffi_type_double_ptr())),
        NativeAbiType::CString | NativeAbiType::CPtr => {
            Ok(FfiTypeRef::Borrowed(ffi_type_pointer_ptr()))
        }
        NativeAbiType::Transparent { inner, .. } => build_ffi_type(inner),
        NativeAbiType::ReprCProduct { fields, .. } => build_struct_ffi_type(fields),
    }
}

fn build_struct_ffi_type(fields: &[NativeAbiType]) -> VmResult<FfiTypeRef> {
    let mut children = fields
        .iter()
        .map(build_ffi_type)
        .collect::<VmResult<Vec<_>>>()?;
    let mut elements = children
        .iter_mut()
        .map(FfiTypeRef::as_mut_ptr)
        .collect::<Vec<_>>();
    elements.push(null_mut());
    let mut owned = Box::new(OwnedStructType {
        raw: FfiType {
            size: 0,
            alignment: 0,
            type_code: FFI_TYPE_STRUCT,
            elements: null_mut(),
        },
        children,
        elements: elements.into_boxed_slice(),
        offsets: vec![0; fields.len()].into_boxed_slice(),
    });
    owned.raw.elements = owned.elements.as_mut_ptr();
    let mut ffi_offsets = vec![c_long::default(); fields.len()];
    let status = {
        // SAFETY: `owned.raw` points to a live struct type and `ffi_offsets` has one slot per field.
        unsafe {
            ffi_get_struct_offsets(
                default_ffi_abi(),
                &raw mut owned.raw,
                ffi_offsets.as_mut_ptr(),
            )
        }
    };
    if status != FFI_OK {
        return Err(native_call_failed(
            "<struct>".into(),
            NativeFailureStage::AbiUnsupported,
            None,
            None,
            format!(
                "ffi_get_struct_offsets failed with status `{status}` for abi `{}`",
                default_ffi_abi()
            )
            .into(),
        ));
    }
    owned.offsets = ffi_offsets
        .into_iter()
        .map(|offset| usize::try_from(offset).unwrap_or(usize::MAX))
        .collect::<Vec<_>>()
        .into_boxed_slice();
    Ok(FfiTypeRef::Owned(owned))
}

fn touch_ffi_type(ffi: &mut FfiTypeRef) {
    if let FfiTypeRef::Owned(owned) = ffi {
        for child in &mut owned.children {
            touch_ffi_type(child);
        }
    }
}

fn marshal_arg_value(
    foreign: &ForeignCall,
    index: usize,
    value: &Value,
    ty: &NativeAbiType,
    ffi: &FfiTypeRef,
) -> VmResult<ArgValue> {
    match ty {
        NativeAbiType::Unit => Ok(ArgValue::Struct {
            bytes: Vec::new(),
            strings: Vec::new(),
        }),
        NativeAbiType::Bool { .. } => bool_flag(value).map_or_else(
            || invalid_arg_type(foreign, index, "Bool", value),
            |flag| Ok(ArgValue::Bool(u8::from(flag))),
        ),
        NativeAbiType::Int => match value {
            Value::Int(number) => Ok(ArgValue::Int(*number)),
            other => invalid_arg_type(foreign, index, "Int", other),
        },
        NativeAbiType::Float => match value {
            Value::Float(number) => Ok(ArgValue::Float(*number)),
            other => invalid_arg_type(foreign, index, "Float", other),
        },
        NativeAbiType::CString => match value {
            Value::String(text) => {
                let storage = CString::new(text.as_ref()).map_err(|_| {
                    native_arg_invalid(
                        foreign,
                        index,
                        "CString argument contains interior NUL".into(),
                    )
                })?;
                let pointer = storage.as_ptr().cast_mut();
                Ok(ArgValue::CString { storage, pointer })
            }
            other => invalid_arg_type(foreign, index, "CString", other),
        },
        NativeAbiType::CPtr => match value {
            Value::CPtr(address) => Ok(ArgValue::Pointer(usize_to_mut_ptr(*address))),
            other => invalid_arg_type(foreign, index, "CPtr", other),
        },
        NativeAbiType::Transparent { inner, .. } => {
            let (inner_value, _) = extract_wrapper_value(foreign, index, value, ty)?;
            marshal_arg_value(foreign, index, &inner_value, inner, ffi)
        }
        NativeAbiType::ReprCProduct { fields, .. } => {
            let (bytes, strings) = marshal_record_bytes(foreign, index, value, fields, ffi)?;
            Ok(ArgValue::Struct { bytes, strings })
        }
    }
}

fn bool_flag(value: &Value) -> Option<bool> {
    let record = value.as_record()?;
    (record.is_empty() && (record.tag() == 0 || record.tag() == 1)).then_some(record.tag() != 0)
}

fn marshal_record_bytes(
    foreign: &ForeignCall,
    index: usize,
    value: &Value,
    fields: &[NativeAbiType],
    ffi: &FfiTypeRef,
) -> VmResult<(Vec<u8>, Vec<CString>)> {
    let Some(data) = value.as_record() else {
        return Err(native_arg_invalid(
            foreign,
            index,
            format!("expected `record data`, found `{:?}`", value.kind()).into(),
        ));
    };
    if data.tag() != 0 {
        return Err(native_arg_invalid(
            foreign,
            index,
            "repr(c) product argument has non-zero tag".into(),
        ));
    }
    if data.len() != fields.len() {
        return Err(native_arg_invalid(
            foreign,
            index,
            "repr(c) product argument field count mismatch".into(),
        ));
    }
    let size = ffi.struct_size().unwrap_or(0);
    let mut bytes = vec![0_u8; size];
    let mut strings = Vec::<CString>::new();
    let offsets = ffi.struct_offsets().unwrap_or(&[]);
    for (field_index, field_ty) in fields.iter().enumerate() {
        let Some(offset) = offsets.get(field_index).copied() else {
            return Err(native_arg_invalid(
                foreign,
                index,
                "repr(c) product argument offset missing".into(),
            ));
        };
        let Some(field_value) = data.get(field_index) else {
            return Err(native_arg_invalid(
                foreign,
                index,
                "repr(c) product argument field missing".into(),
            ));
        };
        let mut write_ctx = FieldWriteCtx {
            foreign,
            arg_index: index,
            out: &mut bytes,
            strings: &mut strings,
        };
        write_field_bytes(
            &mut write_ctx,
            field_value,
            field_ty,
            ffi_child(ffi, field_index),
            offset,
        )?;
    }
    Ok((bytes, strings))
}

fn write_field_bytes(
    ctx: &mut FieldWriteCtx<'_>,
    value: &Value,
    ty: &NativeAbiType,
    ffi: Option<&FfiTypeRef>,
    offset: usize,
) -> VmResult<()> {
    match ty {
        NativeAbiType::Unit => Ok(()),
        NativeAbiType::Bool { .. } => bool_flag(value).map_or_else(
            || {
                Err(native_arg_invalid(
                    ctx.foreign,
                    ctx.arg_index,
                    format!("expected `Bool`, found `{:?}`", value.kind()).into(),
                ))
            },
            |flag| write_bytes(ctx.out, offset, &[u8::from(flag)]),
        ),
        NativeAbiType::Int => match value {
            Value::Int(number) => write_bytes(ctx.out, offset, &number.to_ne_bytes()),
            other => Err(native_arg_invalid(
                ctx.foreign,
                ctx.arg_index,
                format!("expected `Int`, found `{:?}`", other.kind()).into(),
            )),
        },
        NativeAbiType::Float => match value {
            Value::Float(number) => write_bytes(ctx.out, offset, &number.to_ne_bytes()),
            other => Err(native_arg_invalid(
                ctx.foreign,
                ctx.arg_index,
                format!("expected `Float`, found `{:?}`", other.kind()).into(),
            )),
        },
        NativeAbiType::CString => match value {
            Value::String(text) => {
                let c_string = CString::new(text.as_ref()).map_err(|_| {
                    native_arg_invalid(
                        ctx.foreign,
                        ctx.arg_index,
                        "CString argument contains interior NUL".into(),
                    )
                })?;
                let pointer = c_string.as_ptr().addr();
                ctx.strings.push(c_string);
                write_bytes(ctx.out, offset, &pointer.to_ne_bytes())
            }
            other => Err(native_arg_invalid(
                ctx.foreign,
                ctx.arg_index,
                format!("expected `CString`, found `{:?}`", other.kind()).into(),
            )),
        },
        NativeAbiType::CPtr => match value {
            Value::CPtr(address) => write_bytes(ctx.out, offset, &address.to_ne_bytes()),
            other => Err(native_arg_invalid(
                ctx.foreign,
                ctx.arg_index,
                format!("expected `CPtr`, found `{:?}`", other.kind()).into(),
            )),
        },
        NativeAbiType::Transparent { inner, .. } => {
            let (inner_value, _) = extract_wrapper_value(ctx.foreign, ctx.arg_index, value, ty)?;
            write_field_bytes(ctx, &inner_value, inner, ffi, offset)
        }
        NativeAbiType::ReprCProduct { .. } => {
            let Some(ffi) = ffi else {
                return Err(native_arg_invalid(
                    ctx.foreign,
                    ctx.arg_index,
                    "nested repr(c) product ffi metadata missing".into(),
                ));
            };
            let (nested, nested_strings) =
                marshal_record_bytes(ctx.foreign, ctx.arg_index, value, repr_c_fields(ty), ffi)?;
            ctx.strings.extend(nested_strings);
            write_bytes(ctx.out, offset, &nested)
        }
    }
}

fn alloc_result_value(ty: &NativeAbiType, ffi: &FfiTypeRef) -> ResultValue {
    match ty {
        NativeAbiType::Unit => ResultValue::Unit,
        NativeAbiType::Bool { .. } => ResultValue::Bool(0),
        NativeAbiType::Int => ResultValue::Int(0),
        NativeAbiType::Float => ResultValue::Float(0.0),
        NativeAbiType::CString | NativeAbiType::CPtr => ResultValue::Pointer(null_mut()),
        NativeAbiType::Transparent { inner, .. } => alloc_result_value(inner, ffi),
        NativeAbiType::ReprCProduct { .. } => {
            ResultValue::Struct(vec![0_u8; ffi.struct_size().unwrap_or(0)])
        }
    }
}

fn unmarshal_result_value(
    foreign: &ForeignCall,
    ty: &NativeAbiType,
    ffi: &FfiTypeRef,
    value: ResultValue,
) -> VmResult<Value> {
    match (ty, value) {
        (NativeAbiType::Unit, ResultValue::Unit) => Ok(Value::Unit),
        (NativeAbiType::Bool { ty }, ResultValue::Bool(flag)) => {
            Ok(Value::data(*ty, i64::from(flag != 0), []))
        }
        (NativeAbiType::Int, ResultValue::Int(number)) => Ok(Value::Int(number)),
        (NativeAbiType::Float, ResultValue::Float(number)) => Ok(Value::Float(number)),
        (NativeAbiType::CPtr, ResultValue::Pointer(pointer)) => Ok(Value::CPtr(pointer.addr())),
        (NativeAbiType::CString, ResultValue::Pointer(pointer)) => {
            if pointer.is_null() {
                return Err(native_result_invalid(
                    foreign,
                    "CString result was null".into(),
                ));
            }
            let c_text = {
                // SAFETY: native code returned a non-null C string pointer for this result.
                unsafe { CStr::from_ptr(pointer.cast()) }
            };
            let text = c_text.to_str().map_err(|error| {
                native_result_invalid(
                    foreign,
                    format!("CString result is not UTF-8 (`{error}`)").into(),
                )
            })?;
            Ok(Value::string(text))
        }
        (NativeAbiType::Transparent { ty, inner }, result) => {
            let inner_value = unmarshal_result_value(foreign, inner, ffi, result)?;
            Ok(Value::data(*ty, 0, [inner_value]))
        }
        (NativeAbiType::ReprCProduct { ty, fields, .. }, ResultValue::Struct(bytes)) => {
            let offsets = ffi.struct_offsets().unwrap_or(&[]);
            let values = fields
                .iter()
                .enumerate()
                .map(|(index, field_ty)| {
                    let Some(offset) = offsets.get(index).copied() else {
                        return Err(native_result_invalid(
                            foreign,
                            "repr(c) result offset missing".into(),
                        ));
                    };
                    read_field_value(foreign, field_ty, ffi_child(ffi, index), &bytes, offset)
                })
                .collect::<VmResult<Vec<_>>>()?;
            Ok(Value::data(*ty, 0, values))
        }
        _ => Err(native_result_invalid(
            foreign,
            "native result storage shape mismatch".into(),
        )),
    }
}

fn read_field_value(
    foreign: &ForeignCall,
    ty: &NativeAbiType,
    ffi: Option<&FfiTypeRef>,
    bytes: &[u8],
    offset: usize,
) -> VmResult<Value> {
    match ty {
        NativeAbiType::Unit => Ok(Value::Unit),
        NativeAbiType::Bool { ty } => Ok(Value::data(
            *ty,
            i64::from(read_u8(bytes, offset)? != 0),
            [],
        )),
        NativeAbiType::Int => Ok(Value::Int(i64::from_ne_bytes(read_array(bytes, offset)?))),
        NativeAbiType::Float => Ok(Value::Float(f64::from_ne_bytes(read_array(bytes, offset)?))),
        NativeAbiType::CString => {
            let ptr = usize::from_ne_bytes(read_array(bytes, offset)?);
            if ptr == 0 {
                return Err(native_result_invalid(
                    foreign,
                    "CString result was null".into(),
                ));
            }
            let c_text = {
                // SAFETY: native code wrote a non-null C string pointer into the repr(c) field.
                unsafe { CStr::from_ptr(null::<c_void>().with_addr(ptr).cast()) }
            };
            let text = c_text.to_str().map_err(|error| {
                native_result_invalid(
                    foreign,
                    format!("CString result is not UTF-8 (`{error}`)").into(),
                )
            })?;
            Ok(Value::string(text))
        }
        NativeAbiType::CPtr => Ok(Value::CPtr(usize::from_ne_bytes(read_array(
            bytes, offset,
        )?))),
        NativeAbiType::Transparent { ty, inner } => {
            let inner_value = read_field_value(foreign, inner, ffi, bytes, offset)?;
            Ok(Value::data(*ty, 0, [inner_value]))
        }
        NativeAbiType::ReprCProduct { ty, fields, .. } => {
            let Some(ffi) = ffi else {
                return Err(native_result_invalid(
                    foreign,
                    "nested repr(c) result ffi metadata missing".into(),
                ));
            };
            let offsets = ffi.struct_offsets().unwrap_or(&[]);
            let values = fields
                .iter()
                .enumerate()
                .map(|(index, field_ty)| {
                    let Some(field_offset) = offsets.get(index).copied() else {
                        return Err(native_result_invalid(
                            foreign,
                            "nested repr(c) result offset missing".into(),
                        ));
                    };
                    read_field_value(
                        foreign,
                        field_ty,
                        ffi_child(ffi, index),
                        bytes,
                        offset + field_offset,
                    )
                })
                .collect::<VmResult<Vec<_>>>()?;
            Ok(Value::data(*ty, 0, values))
        }
    }
}

fn resolve_symbol(foreign: &ForeignCall) -> VmResult<*mut c_void> {
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

fn library_candidates(link: &str) -> Vec<String> {
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

#[cfg(all(target_arch = "x86_64", not(target_os = "windows")))]
const fn default_ffi_abi() -> c_uint {
    2
}

#[cfg(all(target_arch = "x86_64", target_os = "windows", target_env = "gnu"))]
const fn default_ffi_abi() -> c_uint {
    2
}

#[cfg(all(target_arch = "x86_64", target_os = "windows", not(target_env = "gnu")))]
const fn default_ffi_abi() -> c_uint {
    1
}

#[cfg(all(target_arch = "aarch64", not(target_os = "windows")))]
const fn default_ffi_abi() -> c_uint {
    1
}

#[cfg(all(target_arch = "aarch64", target_os = "windows"))]
const fn default_ffi_abi() -> c_uint {
    2
}

#[cfg(not(any(
    all(target_arch = "x86_64", not(target_os = "windows")),
    all(target_arch = "x86_64", target_os = "windows", target_env = "gnu"),
    all(target_arch = "x86_64", target_os = "windows", not(target_env = "gnu")),
    all(target_arch = "aarch64", not(target_os = "windows")),
    all(target_arch = "aarch64", target_os = "windows")
)))]
const fn default_ffi_abi() -> c_uint {
    1
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

fn ffi_type_void_ptr() -> *mut FfiType {
    &raw mut ffi_type_void
}

fn ffi_type_uint8_ptr() -> *mut FfiType {
    &raw mut ffi_type_uint8
}

fn ffi_type_sint64_ptr() -> *mut FfiType {
    &raw mut ffi_type_sint64
}

fn ffi_type_double_ptr() -> *mut FfiType {
    &raw mut ffi_type_double
}

fn ffi_type_pointer_ptr() -> *mut FfiType {
    &raw mut ffi_type_pointer
}

fn usize_to_mut_ptr(address: usize) -> *mut c_void {
    null_mut::<c_void>().with_addr(address)
}

fn write_bytes(out: &mut [u8], offset: usize, bytes: &[u8]) -> VmResult<()> {
    let end = offset.saturating_add(bytes.len());
    let Some(target) = out.get_mut(offset..end) else {
        return Err(native_call_failed(
            "<struct>".into(),
            NativeFailureStage::AbiUnsupported,
            None,
            None,
            "native struct layout write overflow".into(),
        ));
    };
    target.copy_from_slice(bytes);
    Ok(())
}

fn read_u8(bytes: &[u8], offset: usize) -> VmResult<u8> {
    bytes.get(offset).copied().ok_or_else(|| {
        native_call_failed(
            "<struct>".into(),
            NativeFailureStage::ResultInvalid,
            None,
            None,
            "native struct result read overflow".into(),
        )
    })
}

fn read_array<const N: usize>(bytes: &[u8], offset: usize) -> VmResult<[u8; N]> {
    let end = offset.saturating_add(N);
    let Some(source) = bytes.get(offset..end) else {
        return Err(native_call_failed(
            "<struct>".into(),
            NativeFailureStage::ResultInvalid,
            None,
            None,
            "native struct result read overflow".into(),
        ));
    };
    let mut out = [0_u8; N];
    out.copy_from_slice(source);
    Ok(out)
}

const fn native_call_failed(
    foreign: NativeErrorText,
    stage: NativeFailureStage,
    subject: Option<NativeErrorText>,
    index: Option<usize>,
    detail: NativeErrorText,
) -> VmError {
    VmError::new(VmErrorKind::NativeCallFailed {
        foreign,
        stage,
        subject,
        index,
        detail,
    })
}

fn native_abi_unsupported(foreign: &ForeignCall, reason: NativeErrorText) -> VmError {
    native_call_failed(
        foreign.name().into(),
        NativeFailureStage::AbiUnsupported,
        None::<NativeErrorText>,
        None,
        reason,
    )
}

fn native_result_invalid(foreign: &ForeignCall, reason: NativeErrorText) -> VmError {
    native_call_failed(
        foreign.name().into(),
        NativeFailureStage::ResultInvalid,
        None::<NativeErrorText>,
        None,
        reason,
    )
}

fn native_symbol_load_failed(
    foreign: &ForeignCall,
    symbol: NativeErrorText,
    detail: NativeErrorText,
) -> VmError {
    native_call_failed(
        foreign.name().into(),
        NativeFailureStage::SymbolLoad,
        Some(symbol),
        None,
        detail,
    )
}

fn native_library_load_failed(
    foreign: &ForeignCall,
    library: NativeErrorText,
    detail: NativeErrorText,
) -> VmError {
    native_call_failed(
        foreign.name().into(),
        NativeFailureStage::LibraryLoad,
        Some(library),
        None,
        detail,
    )
}

fn native_arg_invalid(foreign: &ForeignCall, index: usize, reason: NativeErrorText) -> VmError {
    native_call_failed(
        foreign.name().into(),
        NativeFailureStage::ArgInvalid,
        None::<NativeErrorText>,
        Some(index),
        reason,
    )
}

fn invalid_arg_type<T>(
    foreign: &ForeignCall,
    index: usize,
    expected: &str,
    found: &Value,
) -> VmResult<T> {
    Err(native_arg_invalid(
        foreign,
        index,
        format!("expected `{expected}`, found `{:?}`", found.kind()).into(),
    ))
}

fn extract_wrapper_value(
    foreign: &ForeignCall,
    index: usize,
    value: &Value,
    ty: &NativeAbiType,
) -> VmResult<(Value, TypeId)> {
    let wrapped_ty = match ty {
        NativeAbiType::Transparent { ty, .. } => *ty,
        _ => {
            return Err(native_arg_invalid(
                foreign,
                index,
                "transparent wrapper extraction requires transparent ABI type".into(),
            ));
        }
    };
    let Some(data) = value.as_record() else {
        return Err(native_arg_invalid(
            foreign,
            index,
            "transparent argument is not data".into(),
        ));
    };
    if data.ty() != wrapped_ty || data.tag() != 0 || data.len() != 1 {
        return Err(native_arg_invalid(
            foreign,
            index,
            "transparent argument shape mismatch".into(),
        ));
    }
    let Some(inner) = data.get(0).cloned() else {
        return Err(native_arg_invalid(
            foreign,
            index,
            "transparent argument field missing".into(),
        ));
    };
    Ok((inner, wrapped_ty))
}

fn repr_c_fields(ty: &NativeAbiType) -> &[NativeAbiType] {
    match ty {
        NativeAbiType::ReprCProduct { fields, .. } => fields,
        _ => &[],
    }
}

fn ffi_child(ffi: &FfiTypeRef, index: usize) -> Option<&FfiTypeRef> {
    match ffi {
        FfiTypeRef::Borrowed(_) => None,
        FfiTypeRef::Owned(owned) => owned.children.get(index),
    }
}

#[cfg(test)]
#[allow(clippy::panic, clippy::unwrap_used)]
mod tests {
    use super::{default_ffi_abi, library_candidates};

    #[test]
    fn c_runtime_link_uses_platform_candidates() {
        let candidates = library_candidates("c");
        #[cfg(target_os = "macos")]
        assert_eq!(
            candidates,
            vec!["libSystem.B.dylib", "libc.dylib", "libc.so"]
        );
        #[cfg(target_os = "linux")]
        assert_eq!(candidates, vec!["libc.so.6", "libc.so"]);
        #[cfg(target_os = "windows")]
        assert_eq!(candidates, vec!["ucrtbase.dll", "msvcrt.dll"]);
    }

    #[test]
    fn generic_library_name_keeps_default_candidates() {
        assert_eq!(
            library_candidates("sqlite3"),
            vec![
                "sqlite3".to_owned(),
                "libsqlite3.dylib".to_owned(),
                "libsqlite3.so".to_owned(),
            ]
        );
    }

    #[test]
    fn explicit_path_is_preserved() {
        assert_eq!(
            library_candidates("/usr/lib/libSystem.B.dylib"),
            vec!["/usr/lib/libSystem.B.dylib".to_owned()]
        );
    }

    #[test]
    fn default_ffi_abi_matches_current_target() {
        #[cfg(all(target_arch = "x86_64", not(target_os = "windows")))]
        assert_eq!(default_ffi_abi(), 2);
        #[cfg(all(target_arch = "x86_64", target_os = "windows", target_env = "gnu"))]
        assert_eq!(default_ffi_abi(), 2);
        #[cfg(all(target_arch = "x86_64", target_os = "windows", not(target_env = "gnu")))]
        assert_eq!(default_ffi_abi(), 1);
        #[cfg(all(target_arch = "aarch64", not(target_os = "windows")))]
        assert_eq!(default_ffi_abi(), 1);
        #[cfg(all(target_arch = "aarch64", target_os = "windows"))]
        assert_eq!(default_ffi_abi(), 2);
    }
}
