use std::ffi::{CStr, CString, c_char, c_uint, c_void};
use std::mem::MaybeUninit;
use std::ptr::{from_mut, null, null_mut};

use musi_vm::{
    ForeignCall, Value, VmError, VmErrorKind, VmHostCallContext, VmHostContext, VmResult,
};
use music_seam::TypeId;

use crate::abi::{NativeAbiSignature, NativeAbiType};
use crate::ffi::{
    FFI_OK, FfiCif, FfiTypeRef, default_ffi_abi, ffi_call, ffi_child, ffi_prep_cif, read_array,
    read_u8, usize_to_mut_ptr, write_bytes,
};
use crate::loader::resolve_symbol;
use crate::{invalid_arg_type, native_abi_unsupported, native_arg_invalid, native_result_invalid};

type StructMarshalContext<'call, 'vm> = VmHostCallContext<'call, 'vm>;

enum ArgValue {
    Bool(u8),
    I8(i8),
    U8(u8),
    I16(i16),
    U16(u16),
    I32(i32),
    U32(u32),
    I64(i64),
    U64(u64),
    F32(f32),
    F64(f64),
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
    I8(i8),
    U8(u8),
    I16(i16),
    U16(u16),
    I32(i32),
    U32(u32),
    I64(i64),
    U64(u64),
    F32(f32),
    F64(f64),
    Pointer(*mut c_void),
    Struct(Vec<u8>),
}

struct FieldWriteCtx<'a, 'h> {
    foreign: &'a ForeignCall,
    arg_index: usize,
    out: &'a mut [u8],
    strings: &'a mut Vec<CString>,
    vm: &'a mut VmHostContext<'h>,
}

impl ArgValue {
    const fn ffi_arg_ptr(&mut self) -> *mut c_void {
        match self {
            Self::Bool(value) | Self::U8(value) => from_mut(value).cast(),
            Self::I8(value) => from_mut(value).cast(),
            Self::I16(value) => from_mut(value).cast(),
            Self::U16(value) => from_mut(value).cast(),
            Self::I32(value) => from_mut(value).cast(),
            Self::U32(value) => from_mut(value).cast(),
            Self::I64(value) => from_mut(value).cast(),
            Self::U64(value) => from_mut(value).cast(),
            Self::F32(value) => from_mut(value).cast(),
            Self::F64(value) => from_mut(value).cast(),
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
            Self::Bool(value) | Self::U8(value) => from_mut(value).cast(),
            Self::I8(value) => from_mut(value).cast(),
            Self::I16(value) => from_mut(value).cast(),
            Self::U16(value) => from_mut(value).cast(),
            Self::I32(value) => from_mut(value).cast(),
            Self::U32(value) => from_mut(value).cast(),
            Self::I64(value) => from_mut(value).cast(),
            Self::U64(value) => from_mut(value).cast(),
            Self::F32(value) => from_mut(value).cast(),
            Self::F64(value) => from_mut(value).cast(),
            Self::Pointer(value) => from_mut(value).cast(),
            Self::Struct(bytes) => bytes.as_mut_ptr().cast(),
        }
    }
}

/// # Errors
///
/// Returns [`VmError`] when symbol resolution, ABI preparation, argument marshalling, the native
/// call itself, or result unmarshalling fails.
pub fn call_foreign(
    ctx: StructMarshalContext<'_, '_>,
    foreign: &ForeignCall,
    args: &[Value],
) -> VmResult<Value> {
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
            marshal_arg_value(ctx, foreign, index, value, ty, &signature.arg_ffi[index])
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
    let arg_count = c_uint::try_from(arg_type_ptrs.len()).map_err(|_| {
        native_abi_unsupported(foreign, "native FFI argument count overflow".into())
    })?;
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
                "`ffi_prep_cif` failed with status `{prep_status}` for ABI `{}`",
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
        ctx,
        foreign,
        &signature.result_ty,
        &signature.result_ffi,
        result_value,
    )
}

fn extract_wrapper_value(
    ctx: &VmHostContext<'_>,
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
    let Some(data) = ctx.record(value) else {
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

fn marshal_arg_value(
    ctx: &mut VmHostContext<'_>,
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
        NativeAbiType::Bool { .. } => ctx.bool_flag(value).map_or_else(
            || invalid_arg_type(foreign, index, "Bool", value),
            |flag| Ok(ArgValue::Bool(u8::from(flag))),
        ),
        NativeAbiType::Int { signed, bits } => {
            marshal_int_arg(foreign, index, value, *signed, *bits)
        }
        NativeAbiType::Float { bits } => match value {
            Value::Float(number) if *bits == 32 => Ok(ArgValue::F32(f32_from_f64(*number))),
            Value::Float(number) => Ok(ArgValue::F64(*number)),
            other => invalid_arg_type(foreign, index, "Float", other),
        },
        NativeAbiType::CString => match ctx.string(value) {
            Some(text) => {
                let storage = CString::new(text.as_str()).map_err(|_| {
                    native_arg_invalid(
                        foreign,
                        index,
                        "`CString` argument contains interior NULL".into(),
                    )
                })?;
                let pointer = storage.as_ptr().cast_mut();
                Ok(ArgValue::CString { storage, pointer })
            }
            None => invalid_arg_type(foreign, index, "CString", value),
        },
        NativeAbiType::CPtr => match value {
            Value::CPtr(address) => Ok(ArgValue::Pointer(usize_to_mut_ptr(*address))),
            other => invalid_arg_type(foreign, index, "CPtr", other),
        },
        NativeAbiType::Transparent { inner, .. } => {
            let (inner_value, _) = extract_wrapper_value(ctx, foreign, index, value, ty)?;
            marshal_arg_value(ctx, foreign, index, &inner_value, inner, ffi)
        }
        NativeAbiType::ReprCProduct { fields, .. } => {
            let (bytes, strings) = marshal_record_bytes(ctx, foreign, index, value, fields, ffi)?;
            Ok(ArgValue::Struct { bytes, strings })
        }
    }
}

fn marshal_int_arg(
    foreign: &ForeignCall,
    index: usize,
    value: &Value,
    signed: bool,
    bits: u8,
) -> VmResult<ArgValue> {
    if signed {
        let number = signed_arg_number(foreign, index, value)?;
        return match bits {
            8 => i8::try_from(number)
                .map(ArgValue::I8)
                .map_err(|_| int_arg_out_of_range(foreign, index, "Int8")),
            16 => i16::try_from(number)
                .map(ArgValue::I16)
                .map_err(|_| int_arg_out_of_range(foreign, index, "Int16")),
            32 => i32::try_from(number)
                .map(ArgValue::I32)
                .map_err(|_| int_arg_out_of_range(foreign, index, "Int32")),
            _ => Ok(ArgValue::I64(number)),
        };
    }
    let number = unsigned_arg_number(foreign, index, value)?;
    match bits {
        8 => u8::try_from(number)
            .map(ArgValue::U8)
            .map_err(|_| int_arg_out_of_range(foreign, index, "Nat8")),
        16 => u16::try_from(number)
            .map(ArgValue::U16)
            .map_err(|_| int_arg_out_of_range(foreign, index, "Nat16")),
        32 => u32::try_from(number)
            .map(ArgValue::U32)
            .map_err(|_| int_arg_out_of_range(foreign, index, "Nat32")),
        _ => Ok(ArgValue::U64(number)),
    }
}

fn signed_arg_number(foreign: &ForeignCall, index: usize, value: &Value) -> VmResult<i64> {
    match value {
        Value::Int(number) => Ok(*number),
        other => invalid_arg_type(foreign, index, "Int", other),
    }
}

fn unsigned_arg_number(foreign: &ForeignCall, index: usize, value: &Value) -> VmResult<u64> {
    match value {
        Value::Nat(number) => Ok(*number),
        Value::Int(number) => u64::try_from(*number).map_err(|_| {
            native_arg_invalid(foreign, index, "unsigned argument is negative".into())
        }),
        other => invalid_arg_type(foreign, index, "Nat", other),
    }
}

fn int_arg_out_of_range(foreign: &ForeignCall, index: usize, ty: &str) -> VmError {
    native_arg_invalid(
        foreign,
        index,
        format!("argument out of range for `{ty}`").into(),
    )
}

fn f32_from_f64(value: f64) -> f32 {
    value.to_string().parse::<f32>().unwrap_or_else(|_| {
        if value.is_sign_negative() {
            f32::NEG_INFINITY
        } else {
            f32::INFINITY
        }
    })
}

fn marshal_record_bytes(
    ctx: &mut VmHostContext<'_>,
    foreign: &ForeignCall,
    index: usize,
    value: &Value,
    fields: &[NativeAbiType],
    ffi: &FfiTypeRef,
) -> VmResult<(Vec<u8>, Vec<CString>)> {
    let Some(data) = ctx.record(value) else {
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
            "`@repr(\"c\")` product argument has non-zero tag".into(),
        ));
    }
    if data.len() != fields.len() {
        return Err(native_arg_invalid(
            foreign,
            index,
            "`@repr(\"c\")` product argument field count mismatch".into(),
        ));
    }
    let field_values = (0..fields.len())
        .map(|field_index| {
            data.get(field_index).cloned().ok_or_else(|| {
                native_arg_invalid(
                    foreign,
                    index,
                    "`@repr(\"c\")` product argument field missing".into(),
                )
            })
        })
        .collect::<VmResult<Vec<_>>>()?;
    let size = ffi.struct_size().unwrap_or(0);
    let mut bytes = vec![0_u8; size];
    let mut strings = Vec::<CString>::new();
    let offsets = ffi.struct_offsets().unwrap_or(&[]);
    for (field_index, field_ty) in fields.iter().enumerate() {
        let Some(offset) = offsets.get(field_index).copied() else {
            return Err(native_arg_invalid(
                foreign,
                index,
                "`@repr(\"c\")` product argument offset missing".into(),
            ));
        };
        let mut write_ctx = FieldWriteCtx {
            foreign,
            arg_index: index,
            out: &mut bytes,
            strings: &mut strings,
            vm: ctx,
        };
        write_field_bytes(
            &mut write_ctx,
            &field_values[field_index],
            field_ty,
            ffi_child(ffi, field_index),
            offset,
        )?;
    }
    Ok((bytes, strings))
}

fn write_field_bytes(
    ctx: &mut FieldWriteCtx<'_, '_>,
    value: &Value,
    ty: &NativeAbiType,
    ffi: Option<&FfiTypeRef>,
    offset: usize,
) -> VmResult<()> {
    match ty {
        NativeAbiType::Unit => Ok(()),
        NativeAbiType::Bool { .. } => ctx.vm.bool_flag(value).map_or_else(
            || {
                Err(native_arg_invalid(
                    ctx.foreign,
                    ctx.arg_index,
                    format!("expected type `Bool`, found `{:?}`", value.kind()).into(),
                ))
            },
            |flag| write_bytes(ctx.out, offset, &[u8::from(flag)]),
        ),
        NativeAbiType::Int { signed, bits } => {
            let arg = marshal_int_arg(ctx.foreign, ctx.arg_index, value, *signed, *bits)?;
            write_arg_bytes(ctx.out, offset, &arg)
        }
        NativeAbiType::Float { bits } => match value {
            Value::Float(number) if *bits == 32 => {
                write_bytes(ctx.out, offset, &f32_from_f64(*number).to_ne_bytes())
            }
            Value::Float(number) => write_bytes(ctx.out, offset, &number.to_ne_bytes()),
            other => Err(native_arg_invalid(
                ctx.foreign,
                ctx.arg_index,
                format!("expected type `Float`, found `{:?}`", other.kind()).into(),
            )),
        },
        NativeAbiType::CString => match ctx.vm.string(value) {
            Some(text) => {
                let c_string = CString::new(text.as_str()).map_err(|_| {
                    native_arg_invalid(
                        ctx.foreign,
                        ctx.arg_index,
                        "`CString` argument contains interior NULL".into(),
                    )
                })?;
                let pointer = c_string.as_ptr().addr();
                ctx.strings.push(c_string);
                write_bytes(ctx.out, offset, &pointer.to_ne_bytes())
            }
            None => Err(native_arg_invalid(
                ctx.foreign,
                ctx.arg_index,
                format!("expected type `CString`, found `{:?}`", value.kind()).into(),
            )),
        },
        NativeAbiType::CPtr => match value {
            Value::CPtr(address) => write_bytes(ctx.out, offset, &address.to_ne_bytes()),
            other => Err(native_arg_invalid(
                ctx.foreign,
                ctx.arg_index,
                format!("expected type `CPtr`, found `{:?}`", other.kind()).into(),
            )),
        },
        NativeAbiType::Transparent { inner, .. } => {
            let (inner_value, _) =
                extract_wrapper_value(ctx.vm, ctx.foreign, ctx.arg_index, value, ty)?;
            write_field_bytes(ctx, &inner_value, inner, ffi, offset)
        }
        NativeAbiType::ReprCProduct { .. } => {
            let Some(ffi) = ffi else {
                return Err(native_arg_invalid(
                    ctx.foreign,
                    ctx.arg_index,
                    "nested `@repr(\"c\")` product FFI metadata missing".into(),
                ));
            };
            let (nested, nested_strings) = marshal_record_bytes(
                ctx.vm,
                ctx.foreign,
                ctx.arg_index,
                value,
                repr_c_fields(ty),
                ffi,
            )?;
            ctx.strings.extend(nested_strings);
            write_bytes(ctx.out, offset, &nested)
        }
    }
}

fn alloc_result_value(ty: &NativeAbiType, ffi: &FfiTypeRef) -> ResultValue {
    match ty {
        NativeAbiType::Unit => ResultValue::Unit,
        NativeAbiType::Bool { .. } => ResultValue::Bool(0),
        NativeAbiType::Int { signed, bits } => match (*signed, *bits) {
            (true, 8) => ResultValue::I8(0),
            (false, 8) => ResultValue::U8(0),
            (true, 16) => ResultValue::I16(0),
            (false, 16) => ResultValue::U16(0),
            (true, 32) => ResultValue::I32(0),
            (false, 32) => ResultValue::U32(0),
            (true, _) => ResultValue::I64(0),
            (false, _) => ResultValue::U64(0),
        },
        NativeAbiType::Float { bits: 32 } => ResultValue::F32(0.0),
        NativeAbiType::Float { .. } => ResultValue::F64(0.0),
        NativeAbiType::CString | NativeAbiType::CPtr => ResultValue::Pointer(null_mut()),
        NativeAbiType::Transparent { inner, .. } => alloc_result_value(inner, ffi),
        NativeAbiType::ReprCProduct { .. } => {
            ResultValue::Struct(vec![0_u8; ffi.struct_size().unwrap_or(0)])
        }
    }
}

fn unmarshal_result_value(
    ctx: VmHostCallContext<'_, '_>,
    foreign: &ForeignCall,
    ty: &NativeAbiType,
    ffi: &FfiTypeRef,
    value: ResultValue,
) -> VmResult<Value> {
    match (ty, value) {
        (NativeAbiType::Unit, ResultValue::Unit) => Ok(Value::Unit),
        (NativeAbiType::Bool { ty }, ResultValue::Bool(flag)) => {
            ctx.alloc_data(*ty, i64::from(flag != 0), [])
        }
        (NativeAbiType::Int { signed: true, .. }, ResultValue::I8(number)) => {
            Ok(Value::Int(i64::from(number)))
        }
        (NativeAbiType::Int { signed: true, .. }, ResultValue::I16(number)) => {
            Ok(Value::Int(i64::from(number)))
        }
        (NativeAbiType::Int { signed: true, .. }, ResultValue::I32(number)) => {
            Ok(Value::Int(i64::from(number)))
        }
        (NativeAbiType::Int { signed: true, .. }, ResultValue::I64(number)) => {
            Ok(Value::Int(number))
        }
        (NativeAbiType::Int { signed: false, .. }, ResultValue::U8(number)) => {
            Ok(Value::Nat(u64::from(number)))
        }
        (NativeAbiType::Int { signed: false, .. }, ResultValue::U16(number)) => {
            Ok(Value::Nat(u64::from(number)))
        }
        (NativeAbiType::Int { signed: false, .. }, ResultValue::U32(number)) => {
            Ok(Value::Nat(u64::from(number)))
        }
        (NativeAbiType::Int { signed: false, .. }, ResultValue::U64(number)) => {
            Ok(Value::Nat(number))
        }
        (NativeAbiType::Float { .. }, ResultValue::F32(number)) => {
            Ok(Value::Float(f64::from(number)))
        }
        (NativeAbiType::Float { .. }, ResultValue::F64(number)) => Ok(Value::Float(number)),
        (NativeAbiType::CPtr, ResultValue::Pointer(pointer)) => Ok(Value::CPtr(pointer.addr())),
        (NativeAbiType::CString, ResultValue::Pointer(pointer)) => {
            if pointer.is_null() {
                return Err(native_result_invalid(
                    foreign,
                    "`CString` result was null".into(),
                ));
            }
            let c_text = {
                // SAFETY: native code returned a non-null C string pointer for this result.
                unsafe { CStr::from_ptr(pointer.cast()) }
            };
            let text = c_text.to_str().map_err(|error| {
                native_result_invalid(
                    foreign,
                    format!("`CString` result is not UTF-8 (`{error}`)").into(),
                )
            })?;
            ctx.alloc_string(text)
        }
        (NativeAbiType::Transparent { ty, inner }, result) => {
            let inner_value = unmarshal_result_value(ctx, foreign, inner, ffi, result)?;
            ctx.alloc_data(*ty, 0, [inner_value])
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
                            "`@repr(\"c\")` result offset missing".into(),
                        ));
                    };
                    read_field_value(
                        ctx,
                        foreign,
                        field_ty,
                        ffi_child(ffi, index),
                        &bytes,
                        offset,
                    )
                })
                .collect::<VmResult<Vec<_>>>()?;
            ctx.alloc_data(*ty, 0, values)
        }
        _ => Err(native_result_invalid(
            foreign,
            "`@repr(\"c\")` result storage shape mismatch".into(),
        )),
    }
}

fn read_field_value(
    ctx: VmHostCallContext<'_, '_>,
    foreign: &ForeignCall,
    ty: &NativeAbiType,
    ffi: Option<&FfiTypeRef>,
    bytes: &[u8],
    offset: usize,
) -> VmResult<Value> {
    match ty {
        NativeAbiType::Unit => Ok(Value::Unit),
        NativeAbiType::Bool { ty } => {
            ctx.alloc_data(*ty, i64::from(read_u8(bytes, offset)? != 0), [])
        }
        NativeAbiType::Int { signed, bits } => read_int_field(bytes, offset, *signed, *bits),
        NativeAbiType::Float { bits: 32 } => Ok(Value::Float(f64::from(f32::from_ne_bytes(
            read_array(bytes, offset)?,
        )))),
        NativeAbiType::Float { .. } => {
            Ok(Value::Float(f64::from_ne_bytes(read_array(bytes, offset)?)))
        }
        NativeAbiType::CString => {
            let ptr = usize::from_ne_bytes(read_array(bytes, offset)?);
            if ptr == 0 {
                return Err(native_result_invalid(
                    foreign,
                    "`CString` result was null".into(),
                ));
            }
            let c_text = {
                // SAFETY: native code wrote a non-null C string pointer into the repr(c) field.
                unsafe { CStr::from_ptr(null::<c_void>().with_addr(ptr).cast()) }
            };
            let text = c_text.to_str().map_err(|error| {
                native_result_invalid(
                    foreign,
                    format!("`CString` result is not UTF-8 (`{error}`)").into(),
                )
            })?;
            ctx.alloc_string(text)
        }
        NativeAbiType::CPtr => Ok(Value::CPtr(usize::from_ne_bytes(read_array(
            bytes, offset,
        )?))),
        NativeAbiType::Transparent { ty, inner } => {
            let inner_value = read_field_value(ctx, foreign, inner, ffi, bytes, offset)?;
            ctx.alloc_data(*ty, 0, [inner_value])
        }
        NativeAbiType::ReprCProduct { ty, fields, .. } => {
            let Some(ffi) = ffi else {
                return Err(native_result_invalid(
                    foreign,
                    "`@repr(\"c\")` result foreign function interface metadata missing".into(),
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
                            "`@repr(\"c\")` result offset missing".into(),
                        ));
                    };
                    read_field_value(
                        ctx,
                        foreign,
                        field_ty,
                        ffi_child(ffi, index),
                        bytes,
                        offset + field_offset,
                    )
                })
                .collect::<VmResult<Vec<_>>>()?;
            ctx.alloc_data(*ty, 0, values)
        }
    }
}

fn write_arg_bytes(out: &mut [u8], offset: usize, value: &ArgValue) -> VmResult {
    match value {
        ArgValue::Bool(value) => write_bytes(out, offset, &[*value]),
        ArgValue::I8(value) => write_bytes(out, offset, &value.to_ne_bytes()),
        ArgValue::U8(value) => write_bytes(out, offset, &value.to_ne_bytes()),
        ArgValue::I16(value) => write_bytes(out, offset, &value.to_ne_bytes()),
        ArgValue::U16(value) => write_bytes(out, offset, &value.to_ne_bytes()),
        ArgValue::I32(value) => write_bytes(out, offset, &value.to_ne_bytes()),
        ArgValue::U32(value) => write_bytes(out, offset, &value.to_ne_bytes()),
        ArgValue::I64(value) => write_bytes(out, offset, &value.to_ne_bytes()),
        ArgValue::U64(value) => write_bytes(out, offset, &value.to_ne_bytes()),
        ArgValue::F32(value) => write_bytes(out, offset, &value.to_ne_bytes()),
        ArgValue::F64(value) => write_bytes(out, offset, &value.to_ne_bytes()),
        ArgValue::Pointer(value) => write_bytes(out, offset, &value.addr().to_ne_bytes()),
        ArgValue::CString { pointer, .. } => {
            write_bytes(out, offset, &pointer.addr().to_ne_bytes())
        }
        ArgValue::Struct { bytes, .. } => write_bytes(out, offset, bytes),
    }
}

fn read_int_field(bytes: &[u8], offset: usize, signed: bool, bits: u8) -> VmResult<Value> {
    match (signed, bits) {
        (true, 8) => Ok(Value::Int(i64::from(i8::from_ne_bytes(read_array(
            bytes, offset,
        )?)))),
        (false, 8) => Ok(Value::Nat(u64::from(u8::from_ne_bytes(read_array(
            bytes, offset,
        )?)))),
        (true, 16) => Ok(Value::Int(i64::from(i16::from_ne_bytes(read_array(
            bytes, offset,
        )?)))),
        (false, 16) => Ok(Value::Nat(u64::from(u16::from_ne_bytes(read_array(
            bytes, offset,
        )?)))),
        (true, 32) => Ok(Value::Int(i64::from(i32::from_ne_bytes(read_array(
            bytes, offset,
        )?)))),
        (false, 32) => Ok(Value::Nat(u64::from(u32::from_ne_bytes(read_array(
            bytes, offset,
        )?)))),
        (true, _) => Ok(Value::Int(i64::from_ne_bytes(read_array(bytes, offset)?))),
        (false, _) => Ok(Value::Nat(u64::from_ne_bytes(read_array(bytes, offset)?))),
    }
}
