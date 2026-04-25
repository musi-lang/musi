use std::ptr::{
    read_unaligned, with_exposed_provenance, with_exposed_provenance_mut, write_unaligned,
};

use musi_vm::{ForeignCall, Value, VmError, VmErrorKind, VmHostContext, VmResult};

pub fn call_musi_pointer_intrinsic(
    ctx: &VmHostContext<'_>,
    foreign: &ForeignCall,
    args: &[Value],
) -> Option<VmResult<Value>> {
    if foreign.abi() != "musi" {
        return None;
    }
    let intrinsic_result = match foreign.symbol() {
        "ffi.ptr.read.i8" => {
            ptr_read::<i8>(ctx, foreign, args).map(|value| Value::Int(value.into()))
        }
        "ffi.ptr.read.u8" => {
            ptr_read::<u8>(ctx, foreign, args).map(|value| Value::Nat(value.into()))
        }
        "ffi.ptr.read.i16" => {
            ptr_read::<i16>(ctx, foreign, args).map(|value| Value::Int(value.into()))
        }
        "ffi.ptr.read.u16" => {
            ptr_read::<u16>(ctx, foreign, args).map(|value| Value::Nat(value.into()))
        }
        "ffi.ptr.read.i32" => {
            ptr_read::<i32>(ctx, foreign, args).map(|value| Value::Int(value.into()))
        }
        "ffi.ptr.read.u32" => {
            ptr_read::<u32>(ctx, foreign, args).map(|value| Value::Nat(value.into()))
        }
        "ffi.ptr.read.i64" => ptr_read::<i64>(ctx, foreign, args).map(Value::Int),
        "ffi.ptr.read.u64" => ptr_read::<u64>(ctx, foreign, args).map(Value::Nat),
        "ffi.ptr.read.f32" => {
            ptr_read::<f32>(ctx, foreign, args).map(|value| Value::Float(value.into()))
        }
        "ffi.ptr.read.f64" => ptr_read::<f64>(ctx, foreign, args).map(Value::Float),
        "ffi.ptr.read.ptr" => ptr_read::<usize>(ctx, foreign, args).map(Value::CPtr),
        "ffi.ptr.write.i8" => ptr_write_int::<i8>(ctx, foreign, args),
        "ffi.ptr.write.u8" => ptr_write_nat::<u8>(ctx, foreign, args),
        "ffi.ptr.write.i16" => ptr_write_int::<i16>(ctx, foreign, args),
        "ffi.ptr.write.u16" => ptr_write_nat::<u16>(ctx, foreign, args),
        "ffi.ptr.write.i32" => ptr_write_int::<i32>(ctx, foreign, args),
        "ffi.ptr.write.u32" => ptr_write_nat::<u32>(ctx, foreign, args),
        "ffi.ptr.write.i64" => ptr_write_int::<i64>(ctx, foreign, args),
        "ffi.ptr.write.u64" => ptr_write_nat::<u64>(ctx, foreign, args),
        "ffi.ptr.write.f32" => ptr_write_float::<f32>(ctx, foreign, args),
        "ffi.ptr.write.f64" => ptr_write_float::<f64>(ctx, foreign, args),
        "ffi.ptr.write.ptr" => ptr_write_ptr(ctx, foreign, args),
        _ => return None,
    };
    Some(intrinsic_result)
}

fn ptr_read<T: Copy>(
    ctx: &VmHostContext<'_>,
    foreign: &ForeignCall,
    args: &[Value],
) -> VmResult<T> {
    let address = ptr_arg(ctx, foreign, args, 0)?;
    if address == 0 {
        return Err(pointer_intrinsic_failed(foreign, "null pointer read"));
    }
    let ptr = with_exposed_provenance::<T>(address);
    Ok(unsafe { read_unaligned(ptr) })
}

fn ptr_write_int<T>(
    ctx: &VmHostContext<'_>,
    foreign: &ForeignCall,
    args: &[Value],
) -> VmResult<Value>
where
    T: TryFrom<i64>,
{
    let source_int = int_arg(foreign, args, 1)?;
    let typed_int = T::try_from(source_int).map_err(|_| {
        pointer_intrinsic_failed(foreign, "integer value out of pointer storage range")
    })?;
    ptr_write_raw(ctx, foreign, args, typed_int)
}

fn ptr_write_nat<T>(
    ctx: &VmHostContext<'_>,
    foreign: &ForeignCall,
    args: &[Value],
) -> VmResult<Value>
where
    T: TryFrom<u64>,
{
    let source_nat = nat_arg(foreign, args, 1)?;
    let typed_nat = T::try_from(source_nat).map_err(|_| {
        pointer_intrinsic_failed(foreign, "integer value out of pointer storage range")
    })?;
    ptr_write_raw(ctx, foreign, args, typed_nat)
}

fn ptr_write_float<T>(
    ctx: &VmHostContext<'_>,
    foreign: &ForeignCall,
    args: &[Value],
) -> VmResult<Value>
where
    T: FromF64,
{
    let source_float = float_arg(foreign, args, 1)?;
    ptr_write_raw(ctx, foreign, args, T::from_f64(source_float))
}

fn ptr_write_ptr(
    ctx: &VmHostContext<'_>,
    foreign: &ForeignCall,
    args: &[Value],
) -> VmResult<Value> {
    let pointer_address = ptr_arg(ctx, foreign, args, 1)?;
    ptr_write_raw(ctx, foreign, args, pointer_address)
}

fn ptr_write_raw<T>(
    ctx: &VmHostContext<'_>,
    foreign: &ForeignCall,
    args: &[Value],
    value: T,
) -> VmResult<Value> {
    let address = ptr_arg(ctx, foreign, args, 0)?;
    if address == 0 {
        return Err(pointer_intrinsic_failed(foreign, "null pointer write"));
    }
    let ptr = with_exposed_provenance_mut::<T>(address);
    unsafe { write_unaligned(ptr, value) };
    Ok(Value::Unit)
}

fn ptr_arg(
    ctx: &VmHostContext<'_>,
    foreign: &ForeignCall,
    args: &[Value],
    index: usize,
) -> VmResult<usize> {
    match args.get(index) {
        Some(Value::CPtr(address)) => Ok(*address),
        Some(pointer) => match ctx
            .record(pointer)
            .and_then(|record| record.get(0).cloned())
        {
            Some(Value::CPtr(address)) => Ok(address),
            Some(_) => Err(pointer_intrinsic_failed(
                foreign,
                format!("pointer argument `{index}` data field must be type 'CPtr'"),
            )),
            None if matches!(pointer, Value::Data(_)) => Err(pointer_intrinsic_failed(
                foreign,
                format!("pointer argument `{index}` data field missing"),
            )),
            None => Err(pointer_intrinsic_failed(
                foreign,
                match pointer {
                    Value::CPtr(_) | Value::Data(_) => {
                        format!("pointer argument `{index}` invalid")
                    }
                    _ => format!("pointer argument `{index}` must be type 'Ptr' or 'CPtr'"),
                },
            )),
        },
        None => Err(pointer_intrinsic_failed(
            foreign,
            format!("pointer intrinsic argument `{index}` missing"),
        )),
    }
}

fn int_arg(foreign: &ForeignCall, args: &[Value], index: usize) -> VmResult<i64> {
    match args.get(index) {
        Some(Value::Int(value)) => Ok(*value),
        Some(_) => Err(pointer_intrinsic_failed(
            foreign,
            format!("pointer argument `{index}` must be type 'Int'"),
        )),
        None => Err(pointer_intrinsic_failed(
            foreign,
            format!("pointer intrinsic argument `{index}` missing"),
        )),
    }
}

fn nat_arg(foreign: &ForeignCall, args: &[Value], index: usize) -> VmResult<u64> {
    match args.get(index) {
        Some(Value::Nat(value)) => Ok(*value),
        Some(Value::Int(value)) => u64::try_from(*value).map_err(|_| {
            pointer_intrinsic_failed(foreign, format!("pointer argument `{index}` must be Nat"))
        }),
        Some(_) => Err(pointer_intrinsic_failed(
            foreign,
            format!("pointer argument `{index}` must be type 'Nat'"),
        )),
        None => Err(pointer_intrinsic_failed(
            foreign,
            format!("pointer intrinsic argument `{index}` missing"),
        )),
    }
}

fn float_arg(foreign: &ForeignCall, args: &[Value], index: usize) -> VmResult<f64> {
    match args.get(index) {
        Some(Value::Float(value)) => Ok(*value),
        Some(_) => Err(pointer_intrinsic_failed(
            foreign,
            format!("pointer argument `{index}` must be type 'Float'"),
        )),
        None => Err(pointer_intrinsic_failed(
            foreign,
            format!("pointer intrinsic argument `{index}` missing"),
        )),
    }
}

fn pointer_intrinsic_failed(foreign: &ForeignCall, detail: impl Into<Box<str>>) -> VmError {
    VmError::new(VmErrorKind::PointerIntrinsicFailed {
        intrinsic: foreign.symbol().into(),
        detail: detail.into(),
    })
}

trait FromF64 {
    fn from_f64(value: f64) -> Self;
}

impl FromF64 for f32 {
    fn from_f64(value: f64) -> Self {
        value.to_string().parse::<Self>().unwrap_or_else(|_| {
            if value.is_sign_negative() {
                Self::NEG_INFINITY
            } else {
                Self::INFINITY
            }
        })
    }
}

impl FromF64 for f64 {
    fn from_f64(value: f64) -> Self {
        value
    }
}
