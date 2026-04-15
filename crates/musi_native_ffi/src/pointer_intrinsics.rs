use std::ptr::{
    read_unaligned, with_exposed_provenance, with_exposed_provenance_mut, write_unaligned,
};

use musi_vm::{ForeignCall, Value, VmError, VmErrorKind, VmResult};

pub fn call_musi_pointer_intrinsic(
    foreign: &ForeignCall,
    args: &[Value],
) -> Option<VmResult<Value>> {
    if foreign.abi() != "musi" {
        return None;
    }
    let result = match foreign.symbol() {
        "ffi.ptr.read.i8" => ptr_read::<i8>(foreign, args).map(|value| Value::Int(value.into())),
        "ffi.ptr.read.u8" => ptr_read::<u8>(foreign, args).map(|value| Value::Int(value.into())),
        "ffi.ptr.read.i16" => ptr_read::<i16>(foreign, args).map(|value| Value::Int(value.into())),
        "ffi.ptr.read.u16" => ptr_read::<u16>(foreign, args).map(|value| Value::Int(value.into())),
        "ffi.ptr.read.i32" => ptr_read::<i32>(foreign, args).map(|value| Value::Int(value.into())),
        "ffi.ptr.read.u32" => ptr_read::<u32>(foreign, args).map(|value| Value::Int(value.into())),
        "ffi.ptr.read.i64" | "ffi.ptr.read.u64" => ptr_read::<i64>(foreign, args).map(Value::Int),
        "ffi.ptr.read.f32" => {
            ptr_read::<f32>(foreign, args).map(|value| Value::Float(value.into()))
        }
        "ffi.ptr.read.f64" => ptr_read::<f64>(foreign, args).map(Value::Float),
        "ffi.ptr.read.ptr" => ptr_read::<usize>(foreign, args).map(Value::CPtr),
        "ffi.ptr.write.i8" => ptr_write_int::<i8>(foreign, args),
        "ffi.ptr.write.u8" => ptr_write_int::<u8>(foreign, args),
        "ffi.ptr.write.i16" => ptr_write_int::<i16>(foreign, args),
        "ffi.ptr.write.u16" => ptr_write_int::<u16>(foreign, args),
        "ffi.ptr.write.i32" => ptr_write_int::<i32>(foreign, args),
        "ffi.ptr.write.u32" => ptr_write_int::<u32>(foreign, args),
        "ffi.ptr.write.i64" | "ffi.ptr.write.u64" => ptr_write_int::<i64>(foreign, args),
        "ffi.ptr.write.f32" => ptr_write_float::<f32>(foreign, args),
        "ffi.ptr.write.f64" => ptr_write_float::<f64>(foreign, args),
        "ffi.ptr.write.ptr" => ptr_write_ptr(foreign, args),
        _ => return None,
    };
    Some(result)
}

fn ptr_read<T: Copy>(foreign: &ForeignCall, args: &[Value]) -> VmResult<T> {
    let address = ptr_arg(foreign, args, 0)?;
    if address == 0 {
        return Err(pointer_intrinsic_failed(foreign, "null pointer read"));
    }
    let ptr = with_exposed_provenance::<T>(address);
    Ok(unsafe { read_unaligned(ptr) })
}

fn ptr_write_int<T>(foreign: &ForeignCall, args: &[Value]) -> VmResult<Value>
where
    T: TryFrom<i64>,
{
    let value = int_arg(foreign, args, 1)?;
    let value = T::try_from(value).map_err(|_| {
        pointer_intrinsic_failed(foreign, "integer value out of pointer storage range")
    })?;
    ptr_write_raw(foreign, args, value)
}

fn ptr_write_float<T>(foreign: &ForeignCall, args: &[Value]) -> VmResult<Value>
where
    T: FromF64,
{
    let value = float_arg(foreign, args, 1)?;
    ptr_write_raw(foreign, args, T::from_f64(value))
}

fn ptr_write_ptr(foreign: &ForeignCall, args: &[Value]) -> VmResult<Value> {
    let value = ptr_arg(foreign, args, 1)?;
    ptr_write_raw(foreign, args, value)
}

fn ptr_write_raw<T>(foreign: &ForeignCall, args: &[Value], value: T) -> VmResult<Value> {
    let address = ptr_arg(foreign, args, 0)?;
    if address == 0 {
        return Err(pointer_intrinsic_failed(foreign, "null pointer write"));
    }
    let ptr = with_exposed_provenance_mut::<T>(address);
    unsafe { write_unaligned(ptr, value) };
    Ok(Value::Unit)
}

fn ptr_arg(foreign: &ForeignCall, args: &[Value], index: usize) -> VmResult<usize> {
    match args.get(index) {
        Some(Value::CPtr(address)) => Ok(*address),
        Some(pointer) => match pointer
            .as_record()
            .and_then(|record| record.get(0).cloned())
        {
            Some(Value::CPtr(address)) => Ok(address),
            Some(_) => Err(pointer_intrinsic_failed(
                foreign,
                "pointer data field must be CPtr",
            )),
            None if matches!(pointer, Value::Data(_)) => Err(pointer_intrinsic_failed(
                foreign,
                "pointer data field missing",
            )),
            None => Err(pointer_intrinsic_failed(
                foreign,
                match pointer {
                    Value::CPtr(_) | Value::Data(_) => "pointer argument invalid",
                    _ => "pointer argument must be Ptr or CPtr",
                },
            )),
        },
        None => Err(pointer_intrinsic_failed(
            foreign,
            "pointer intrinsic argument missing",
        )),
    }
}

fn int_arg(foreign: &ForeignCall, args: &[Value], index: usize) -> VmResult<i64> {
    match args.get(index) {
        Some(Value::Int(value)) => Ok(*value),
        Some(_) => Err(pointer_intrinsic_failed(
            foreign,
            "pointer value must be Int",
        )),
        None => Err(pointer_intrinsic_failed(
            foreign,
            "pointer intrinsic argument missing",
        )),
    }
}

fn float_arg(foreign: &ForeignCall, args: &[Value], index: usize) -> VmResult<f64> {
    match args.get(index) {
        Some(Value::Float(value)) => Ok(*value),
        Some(_) => Err(pointer_intrinsic_failed(
            foreign,
            "pointer value must be Float",
        )),
        None => Err(pointer_intrinsic_failed(
            foreign,
            "pointer intrinsic argument missing",
        )),
    }
}

fn pointer_intrinsic_failed(foreign: &ForeignCall, detail: &'static str) -> VmError {
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
