use crate::errors::{MusiError, MusiResult};
use crate::value::Value;

pub fn abs(args: &[Value]) -> MusiResult<Value> {
    if args.len() != 1 {
        return Err(MusiError::InvalidVmState(
            "'abs' expects 1 argument".to_string(),
        ));
    }
    match &args[0] {
        Value::Int(i) => Ok(Value::Int(i.abs())),
        Value::Int32(i) => Ok(Value::Int32(i.abs())),
        Value::Int64(i) => Ok(Value::Int64(i.abs())),
        Value::Bin32(f) => Ok(Value::Bin32(f.abs())),
        Value::Bin64(f) => Ok(Value::Bin64(f.abs())),
        _ => Err(MusiError::TypeMismatch),
    }
}

pub fn min(args: &[Value]) -> MusiResult<Value> {
    if args.len() != 2 {
        return Err(MusiError::InvalidVmState(
            "'min' expects 2 arguments".to_string(),
        ));
    }
    match (&args[0], &args[1]) {
        (Value::Int(a), Value::Int(b)) => Ok(Value::Int(*a.min(b))),
        (Value::Int32(a), Value::Int32(b)) => Ok(Value::Int32(*a.min(b))),
        (Value::Int64(a), Value::Int64(b)) => Ok(Value::Int64(*a.min(b))),
        (Value::Nat(a), Value::Nat(b)) => Ok(Value::Nat(*a.min(b))),
        (Value::Nat32(a), Value::Nat32(b)) => Ok(Value::Nat32(*a.min(b))),
        (Value::Nat64(a), Value::Nat64(b)) => Ok(Value::Nat64(*a.min(b))),
        (Value::Bin32(a), Value::Bin32(b)) => Ok(Value::Bin32(a.min(*b))),
        (Value::Bin64(a), Value::Bin64(b)) => Ok(Value::Bin64(a.min(*b))),
        _ => Err(MusiError::TypeMismatch),
    }
}

pub fn max(args: &[Value]) -> MusiResult<Value> {
    if args.len() != 2 {
        return Err(MusiError::InvalidVmState(
            "'max' expects 2 arguments".to_string(),
        ));
    }
    match (&args[0], &args[1]) {
        (Value::Int(a), Value::Int(b)) => Ok(Value::Int(*a.max(b))),
        (Value::Int32(a), Value::Int32(b)) => Ok(Value::Int32(*a.max(b))),
        (Value::Int64(a), Value::Int64(b)) => Ok(Value::Int64(*a.max(b))),
        (Value::Nat(a), Value::Nat(b)) => Ok(Value::Nat(*a.max(b))),
        (Value::Nat32(a), Value::Nat32(b)) => Ok(Value::Nat32(*a.max(b))),
        (Value::Nat64(a), Value::Nat64(b)) => Ok(Value::Nat64(*a.max(b))),
        (Value::Bin32(a), Value::Bin32(b)) => Ok(Value::Bin32(a.max(*b))),
        (Value::Bin64(a), Value::Bin64(b)) => Ok(Value::Bin64(a.max(*b))),
        _ => Err(MusiError::TypeMismatch),
    }
}

pub fn sqrt(args: &[Value]) -> MusiResult<Value> {
    if args.len() != 1 {
        return Err(MusiError::InvalidVmState(
            "'sqrt' expects 1 argument".to_string(),
        ));
    }
    match &args[0] {
        Value::Bin32(f) => {
            if *f < 0.0 {
                return Err(MusiError::InvalidVmState(
                    "'sqrt' of negative number".to_string(),
                ));
            }
            Ok(Value::Bin32(f.sqrt()))
        }
        Value::Bin64(f) => {
            if *f < 0.0 {
                return Err(MusiError::InvalidVmState(
                    "'sqrt' of negative number".to_string(),
                ));
            }
            Ok(Value::Bin64(f.sqrt()))
        }
        _ => Err(MusiError::TypeMismatch),
    }
}
