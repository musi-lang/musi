use crate::errors::{MusiError, MusiResult};
use crate::value::Value;

fn idx_from_val(val: &Value) -> MusiResult<usize> {
    match val {
        Value::Int(i) if *i >= 0 => Ok(*i as usize),
        Value::Nat(n) => Ok(*n),
        Value::Int32(i) if *i >= 0 => Ok(*i as usize),
        Value::Nat32(n) => Ok(*n as usize),
        _ => Err(MusiError::TypeMismatch),
    }
}

pub fn int_to_str(args: &[Value]) -> MusiResult<Value> {
    if args.len() != 1 {
        return Err(MusiError::InvalidVmState(
            "'int_to_str' expects 1 argument".to_string(),
        ));
    }
    match &args[0] {
        Value::Int(i) => Ok(Value::Str(i.to_string())),
        Value::Int32(i) => Ok(Value::Str(i.to_string())),
        Value::Int64(i) => Ok(Value::Str(i.to_string())),
        Value::Nat(n) => Ok(Value::Str(n.to_string())),
        Value::Nat32(n) => Ok(Value::Str(n.to_string())),
        Value::Nat64(n) => Ok(Value::Str(n.to_string())),
        _ => Err(MusiError::TypeMismatch),
    }
}

pub fn str_len(args: &[Value]) -> MusiResult<Value> {
    if args.len() != 1 {
        return Err(MusiError::InvalidVmState(
            "'str_len' expects 1 argument".to_string(),
        ));
    }
    match &args[0] {
        Value::Str(s) => Ok(Value::Nat(s.len())),
        _ => Err(MusiError::TypeMismatch),
    }
}

pub fn array_len(args: &[Value]) -> MusiResult<Value> {
    if args.len() != 1 {
        return Err(MusiError::InvalidVmState(
            "'array_len' expects 1 argument".to_string(),
        ));
    }
    match &args[0] {
        Value::ArrayList(arr) => Ok(Value::Nat(arr.len())),
        Value::Array { elems, .. } => Ok(Value::Nat(elems.len())),
        _ => Err(MusiError::TypeMismatch),
    }
}

pub fn array_get(args: &[Value]) -> MusiResult<Value> {
    if args.len() != 2 {
        return Err(MusiError::InvalidVmState(
            "'array_get' expects 2 arguments".to_string(),
        ));
    }

    let idx = idx_from_val(&args[1])?;

    match &args[0] {
        Value::ArrayList(arr) => {
            if idx >= arr.len() {
                return Err(MusiError::ArrayBounds {
                    idx: idx as isize,
                    size: arr.len(),
                });
            }
            Ok(arr[idx].clone())
        }
        Value::Array { elems, .. } => {
            if idx >= elems.len() {
                return Err(MusiError::ArrayBounds {
                    idx: idx as isize,
                    size: elems.len(),
                });
            }
            Ok(elems[idx].clone())
        }
        _ => Err(MusiError::TypeMismatch),
    }
}

pub fn array_set(args: &[Value]) -> MusiResult<Value> {
    if args.len() != 3 {
        return Err(MusiError::InvalidVmState(
            "'array_set' expects 3 arguments".to_string(),
        ));
    }

    let _idx = idx_from_val(&args[1])?;

    match &args[0] {
        Value::ArrayList(_) | Value::Array { .. } => Ok(Value::Unit),
        _ => Err(MusiError::TypeMismatch),
    }
}
