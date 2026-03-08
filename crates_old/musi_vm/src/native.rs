//! Native built-in function dispatch (core prelude ops only).
//!
//! `musi:string` / `musi:math` / `musi:io` functions are served by the `NativeRegistry`
//! and take priority over this fallback. Only irreducible VM primitives live here:
//! `writeln`, `write`, `int_to_string`, `float_to_string`, `string_concat`, and arrays.

use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

use musi_codegen::intrinsics::Intrinsic;

use crate::value::{HashKey, Value};
use crate::vm::Vm;

#[must_use]
pub fn dispatch(vm: &Vm, intrinsic: Intrinsic, args: &[Value]) -> Value {
    match intrinsic {
        Intrinsic::Writeln => intrinsic_writeln(vm, args),
        Intrinsic::Write => intrinsic_write(vm, args),
        Intrinsic::IntToString => intrinsic_int_to_string(vm, args),
        Intrinsic::FloatToString => intrinsic_float_to_string(vm, args),
        Intrinsic::StringConcat => intrinsic_string_concat(vm, args),
        Intrinsic::ArrayLength => intrinsic_array_length(vm, args),
        Intrinsic::ArrayPush => intrinsic_array_push(vm, args),
        Intrinsic::ArrayPop => intrinsic_array_pop(vm, args),
        Intrinsic::ArrayGet => intrinsic_array_get(vm, args),
        Intrinsic::ArraySet => intrinsic_array_set(vm, args),
        Intrinsic::ArraySlice => intrinsic_array_slice(vm, args),
        // -- string extended --
        Intrinsic::StringSplit => intrinsic_string_split(vm, args),
        Intrinsic::StringTrim => intrinsic_string_trim(vm, args),
        Intrinsic::StringToLower => intrinsic_string_to_lower(vm, args),
        Intrinsic::StringToUpper => intrinsic_string_to_upper(vm, args),
        Intrinsic::StringToFloat => intrinsic_string_to_float(vm, args),
        Intrinsic::StringIndexOf => intrinsic_string_index_of(vm, args),
        // -- numeric casts --
        Intrinsic::IntToFloat => intrinsic_int_to_float(vm, args),
        Intrinsic::FloatToInt => intrinsic_float_to_int(vm, args),
        // -- math trig --
        Intrinsic::Sin => intrinsic_trig1(vm, args, f64::sin),
        Intrinsic::Cos => intrinsic_trig1(vm, args, f64::cos),
        Intrinsic::Tan => intrinsic_trig1(vm, args, f64::tan),
        Intrinsic::Log => intrinsic_trig1(vm, args, f64::ln),
        Intrinsic::Exp => intrinsic_trig1(vm, args, f64::exp),
        Intrinsic::Atan2 => intrinsic_atan2(vm, args),
        // -- map --
        Intrinsic::MapNew => intrinsic_map_new(vm, args),
        Intrinsic::MapGet => intrinsic_map_get(vm, args),
        Intrinsic::MapSet => intrinsic_map_set(vm, args),
        Intrinsic::MapHas => intrinsic_map_has(vm, args),
        Intrinsic::MapDelete => intrinsic_map_delete(vm, args),
        Intrinsic::MapKeys => intrinsic_map_keys(vm, args),
        Intrinsic::MapValues => intrinsic_map_values(vm, args),
        Intrinsic::MapLen => intrinsic_map_len(vm, args),
        // All other intrinsics are intercepted before dispatch or served by NativeRegistry.
        _ => Value::Unit,
    }
}

// -- string extended ----------------------------------------------------------

fn strings_to_array(parts: impl Iterator<Item = String>) -> Value {
    let vec: Vec<Value> = parts.map(|s| Value::String(Rc::from(s.as_str()))).collect();
    Value::Array(Rc::new(RefCell::new(vec)))
}

fn intrinsic_string_split(_vm: &Vm, args: &[Value]) -> Value {
    match (args.first(), args.get(1)) {
        (Some(Value::String(s)), Some(Value::String(delim))) => {
            if delim.is_empty() {
                strings_to_array(s.chars().map(|c| c.to_string()))
            } else {
                strings_to_array(s.split(delim.as_ref()).map(str::to_owned))
            }
        }
        _ => Value::Array(Rc::new(RefCell::new(Vec::new()))),
    }
}

fn intrinsic_string_trim(_vm: &Vm, args: &[Value]) -> Value {
    match args.first() {
        Some(Value::String(s)) => Value::String(Rc::from(s.trim())),
        _ => Value::String(Rc::from("")),
    }
}

fn intrinsic_string_to_lower(_vm: &Vm, args: &[Value]) -> Value {
    match args.first() {
        Some(Value::String(s)) => Value::String(Rc::from(s.to_lowercase().as_str())),
        _ => Value::String(Rc::from("")),
    }
}

fn intrinsic_string_to_upper(_vm: &Vm, args: &[Value]) -> Value {
    match args.first() {
        Some(Value::String(s)) => Value::String(Rc::from(s.to_uppercase().as_str())),
        _ => Value::String(Rc::from("")),
    }
}

fn intrinsic_string_to_float(_vm: &Vm, args: &[Value]) -> Value {
    match args.first() {
        Some(Value::String(s)) => s
            .parse::<f64>()
            .map_or_else(|_| option_none(), |f| option_some(Value::Float(f))),
        _ => option_none(),
    }
}

fn intrinsic_string_index_of(_vm: &Vm, args: &[Value]) -> Value {
    match (args.first(), args.get(1)) {
        (Some(Value::String(s)), Some(Value::String(sub))) => {
            s.find(sub.as_ref()).map_or_else(option_none, |i| {
                option_some(Value::Int(i64::try_from(i).unwrap_or(i64::MAX)))
            })
        }
        _ => option_none(),
    }
}

// -- numeric casts ------------------------------------------------------------

#[allow(
    clippy::cast_precision_loss,
    clippy::as_conversions,
    clippy::missing_const_for_fn
)]
fn intrinsic_int_to_float(_vm: &Vm, args: &[Value]) -> Value {
    match args.first() {
        Some(Value::Int(n)) => Value::Float(*n as f64),
        _ => Value::Float(0.0),
    }
}

#[allow(
    clippy::cast_possible_truncation,
    clippy::as_conversions,
    clippy::missing_const_for_fn
)]
fn intrinsic_float_to_int(_vm: &Vm, args: &[Value]) -> Value {
    match args.first() {
        Some(Value::Float(f)) => Value::Int(*f as i64),
        _ => Value::Int(0),
    }
}

// -- math trig ----------------------------------------------------------------

fn intrinsic_trig1(_vm: &Vm, args: &[Value], f: fn(f64) -> f64) -> Value {
    match args.first() {
        Some(Value::Float(x)) => Value::Float(f(*x)),
        _ => Value::Float(0.0),
    }
}

fn intrinsic_atan2(_vm: &Vm, args: &[Value]) -> Value {
    match (args.first(), args.get(1)) {
        (Some(Value::Float(y)), Some(Value::Float(x))) => Value::Float(y.atan2(*x)),
        _ => Value::Float(0.0),
    }
}

// -- map ----------------------------------------------------------------------

fn bool_object(b: bool) -> Value {
    Value::Object {
        type_tag: 0,
        fields: Rc::new(vec![Value::Int(i64::from(b))]),
    }
}

fn intrinsic_map_new(_vm: &Vm, _args: &[Value]) -> Value {
    Value::Map(Rc::new(RefCell::new(HashMap::new())))
}

fn intrinsic_map_get(_vm: &Vm, args: &[Value]) -> Value {
    match (args.first(), args.get(1)) {
        (Some(Value::Map(m)), Some(key)) => HashKey::try_from(key).map_or_else(
            |_| option_none(),
            |hk| {
                m.borrow()
                    .get(&hk)
                    .map_or_else(option_none, |v| option_some(v.clone()))
            },
        ),
        _ => option_none(),
    }
}

fn intrinsic_map_set(_vm: &Vm, args: &[Value]) -> Value {
    match (args.first(), args.get(1), args.get(2)) {
        (Some(Value::Map(m)), Some(key), Some(val)) => {
            if let Ok(hk) = HashKey::try_from(key) {
                let _prev = m.borrow_mut().insert(hk, val.clone());
            }
            Value::Unit
        }
        _ => Value::Unit,
    }
}

fn intrinsic_map_has(_vm: &Vm, args: &[Value]) -> Value {
    match (args.first(), args.get(1)) {
        (Some(Value::Map(m)), Some(key)) => HashKey::try_from(key).map_or_else(
            |_| bool_object(false),
            |hk| bool_object(m.borrow().contains_key(&hk)),
        ),
        _ => bool_object(false),
    }
}

fn intrinsic_map_delete(_vm: &Vm, args: &[Value]) -> Value {
    match (args.first(), args.get(1)) {
        (Some(Value::Map(m)), Some(key)) => {
            if let Ok(hk) = HashKey::try_from(key) {
                let _prev = m.borrow_mut().remove(&hk);
            }
            Value::Unit
        }
        _ => Value::Unit,
    }
}

fn intrinsic_map_keys(_vm: &Vm, args: &[Value]) -> Value {
    match args.first() {
        Some(Value::Map(m)) => {
            let keys: Vec<Value> = m.borrow().keys().map(Value::from).collect();
            Value::Array(Rc::new(RefCell::new(keys)))
        }
        _ => Value::Array(Rc::new(RefCell::new(Vec::new()))),
    }
}

fn intrinsic_map_values(_vm: &Vm, args: &[Value]) -> Value {
    match args.first() {
        Some(Value::Map(m)) => {
            let vals: Vec<Value> = m.borrow().values().cloned().collect();
            Value::Array(Rc::new(RefCell::new(vals)))
        }
        _ => Value::Array(Rc::new(RefCell::new(Vec::new()))),
    }
}

fn intrinsic_map_len(_vm: &Vm, args: &[Value]) -> Value {
    match args.first() {
        Some(Value::Map(m)) => Value::Int(i64::try_from(m.borrow().len()).unwrap_or(i64::MAX)),
        _ => Value::Int(0),
    }
}

// -- option / bool helpers ----------------------------------------------------

fn option_none() -> Value {
    Value::Object {
        type_tag: 0,
        fields: Rc::new(vec![Value::Int(0)]),
    }
}

fn option_some(v: Value) -> Value {
    Value::Object {
        type_tag: 0,
        fields: Rc::new(vec![Value::Int(1), v]),
    }
}

// -- I/O ----------------------------------------------------------------------

fn do_write(args: &[Value], newline: bool) -> Value {
    for arg in args {
        print!("{arg}");
    }
    if newline {
        println!();
    }
    Value::Unit
}

fn intrinsic_writeln(_vm: &Vm, args: &[Value]) -> Value {
    do_write(args, true)
}
fn intrinsic_write(_vm: &Vm, args: &[Value]) -> Value {
    do_write(args, false)
}

// -- type conversions ---------------------------------------------------------

macro_rules! to_string_intrinsic {
    ($name:ident, $variant:ident) => {
        fn $name(_vm: &Vm, args: &[Value]) -> Value {
            match args.first() {
                Some(Value::$variant(n)) => Value::String(Rc::from(n.to_string().as_str())),
                _ => Value::String(Rc::from("")),
            }
        }
    };
}
to_string_intrinsic!(intrinsic_int_to_string, Int);
to_string_intrinsic!(intrinsic_float_to_string, Float);

fn intrinsic_string_concat(_vm: &Vm, args: &[Value]) -> Value {
    match (args.first(), args.get(1)) {
        (Some(Value::String(a)), Some(Value::String(b))) => {
            let mut s = String::with_capacity(a.len() + b.len());
            s.push_str(a);
            s.push_str(b);
            Value::String(Rc::from(s.as_str()))
        }
        _ => Value::String(Rc::from("")),
    }
}

// -- arrays -------------------------------------------------------------------

fn slice_range(start: i64, end: i64, len: i64) -> (usize, usize) {
    let lo = usize::try_from(start.clamp(0, len)).unwrap_or(0);
    let hi = usize::try_from(end.clamp(0, len)).unwrap_or(0);
    (lo, hi.max(lo))
}

fn intrinsic_array_length(_vm: &Vm, args: &[Value]) -> Value {
    match args.first() {
        Some(Value::Array(a)) => Value::Int(i64::try_from(a.borrow().len()).unwrap_or(i64::MAX)),
        _ => Value::Int(0),
    }
}

fn intrinsic_array_push(_vm: &Vm, args: &[Value]) -> Value {
    match (args.first(), args.get(1)) {
        (Some(Value::Array(a)), Some(val)) => {
            a.borrow_mut().push(val.clone());
            Value::Unit
        }
        _ => Value::Unit,
    }
}

fn intrinsic_array_pop(_vm: &Vm, args: &[Value]) -> Value {
    match args.first() {
        Some(Value::Array(a)) => a.borrow_mut().pop().map_or_else(option_none, option_some),
        _ => option_none(),
    }
}

fn intrinsic_array_get(_vm: &Vm, args: &[Value]) -> Value {
    match (args.first(), args.get(1)) {
        (Some(Value::Array(a)), Some(Value::Int(idx))) => {
            let borrowed = a.borrow();
            usize::try_from(*idx)
                .ok()
                .and_then(|i| borrowed.get(i))
                .map_or(Value::Unit, Clone::clone)
        }
        _ => Value::Unit,
    }
}

fn intrinsic_array_set(_vm: &Vm, args: &[Value]) -> Value {
    match (args.first(), args.get(1), args.get(2)) {
        (Some(Value::Array(a)), Some(Value::Int(idx)), Some(val)) => {
            if let Ok(i) = usize::try_from(*idx)
                && let Some(slot) = a.borrow_mut().get_mut(i)
            {
                *slot = val.clone();
            }
            Value::Unit
        }
        _ => Value::Unit,
    }
}

fn intrinsic_array_slice(_vm: &Vm, args: &[Value]) -> Value {
    match (args.first(), args.get(1), args.get(2)) {
        (Some(Value::Array(a)), Some(Value::Int(start)), Some(Value::Int(end))) => {
            let borrowed = a.borrow();
            let len = i64::try_from(borrowed.len()).unwrap_or(i64::MAX);
            let (lo, hi) = slice_range(*start, *end, len);
            Value::Array(Rc::new(RefCell::new(borrowed[lo..hi].to_vec())))
        }
        _ => Value::Array(Rc::new(RefCell::new(Vec::new()))),
    }
}

#[cfg(test)]
mod tests;
