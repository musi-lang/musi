//! Native built-in function dispatch (core prelude ops only).
//!
//! musi:string / musi:math / musi:io functions are served by the NativeRegistry
//! and take priority over this fallback. Only irreducible VM primitives live here:
//! writeln, write, int_to_string, float_to_string, string_concat, and arrays.

use std::cell::RefCell;
use std::rc::Rc;

use musi_codegen::intrinsics::Intrinsic;

use crate::value::Value;
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
        // All other intrinsics are intercepted before dispatch or served by NativeRegistry.
        _ => Value::Unit,
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
            if let Ok(i) = usize::try_from(*idx) {
                if let Some(slot) = a.borrow_mut().get_mut(i) {
                    *slot = val.clone();
                }
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
