//! Native built-in function dispatch.

use std::cell::RefCell;
use std::io::BufRead as _;
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
        Intrinsic::StringLength => intrinsic_string_length(vm, args),
        Intrinsic::NatToString => intrinsic_nat_to_string(vm, args),
        Intrinsic::StringConcat => intrinsic_string_concat(vm, args),
        Intrinsic::StringSlice => intrinsic_string_slice(vm, args),
        Intrinsic::StringToInt => intrinsic_string_to_int(vm, args),
        Intrinsic::StringContains => intrinsic_string_contains(vm, args),
        Intrinsic::FloatSqrt => intrinsic_float_sqrt(vm, args),
        Intrinsic::FloatPow => intrinsic_float_pow(vm, args),
        Intrinsic::FloatFloor => intrinsic_float_floor(vm, args),
        Intrinsic::FloatCeil => intrinsic_float_ceil(vm, args),
        Intrinsic::ReadLine => intrinsic_read_line(vm, args),
        Intrinsic::ArrayLength => intrinsic_array_length(vm, args),
        Intrinsic::ArrayPush => intrinsic_array_push(vm, args),
        Intrinsic::ArrayPop => intrinsic_array_pop(vm, args),
        Intrinsic::ArrayGet => intrinsic_array_get(vm, args),
        Intrinsic::ArraySet => intrinsic_array_set(vm, args),
        Intrinsic::ArraySlice => intrinsic_array_slice(vm, args),
        // Assert/AssertMsg/Test are handled directly in vm::exec_call before dispatch reaches here.
        Intrinsic::Assert | Intrinsic::AssertMsg | Intrinsic::Test => Value::Unit,
        // System module intrinsics (musi:fs, musi:path, musi:os, musi:process, musi:time)
        // are dispatched via Vm::registry before this function is called; unreachable here.
        _ => Value::Unit,
    }
}

// -- option helpers -----------------------------------------------------------

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

fn bool_val(b: bool) -> Value {
    Value::Object {
        type_tag: 0,
        fields: Rc::new(vec![Value::Int(i64::from(b))]),
    }
}

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

fn intrinsic_string_length(_vm: &Vm, args: &[Value]) -> Value {
    match args.first() {
        Some(Value::String(s)) => {
            Value::Int(i64::try_from(s.chars().count()).expect("string too long"))
        }
        _ => Value::Int(0),
    }
}

fn intrinsic_nat_to_string(vm: &Vm, args: &[Value]) -> Value {
    intrinsic_int_to_string(vm, args)
}

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

fn slice_range(start: i64, end: i64, len: i64) -> (usize, usize) {
    let lo = start.clamp(0, len) as usize;
    let hi = end.clamp(0, len) as usize;
    (lo, hi.max(lo))
}

fn intrinsic_string_slice(_vm: &Vm, args: &[Value]) -> Value {
    match (args.first(), args.get(1), args.get(2)) {
        (Some(Value::String(s)), Some(Value::Int(start)), Some(Value::Int(end))) => {
            let chars: Vec<char> = s.chars().collect();
            let len = i64::try_from(chars.len()).unwrap_or(i64::MAX);
            let (lo, hi) = slice_range(*start, *end, len);
            let slice: String = chars[lo..hi].iter().collect();
            Value::String(Rc::from(slice.as_str()))
        }
        _ => Value::String(Rc::from("")),
    }
}

fn intrinsic_string_to_int(_vm: &Vm, args: &[Value]) -> Value {
    match args.first() {
        Some(Value::String(s)) => match s.parse::<i64>() {
            Ok(n) => option_some(Value::Int(n)),
            Err(_) => option_none(),
        },
        _ => option_none(),
    }
}

fn intrinsic_string_contains(_vm: &Vm, args: &[Value]) -> Value {
    match (args.first(), args.get(1)) {
        (Some(Value::String(s)), Some(Value::String(sub))) => bool_val(s.contains(sub.as_ref())),
        _ => bool_val(false),
    }
}

fn unary_float(args: &[Value], op: fn(f64) -> f64) -> Value {
    match args.first() {
        Some(Value::Float(f)) => Value::Float(op(*f)),
        _ => Value::Float(0.0),
    }
}

fn intrinsic_float_sqrt(_vm: &Vm, args: &[Value]) -> Value {
    unary_float(args, f64::sqrt)
}
fn intrinsic_float_floor(_vm: &Vm, args: &[Value]) -> Value {
    unary_float(args, f64::floor)
}
fn intrinsic_float_ceil(_vm: &Vm, args: &[Value]) -> Value {
    unary_float(args, f64::ceil)
}

fn intrinsic_float_pow(_vm: &Vm, args: &[Value]) -> Value {
    match (args.first(), args.get(1)) {
        (Some(Value::Float(base)), Some(Value::Float(exp))) => Value::Float(base.powf(*exp)),
        _ => Value::Float(0.0),
    }
}

fn intrinsic_read_line(_vm: &Vm, _args: &[Value]) -> Value {
    let stdin = std::io::stdin();
    let mut line = String::new();
    let _ = stdin.lock().read_line(&mut line);
    if line.ends_with('\n') {
        let _ = line.pop();
        if line.ends_with('\r') {
            let _ = line.pop();
        }
    }
    Value::String(Rc::from(line.as_str()))
}

fn intrinsic_array_length(_vm: &Vm, args: &[Value]) -> Value {
    match args.first() {
        Some(Value::Array(a)) => Value::Int(a.borrow().len() as i64),
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
        Some(Value::Array(a)) => match a.borrow_mut().pop() {
            Some(v) => option_some(v),
            None => option_none(),
        },
        _ => option_none(),
    }
}

fn intrinsic_array_get(_vm: &Vm, args: &[Value]) -> Value {
    match (args.first(), args.get(1)) {
        (Some(Value::Array(a)), Some(Value::Int(idx))) => {
            let borrowed = a.borrow();
            match usize::try_from(*idx).ok().and_then(|i| borrowed.get(i)) {
                Some(v) => v.clone(),
                None => Value::Unit,
            }
        }
        _ => Value::Unit,
    }
}

fn intrinsic_array_set(_vm: &Vm, args: &[Value]) -> Value {
    match (args.first(), args.get(1), args.get(2)) {
        (Some(Value::Array(a)), Some(Value::Int(idx)), Some(val)) => {
            if let Ok(i) = usize::try_from(*idx) {
                let mut borrowed = a.borrow_mut();
                if let Some(slot) = borrowed.get_mut(i) {
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
            let len = borrowed.len() as i64;
            let (lo, hi) = slice_range(*start, *end, len);
            let slice: Vec<Value> = borrowed[lo..hi].to_vec();
            Value::Array(Rc::new(RefCell::new(slice)))
        }
        _ => Value::Array(Rc::new(RefCell::new(Vec::new()))),
    }
}

#[cfg(test)]
mod tests;
