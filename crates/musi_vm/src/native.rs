//! Native built-in function dispatch.

use std::io::BufRead as _;
use std::rc::Rc;

use musi_codegen::intrinsics::Intrinsic;

use crate::value::Value;
use crate::vm::Vm;

#[must_use]
pub fn dispatch(vm: &Vm, intrinsic: Intrinsic, args: &[Value]) -> Value {
    match intrinsic {
        Intrinsic::Writeln        => intrinsic_writeln(vm, args),
        Intrinsic::Write          => intrinsic_write(vm, args),
        Intrinsic::IntToString    => intrinsic_int_to_string(vm, args),
        Intrinsic::FloatToString  => intrinsic_float_to_string(vm, args),
        Intrinsic::StringLength   => intrinsic_string_length(vm, args),
        Intrinsic::NatToString    => intrinsic_nat_to_string(vm, args),
        Intrinsic::StringConcat   => intrinsic_string_concat(vm, args),
        Intrinsic::StringSlice    => intrinsic_string_slice(vm, args),
        Intrinsic::StringToInt    => intrinsic_string_to_int(vm, args),
        Intrinsic::StringContains => intrinsic_string_contains(vm, args),
        Intrinsic::FloatSqrt      => intrinsic_float_sqrt(vm, args),
        Intrinsic::FloatPow       => intrinsic_float_pow(vm, args),
        Intrinsic::FloatFloor     => intrinsic_float_floor(vm, args),
        Intrinsic::FloatCeil      => intrinsic_float_ceil(vm, args),
        Intrinsic::ReadLine       => intrinsic_read_line(vm, args),
    }
}

// -- option helpers -----------------------------------------------------------

fn option_none() -> Value {
    Value::Object { type_tag: 0, fields: Rc::new(vec![Value::Int(0)]) }
}

fn option_some(v: Value) -> Value {
    Value::Object { type_tag: 0, fields: Rc::new(vec![Value::Int(1), v]) }
}

fn bool_val(b: bool) -> Value {
    Value::Object { type_tag: 0, fields: Rc::new(vec![Value::Int(i64::from(b))]) }
}

fn intrinsic_writeln(_vm: &Vm, args: &[Value]) -> Value {
    for arg in args {
        print!("{arg}");
    }
    println!();
    Value::Unit
}

fn intrinsic_write(_vm: &Vm, args: &[Value]) -> Value {
    for arg in args {
        print!("{arg}");
    }
    Value::Unit
}

fn intrinsic_int_to_string(_vm: &Vm, args: &[Value]) -> Value {
    match args.first() {
        Some(Value::Int(n)) => Value::String(Rc::from(n.to_string().as_str())),
        _ => Value::String(Rc::from("")),
    }
}

fn intrinsic_float_to_string(_vm: &Vm, args: &[Value]) -> Value {
    match args.first() {
        Some(Value::Float(f)) => Value::String(Rc::from(f.to_string().as_str())),
        _ => Value::String(Rc::from("")),
    }
}

fn intrinsic_string_length(_vm: &Vm, args: &[Value]) -> Value {
    match args.first() {
        Some(Value::String(s)) => Value::Int(i64::try_from(s.chars().count()).expect("string too long")),
        _ => Value::Int(0),
    }
}

fn intrinsic_nat_to_string(_vm: &Vm, args: &[Value]) -> Value {
    match args.first() {
        Some(Value::Int(n)) => Value::String(Rc::from(n.to_string().as_str())),
        _ => Value::String(Rc::from("")),
    }
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

fn intrinsic_string_slice(_vm: &Vm, args: &[Value]) -> Value {
    match (args.first(), args.get(1), args.get(2)) {
        (Some(Value::String(s)), Some(Value::Int(start)), Some(Value::Int(end))) => {
            let chars: Vec<char> = s.chars().collect();
            let len = i64::try_from(chars.len()).unwrap_or(i64::MAX);
            let lo = (*start).clamp(0, len) as usize;
            let hi = (*end).clamp(0, len) as usize;
            let slice: String = chars[lo..hi.max(lo)].iter().collect();
            Value::String(Rc::from(slice.as_str()))
        }
        _ => Value::String(Rc::from("")),
    }
}

fn intrinsic_string_to_int(_vm: &Vm, args: &[Value]) -> Value {
    match args.first() {
        Some(Value::String(s)) => match s.parse::<i64>() {
            Ok(n)  => option_some(Value::Int(n)),
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

fn intrinsic_float_sqrt(_vm: &Vm, args: &[Value]) -> Value {
    match args.first() {
        Some(Value::Float(f)) => Value::Float(f.sqrt()),
        _ => Value::Float(0.0),
    }
}

fn intrinsic_float_pow(_vm: &Vm, args: &[Value]) -> Value {
    match (args.first(), args.get(1)) {
        (Some(Value::Float(base)), Some(Value::Float(exp))) => Value::Float(base.powf(*exp)),
        _ => Value::Float(0.0),
    }
}

fn intrinsic_float_floor(_vm: &Vm, args: &[Value]) -> Value {
    match args.first() {
        Some(Value::Float(f)) => Value::Float(f.floor()),
        _ => Value::Float(0.0),
    }
}

fn intrinsic_float_ceil(_vm: &Vm, args: &[Value]) -> Value {
    match args.first() {
        Some(Value::Float(f)) => Value::Float(f.ceil()),
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

#[cfg(test)]
mod tests;
