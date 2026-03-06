//! Native built-in function dispatch.

use std::rc::Rc;

use musi_codegen::intrinsics::Intrinsic;

use crate::value::Value;
use crate::vm::Vm;

#[must_use]
pub fn dispatch(vm: &Vm, intrinsic: Intrinsic, args: &[Value]) -> Value {
    match intrinsic {
        Intrinsic::Writeln       => intrinsic_writeln(vm, args),
        Intrinsic::Write         => intrinsic_write(vm, args),
        Intrinsic::IntToString   => intrinsic_int_to_string(vm, args),
        Intrinsic::FloatToString => intrinsic_float_to_string(vm, args),
        Intrinsic::StringLength  => intrinsic_string_length(vm, args),
        Intrinsic::NatToString   => intrinsic_nat_to_string(vm, args),
    }
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

#[cfg(test)]
mod tests;
