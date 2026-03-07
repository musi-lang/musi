//! Shared types and helpers re-exported into every native module's scope.

pub use musi_vm::value::Value;

/// A native function callable from Musi: takes a slice of argument values,
/// returns a result value.
pub type NativeFn = fn(&[Value]) -> Value;

/// One `musi:*` built-in module entry: its import specifier, the Musi source
/// text the compiler sees, and the flat function table used for VM dispatch.
pub struct NativeModuleEntry {
    pub specifier: &'static str,
    pub source:    &'static str,
    pub functions: &'static [(&'static str, NativeFn)],
}

// -- Value constructors -------------------------------------------------------

use std::rc::Rc;

#[must_use]
pub fn option_none() -> Value {
    Value::Object { type_tag: 0, fields: Rc::new(vec![Value::Int(0)]) }
}

#[must_use]
pub fn option_some(v: Value) -> Value {
    Value::Object { type_tag: 0, fields: Rc::new(vec![Value::Int(1), v]) }
}

#[must_use]
pub fn bool_val(b: bool) -> Value {
    Value::Object { type_tag: 0, fields: Rc::new(vec![Value::Int(i64::from(b))]) }
}

/// Clamp a `[start, end)` pair to `[0, len]` and return `(lo, hi)` as `usize`.
#[must_use]
pub fn slice_range(start: i64, end: i64, len: i64) -> (usize, usize) {
    let lo = start.max(0).min(len);
    let hi = end.max(lo).min(len);
    (
        usize::try_from(lo).expect("lo fits usize"),
        usize::try_from(hi).expect("hi fits usize"),
    )
}
