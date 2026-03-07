//! Shared types and helpers re-exported into every native module's scope.

pub use musi_vm::native_registry::{NativeFn, NativeModuleEntry};
pub use musi_vm::value::Value;

use std::cell::RefCell;
use std::rc::Rc;

// -- Value constructors -------------------------------------------------------

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
///
/// # Panics
/// Never panics: both values are clamped to `[0, len]` before conversion,
/// guaranteeing they fit in `usize` on any platform where `i64::MAX <= usize::MAX`.
#[must_use]
pub fn slice_range(start: i64, end: i64, len: i64) -> (usize, usize) {
    let lo = start.max(0).min(len);
    let hi = end.max(lo).min(len);
    (
        usize::try_from(lo).expect("lo fits usize"),
        usize::try_from(hi).expect("hi fits usize"),
    )
}

/// Build a `Value::Array` from a `Vec<String>`.
#[must_use]
pub fn array_of_strings(strings: Vec<String>) -> Value {
    let items: Vec<Value> = strings
        .into_iter()
        .map(|s| Value::String(Rc::from(s.as_str())))
        .collect();
    Value::Array(Rc::new(RefCell::new(items)))
}
