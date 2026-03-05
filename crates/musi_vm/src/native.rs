//! Native function registry and built-in intrinsics.

use std::collections::HashMap;
use std::rc::Rc;

use crate::value::Value;
use crate::vm::Vm;

/// A native function handler.
///
/// Receives a shared reference to the VM and the arguments that were popped
/// from the operand stack.  Returns the call result.
pub type NativeFn = fn(&Vm, &[Value]) -> Value;

/// Intrinsic IDs for the built-in native functions.
pub mod intrinsics {
    /// `writeln(value)` — print a value followed by a newline.
    pub const WRITELN: u16 = 1;
    /// `write(value)` — print a value without a trailing newline.
    pub const WRITE: u16 = 2;
    /// `int_to_string(n)` — convert an i64 to its decimal string representation.
    pub const INT_TO_STRING: u16 = 3;
}

/// Registry mapping intrinsic IDs to native function handlers.
///
/// Pre-populated with the built-in `writeln` and `write` intrinsics.
pub struct NativeRegistry {
    handlers: HashMap<u16, NativeFn>,
}

impl NativeRegistry {
    /// Creates a registry pre-loaded with the built-in intrinsics.
    #[must_use]
    pub fn new() -> Self {
        let mut reg = Self {
            handlers: HashMap::new(),
        };
        let _prev = reg.handlers.insert(intrinsics::WRITELN, intrinsic_writeln);
        let _prev = reg.handlers.insert(intrinsics::WRITE, intrinsic_write);
        let _prev = reg
            .handlers
            .insert(intrinsics::INT_TO_STRING, intrinsic_int_to_string);
        reg
    }

    /// Registers a custom native handler for `intrinsic_id`.
    ///
    /// Replaces any previously registered handler for the same ID.
    pub fn register(&mut self, intrinsic_id: u16, handler: NativeFn) {
        let _prev = self.handlers.insert(intrinsic_id, handler);
    }

    /// Looks up a handler by intrinsic ID.
    #[must_use]
    pub fn get(&self, intrinsic_id: u16) -> Option<NativeFn> {
        self.handlers.get(&intrinsic_id).copied()
    }
}

impl Default for NativeRegistry {
    fn default() -> Self {
        Self::new()
    }
}

// ── Built-in intrinsics ───────────────────────────────────────────────────────

/// Prints all arguments followed by a newline.
///
/// Intrinsic ID: 1.
fn intrinsic_writeln(_vm: &Vm, args: &[Value]) -> Value {
    for arg in args {
        print!("{arg}");
    }
    println!();
    Value::Unit
}

/// Prints all arguments without a trailing newline.
///
/// Intrinsic ID: 2.
fn intrinsic_write(_vm: &Vm, args: &[Value]) -> Value {
    for arg in args {
        print!("{arg}");
    }
    Value::Unit
}

/// Converts an integer argument to its decimal string representation.
///
/// Intrinsic ID: 3.
fn intrinsic_int_to_string(_vm: &Vm, args: &[Value]) -> Value {
    match args.first() {
        Some(Value::Int(n)) => Value::String(Rc::from(n.to_string().as_str())),
        _ => Value::String(Rc::from("")),
    }
}

#[cfg(test)]
mod tests;
