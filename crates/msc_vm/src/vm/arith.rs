//! Polymorphic arithmetic, bitwise, and comparison dispatch.
//!
//! The new ISA collapses INT/NAT/FLT typed opcodes into a single `ADD`/`SUB`/etc.
//! set. At runtime we check the NaN-box tag: both floats → f64 op, otherwise → i64
//! via the wide-int path (which handles heap-boxed values automatically).

use msc_bc::Opcode;

use crate::error::VmError;
use crate::heap::Heap;
use crate::value::{Value, wide_values_equal};
use crate::vm::Frame;

// --------------------------------------------------------------------------
// Internal helpers
// --------------------------------------------------------------------------

/// Dispatch a binary arithmetic opcode polymorphically.
///
/// If both operands are floats, performs an f64 operation. Otherwise promotes
/// both to i64 via `as_int_wide` (which handles heap-boxed wide ints).
macro_rules! poly_binary {
    ($frame:expr, $heap:expr, float_op $fop:tt, wide_int $method:ident) => {{
        let (b, a) = $frame.pop2()?;
        if a.is_float() && b.is_float() {
            let result = a.as_float()? $fop b.as_float()?;
            $frame.stack.push(Value::from_float(result));
        } else {
            let ai = a.as_int_wide($heap)?;
            let bi = b.as_int_wide($heap)?;
            let result = ai.$method(bi);
            $frame.stack.push(Value::from_int_wide(result, $heap));
        }
    }};
}

macro_rules! poly_div {
    ($frame:expr, $heap:expr, float_op $fop:tt, wide_int $method:ident) => {{
        let (b, a) = $frame.pop2()?;
        if a.is_float() && b.is_float() {
            let result = a.as_float()? $fop b.as_float()?;
            $frame.stack.push(Value::from_float(result));
        } else {
            let divisor = b.as_int_wide($heap)?;
            if divisor == 0 {
                return Err(VmError::DivideByZero);
            }
            let result = a.as_int_wide($heap)?.$method(divisor);
            $frame.stack.push(Value::from_int_wide(result, $heap));
        }
    }};
}

macro_rules! poly_cmp {
    ($frame:expr, $heap:expr, $op:tt) => {{
        let (b, a) = $frame.pop2()?;
        if a.is_float() && b.is_float() {
            $frame
                .stack
                .push(Value::from_bool(a.as_float()? $op b.as_float()?));
        } else {
            $frame.stack.push(Value::from_bool(
                a.as_int_wide($heap)? $op b.as_int_wide($heap)?,
            ));
        }
    }};
}

/// Dispatch arithmetic / comparison / bitwise opcodes.
///
/// Returns `true` if the opcode was handled, `false` if the caller should try
/// the next dispatch group.
#[allow(clippy::float_cmp)]
pub fn exec(op: Opcode, frame: &mut Frame, heap: &mut Heap) -> Result<bool, VmError> {
    match op {
        // §4.3 Polymorphic arithmetic
        Opcode::ADD => {
            poly_binary!(frame, heap, float_op +, wide_int wrapping_add);
        }
        Opcode::SUB => {
            poly_binary!(frame, heap, float_op -, wide_int wrapping_sub);
        }
        Opcode::MUL => {
            poly_binary!(frame, heap, float_op *, wide_int wrapping_mul);
        }
        Opcode::DIV => {
            poly_div!(frame, heap, float_op /, wide_int wrapping_div);
        }
        Opcode::REM => {
            poly_div!(frame, heap, float_op %, wide_int wrapping_rem);
        }
        Opcode::NEG => {
            let a = frame.pop()?;
            if a.is_float() {
                frame.stack.push(Value::from_float(-a.as_float()?));
            } else {
                let n = a.as_int_wide(heap)?;
                frame
                    .stack
                    .push(Value::from_int_wide(n.wrapping_neg(), heap));
            }
        }

        // §4.4 Bitwise / logic (integer-only)
        Opcode::AND => {
            let (b, a) = frame.pop2()?;
            // Boolean AND: if both are bools, keep as bool.
            if a.is_bool() && b.is_bool() {
                frame
                    .stack
                    .push(Value::from_bool(a.as_bool()? && b.as_bool()?));
            } else {
                let ai = a.as_int_wide(heap)?;
                let bi = b.as_int_wide(heap)?;
                frame.stack.push(Value::from_int_wide(ai & bi, heap));
            }
        }
        Opcode::OR => {
            let (b, a) = frame.pop2()?;
            if a.is_bool() && b.is_bool() {
                frame
                    .stack
                    .push(Value::from_bool(a.as_bool()? || b.as_bool()?));
            } else {
                let ai = a.as_int_wide(heap)?;
                let bi = b.as_int_wide(heap)?;
                frame.stack.push(Value::from_int_wide(ai | bi, heap));
            }
        }
        Opcode::XOR => {
            let (b, a) = frame.pop2()?;
            if a.is_bool() && b.is_bool() {
                frame
                    .stack
                    .push(Value::from_bool(a.as_bool()? ^ b.as_bool()?));
            } else {
                let ai = a.as_int_wide(heap)?;
                let bi = b.as_int_wide(heap)?;
                frame.stack.push(Value::from_int_wide(ai ^ bi, heap));
            }
        }
        Opcode::NOT => {
            let a = frame.pop()?;
            if let Ok(b) = a.as_bool() {
                frame.stack.push(Value::from_bool(!b));
            } else {
                let n = a.as_int_wide(heap)?;
                frame.stack.push(Value::from_int_wide(!n, heap));
            }
        }
        Opcode::SHL => {
            let (b, a) = frame.pop2()?;
            let shift = u32::try_from(b.as_int_wide(heap)? & 63).unwrap_or(63);
            let result = a.as_int_wide(heap)?.wrapping_shl(shift);
            frame.stack.push(Value::from_int_wide(result, heap));
        }
        Opcode::SHR => {
            let (b, a) = frame.pop2()?;
            let shift = u32::try_from(b.as_int_wide(heap)? & 63).unwrap_or(63);
            let result = a.as_int_wide(heap)?.wrapping_shr(shift);
            frame.stack.push(Value::from_int_wide(result, heap));
        }

        // §4.5 Comparison (polymorphic: float or int)
        Opcode::CMP_EQ => {
            let (b, a) = frame.pop2()?;
            let eq = a.0 == b.0 || wide_values_equal(a, b, heap);
            frame.stack.push(Value::from_bool(eq));
        }
        Opcode::CMP_NE => {
            let (b, a) = frame.pop2()?;
            let eq = a.0 == b.0 || wide_values_equal(a, b, heap);
            frame.stack.push(Value::from_bool(!eq));
        }
        Opcode::CMP_LT => {
            poly_cmp!(frame, heap, <);
        }
        Opcode::CMP_LE => {
            poly_cmp!(frame, heap, <=);
        }
        Opcode::CMP_GT => {
            poly_cmp!(frame, heap, >);
        }
        Opcode::CMP_GE => {
            poly_cmp!(frame, heap, >=);
        }

        _ => return Ok(false),
    }
    Ok(true)
}
