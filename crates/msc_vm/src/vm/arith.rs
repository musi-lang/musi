//! Polymorphic arithmetic, bitwise, and comparison dispatch.
//!
//! The new ISA collapses INT/NAT/FLT typed opcodes into a single `ADD`/`SUB`/etc.
//! set. At runtime we check the NaN-box tag: both floats → f64 op, both/mixed
//! int/nat → i128 via `as_numeric_wide` (which handles heap-boxed values and
//! lossless Int↔Nat promotion automatically).

use msc_bc::Opcode;

use crate::VmResult;
use crate::error::VmError;
use crate::heap::Heap;
use crate::value::{Value, wide_values_equal};
use crate::vm::Frame;

// --------------------------------------------------------------------------
// Internal helpers
// --------------------------------------------------------------------------

/// Dispatch a binary arithmetic opcode polymorphically.
///
/// Both floats → f64. Otherwise promotes both to i128 via `as_numeric_wide`.
/// Result preserves Nat when both operands are Nat; otherwise produces Int.
macro_rules! poly_binary {
    ($frame:expr, $heap:expr, float_op $fop:tt, wide_int $method:ident) => {{
        let (b, a) = $frame.pop2()?;
        if a.is_float() && b.is_float() {
            let result = a.as_float()? $fop b.as_float()?;
            $frame.stack.push(Value::from_float(result));
        } else {
            let both_nat = a.is_nat() && b.is_nat();
            let ai = a.as_numeric_wide($heap)?;
            let bi = b.as_numeric_wide($heap)?;
            let result = ai.$method(bi);
            $frame.stack.push(Value::from_numeric_wide(result, both_nat, $heap));
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
            let both_nat = a.is_nat() && b.is_nat();
            let divisor = b.as_numeric_wide($heap)?;
            if divisor == 0 {
                return Err(VmError::DivideByZero);
            }
            let result = a.as_numeric_wide($heap)?.$method(divisor);
            $frame.stack.push(Value::from_numeric_wide(result, both_nat, $heap));
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
                a.as_numeric_wide($heap)? $op b.as_numeric_wide($heap)?,
            ));
        }
    }};
}

/// Dispatch a bitwise binary opcode. Both operands promoted to i128, result
/// preserves Nat when both operands are Nat.
macro_rules! poly_bitwise {
    ($frame:expr, $heap:expr, $op:tt) => {{
        let (b, a) = $frame.pop2()?;
        let both_nat = a.is_nat() && b.is_nat();
        let ai = a.as_numeric_wide($heap)?;
        let bi = b.as_numeric_wide($heap)?;
        $frame
            .stack
            .push(Value::from_numeric_wide(ai $op bi, both_nat, $heap));
    }};
}

/// Dispatch arithmetic / comparison / bitwise opcodes.
///
/// Returns `true` if the opcode was handled, `false` if the caller should try
/// the next dispatch group.
pub fn exec(op: Opcode, frame: &mut Frame, heap: &mut Heap) -> VmResult<bool> {
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
                // Negation always produces Int (negating unsigned → signed).
                let n = a.as_numeric_wide(heap)?;
                let result = i64::try_from(n.wrapping_neg()).unwrap_or(if n > 0 {
                    i64::MIN
                } else {
                    i64::MAX
                });
                frame.stack.push(Value::from_int_wide(result, heap));
            }
        }

        // §4.4 Bitwise (Int/Nat — Bool and/or/not compile to branch sequences)
        Opcode::BAND => {
            poly_bitwise!(frame, heap, &);
        }
        Opcode::BOR => {
            poly_bitwise!(frame, heap, |);
        }
        Opcode::BXOR => {
            poly_bitwise!(frame, heap, ^);
        }
        Opcode::BNOT => {
            let a = frame.pop()?;
            let is_nat = a.is_nat();
            let n = a.as_numeric_wide(heap)?;
            if is_nat {
                // For Nat, truncate to u64 and invert within 64-bit range.
                let truncated = u64::try_from(n).unwrap_or(u64::MAX);
                let inverted = i128::from(!truncated);
                frame
                    .stack
                    .push(Value::from_numeric_wide(inverted, true, heap));
            } else {
                let inverted = !n;
                let result = i64::try_from(inverted).unwrap_or(if inverted < 0 {
                    i64::MIN
                } else {
                    i64::MAX
                });
                frame.stack.push(Value::from_int_wide(result, heap));
            }
        }

        // §4.6 Comparison (polymorphic: float, int, or nat)
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
