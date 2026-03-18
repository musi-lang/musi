//! §1–§4 arithmetic, bitwise, comparison, and conversion dispatch.

use msc_bc::Opcode;

use crate::error::VmError;
use crate::heap::Heap;
use crate::value::{Value, wide_values_equal};
use crate::vm::Frame;

macro_rules! binary_op {
    ($frame:expr, int_op $op:tt) => {{
        let (b, a) = $frame.pop2()?;
        $frame.stack.push(Value::from_int(a.as_int()? $op b.as_int()?));
    }};
    ($frame:expr, float_op $op:tt) => {{
        let (b, a) = $frame.pop2()?;
        $frame.stack.push(Value::from_float(a.as_float()? $op b.as_float()?));
    }};
    ($frame:expr, cmp_float $op:tt) => {{
        let (b, a) = $frame.pop2()?;
        $frame.stack.push(Value::from_bool(a.as_float()? $op b.as_float()?));
    }};
}

macro_rules! binary_op_wide {
    ($frame:expr, $heap:expr, int $method:ident) => {{
        let (b, a) = $frame.pop2()?;
        let ai = a.as_int_wide($heap)?;
        let bi = b.as_int_wide($heap)?;
        let result = ai.$method(bi);
        $frame.stack.push(Value::from_int_wide(result, $heap));
    }};
    ($frame:expr, $heap:expr, nat $method:ident) => {{
        let (b, a) = $frame.pop2()?;
        let an = a.as_nat_wide($heap)?;
        let bn = b.as_nat_wide($heap)?;
        let result = an.$method(bn);
        $frame.stack.push(Value::from_nat_wide(result, $heap));
    }};
}

macro_rules! div_op_wide {
    ($frame:expr, $heap:expr, int $method:ident) => {{
        let (b, a) = $frame.pop2()?;
        let divisor = b.as_int_wide($heap)?;
        if divisor == 0 { return Err(VmError::DivideByZero); }
        let result = a.as_int_wide($heap)?.$method(divisor);
        $frame.stack.push(Value::from_int_wide(result, $heap));
    }};
    ($frame:expr, $heap:expr, nat $op:tt) => {{
        let (b, a) = $frame.pop2()?;
        let divisor = b.as_nat_wide($heap)?;
        if divisor == 0 { return Err(VmError::DivideByZero); }
        let result = a.as_nat_wide($heap)? $op divisor;
        $frame.stack.push(Value::from_nat_wide(result, $heap));
    }};
}

macro_rules! cmp_wide {
    ($frame:expr, $heap:expr, int $op:tt) => {{
        let (b, a) = $frame.pop2()?;
        $frame.stack.push(Value::from_bool(a.as_int_wide($heap)? $op b.as_int_wide($heap)?));
    }};
    ($frame:expr, $heap:expr, nat $op:tt) => {{
        let (b, a) = $frame.pop2()?;
        $frame.stack.push(Value::from_bool(a.as_nat_wide($heap)? $op b.as_nat_wide($heap)?));
    }};
}

/// Dispatch §1–§4 arithmetic/compare/convert opcodes.
///
/// Returns `true` if the opcode was handled.
#[allow(clippy::float_cmp, clippy::too_many_lines)]
pub fn exec(op: Opcode, frame: &mut Frame, heap: &mut Heap) -> Result<bool, VmError> {
    match op {
        // §INT - Signed integer arithmetic (wide)
        Opcode::INT_ADD => binary_op_wide!(frame, heap, int wrapping_add),
        Opcode::INT_SUB => binary_op_wide!(frame, heap, int wrapping_sub),
        Opcode::INT_MUL => binary_op_wide!(frame, heap, int wrapping_mul),
        Opcode::INT_DIV => div_op_wide!(frame, heap, int wrapping_div),
        Opcode::INT_REM => div_op_wide!(frame, heap, int wrapping_rem),
        Opcode::INT_NEG => {
            let a = frame.pop()?;
            let n = a.as_int_wide(heap)?;
            frame
                .stack
                .push(Value::from_int_wide(n.wrapping_neg(), heap));
        }

        // §NAT - Unsigned integer arithmetic (wide)
        Opcode::NAT_ADD => binary_op_wide!(frame, heap, nat wrapping_add),
        Opcode::NAT_SUB => binary_op_wide!(frame, heap, nat wrapping_sub),
        Opcode::NAT_MUL => binary_op_wide!(frame, heap, nat wrapping_mul),
        Opcode::NAT_DIV => div_op_wide!(frame, heap, nat /),
        Opcode::NAT_REM => div_op_wide!(frame, heap, nat %),

        // §FLT - Float arithmetic
        Opcode::FLT_ADD => binary_op!(frame, float_op+),
        Opcode::FLT_SUB => binary_op!(frame, float_op -),
        Opcode::FLT_MUL => binary_op!(frame, float_op *),
        Opcode::FLT_DIV => binary_op!(frame, float_op /),
        Opcode::FLT_REM => binary_op!(frame, float_op %),
        Opcode::FLT_NEG => {
            let a = frame.pop()?;
            frame.stack.push(Value::from_float(-a.as_float()?));
        }

        // §BIT - Bitwise / logical (wide)
        Opcode::BIT_AND => {
            let (b, a) = frame.pop2()?;
            let ai = a.as_int_wide(heap)?;
            let bi = b.as_int_wide(heap)?;
            frame.stack.push(Value::from_int_wide(ai & bi, heap));
        }
        Opcode::BIT_OR => {
            let (b, a) = frame.pop2()?;
            let ai = a.as_int_wide(heap)?;
            let bi = b.as_int_wide(heap)?;
            frame.stack.push(Value::from_int_wide(ai | bi, heap));
        }
        Opcode::BIT_XOR => {
            let (b, a) = frame.pop2()?;
            let ai = a.as_int_wide(heap)?;
            let bi = b.as_int_wide(heap)?;
            frame.stack.push(Value::from_int_wide(ai ^ bi, heap));
        }
        Opcode::BIT_NOT => {
            let a = frame.pop()?;
            if let Ok(b) = a.as_bool() {
                frame.stack.push(Value::from_bool(!b));
            } else {
                let n = a.as_int_wide(heap)?;
                frame.stack.push(Value::from_int_wide(!n, heap));
            }
        }
        Opcode::BIT_SHL => {
            let (b, a) = frame.pop2()?;
            let shift = u32::try_from(b.as_int_wide(heap)? & 63).unwrap_or(63);
            let result = a.as_int_wide(heap)?.wrapping_shl(shift);
            frame.stack.push(Value::from_int_wide(result, heap));
        }
        Opcode::BIT_SHR => {
            let (b, a) = frame.pop2()?;
            let shift = u32::try_from(b.as_int_wide(heap)? & 63).unwrap_or(63);
            let result = a.as_int_wide(heap)?.wrapping_shr(shift);
            frame.stack.push(Value::from_int_wide(result, heap));
        }
        Opcode::BIT_SRU => {
            let (b, a) = frame.pop2()?;
            let shift = u32::try_from(b.as_nat_wide(heap)? & 63).unwrap_or(63);
            let result = a.as_nat_wide(heap)?.wrapping_shr(shift);
            frame.stack.push(Value::from_nat_wide(result, heap));
        }

        // §CMP - Comparison - equality uses fast bit-path + wide fallback
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
        Opcode::CMP_LT => cmp_wide!(frame, heap, int <),
        Opcode::CMP_LE => cmp_wide!(frame, heap, int <=),
        Opcode::CMP_GT => cmp_wide!(frame, heap, int >),
        Opcode::CMP_GE => cmp_wide!(frame, heap, int >=),
        Opcode::CMP_LTU => cmp_wide!(frame, heap, nat <),
        Opcode::CMP_LEU => cmp_wide!(frame, heap, nat <=),
        Opcode::CMP_GTU => cmp_wide!(frame, heap, nat >),
        Opcode::CMP_GEU => cmp_wide!(frame, heap, nat >=),
        Opcode::CMP_FLT => binary_op!(frame, cmp_float <),
        Opcode::CMP_FLE => binary_op!(frame, cmp_float <=),
        Opcode::CMP_FGT => binary_op!(frame, cmp_float >),
        Opcode::CMP_FGE => binary_op!(frame, cmp_float >=),

        // §CNV - Conversion (wide)
        Opcode::CNV_ITF => {
            let a = frame.pop()?;
            #[allow(clippy::as_conversions, clippy::cast_precision_loss)]
            let float_val = a.as_int_wide(heap)? as f64;
            frame.stack.push(Value::from_float(float_val));
        }
        Opcode::CNV_FTI => {
            let a = frame.pop()?;
            #[allow(clippy::as_conversions, clippy::cast_possible_truncation)]
            let n = a.as_float()? as i64;
            frame.stack.push(Value::from_int_wide(n, heap));
        }
        _ => return Ok(false),
    }
    Ok(true)
}
