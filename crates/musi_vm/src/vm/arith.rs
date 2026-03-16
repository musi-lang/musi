//! §1–§4 arithmetic, bitwise, comparison, and conversion dispatch.

use musi_bc::Opcode;

use crate::error::{VmError, malformed};
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

macro_rules! shift_op {
    ($frame:expr, int $method:ident) => {{
        let (b, a) = $frame.pop2()?;
        let shift =
            u32::try_from(b.as_int()? & 63).map_err(|_| malformed!("shift amount overflow"))?;
        $frame
            .stack
            .push(Value::from_int(a.as_int()?.$method(shift)));
    }};
    ($frame:expr, nat $method:ident) => {{
        let (b, a) = $frame.pop2()?;
        let shift =
            u32::try_from(b.as_nat()? & 63).map_err(|_| malformed!("shift amount overflow"))?;
        $frame
            .stack
            .push(Value::from_nat(a.as_nat()?.$method(shift)));
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
#[allow(clippy::float_cmp)]
pub fn exec(op: Opcode, frame: &mut Frame, heap: &mut Heap) -> Result<bool, VmError> {
    match op {
        // §1 Integer arithmetic (wide)
        Opcode::I_ADD => binary_op_wide!(frame, heap, int wrapping_add),
        Opcode::I_ADD_UN => binary_op_wide!(frame, heap, nat wrapping_add),
        Opcode::I_SUB => binary_op_wide!(frame, heap, int wrapping_sub),
        Opcode::I_SUB_UN => binary_op_wide!(frame, heap, nat wrapping_sub),
        Opcode::I_MUL => binary_op_wide!(frame, heap, int wrapping_mul),
        Opcode::I_MUL_UN => binary_op_wide!(frame, heap, nat wrapping_mul),
        Opcode::I_DIV => div_op_wide!(frame, heap, int wrapping_div),
        Opcode::I_DIV_UN => div_op_wide!(frame, heap, nat /),
        Opcode::I_REM => div_op_wide!(frame, heap, int wrapping_rem),
        Opcode::I_REM_UN => div_op_wide!(frame, heap, nat %),
        Opcode::I_NEG => {
            let a = frame.pop()?;
            let n = a.as_int_wide(heap)?;
            frame
                .stack
                .push(Value::from_int_wide(n.wrapping_neg(), heap));
        }

        // §2 Float arithmetic (unchanged — always inline)
        Opcode::F_ADD => binary_op!(frame, float_op+),
        Opcode::F_SUB => binary_op!(frame, float_op -),
        Opcode::F_MUL => binary_op!(frame, float_op *),
        Opcode::F_DIV => binary_op!(frame, float_op /),
        Opcode::F_REM => binary_op!(frame, float_op %),
        Opcode::F_NEG => {
            let a = frame.pop()?;
            frame.stack.push(Value::from_float(-a.as_float()?));
        }

        // §3 Bitwise (48-bit inline)
        Opcode::B_AND => binary_op!(frame, int_op &),
        Opcode::B_OR => binary_op!(frame, int_op |),
        Opcode::B_XOR => binary_op!(frame, int_op ^),
        Opcode::B_NOT => {
            let a = frame.pop()?;
            frame.stack.push(Value::from_int(!a.as_int()?));
        }
        Opcode::B_SHL => shift_op!(frame, int wrapping_shl),
        Opcode::B_SHR => shift_op!(frame, int wrapping_shr),
        Opcode::B_SHR_UN => shift_op!(frame, nat wrapping_shr),

        // §4 Comparison — equality uses fast bit-path + wide fallback
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
        Opcode::CMP_LT_UN => cmp_wide!(frame, heap, nat <),
        Opcode::CMP_LE => cmp_wide!(frame, heap, int <=),
        Opcode::CMP_LE_UN => cmp_wide!(frame, heap, nat <=),
        Opcode::CMP_GT => cmp_wide!(frame, heap, int >),
        Opcode::CMP_GT_UN => cmp_wide!(frame, heap, nat >),
        Opcode::CMP_GE => cmp_wide!(frame, heap, int >=),
        Opcode::CMP_GE_UN => cmp_wide!(frame, heap, nat >=),
        Opcode::CMP_F_EQ => binary_op!(frame, cmp_float ==),
        Opcode::CMP_F_NE => binary_op!(frame, cmp_float !=),
        Opcode::CMP_F_LT => binary_op!(frame, cmp_float <),
        Opcode::CMP_F_LE => binary_op!(frame, cmp_float <=),
        Opcode::CMP_F_GT => binary_op!(frame, cmp_float >),
        Opcode::CMP_F_GE => binary_op!(frame, cmp_float >=),

        // §8 Conversion (wide)
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
