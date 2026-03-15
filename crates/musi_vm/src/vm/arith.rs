//! §1–§4 arithmetic, bitwise, comparison, and conversion dispatch.

use musi_bc::Opcode;

use crate::error::{VmError, malformed};
use crate::value::Value;
use crate::vm::Frame;

macro_rules! binary_op {
    ($frame:expr, int $method:ident) => {{
        let (b, a) = $frame.pop2()?;
        $frame.stack.push(Value::from_int(a.as_int()?.$method(b.as_int()?)));
    }};
    ($frame:expr, uint $method:ident) => {{
        let (b, a) = $frame.pop2()?;
        $frame.stack.push(Value::from_uint(a.as_uint()?.$method(b.as_uint()?)));
    }};
    ($frame:expr, int_op $op:tt) => {{
        let (b, a) = $frame.pop2()?;
        $frame.stack.push(Value::from_int(a.as_int()? $op b.as_int()?));
    }};
    ($frame:expr, float_op $op:tt) => {{
        let (b, a) = $frame.pop2()?;
        $frame.stack.push(Value::from_float(a.as_float()? $op b.as_float()?));
    }};
    ($frame:expr, cmp_raw $op:tt) => {{
        let (b, a) = $frame.pop2()?;
        $frame.stack.push(Value::from_bool(a.0 $op b.0));
    }};
    ($frame:expr, cmp_int $op:tt) => {{
        let (b, a) = $frame.pop2()?;
        $frame.stack.push(Value::from_bool(a.as_int()? $op b.as_int()?));
    }};
    ($frame:expr, cmp_uint $op:tt) => {{
        let (b, a) = $frame.pop2()?;
        $frame.stack.push(Value::from_bool(a.as_uint()? $op b.as_uint()?));
    }};
    ($frame:expr, cmp_float $op:tt) => {{
        let (b, a) = $frame.pop2()?;
        $frame.stack.push(Value::from_bool(a.as_float()? $op b.as_float()?));
    }};
}

macro_rules! div_op {
    ($frame:expr, int $method:ident) => {{
        let (b, a) = $frame.pop2()?;
        let divisor = b.as_int()?;
        if divisor == 0 { return Err(VmError::DivideByZero); }
        $frame.stack.push(Value::from_int(a.as_int()?.$method(divisor)));
    }};
    ($frame:expr, uint $op:tt) => {{
        let (b, a) = $frame.pop2()?;
        let divisor = b.as_uint()?;
        if divisor == 0 { return Err(VmError::DivideByZero); }
        $frame.stack.push(Value::from_uint(a.as_uint()? $op divisor));
    }};
}

macro_rules! shift_op {
    ($frame:expr, int $method:ident) => {{
        let (b, a) = $frame.pop2()?;
        let shift = u32::try_from(b.as_int()? & 63)
            .map_err(|_| malformed!("shift amount overflow"))?;
        $frame.stack.push(Value::from_int(a.as_int()?.$method(shift)));
    }};
    ($frame:expr, uint $method:ident) => {{
        let (b, a) = $frame.pop2()?;
        let shift = u32::try_from(b.as_uint()? & 63)
            .map_err(|_| malformed!("shift amount overflow"))?;
        $frame.stack.push(Value::from_uint(a.as_uint()?.$method(shift)));
    }};
}

/// Dispatch §1–§4 arithmetic/compare/convert opcodes.
///
/// Returns `true` if the opcode was handled.
#[allow(clippy::float_cmp)]
pub fn exec(op: Opcode, frame: &mut Frame) -> Result<bool, VmError> {
    match op {
        // §1 Integer arithmetic
        Opcode::I_ADD    => binary_op!(frame, int wrapping_add),
        Opcode::I_ADD_UN => binary_op!(frame, uint wrapping_add),
        Opcode::I_SUB    => binary_op!(frame, int wrapping_sub),
        Opcode::I_SUB_UN => binary_op!(frame, uint wrapping_sub),
        Opcode::I_MUL    => binary_op!(frame, int wrapping_mul),
        Opcode::I_MUL_UN => binary_op!(frame, uint wrapping_mul),
        Opcode::I_DIV    => div_op!(frame, int wrapping_div),
        Opcode::I_DIV_UN => div_op!(frame, uint /),
        Opcode::I_REM    => div_op!(frame, int wrapping_rem),
        Opcode::I_REM_UN => div_op!(frame, uint %),
        Opcode::I_NEG => {
            let a = frame.pop()?;
            frame.stack.push(Value::from_int(a.as_int()?.wrapping_neg()));
        }

        // §2 Float arithmetic
        Opcode::F_ADD => binary_op!(frame, float_op +),
        Opcode::F_SUB => binary_op!(frame, float_op -),
        Opcode::F_MUL => binary_op!(frame, float_op *),
        Opcode::F_DIV => binary_op!(frame, float_op /),
        Opcode::F_REM => binary_op!(frame, float_op %),
        Opcode::F_NEG => {
            let a = frame.pop()?;
            frame.stack.push(Value::from_float(-a.as_float()?));
        }

        // §3 Bitwise
        Opcode::B_AND => binary_op!(frame, int_op &),
        Opcode::B_OR  => binary_op!(frame, int_op |),
        Opcode::B_XOR => binary_op!(frame, int_op ^),
        Opcode::B_NOT => {
            let a = frame.pop()?;
            frame.stack.push(Value::from_int(!a.as_int()?));
        }
        Opcode::B_SHL    => shift_op!(frame, int wrapping_shl),
        Opcode::B_SHR    => shift_op!(frame, int wrapping_shr),
        Opcode::B_SHR_UN => shift_op!(frame, uint wrapping_shr),

        // §4 Comparison
        Opcode::CMP_EQ    => binary_op!(frame, cmp_raw ==),
        Opcode::CMP_NE    => binary_op!(frame, cmp_raw !=),
        Opcode::CMP_LT    => binary_op!(frame, cmp_int <),
        Opcode::CMP_LT_UN => binary_op!(frame, cmp_uint <),
        Opcode::CMP_LE    => binary_op!(frame, cmp_int <=),
        Opcode::CMP_LE_UN => binary_op!(frame, cmp_uint <=),
        Opcode::CMP_GT    => binary_op!(frame, cmp_int >),
        Opcode::CMP_GT_UN => binary_op!(frame, cmp_uint >),
        Opcode::CMP_GE    => binary_op!(frame, cmp_int >=),
        Opcode::CMP_GE_UN => binary_op!(frame, cmp_uint >=),
        Opcode::CMP_F_EQ  => binary_op!(frame, cmp_float ==),
        Opcode::CMP_F_NE  => binary_op!(frame, cmp_float !=),
        Opcode::CMP_F_LT  => binary_op!(frame, cmp_float <),
        Opcode::CMP_F_LE  => binary_op!(frame, cmp_float <=),
        Opcode::CMP_F_GT  => binary_op!(frame, cmp_float >),
        Opcode::CMP_F_GE  => binary_op!(frame, cmp_float >=),

        // §8 Conversion
        Opcode::CNV_ITF => {
            let a = frame.pop()?;
            #[allow(clippy::as_conversions, clippy::cast_precision_loss)]
            let float_val = a.as_int()? as f64;
            frame.stack.push(Value::from_float(float_val));
        }
        Opcode::CNV_FTI => {
            let a = frame.pop()?;
            #[allow(clippy::as_conversions, clippy::cast_possible_truncation)]
            let n = a.as_float()? as i64;
            frame.stack.push(Value::from_int(n));
        }
        Opcode::CNV_TRM => {
            let a = frame.pop()?;
            frame.stack.push(a);
        }

        _ => return Ok(false),
    }
    Ok(true)
}
