//! §1–§4 arithmetic, bitwise, comparison, and conversion dispatch.

use musi_bc::Opcode;

use crate::error::VmError;
use crate::value::Value;
use crate::vm::Frame;

/// Dispatch §1–§4 arithmetic/compare/convert opcodes.
///
/// Returns `true` if the opcode was handled.
pub fn exec(op: Opcode, frame: &mut Frame) -> Result<bool, VmError> {
    if exec_int_arith(op, frame)? {
        return Ok(true);
    }
    if exec_float_arith(op, frame)? {
        return Ok(true);
    }
    if exec_bitwise(op, frame)? {
        return Ok(true);
    }
    if exec_cmp(op, frame)? {
        return Ok(true);
    }
    exec_conv(op, frame)
}

/// §1 Integer arithmetic: `I_ADD`..`I_NEG`.
fn exec_int_arith(op: Opcode, frame: &mut Frame) -> Result<bool, VmError> {
    match op {
        Opcode::I_ADD => {
            let (b, a) = pop2(frame)?;
            frame
                .stack
                .push(Value::from_int(a.as_int()?.wrapping_add(b.as_int()?)));
        }
        Opcode::I_ADD_UN => {
            let (b, a) = pop2(frame)?;
            frame
                .stack
                .push(Value::from_uint(a.as_uint()?.wrapping_add(b.as_uint()?)));
        }
        Opcode::I_SUB => {
            let (b, a) = pop2(frame)?;
            frame
                .stack
                .push(Value::from_int(a.as_int()?.wrapping_sub(b.as_int()?)));
        }
        Opcode::I_SUB_UN => {
            let (b, a) = pop2(frame)?;
            frame
                .stack
                .push(Value::from_uint(a.as_uint()?.wrapping_sub(b.as_uint()?)));
        }
        Opcode::I_MUL => {
            let (b, a) = pop2(frame)?;
            frame
                .stack
                .push(Value::from_int(a.as_int()?.wrapping_mul(b.as_int()?)));
        }
        Opcode::I_MUL_UN => {
            let (b, a) = pop2(frame)?;
            frame
                .stack
                .push(Value::from_uint(a.as_uint()?.wrapping_mul(b.as_uint()?)));
        }
        Opcode::I_DIV => {
            let (b, a) = pop2(frame)?;
            let divisor = b.as_int()?;
            if divisor == 0 {
                return Err(VmError::DivideByZero);
            }
            frame
                .stack
                .push(Value::from_int(a.as_int()?.wrapping_div(divisor)));
        }
        Opcode::I_DIV_UN => {
            let (b, a) = pop2(frame)?;
            let divisor = b.as_uint()?;
            if divisor == 0 {
                return Err(VmError::DivideByZero);
            }
            frame.stack.push(Value::from_uint(a.as_uint()? / divisor));
        }
        Opcode::I_REM => {
            let (b, a) = pop2(frame)?;
            let divisor = b.as_int()?;
            if divisor == 0 {
                return Err(VmError::DivideByZero);
            }
            frame
                .stack
                .push(Value::from_int(a.as_int()?.wrapping_rem(divisor)));
        }
        Opcode::I_REM_UN => {
            let (b, a) = pop2(frame)?;
            let divisor = b.as_uint()?;
            if divisor == 0 {
                return Err(VmError::DivideByZero);
            }
            frame.stack.push(Value::from_uint(a.as_uint()? % divisor));
        }
        Opcode::I_NEG => {
            let a = pop1(frame)?;
            frame
                .stack
                .push(Value::from_int(a.as_int()?.wrapping_neg()));
        }
        _ => return Ok(false),
    }
    Ok(true)
}

/// §2 Float arithmetic: `F_ADD`..`F_NEG`.
fn exec_float_arith(op: Opcode, frame: &mut Frame) -> Result<bool, VmError> {
    match op {
        Opcode::F_ADD => {
            let (b, a) = pop2(frame)?;
            frame
                .stack
                .push(Value::from_float(a.as_float()? + b.as_float()?));
        }
        Opcode::F_SUB => {
            let (b, a) = pop2(frame)?;
            frame
                .stack
                .push(Value::from_float(a.as_float()? - b.as_float()?));
        }
        Opcode::F_MUL => {
            let (b, a) = pop2(frame)?;
            frame
                .stack
                .push(Value::from_float(a.as_float()? * b.as_float()?));
        }
        Opcode::F_DIV => {
            let (b, a) = pop2(frame)?;
            frame
                .stack
                .push(Value::from_float(a.as_float()? / b.as_float()?));
        }
        Opcode::F_REM => {
            let (b, a) = pop2(frame)?;
            frame
                .stack
                .push(Value::from_float(a.as_float()? % b.as_float()?));
        }
        Opcode::F_NEG => {
            let a = pop1(frame)?;
            frame.stack.push(Value::from_float(-a.as_float()?));
        }
        _ => return Ok(false),
    }
    Ok(true)
}

/// §3 Bitwise: `B_AND`..`B_SHR_UN`.
fn exec_bitwise(op: Opcode, frame: &mut Frame) -> Result<bool, VmError> {
    match op {
        Opcode::B_AND => {
            let (b, a) = pop2(frame)?;
            frame.stack.push(Value::from_int(a.as_int()? & b.as_int()?));
        }
        Opcode::B_OR => {
            let (b, a) = pop2(frame)?;
            frame.stack.push(Value::from_int(a.as_int()? | b.as_int()?));
        }
        Opcode::B_XOR => {
            let (b, a) = pop2(frame)?;
            frame.stack.push(Value::from_int(a.as_int()? ^ b.as_int()?));
        }
        Opcode::B_NOT => {
            let a = pop1(frame)?;
            frame.stack.push(Value::from_int(!a.as_int()?));
        }
        Opcode::B_SHL => {
            let (b, a) = pop2(frame)?;
            let shift = u32::try_from(b.as_int()? & 63).map_err(|_| VmError::Malformed {
                desc: "shift amount overflow".into(),
            })?;
            frame
                .stack
                .push(Value::from_int(a.as_int()?.wrapping_shl(shift)));
        }
        Opcode::B_SHR => {
            let (b, a) = pop2(frame)?;
            let shift = u32::try_from(b.as_int()? & 63).map_err(|_| VmError::Malformed {
                desc: "shift amount overflow".into(),
            })?;
            frame
                .stack
                .push(Value::from_int(a.as_int()?.wrapping_shr(shift)));
        }
        Opcode::B_SHR_UN => {
            let (b, a) = pop2(frame)?;
            let shift = u32::try_from(b.as_uint()? & 63).map_err(|_| VmError::Malformed {
                desc: "shift amount overflow".into(),
            })?;
            frame
                .stack
                .push(Value::from_uint(a.as_uint()?.wrapping_shr(shift)));
        }
        _ => return Ok(false),
    }
    Ok(true)
}

/// §4–§7 Comparison: `CMP_EQ`..`CMP_F_GE`.
fn exec_cmp(op: Opcode, frame: &mut Frame) -> Result<bool, VmError> {
    match op {
        Opcode::CMP_EQ => {
            let (b, a) = pop2(frame)?;
            frame.stack.push(Value::from_bool(a.0 == b.0));
        }
        Opcode::CMP_NE => {
            let (b, a) = pop2(frame)?;
            frame.stack.push(Value::from_bool(a.0 != b.0));
        }
        Opcode::CMP_LT => {
            let (b, a) = pop2(frame)?;
            frame
                .stack
                .push(Value::from_bool(a.as_int()? < b.as_int()?));
        }
        Opcode::CMP_LT_UN => {
            let (b, a) = pop2(frame)?;
            frame
                .stack
                .push(Value::from_bool(a.as_uint()? < b.as_uint()?));
        }
        Opcode::CMP_LE => {
            let (b, a) = pop2(frame)?;
            frame
                .stack
                .push(Value::from_bool(a.as_int()? <= b.as_int()?));
        }
        Opcode::CMP_LE_UN => {
            let (b, a) = pop2(frame)?;
            frame
                .stack
                .push(Value::from_bool(a.as_uint()? <= b.as_uint()?));
        }
        Opcode::CMP_GT => {
            let (b, a) = pop2(frame)?;
            frame
                .stack
                .push(Value::from_bool(a.as_int()? > b.as_int()?));
        }
        Opcode::CMP_GT_UN => {
            let (b, a) = pop2(frame)?;
            frame
                .stack
                .push(Value::from_bool(a.as_uint()? > b.as_uint()?));
        }
        Opcode::CMP_GE => {
            let (b, a) = pop2(frame)?;
            frame
                .stack
                .push(Value::from_bool(a.as_int()? >= b.as_int()?));
        }
        Opcode::CMP_GE_UN => {
            let (b, a) = pop2(frame)?;
            frame
                .stack
                .push(Value::from_bool(a.as_uint()? >= b.as_uint()?));
        }
        #[allow(clippy::float_cmp)]
        Opcode::CMP_F_EQ => {
            let (b, a) = pop2(frame)?;
            frame
                .stack
                .push(Value::from_bool(a.as_float()? == b.as_float()?));
        }
        #[allow(clippy::float_cmp)]
        Opcode::CMP_F_NE => {
            let (b, a) = pop2(frame)?;
            frame
                .stack
                .push(Value::from_bool(a.as_float()? != b.as_float()?));
        }
        Opcode::CMP_F_LT => {
            let (b, a) = pop2(frame)?;
            frame
                .stack
                .push(Value::from_bool(a.as_float()? < b.as_float()?));
        }
        Opcode::CMP_F_LE => {
            let (b, a) = pop2(frame)?;
            frame
                .stack
                .push(Value::from_bool(a.as_float()? <= b.as_float()?));
        }
        Opcode::CMP_F_GT => {
            let (b, a) = pop2(frame)?;
            frame
                .stack
                .push(Value::from_bool(a.as_float()? > b.as_float()?));
        }
        Opcode::CMP_F_GE => {
            let (b, a) = pop2(frame)?;
            frame
                .stack
                .push(Value::from_bool(a.as_float()? >= b.as_float()?));
        }
        _ => return Ok(false),
    }
    Ok(true)
}

/// §8 Conversion: `CNV_ITF`..`CNV_NRW`.
fn exec_conv(op: Opcode, frame: &mut Frame) -> Result<bool, VmError> {
    match op {
        Opcode::CNV_ITF => {
            let a = pop1(frame)?;
            #[allow(clippy::as_conversions, clippy::cast_precision_loss)]
            let float_val = a.as_int()? as f64;
            frame.stack.push(Value::from_float(float_val));
        }
        Opcode::CNV_FTI => {
            let a = pop1(frame)?;
            #[allow(clippy::as_conversions, clippy::cast_possible_truncation)]
            let n = a.as_float()? as i64;
            frame.stack.push(Value::from_int(n));
        }
        Opcode::CNV_TRM => {
            // Bit transmute: in NaN-boxed representation the raw u64 stays
            // the same — type reinterpretation is a compile-time concern.
            let a = pop1(frame)?;
            frame.stack.push(a);
        }
        Opcode::CNV_WDN | Opcode::CNV_NRW => {
            let a = pop1(frame)?;
            frame.stack.push(Value::from_int(a.as_int()?));
        }
        Opcode::CNV_WDN_UN => {
            let a = pop1(frame)?;
            frame.stack.push(Value::from_uint(a.as_uint()?));
        }

        _ => return Ok(false),
    }
    Ok(true)
}

fn pop1(frame: &mut Frame) -> Result<Value, VmError> {
    frame.stack.pop().ok_or_else(|| VmError::Malformed {
        desc: "operand stack underflow".into(),
    })
}

fn pop2(frame: &mut Frame) -> Result<(Value, Value), VmError> {
    let b = pop1(frame)?;
    let a = pop1(frame)?;
    Ok((b, a))
}
