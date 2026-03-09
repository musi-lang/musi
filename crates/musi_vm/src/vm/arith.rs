//! §1–§4 arithmetic, bitwise, comparison, and conversion dispatch.

use crate::error::VmError;
use crate::value::Value;
use crate::vm::Frame;

// Opcode constants.
const I_ADD: u8 = 0x10;
const I_ADD_UN: u8 = 0x11;
const I_SUB: u8 = 0x12;
const I_SUB_UN: u8 = 0x13;
const I_MUL: u8 = 0x14;
const I_MUL_UN: u8 = 0x15;
const I_DIV: u8 = 0x16;
const I_DIV_UN: u8 = 0x17;
const I_REM: u8 = 0x18;
const I_REM_UN: u8 = 0x19;
const I_NEG: u8 = 0x1A;

const F_ADD: u8 = 0x20;
const F_SUB: u8 = 0x21;
const F_MUL: u8 = 0x22;
const F_DIV: u8 = 0x23;
const F_REM: u8 = 0x24;
const F_NEG: u8 = 0x25;

const B_AND: u8 = 0x30;
const B_OR: u8 = 0x31;
const B_XOR: u8 = 0x32;
const B_NOT: u8 = 0x33;
const B_SHL: u8 = 0x34;
const B_SHR: u8 = 0x36;
const B_SHR_UN: u8 = 0x37;

const CMP_EQ: u8 = 0x3B;
const CMP_NE: u8 = 0x3C;

const CMP_LT: u8 = 0x50;
const CMP_LT_UN: u8 = 0x51;
const CMP_LE: u8 = 0x52;
const CMP_LE_UN: u8 = 0x53;
const CMP_GT: u8 = 0x54;
const CMP_GT_UN: u8 = 0x55;
const CMP_GE: u8 = 0x56;
const CMP_GE_UN: u8 = 0x57;

const CMP_F_EQ: u8 = 0x58;
const CMP_F_NE: u8 = 0x59;
const CMP_F_LT: u8 = 0x5A;
const CMP_F_LE: u8 = 0x5B;
const CMP_F_GT: u8 = 0x5C;
const CMP_F_GE: u8 = 0x5D;

const CNV_ITF: u8 = 0x5E;
const CNV_FTI: u8 = 0x5F;
const CNV_TRM: u8 = 0x60;

const CNV_WDN: u8 = 0x49;
const CNV_WDN_UN: u8 = 0x4A;
const CNV_NRW: u8 = 0x4B;

/// Dispatch §1–§4 arithmetic/compare/convert opcodes.
///
/// Returns `true` if the opcode was handled.
pub fn exec(op: u8, frame: &mut Frame) -> Result<bool, VmError> {
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
fn exec_int_arith(op: u8, frame: &mut Frame) -> Result<bool, VmError> {
    match op {
        I_ADD => {
            let (b, a) = pop2(frame)?;
            frame
                .stack
                .push(Value::from_int(a.as_int()?.wrapping_add(b.as_int()?)));
        }
        I_ADD_UN => {
            let (b, a) = pop2(frame)?;
            frame
                .stack
                .push(Value::from_uint(a.as_uint()?.wrapping_add(b.as_uint()?)));
        }
        I_SUB => {
            let (b, a) = pop2(frame)?;
            frame
                .stack
                .push(Value::from_int(a.as_int()?.wrapping_sub(b.as_int()?)));
        }
        I_SUB_UN => {
            let (b, a) = pop2(frame)?;
            frame
                .stack
                .push(Value::from_uint(a.as_uint()?.wrapping_sub(b.as_uint()?)));
        }
        I_MUL => {
            let (b, a) = pop2(frame)?;
            frame
                .stack
                .push(Value::from_int(a.as_int()?.wrapping_mul(b.as_int()?)));
        }
        I_MUL_UN => {
            let (b, a) = pop2(frame)?;
            frame
                .stack
                .push(Value::from_uint(a.as_uint()?.wrapping_mul(b.as_uint()?)));
        }
        I_DIV => {
            let (b, a) = pop2(frame)?;
            let divisor = b.as_int()?;
            if divisor == 0 {
                return Err(VmError::DivideByZero);
            }
            frame
                .stack
                .push(Value::from_int(a.as_int()?.wrapping_div(divisor)));
        }
        I_DIV_UN => {
            let (b, a) = pop2(frame)?;
            let divisor = b.as_uint()?;
            if divisor == 0 {
                return Err(VmError::DivideByZero);
            }
            frame.stack.push(Value::from_uint(a.as_uint()? / divisor));
        }
        I_REM => {
            let (b, a) = pop2(frame)?;
            let divisor = b.as_int()?;
            if divisor == 0 {
                return Err(VmError::DivideByZero);
            }
            frame
                .stack
                .push(Value::from_int(a.as_int()?.wrapping_rem(divisor)));
        }
        I_REM_UN => {
            let (b, a) = pop2(frame)?;
            let divisor = b.as_uint()?;
            if divisor == 0 {
                return Err(VmError::DivideByZero);
            }
            frame.stack.push(Value::from_uint(a.as_uint()? % divisor));
        }
        I_NEG => {
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
fn exec_float_arith(op: u8, frame: &mut Frame) -> Result<bool, VmError> {
    match op {
        F_ADD => {
            let (b, a) = pop2(frame)?;
            frame
                .stack
                .push(Value::from_float(a.as_float()? + b.as_float()?));
        }
        F_SUB => {
            let (b, a) = pop2(frame)?;
            frame
                .stack
                .push(Value::from_float(a.as_float()? - b.as_float()?));
        }
        F_MUL => {
            let (b, a) = pop2(frame)?;
            frame
                .stack
                .push(Value::from_float(a.as_float()? * b.as_float()?));
        }
        F_DIV => {
            let (b, a) = pop2(frame)?;
            frame
                .stack
                .push(Value::from_float(a.as_float()? / b.as_float()?));
        }
        F_REM => {
            let (b, a) = pop2(frame)?;
            frame
                .stack
                .push(Value::from_float(a.as_float()? % b.as_float()?));
        }
        F_NEG => {
            let a = pop1(frame)?;
            frame.stack.push(Value::from_float(-a.as_float()?));
        }
        _ => return Ok(false),
    }
    Ok(true)
}

/// §3 Bitwise: `B_AND`..`B_SHR_UN`.
fn exec_bitwise(op: u8, frame: &mut Frame) -> Result<bool, VmError> {
    match op {
        B_AND => {
            let (b, a) = pop2(frame)?;
            frame.stack.push(Value::from_int(a.as_int()? & b.as_int()?));
        }
        B_OR => {
            let (b, a) = pop2(frame)?;
            frame.stack.push(Value::from_int(a.as_int()? | b.as_int()?));
        }
        B_XOR => {
            let (b, a) = pop2(frame)?;
            frame.stack.push(Value::from_int(a.as_int()? ^ b.as_int()?));
        }
        B_NOT => {
            let a = pop1(frame)?;
            frame.stack.push(Value::from_int(!a.as_int()?));
        }
        B_SHL => {
            let (b, a) = pop2(frame)?;
            let shift = u32::try_from(b.as_int()? & 63).unwrap_or(0);
            frame
                .stack
                .push(Value::from_int(a.as_int()?.wrapping_shl(shift)));
        }
        B_SHR => {
            let (b, a) = pop2(frame)?;
            let shift = u32::try_from(b.as_int()? & 63).unwrap_or(0);
            frame
                .stack
                .push(Value::from_int(a.as_int()?.wrapping_shr(shift)));
        }
        B_SHR_UN => {
            let (b, a) = pop2(frame)?;
            let shift = u32::try_from(b.as_uint()? & 63).unwrap_or(0);
            frame
                .stack
                .push(Value::from_uint(a.as_uint()?.wrapping_shr(shift)));
        }
        _ => return Ok(false),
    }
    Ok(true)
}

/// §4–§7 Comparison: `CMP_EQ`..`CMP_F_GE`.
fn exec_cmp(op: u8, frame: &mut Frame) -> Result<bool, VmError> {
    match op {
        CMP_EQ => {
            let (b, a) = pop2(frame)?;
            frame.stack.push(Value::from_bool(a.0 == b.0));
        }
        CMP_NE => {
            let (b, a) = pop2(frame)?;
            frame.stack.push(Value::from_bool(a.0 != b.0));
        }
        CMP_LT => {
            let (b, a) = pop2(frame)?;
            frame
                .stack
                .push(Value::from_bool(a.as_int()? < b.as_int()?));
        }
        CMP_LT_UN => {
            let (b, a) = pop2(frame)?;
            frame
                .stack
                .push(Value::from_bool(a.as_uint()? < b.as_uint()?));
        }
        CMP_LE => {
            let (b, a) = pop2(frame)?;
            frame
                .stack
                .push(Value::from_bool(a.as_int()? <= b.as_int()?));
        }
        CMP_LE_UN => {
            let (b, a) = pop2(frame)?;
            frame
                .stack
                .push(Value::from_bool(a.as_uint()? <= b.as_uint()?));
        }
        CMP_GT => {
            let (b, a) = pop2(frame)?;
            frame
                .stack
                .push(Value::from_bool(a.as_int()? > b.as_int()?));
        }
        CMP_GT_UN => {
            let (b, a) = pop2(frame)?;
            frame
                .stack
                .push(Value::from_bool(a.as_uint()? > b.as_uint()?));
        }
        CMP_GE => {
            let (b, a) = pop2(frame)?;
            frame
                .stack
                .push(Value::from_bool(a.as_int()? >= b.as_int()?));
        }
        CMP_GE_UN => {
            let (b, a) = pop2(frame)?;
            frame
                .stack
                .push(Value::from_bool(a.as_uint()? >= b.as_uint()?));
        }
        #[allow(clippy::float_cmp)]
        CMP_F_EQ => {
            let (b, a) = pop2(frame)?;
            frame
                .stack
                .push(Value::from_bool(a.as_float()? == b.as_float()?));
        }
        #[allow(clippy::float_cmp)]
        CMP_F_NE => {
            let (b, a) = pop2(frame)?;
            frame
                .stack
                .push(Value::from_bool(a.as_float()? != b.as_float()?));
        }
        CMP_F_LT => {
            let (b, a) = pop2(frame)?;
            frame
                .stack
                .push(Value::from_bool(a.as_float()? < b.as_float()?));
        }
        CMP_F_LE => {
            let (b, a) = pop2(frame)?;
            frame
                .stack
                .push(Value::from_bool(a.as_float()? <= b.as_float()?));
        }
        CMP_F_GT => {
            let (b, a) = pop2(frame)?;
            frame
                .stack
                .push(Value::from_bool(a.as_float()? > b.as_float()?));
        }
        CMP_F_GE => {
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
fn exec_conv(op: u8, frame: &mut Frame) -> Result<bool, VmError> {
    match op {
        CNV_ITF => {
            let a = pop1(frame)?;
            #[allow(clippy::as_conversions, clippy::cast_precision_loss)]
            let float_val = a.as_int()? as f64;
            frame.stack.push(Value::from_float(float_val));
        }
        CNV_FTI => {
            let a = pop1(frame)?;
            #[allow(clippy::as_conversions, clippy::cast_possible_truncation)]
            let n = a.as_float()? as i64;
            frame.stack.push(Value::from_int(n));
        }
        CNV_TRM => {
            return Err(VmError::Unimplemented {
                desc: "cnv.trm requires unsafe effect context",
            });
        }
        CNV_WDN | CNV_NRW => {
            let a = pop1(frame)?;
            frame.stack.push(Value::from_int(a.as_int()?));
        }
        CNV_WDN_UN => {
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
