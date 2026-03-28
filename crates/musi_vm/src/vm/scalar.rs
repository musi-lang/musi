use music_il::opcode::Opcode;

use super::Vm;
use crate::errors::{VmError, VmResult};
use crate::frame::CallFrame;
use crate::heap::HeapObject;
#[cfg(test)]
use crate::heap::Heap;
use crate::value::Value;
use crate::vm::array::exec_arr_geti;

impl Vm {
    pub(super) fn dispatch_equality(&mut self, op: Opcode) -> VmResult {
        let frame = self.frames.last_mut().ok_or(VmError::StackUnderflow)?;
        let b = frame.pop()?;
        let a = frame.pop()?;
        let equal = self.values_equal(a, b);
        let result = if op == Opcode::CmpEq { equal } else { !equal };
        self.frames
            .last_mut()
            .unwrap()
            .push(Value::from_bool(result));
        Ok(())
    }

    pub(super) fn values_equal(&self, a: Value, b: Value) -> bool {
        let mut seen = Vec::new();
        self.values_equal_inner(a, b, &mut seen)
    }

    pub(super) fn values_equal_inner(
        &self,
        a: Value,
        b: Value,
        seen: &mut Vec<(usize, usize)>,
    ) -> bool {
        if a == b {
            return true;
        }
        if !a.is_ptr() || !b.is_ptr() {
            return false;
        }
        let pair = (a.as_ptr_idx(), b.as_ptr_idx());
        if seen.contains(&pair) {
            return true;
        }
        seen.push(pair);
        let (Some(obj_a), Some(obj_b)) =
            (self.heap.get(a.as_ptr_idx()), self.heap.get(b.as_ptr_idx()))
        else {
            return false;
        };

        match (obj_a, obj_b) {
            (HeapObject::String(sa), HeapObject::String(sb)) => sa == sb,
            (HeapObject::Array(arr_a), HeapObject::Array(arr_b)) => {
                arr_a.tag == arr_b.tag
                    && arr_a.elements.len() == arr_b.elements.len()
                    && arr_a
                        .elements
                        .iter()
                        .zip(&arr_b.elements)
                        .all(|(&lhs, &rhs)| self.values_equal_inner(lhs, rhs, seen))
            }
            (HeapObject::Slice(slice_a), HeapObject::Slice(slice_b)) => {
                let len_a = slice_a.end.saturating_sub(slice_a.start);
                let len_b = slice_b.end.saturating_sub(slice_b.start);
                len_a == len_b
                    && (0..len_a).all(|i| {
                        let lhs = exec_arr_geti(&self.heap, a, i).ok();
                        let rhs = exec_arr_geti(&self.heap, b, i).ok();
                        matches!((lhs, rhs), (Some(lhs), Some(rhs)) if self.values_equal_inner(lhs, rhs, seen))
                    })
            }
            _ => false,
        }
    }
}

pub(super) fn exec_scalar_op(op: Opcode, frame: &mut CallFrame) -> VmResult {
    match op {
        Opcode::IAdd => apply_int_binop(frame, i64::wrapping_add)?,
        Opcode::ISub => apply_int_binop(frame, i64::wrapping_sub)?,
        Opcode::IMul => apply_int_binop(frame, i64::wrapping_mul)?,
        Opcode::IDiv => {
            let (a, b) = pop_two_ints(frame)?;
            if b == 0 {
                return Err(VmError::DivisionByZero);
            }
            frame.push(Value::from_int(a.wrapping_div(b)));
        }
        Opcode::IRem => {
            let (a, b) = pop_two_ints(frame)?;
            if b == 0 {
                return Err(VmError::DivisionByZero);
            }
            frame.push(Value::from_int(a.wrapping_rem(b)));
        }
        Opcode::INeg => {
            let a = pop_int(frame)?;
            frame.push(Value::from_int(a.wrapping_neg()));
        }
        Opcode::FAdd => apply_float_binop(frame, |a, b| a + b)?,
        Opcode::FSub => apply_float_binop(frame, |a, b| a - b)?,
        Opcode::FMul => apply_float_binop(frame, |a, b| a * b)?,
        Opcode::FDiv => apply_float_binop(frame, |a, b| a / b)?,
        Opcode::FNeg => {
            let a = pop_float(frame)?;
            frame.push(Value::from_float(-a));
        }
        Opcode::And => apply_int_binop(frame, |a, b| a & b)?,
        Opcode::Or => apply_int_binop(frame, |a, b| a | b)?,
        Opcode::Xor => apply_int_binop(frame, |a, b| a ^ b)?,
        Opcode::Not => {
            let a = pop_int(frame)?;
            frame.push(Value::from_int(!a));
        }
        Opcode::Shl => {
            let (a, b) = pop_two_ints(frame)?;
            frame.push(Value::from_int(
                a.wrapping_shl(u32::try_from(b).unwrap_or(u32::MAX)),
            ));
        }
        Opcode::Shr => {
            let (a, b) = pop_two_ints(frame)?;
            frame.push(Value::from_int(
                a.wrapping_shr(u32::try_from(b).unwrap_or(u32::MAX)),
            ));
        }
        Opcode::CmpLt => apply_cmp(frame, i64::lt, f64::lt)?,
        Opcode::CmpGt => apply_cmp(frame, i64::gt, f64::gt)?,
        Opcode::CmpLeq => apply_cmp(frame, i64::le, f64::le)?,
        Opcode::CmpGeq => apply_cmp(frame, i64::ge, f64::ge)?,
        op => return Err(VmError::UnsupportedOpcode(op)),
    }
    Ok(())
}

fn pop_int(frame: &mut CallFrame) -> VmResult<i64> {
    let v = frame.pop()?;
    if !v.is_int() {
        return Err(VmError::TypeError {
            expected: "`Int`",
            found: "non-`Int`",
        });
    }
    Ok(v.as_int())
}

fn pop_float(frame: &mut CallFrame) -> VmResult<f64> {
    let v = frame.pop()?;
    if !v.is_float() {
        return Err(VmError::TypeError {
            expected: "`Float`",
            found: "non-`Float`",
        });
    }
    Ok(v.as_float())
}

fn pop_two_ints(frame: &mut CallFrame) -> VmResult<(i64, i64)> {
    let b = pop_int(frame)?;
    let a = pop_int(frame)?;
    Ok((a, b))
}

fn pop_two_floats(frame: &mut CallFrame) -> VmResult<(f64, f64)> {
    let b = pop_float(frame)?;
    let a = pop_float(frame)?;
    Ok((a, b))
}

fn apply_int_binop(frame: &mut CallFrame, f: impl Fn(i64, i64) -> i64) -> VmResult {
    let (a, b) = pop_two_ints(frame)?;
    frame.push(Value::from_int(f(a, b)));
    Ok(())
}

fn apply_float_binop(frame: &mut CallFrame, f: impl Fn(f64, f64) -> f64) -> VmResult {
    let (a, b) = pop_two_floats(frame)?;
    frame.push(Value::from_float(f(a, b)));
    Ok(())
}

fn apply_cmp(
    frame: &mut CallFrame,
    int_cmp: impl Fn(&i64, &i64) -> bool,
    float_cmp: impl Fn(&f64, &f64) -> bool,
) -> VmResult {
    let b = frame.pop()?;
    let a = frame.pop()?;
    let result = if a.is_int() && b.is_int() {
        int_cmp(&a.as_int(), &b.as_int())
    } else if a.is_float() && b.is_float() {
        float_cmp(&a.as_float(), &b.as_float())
    } else {
        return Err(VmError::TypeError {
            expected: "`Int` or `Float` (matching)",
            found: "mixed types",
        });
    };
    frame.push(Value::from_bool(result));
    Ok(())
}

/// Format a value for display, resolving heap pointers to their contents.
#[must_use]
#[cfg(test)]
pub(crate) fn display_value(val: Value, heap: &Heap) -> String {
    if val.is_ptr() {
        if let Some(obj) = heap.get(val.as_ptr_idx()) {
            return match obj {
                HeapObject::String(s) => s.data.clone(),
                HeapObject::Array(arr) => format!("[Array; len={}]", arr.elements.len()),
                HeapObject::Slice(sl) => format!("[Slice; len={}]", sl.end - sl.start),
                HeapObject::Closure(_) => "<closure>".into(),
                HeapObject::Continuation(_) => "<continuation>".into(),
                HeapObject::CPtr(_) => "<cptr>".into(),
                HeapObject::Cell(v) => format!("<cell:{:?}>", v.value),
            };
        }
    }
    format!("{val:?}")
}
