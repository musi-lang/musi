//! The Musi stack-based virtual machine.

use std::rc::Rc;

use musi_codegen::intrinsics::Intrinsic;
use musi_codegen::{ConstEntry, Module, Opcode};

use crate::error::VmError;
use crate::value::Value;

pub struct CallFrame {
    pub function_idx: u16,
    pub pc: usize,
    pub locals: Vec<Value>,
    /// Operand-stack depth when this frame was pushed; restored on `ret`.
    pub stack_base: usize,
}

enum Signal {
    Continue,
    Return(Value),
}

pub struct Vm {
    pub(crate) stack: Vec<Value>,
    pub(crate) frames: Vec<CallFrame>,
    pub(crate) module: Module,
}

impl Vm {
    #[must_use]
    pub fn new(module: Module) -> Self {
        Self { stack: Vec::new(), frames: Vec::new(), module }
    }

    /// Executes `entry_fn` (function-table index) and returns its result.
    ///
    /// # Errors
    ///
    /// Returns [`VmError`] on stack underflow, out-of-bounds access,
    /// unregistered intrinsic, or decode failure.
    pub fn run(&mut self, entry_fn: u16) -> Result<Value, VmError> {
        self.push_entry_frame(entry_fn)?;
        loop {
            let op = self.fetch_and_advance()?;
            match self.step(op)? {
                Signal::Continue => {}
                Signal::Return(v) => return Ok(v),
            }
        }
    }

    fn push_entry_frame(&mut self, entry_fn: u16) -> Result<(), VmError> {
        let local_count = self
            .module
            .function_table
            .get(usize::from(entry_fn))
            .ok_or(VmError::FunctionOutOfBounds(entry_fn))?
            .local_count;
        self.frames.push(CallFrame {
            function_idx: entry_fn,
            pc: 0,
            locals: vec![Value::Unit; usize::from(local_count)],
            stack_base: 0,
        });
        Ok(())
    }

    fn fetch_and_advance(&mut self) -> Result<Opcode, VmError> {
        let (fn_idx, pc) = {
            let frame = self.frames.last().ok_or(VmError::NoFrames)?;
            (frame.function_idx, frame.pc)
        };
        let (op, size) = {
            let func = self
                .module
                .function_table
                .get(usize::from(fn_idx))
                .ok_or(VmError::FunctionOutOfBounds(fn_idx))?;
            let code_start =
                usize::try_from(func.code_offset).map_err(|_| VmError::CodeOutOfBounds)?;
            let code_len =
                usize::try_from(func.code_length).map_err(|_| VmError::CodeOutOfBounds)?;
            let code_end = code_start
                .checked_add(code_len)
                .ok_or(VmError::CodeOutOfBounds)?;
            let code = self
                .module
                .code
                .get(code_start..code_end)
                .ok_or(VmError::CodeOutOfBounds)?;
            Opcode::decode(code, pc)?
        };
        self.frames.last_mut().ok_or(VmError::NoFrames)?.pc = pc + size;
        Ok(op)
    }

    fn step(&mut self, op: Opcode) -> Result<Signal, VmError> {
        match op {
            Opcode::Nop => Ok(Signal::Continue),
            Opcode::Halt => Ok(Signal::Return(
                self.stack.last().map_or(Value::Unit, Clone::clone),
            )),
            Opcode::Ret => self.exec_ret(),
            Opcode::Drop => {
                let _ = self.stack.pop().ok_or(VmError::StackUnderflow)?;
                Ok(Signal::Continue)
            }
            Opcode::LdImmI64(v) => {
                self.stack.push(Value::Int(v));
                Ok(Signal::Continue)
            }
            Opcode::LdImmF64(v) => {
                self.stack.push(Value::Float(v));
                Ok(Signal::Continue)
            }
            Opcode::LdImmBool(v) => {
                self.stack.push(Value::Bool(v));
                Ok(Signal::Continue)
            }
            Opcode::LdImmUnit => {
                self.stack.push(Value::Unit);
                Ok(Signal::Continue)
            }
            Opcode::LdConst(idx) => self.exec_ld_const(idx),
            Opcode::LdLoc(idx) => self.exec_ld_loc(idx),
            Opcode::StLoc(idx) => self.exec_st_loc(idx),
            Opcode::Call(fn_idx) => {
                self.execute_call(fn_idx)?;
                Ok(Signal::Continue)
            }
            Opcode::LdFnIdx(idx) => {
                self.stack.push(Value::Function(idx));
                Ok(Signal::Continue)
            }
            Opcode::CallDynamic => {
                let v = self.stack.pop().ok_or(VmError::StackUnderflow)?;
                let Value::Function(fn_idx) = v else {
                    return Err(VmError::NotAFunction);
                };
                self.execute_call(fn_idx)?;
                Ok(Signal::Continue)
            }
            Opcode::Dup => {
                let top = self.stack.last().ok_or(VmError::StackUnderflow)?.clone();
                self.stack.push(top);
                Ok(Signal::Continue)
            }
            Opcode::HaltError => Err(VmError::MatchFailure),
            Opcode::NewObj(n) => {
                let n_usize = usize::from(n);
                let stack_len = self.stack.len();
                if stack_len < n_usize {
                    return Err(VmError::StackUnderflow);
                }
                let fields: Vec<Value> = self.stack.drain(stack_len - n_usize..).collect();
                self.stack.push(Value::Object(Rc::new(fields)));
                Ok(Signal::Continue)
            }
            Opcode::LdFld(idx) => {
                let obj = self.stack.pop().ok_or(VmError::StackUnderflow)?;
                let Value::Object(rc) = obj else {
                    return Err(VmError::TypeMismatch);
                };
                let field = rc
                    .get(usize::from(idx))
                    .ok_or(VmError::FieldOutOfBounds(idx))?
                    .clone();
                self.stack.push(field);
                Ok(Signal::Continue)
            }
            Opcode::LdTag => {
                let obj = self.stack.pop().ok_or(VmError::StackUnderflow)?;
                let Value::Object(rc) = obj else {
                    return Err(VmError::TypeMismatch);
                };
                let tag = rc.first().ok_or(VmError::FieldOutOfBounds(0))?.clone();
                self.stack.push(tag);
                Ok(Signal::Continue)
            }
            Opcode::AddI64
            | Opcode::SubI64
            | Opcode::MulI64
            | Opcode::DivI64
            | Opcode::RemI64
            | Opcode::NegI64
            | Opcode::AddF64
            | Opcode::SubF64
            | Opcode::MulF64
            | Opcode::DivF64
            | Opcode::RemF64
            | Opcode::NegF64 => self.exec_arith(op),
            Opcode::EqI64
            | Opcode::NeqI64
            | Opcode::LtI64
            | Opcode::GtI64
            | Opcode::LeqI64
            | Opcode::GeqI64
            | Opcode::EqF64
            | Opcode::NeqF64
            | Opcode::LtF64
            | Opcode::GtF64
            | Opcode::LeqF64
            | Opcode::GeqF64
            | Opcode::EqBool
            | Opcode::NeqBool
            | Opcode::EqStr
            | Opcode::NeqStr => self.exec_cmp(op),
            Opcode::Not
            | Opcode::BitAnd
            | Opcode::BitOr
            | Opcode::BitXor
            | Opcode::BitNot
            | Opcode::Shl
            | Opcode::Shr => self.exec_bitwise(op),
            Opcode::Br(offset) => {
                let frame = self.frames.last_mut().ok_or(VmError::NoFrames)?;
                frame.pc = apply_branch_offset(frame.pc, offset)?;
                Ok(Signal::Continue)
            }
            Opcode::BrTrue(offset) => self.exec_br_cond(offset, true),
            Opcode::BrFalse(offset) => self.exec_br_cond(offset, false),
            Opcode::ConcatStr => self.exec_concat_str(),
        }
    }

    fn exec_ret(&mut self) -> Result<Signal, VmError> {
        let result = self.stack.pop().ok_or(VmError::StackUnderflow)?;
        let frame = self.frames.pop().ok_or(VmError::NoFrames)?;
        self.stack.truncate(frame.stack_base);
        if self.frames.is_empty() {
            return Ok(Signal::Return(result));
        }
        self.stack.push(result);
        Ok(Signal::Continue)
    }

    fn exec_ld_const(&mut self, idx: u16) -> Result<Signal, VmError> {
        let value = {
            let entry = self
                .module
                .const_pool
                .get(usize::from(idx))
                .ok_or(VmError::ConstOutOfBounds(idx))?;
            const_entry_to_value(entry)
        };
        self.stack.push(value);
        Ok(Signal::Continue)
    }

    fn exec_ld_loc(&mut self, idx: u16) -> Result<Signal, VmError> {
        let local = {
            let frame = self.frames.last().ok_or(VmError::NoFrames)?;
            frame
                .locals
                .get(usize::from(idx))
                .ok_or(VmError::LocalOutOfBounds(idx))?
                .clone()
        };
        self.stack.push(local);
        Ok(Signal::Continue)
    }

    fn exec_st_loc(&mut self, idx: u16) -> Result<Signal, VmError> {
        let value = self.stack.pop().ok_or(VmError::StackUnderflow)?;
        let slot = self
            .frames
            .last_mut()
            .ok_or(VmError::NoFrames)?
            .locals
            .get_mut(usize::from(idx))
            .ok_or(VmError::LocalOutOfBounds(idx))?;
        *slot = value;
        Ok(Signal::Continue)
    }

    fn exec_br_cond(&mut self, offset: i32, when: bool) -> Result<Signal, VmError> {
        let v = self.stack.pop().ok_or(VmError::StackUnderflow)?;
        if matches!(v, Value::Bool(b) if b == when) {
            let frame = self.frames.last_mut().ok_or(VmError::NoFrames)?;
            frame.pc = apply_branch_offset(frame.pc, offset)?;
        }
        Ok(Signal::Continue)
    }

    fn exec_concat_str(&mut self) -> Result<Signal, VmError> {
        let rhs = pop_str(&mut self.stack)?;
        let lhs = pop_str(&mut self.stack)?;
        let mut s = String::with_capacity(lhs.len() + rhs.len());
        s.push_str(&lhs);
        s.push_str(&rhs);
        self.stack.push(Value::String(Rc::from(s.as_str())));
        Ok(Signal::Continue)
    }

    fn push_i64_bin<F: FnOnce(i64, i64) -> i64>(&mut self, f: F) -> Result<Signal, VmError> {
        let rhs = pop_i64(&mut self.stack)?;
        let lhs = pop_i64(&mut self.stack)?;
        self.stack.push(Value::Int(f(lhs, rhs)));
        Ok(Signal::Continue)
    }

    fn push_i64_cmp<F: FnOnce(i64, i64) -> bool>(&mut self, f: F) -> Result<Signal, VmError> {
        let rhs = pop_i64(&mut self.stack)?;
        let lhs = pop_i64(&mut self.stack)?;
        self.stack.push(Value::Bool(f(lhs, rhs)));
        Ok(Signal::Continue)
    }

    fn push_f64_bin<F: FnOnce(f64, f64) -> f64>(&mut self, f: F) -> Result<Signal, VmError> {
        let rhs = pop_f64(&mut self.stack)?;
        let lhs = pop_f64(&mut self.stack)?;
        self.stack.push(Value::Float(f(lhs, rhs)));
        Ok(Signal::Continue)
    }

    fn push_f64_cmp<F: FnOnce(f64, f64) -> bool>(&mut self, f: F) -> Result<Signal, VmError> {
        let rhs = pop_f64(&mut self.stack)?;
        let lhs = pop_f64(&mut self.stack)?;
        self.stack.push(Value::Bool(f(lhs, rhs)));
        Ok(Signal::Continue)
    }

    fn exec_arith(&mut self, op: Opcode) -> Result<Signal, VmError> {
        match op {
            Opcode::AddI64 => self.push_i64_bin(i64::wrapping_add),
            Opcode::SubI64 => self.push_i64_bin(i64::wrapping_sub),
            Opcode::MulI64 => self.push_i64_bin(i64::wrapping_mul),
            Opcode::DivI64 => {
                let rhs = pop_i64(&mut self.stack)?;
                let lhs = pop_i64(&mut self.stack)?;
                if rhs == 0 {
                    return Err(VmError::DivisionByZero);
                }
                self.stack.push(Value::Int(lhs.wrapping_div(rhs)));
                Ok(Signal::Continue)
            }
            Opcode::RemI64 => {
                let rhs = pop_i64(&mut self.stack)?;
                let lhs = pop_i64(&mut self.stack)?;
                if rhs == 0 {
                    return Err(VmError::DivisionByZero);
                }
                self.stack.push(Value::Int(lhs.wrapping_rem(rhs)));
                Ok(Signal::Continue)
            }
            Opcode::NegI64 => {
                let v = pop_i64(&mut self.stack)?;
                self.stack.push(Value::Int(v.wrapping_neg()));
                Ok(Signal::Continue)
            }
            Opcode::AddF64 => self.push_f64_bin(|a, b| a + b),
            Opcode::SubF64 => self.push_f64_bin(|a, b| a - b),
            Opcode::MulF64 => self.push_f64_bin(|a, b| a * b),
            Opcode::DivF64 => self.push_f64_bin(|a, b| a / b),
            Opcode::RemF64 => self.push_f64_bin(|a, b| a % b),
            Opcode::NegF64 => {
                let v = pop_f64(&mut self.stack)?;
                self.stack.push(Value::Float(-v));
                Ok(Signal::Continue)
            }
            _ => Ok(Signal::Continue),
        }
    }

    #[allow(clippy::float_cmp)]
    fn exec_cmp(&mut self, op: Opcode) -> Result<Signal, VmError> {
        match op {
            Opcode::EqI64 => self.push_i64_cmp(|a, b| a == b),
            Opcode::NeqI64 => self.push_i64_cmp(|a, b| a != b),
            Opcode::LtI64 => self.push_i64_cmp(|a, b| a < b),
            Opcode::GtI64 => self.push_i64_cmp(|a, b| a > b),
            Opcode::LeqI64 => self.push_i64_cmp(|a, b| a <= b),
            Opcode::GeqI64 => self.push_i64_cmp(|a, b| a >= b),
            Opcode::EqF64 => self.push_f64_cmp(|a, b| a == b),
            Opcode::NeqF64 => self.push_f64_cmp(|a, b| a != b),
            Opcode::LtF64 => self.push_f64_cmp(|a, b| a < b),
            Opcode::GtF64 => self.push_f64_cmp(|a, b| a > b),
            Opcode::LeqF64 => self.push_f64_cmp(|a, b| a <= b),
            Opcode::GeqF64 => self.push_f64_cmp(|a, b| a >= b),
            Opcode::EqBool => {
                let rhs = pop_bool(&mut self.stack)?;
                let lhs = pop_bool(&mut self.stack)?;
                self.stack.push(Value::Bool(lhs == rhs));
                Ok(Signal::Continue)
            }
            Opcode::NeqBool => {
                let rhs = pop_bool(&mut self.stack)?;
                let lhs = pop_bool(&mut self.stack)?;
                self.stack.push(Value::Bool(lhs != rhs));
                Ok(Signal::Continue)
            }
            Opcode::EqStr => {
                let rhs = pop_str(&mut self.stack)?;
                let lhs = pop_str(&mut self.stack)?;
                self.stack.push(Value::Bool(*lhs == *rhs));
                Ok(Signal::Continue)
            }
            Opcode::NeqStr => {
                let rhs = pop_str(&mut self.stack)?;
                let lhs = pop_str(&mut self.stack)?;
                self.stack.push(Value::Bool(*lhs != *rhs));
                Ok(Signal::Continue)
            }
            _ => Ok(Signal::Continue),
        }
    }

    fn exec_bitwise(&mut self, op: Opcode) -> Result<Signal, VmError> {
        match op {
            Opcode::Not => {
                let v = pop_bool(&mut self.stack)?;
                self.stack.push(Value::Bool(!v));
                Ok(Signal::Continue)
            }
            Opcode::BitAnd => self.push_i64_bin(|a, b| a & b),
            Opcode::BitOr => self.push_i64_bin(|a, b| a | b),
            Opcode::BitXor => self.push_i64_bin(|a, b| a ^ b),
            Opcode::BitNot => {
                let v = pop_i64(&mut self.stack)?;
                self.stack.push(Value::Int(!v));
                Ok(Signal::Continue)
            }
            Opcode::Shl => {
                self.push_i64_bin(|a, b| a.wrapping_shl(u32::try_from(b).unwrap_or(u32::MAX)))
            }
            Opcode::Shr => {
                self.push_i64_bin(|a, b| a.wrapping_shr(u32::try_from(b).unwrap_or(u32::MAX)))
            }
            _ => Ok(Signal::Continue),
        }
    }

    fn execute_call(&mut self, fn_idx: u16) -> Result<(), VmError> {
        let (param_count, local_count, symbol_idx) = {
            let func = self
                .module
                .function_table
                .get(usize::from(fn_idx))
                .ok_or(VmError::FunctionOutOfBounds(fn_idx))?;
            (
                usize::from(func.param_count),
                usize::from(func.local_count),
                func.symbol_idx,
            )
        };

        let (is_native, intrinsic_id) = {
            let sym = self
                .module
                .symbol_table
                .get(usize::from(symbol_idx))
                .ok_or(VmError::SymbolOutOfBounds(symbol_idx))?;
            (sym.flags.is_native(), sym.intrinsic_id)
        };

        let stack_len = self.stack.len();
        if stack_len < param_count {
            return Err(VmError::StackUnderflow);
        }
        let args: Vec<Value> = self.stack.drain(stack_len - param_count..).collect();

        if is_native {
            let intrinsic = Intrinsic::from_id(intrinsic_id)
                .ok_or(VmError::UnknownIntrinsic(intrinsic_id))?;
            let result = crate::native::dispatch(self, intrinsic, &args);
            self.stack.push(result);
        } else {
            let stack_base = self.stack.len();
            let mut locals: Vec<Value> = args;
            while locals.len() < local_count {
                locals.push(Value::Unit);
            }
            self.frames.push(CallFrame {
                function_idx: fn_idx,
                pc: 0,
                locals,
                stack_base,
            });
        }
        Ok(())
    }
}

// -- Helpers ------------------------------------------------------------------

fn apply_branch_offset(pc: usize, offset: i32) -> Result<usize, VmError> {
    if offset >= 0 {
        pc.checked_add(usize::try_from(offset).map_err(|_| VmError::CodeOutOfBounds)?)
            .ok_or(VmError::CodeOutOfBounds)
    } else {
        let back = usize::try_from(offset.unsigned_abs()).map_err(|_| VmError::CodeOutOfBounds)?;
        pc.checked_sub(back).ok_or(VmError::CodeOutOfBounds)
    }
}

fn pop_i64(stack: &mut Vec<Value>) -> Result<i64, VmError> {
    match stack.pop().ok_or(VmError::StackUnderflow)? {
        Value::Int(v) => Ok(v),
        _ => Err(VmError::TypeMismatch),
    }
}

fn pop_f64(stack: &mut Vec<Value>) -> Result<f64, VmError> {
    match stack.pop().ok_or(VmError::StackUnderflow)? {
        Value::Float(v) => Ok(v),
        _ => Err(VmError::TypeMismatch),
    }
}

fn pop_bool(stack: &mut Vec<Value>) -> Result<bool, VmError> {
    match stack.pop().ok_or(VmError::StackUnderflow)? {
        Value::Bool(v) => Ok(v),
        _ => Err(VmError::TypeMismatch),
    }
}

fn pop_str(stack: &mut Vec<Value>) -> Result<Rc<str>, VmError> {
    match stack.pop().ok_or(VmError::StackUnderflow)? {
        Value::String(s) => Ok(s),
        _ => Err(VmError::TypeMismatch),
    }
}

fn const_entry_to_value(entry: &ConstEntry) -> Value {
    match entry {
        ConstEntry::Int(v) => Value::Int(*v),
        ConstEntry::Float(v) => Value::Float(*v),
        ConstEntry::Bool(v) => Value::Bool(*v),
        ConstEntry::String(s) => Value::String(Rc::from(s.as_ref())),
    }
}

#[cfg(test)]
mod tests;
