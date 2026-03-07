use std::cell::RefCell;
use std::rc::Rc;

use musi_codegen::intrinsics::Intrinsic;
use musi_codegen::{ConstEntry, Module, Opcode};

use crate::ffi::FfiState;
use crate::native;
use crate::native_registry::NativeRegistry;

use crate::error::VmError;
use crate::value::Value;

pub struct CallFrame {
    pub function_idx: u16,
    pub pc: usize,
    pub locals: Vec<Value>,
    pub stack_base: usize,
}

enum Signal {
    Continue,
    Return(Value),
}

pub struct TestResult {
    pub label: Box<str>,
    pub passed: bool,
    pub error: Option<Box<str>>,
}

pub struct Vm {
    pub(crate) stack: Vec<Value>,
    pub(crate) frames: Vec<CallFrame>,
    pub(crate) module: Module,
    pub(crate) ffi: FfiState,
    pub(crate) registry: NativeRegistry,
    test_mode: bool,
    pub test_results: Vec<TestResult>,
}

impl Vm {
    #[must_use]
    pub fn new(module: Module, registry: NativeRegistry) -> Self {
        Self {
            stack: Vec::new(),
            frames: Vec::new(),
            ffi: FfiState::new(),
            module,
            registry,
            test_mode: false,
            test_results: Vec::new(),
        }
    }

    /// Run the module in test mode: intercepts `test(name, f)` calls, runs each
    /// test function, catches `AssertionFailed`, and collects results.
    ///
    /// # Errors
    ///
    /// Returns `VmError` on any non-assertion runtime error.
    pub fn run_tests(&mut self, entry_fn: u16) -> Result<(), VmError> {
        self.test_mode = true;
        let _ = self.run(entry_fn)?;
        Ok(())
    }

    /// Run the VM starting at the given function table index.
    ///
    /// # Errors
    ///
    /// Returns `VmError` on any runtime error (type mismatch, stack underflow, etc.).
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

    fn run_until_depth(&mut self, depth: usize) -> Result<(), VmError> {
        while self.frames.len() > depth {
            let op = self.fetch_and_advance()?;
            match self.step(op)? {
                Signal::Continue => {}
                Signal::Return(_) => break,
            }
        }
        Ok(())
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
            Opcode::LdImmI64(v) => { self.stack.push(Value::Int(v)); Ok(Signal::Continue) }
            Opcode::LdImmF64(v) => { self.stack.push(Value::Float(v)); Ok(Signal::Continue) }
            Opcode::LdImmUnit => { self.stack.push(Value::Unit); Ok(Signal::Continue) }
            Opcode::LdConst(idx) => self.exec_ld_const(idx),
            Opcode::LdLoc(idx) => self.exec_ld_loc(idx),
            Opcode::StLoc(idx) => self.exec_st_loc(idx),
            Opcode::Call(fn_idx) => {
                self.exec_call(fn_idx)?;
                Ok(Signal::Continue)
            }
            Opcode::LdFnIdx(idx) => {
                self.stack.push(Value::Function(idx));
                Ok(Signal::Continue)
            }
            Opcode::CallDynamic => self.exec_call_dynamic(),
            Opcode::Dup => {
                let top = self.stack.last().ok_or(VmError::StackUnderflow)?.clone();
                self.stack.push(top); Ok(Signal::Continue)
            }
            Opcode::HaltError => Err(VmError::MatchFailure),
            Opcode::NewObj {
                type_tag,
                field_count,
            } => self.exec_new_obj(type_tag, field_count),
            Opcode::LdFld(idx) => self.exec_ld_fld(idx),
            Opcode::LdTag => self.exec_ld_tag(),
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
            | Opcode::NegF64 => self.exec_arith(&op),
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
            | Opcode::NeqStr => self.exec_cmp(&op),
            Opcode::Not
            | Opcode::BitAnd
            | Opcode::BitOr
            | Opcode::BitXor
            | Opcode::BitNot
            | Opcode::Shl
            | Opcode::Shr => self.exec_bitwise(&op),
            Opcode::Br(offset) => {
                let frame = self.frames.last_mut().ok_or(VmError::NoFrames)?;
                frame.pc = apply_branch_offset(frame.pc, offset)?;
                Ok(Signal::Continue)
            }
            Opcode::BrTrue(offset) => self.exec_br_cond(offset, true),
            Opcode::BrFalse(offset) => self.exec_br_cond(offset, false),
            Opcode::ConcatStr => self.exec_concat_str(),
            Opcode::NilCoalesce => self.exec_nil_coalesce(),
            Opcode::OptField(field_idx) => self.exec_opt_field(field_idx),
            Opcode::CallMethod {
                method_idx,
                arg_count,
            } => {
                self.exec_call_method(method_idx, arg_count)?;
                Ok(Signal::Continue)
            }
            Opcode::NewArr(n) => self.exec_new_arr(n),
            Opcode::ArrGet => self.exec_arr_get(),
            Opcode::ArrSet => self.exec_arr_set(),
            Opcode::ArrLen => self.exec_arr_len(),
            Opcode::ArrPush => self.exec_arr_push(),
            Opcode::ArrSlice => self.exec_arr_slice(),
        }
    }

    fn exec_new_obj(&mut self, type_tag: u16, field_count: u16) -> Result<Signal, VmError> {
        let n = usize::from(field_count);
        let stack_len = self.stack.len();
        if stack_len < n {
            return Err(VmError::StackUnderflow);
        }
        let fields: Vec<Value> = self.stack.drain(stack_len - n..).collect();
        self.stack.push(Value::Object {
            type_tag,
            fields: Rc::new(fields),
        });
        Ok(Signal::Continue)
    }

    fn exec_ld_fld(&mut self, idx: u16) -> Result<Signal, VmError> {
        let obj = self.stack.pop().ok_or(VmError::StackUnderflow)?;
        let Value::Object { fields, .. } = obj else {
            return Err(VmError::TypeMismatch);
        };
        let field = fields
            .get(usize::from(idx))
            .ok_or(VmError::FieldOutOfBounds(idx))?
            .clone();
        self.stack.push(field);
        Ok(Signal::Continue)
    }

    fn exec_ld_tag(&mut self) -> Result<Signal, VmError> {
        let obj = self.stack.pop().ok_or(VmError::StackUnderflow)?;
        let Value::Object { fields, .. } = obj else {
            return Err(VmError::TypeMismatch);
        };
        let tag = fields.first().ok_or(VmError::FieldOutOfBounds(0))?.clone();
        self.stack.push(tag);
        Ok(Signal::Continue)
    }

    fn exec_intrinsic_test(&mut self, args: &[Value]) -> Result<(), VmError> {
        let name: Box<str> = args
            .first()
            .and_then(|v| {
                if let Value::String(s) = v {
                    Some(Box::from(s.as_ref()))
                } else {
                    None
                }
            })
            .unwrap_or_else(|| Box::from("<unnamed>"));
        let test_fn_idx = args.get(1).and_then(|v| {
            if let Value::Function(idx) = v {
                Some(*idx)
            } else {
                None
            }
        });
        if let Some(test_fn_idx) = test_fn_idx {
            let depth = self.frames.len();
            self.exec_call(test_fn_idx)?;
            if self.test_mode {
                let run_result = self.run_until_depth(depth);
                let _ = self.stack.pop();
                match run_result {
                    Ok(()) => self.test_results.push(TestResult {
                        label: name,
                        passed: true,
                        error: None,
                    }),
                    Err(VmError::AssertionFailed(msg)) => {
                        self.frames.truncate(depth);
                        self.test_results.push(TestResult {
                            label: name,
                            passed: false,
                            error: Some(msg),
                        });
                    }
                    Err(e) => return Err(e),
                }
            } else {
                self.run_until_depth(depth)?;
                let _ = self.stack.pop();
            }
        }
        self.stack.push(Value::Unit);
        Ok(())
    }

    fn exec_call_dynamic(&mut self) -> Result<Signal, VmError> {
        let v = self.stack.pop().ok_or(VmError::StackUnderflow)?;
        let Value::Function(fn_idx) = v else {
            return Err(VmError::NotAFunction);
        };
        self.exec_call(fn_idx)?;
        Ok(Signal::Continue)
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
        let cond = pop_bool_obj(&mut self.stack)?;
        if cond == when {
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
        self.stack.push(bool_obj(f(lhs, rhs)));
        Ok(Signal::Continue)
    }

    fn dispatch_or_i64_bin(
        &mut self,
        method: &str,
        op: fn(i64, i64) -> i64,
    ) -> Result<Signal, VmError> {
        if let Some(r) = self.try_dispatch_binop(method) {
            return r;
        }
        self.push_i64_bin(op)
    }

    fn dispatch_or_i64_cmp(
        &mut self,
        method: &str,
        op: fn(i64, i64) -> bool,
    ) -> Result<Signal, VmError> {
        if let Some(r) = self.try_dispatch_binop(method) {
            return r;
        }
        self.push_i64_cmp(op)
    }

    fn push_bool_cmp(&mut self, op: fn(bool, bool) -> bool) -> Result<Signal, VmError> {
        let rhs = pop_bool_obj(&mut self.stack)?;
        let lhs = pop_bool_obj(&mut self.stack)?;
        self.stack.push(bool_obj(op(lhs, rhs)));
        Ok(Signal::Continue)
    }

    fn push_str_cmp(&mut self, op: fn(&str, &str) -> bool) -> Result<Signal, VmError> {
        let rhs = pop_str(&mut self.stack)?;
        let lhs = pop_str(&mut self.stack)?;
        self.stack.push(bool_obj(op(&lhs, &rhs)));
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
        self.stack.push(bool_obj(f(lhs, rhs)));
        Ok(Signal::Continue)
    }

    fn try_dispatch_binop(&mut self, method: &str) -> Option<Result<Signal, VmError>> {
        let len = self.stack.len();
        if len < 2 {
            return None;
        }
        let type_tag = match &self.stack[len - 2] {
            Value::Object { type_tag, .. } if *type_tag != 0 => *type_tag,
            _ => return None,
        };
        let fn_idx = self
            .module
            .method_table
            .iter()
            .find(|e| e.name.as_ref() == method && e.type_tag == type_tag)
            .map(|e| e.fn_idx)?;
        Some(self.exec_call(fn_idx).map(|()| Signal::Continue))
    }

    fn exec_arith(&mut self, op: &Opcode) -> Result<Signal, VmError> {
        match op {
            Opcode::AddI64 => self.dispatch_or_i64_bin("add", i64::wrapping_add),
            Opcode::SubI64 => self.dispatch_or_i64_bin("sub", i64::wrapping_sub),
            Opcode::MulI64 => self.dispatch_or_i64_bin("mul", i64::wrapping_mul),
            Opcode::DivI64 => {
                if let Some(r) = self.try_dispatch_binop("div") {
                    return r;
                }
                let rhs = pop_i64(&mut self.stack)?;
                let lhs = pop_i64(&mut self.stack)?;
                if rhs == 0 {
                    return Err(VmError::DivisionByZero);
                }
                self.stack.push(Value::Int(lhs.wrapping_div(rhs)));
                Ok(Signal::Continue)
            }
            Opcode::RemI64 => {
                if let Some(r) = self.try_dispatch_binop("rem") {
                    return r;
                }
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
            _ => Err(VmError::TypeMismatch),
        }
    }

    fn exec_nil_coalesce(&mut self) -> Result<Signal, VmError> {
        let rhs = self.stack.pop().ok_or(VmError::StackUnderflow)?;
        let lhs = self.stack.pop().ok_or(VmError::StackUnderflow)?;
        match lhs {
            Value::Object { ref fields, .. } => {
                if matches!(fields.first(), Some(Value::Int(0))) {
                    self.stack.push(rhs);
                } else {
                    let inner = fields.get(1).ok_or(VmError::FieldOutOfBounds(1))?.clone();
                    self.stack.push(inner);
                }
            }
            _ => return Err(VmError::TypeMismatch),
        }
        Ok(Signal::Continue)
    }

    fn exec_opt_field(&mut self, field_idx: u16) -> Result<Signal, VmError> {
        let obj = self.stack.pop().ok_or(VmError::StackUnderflow)?;
        match obj {
            Value::Object {
                ref fields,
                type_tag,
            } => {
                if matches!(fields.first(), Some(Value::Int(0))) {
                    // None → push None (same structure)
                    self.stack.push(Value::Object {
                        type_tag,
                        fields: Rc::new(vec![Value::Int(0)]),
                    });
                } else {
                    // Some(inner) → access field[field_idx] on inner, wrap in Some
                    let inner = fields.get(1).ok_or(VmError::FieldOutOfBounds(1))?.clone();
                    let Value::Object {
                        fields: inner_fields,
                        ..
                    } = inner
                    else {
                        return Err(VmError::TypeMismatch);
                    };
                    let field_val = inner_fields
                        .get(usize::from(field_idx))
                        .ok_or(VmError::FieldOutOfBounds(field_idx))?
                        .clone();
                    // Wrap in Some: [discriminant=1, field_val]
                    self.stack.push(Value::Object {
                        type_tag,
                        fields: Rc::new(vec![Value::Int(1), field_val]),
                    });
                }
            }
            _ => return Err(VmError::TypeMismatch),
        }
        Ok(Signal::Continue)
    }

    #[allow(clippy::float_cmp)]
    fn exec_cmp(&mut self, op: &Opcode) -> Result<Signal, VmError> {
        match op {
            Opcode::EqI64 => self.dispatch_or_i64_cmp("eq", |a, b| a == b),
            Opcode::NeqI64 => self.dispatch_or_i64_cmp("neq", |a, b| a != b),
            Opcode::LtI64 => self.dispatch_or_i64_cmp("lt", |a, b| a < b),
            Opcode::GtI64 => self.dispatch_or_i64_cmp("gt", |a, b| a > b),
            Opcode::LeqI64 => self.dispatch_or_i64_cmp("leq", |a, b| a <= b),
            Opcode::GeqI64 => self.dispatch_or_i64_cmp("geq", |a, b| a >= b),
            Opcode::EqF64 => self.push_f64_cmp(|a, b| a == b),
            Opcode::NeqF64 => self.push_f64_cmp(|a, b| a != b),
            Opcode::LtF64 => self.push_f64_cmp(|a, b| a < b),
            Opcode::GtF64 => self.push_f64_cmp(|a, b| a > b),
            Opcode::LeqF64 => self.push_f64_cmp(|a, b| a <= b),
            Opcode::GeqF64 => self.push_f64_cmp(|a, b| a >= b),
            Opcode::EqBool => self.push_bool_cmp(|a, b| a == b),
            Opcode::NeqBool => self.push_bool_cmp(|a, b| a != b),
            Opcode::EqStr => self.push_str_cmp(|a, b| a == b),
            Opcode::NeqStr => self.push_str_cmp(|a, b| a != b),
            _ => Err(VmError::TypeMismatch),
        }
    }

    fn exec_bitwise(&mut self, op: &Opcode) -> Result<Signal, VmError> {
        match op {
            Opcode::Not => {
                let v = pop_bool_obj(&mut self.stack)?;
                self.stack.push(bool_obj(!v));
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
            _ => Err(VmError::TypeMismatch),
        }
    }

    fn exec_call_method(&mut self, method_idx: u16, arg_count: u16) -> Result<(), VmError> {
        let stack_len = self.stack.len();
        let receiver_pos = stack_len
            .checked_sub(usize::from(arg_count))
            .ok_or(VmError::StackUnderflow)?;
        let recv_tag: u16 = type_tag_of(&self.stack[receiver_pos]);

        let method_name: Box<str> = match self.module.const_pool.get(usize::from(method_idx)) {
            Some(ConstEntry::String(s)) => s.clone(),
            _ => return Err(VmError::MethodNotFound),
        };

        let fn_idx = self
            .module
            .method_table
            .iter()
            .find(|e| e.name.as_ref() == method_name.as_ref() && e.type_tag == recv_tag)
            .map(|e| e.fn_idx)
            .ok_or(VmError::MethodNotFound)?;

        self.exec_call(fn_idx)
    }

    fn exec_new_arr(&mut self, n: u16) -> Result<Signal, VmError> {
        let count = usize::from(n);
        let stack_len = self.stack.len();
        if stack_len < count {
            return Err(VmError::StackUnderflow);
        }
        let items: Vec<Value> = self.stack.drain(stack_len - count..).collect();
        self.stack.push(Value::Array(Rc::new(RefCell::new(items))));
        Ok(Signal::Continue)
    }

    fn exec_arr_get(&mut self) -> Result<Signal, VmError> {
        let idx = pop_i64(&mut self.stack)?;
        let a = pop_array(&mut self.stack)?;
        let borrowed = a.borrow();
        let len = borrowed.len();
        let i = usize::try_from(idx).map_err(|_| VmError::IndexOutOfBounds { index: idx, len })?;
        let val = borrowed
            .get(i)
            .ok_or(VmError::IndexOutOfBounds { index: idx, len })?
            .clone();
        drop(borrowed);
        self.stack.push(val);
        Ok(Signal::Continue)
    }

    fn exec_arr_set(&mut self) -> Result<Signal, VmError> {
        let val = self.stack.pop().ok_or(VmError::StackUnderflow)?;
        let idx = pop_i64(&mut self.stack)?;
        let a = pop_array(&mut self.stack)?;
        let mut borrowed = a.borrow_mut();
        let len = borrowed.len();
        let i = usize::try_from(idx).map_err(|_| VmError::IndexOutOfBounds { index: idx, len })?;
        let slot = borrowed
            .get_mut(i)
            .ok_or(VmError::IndexOutOfBounds { index: idx, len })?;
        *slot = val;
        drop(borrowed);
        self.stack.push(Value::Unit);
        Ok(Signal::Continue)
    }

    fn exec_arr_len(&mut self) -> Result<Signal, VmError> {
        let a = pop_array(&mut self.stack)?;
        let len = i64::try_from(a.borrow().len()).unwrap_or(i64::MAX);
        self.stack.push(Value::Int(len));
        Ok(Signal::Continue)
    }

    fn exec_arr_push(&mut self) -> Result<Signal, VmError> {
        let val = self.stack.pop().ok_or(VmError::StackUnderflow)?;
        let a = pop_array(&mut self.stack)?;
        a.borrow_mut().push(val);
        self.stack.push(Value::Unit);
        Ok(Signal::Continue)
    }

    fn exec_arr_slice(&mut self) -> Result<Signal, VmError> {
        let end = pop_i64(&mut self.stack)?;
        let start = pop_i64(&mut self.stack)?;
        let a = pop_array(&mut self.stack)?;
        let borrowed = a.borrow();
        let len = i64::try_from(borrowed.len()).unwrap_or(i64::MAX);
        let lo = usize::try_from(start.clamp(0, len)).unwrap_or(0);
        let hi = usize::try_from(end.clamp(0, len)).unwrap_or(0);
        let slice: Vec<Value> = borrowed[lo..hi.max(lo)].to_vec();
        drop(borrowed);
        self.stack.push(Value::Array(Rc::new(RefCell::new(slice))));
        Ok(Signal::Continue)
    }

    fn exec_call(&mut self, fn_idx: u16) -> Result<(), VmError> {
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
            if intrinsic_id == u16::MAX {
                // extrin fn — dispatch via dlopen/dlsym
                let result = self.ffi.call(fn_idx, &args, &self.module)?;
                self.stack.push(result);
            } else {
                let intrinsic = Intrinsic::from_id(intrinsic_id)
                    .ok_or(VmError::UnknownIntrinsic(intrinsic_id))?;
                match intrinsic {
                    Intrinsic::Assert => {
                        let cond = args.first().cloned().unwrap_or(Value::Unit);
                        if !is_truthy(&cond) {
                            return Err(VmError::AssertionFailed("assertion failed".into()));
                        }
                        self.stack.push(Value::Unit);
                    }
                    Intrinsic::AssertMsg => {
                        let cond = args.first().cloned().unwrap_or(Value::Unit);
                        let msg: Box<str> = args
                            .get(1)
                            .and_then(|v| {
                                if let Value::String(s) = v {
                                    Some(Box::from(s.as_ref()))
                                } else {
                                    None
                                }
                            })
                            .unwrap_or_else(|| Box::from("assertion failed"));
                        if !is_truthy(&cond) {
                            return Err(VmError::AssertionFailed(msg));
                        }
                        self.stack.push(Value::Unit);
                    }
                    Intrinsic::Test => self.exec_intrinsic_test(&args)?,
                    _ => {
                        let result = self
                            .registry
                            .lookup_id(intrinsic_id)
                            .map_or_else(|| native::dispatch(self, intrinsic, &args), |f| f(&args));
                        self.stack.push(result);
                    }
                }
            }
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

fn is_truthy(v: &Value) -> bool {
    match v {
        Value::Object { fields, .. } => fields
            .first()
            .is_some_and(|f| matches!(f, Value::Int(n) if *n != 0)),
        _ => false,
    }
}

const fn type_tag_of(v: &Value) -> u16 {
    match v {
        Value::Int(_) => 1,
        Value::Float(_) => 2,
        Value::String(_) => 4,
        Value::Unit => 5,
        Value::Function(_) => 6,
        Value::Object { type_tag, .. } => *type_tag,
        Value::Array(_) => 7,
    }
}

fn apply_branch_offset(pc: usize, offset: i32) -> Result<usize, VmError> {
    if offset >= 0 {
        pc.checked_add(usize::try_from(offset).map_err(|_| VmError::CodeOutOfBounds)?)
            .ok_or(VmError::CodeOutOfBounds)
    } else {
        let back = usize::try_from(offset.unsigned_abs()).map_err(|_| VmError::CodeOutOfBounds)?;
        pc.checked_sub(back).ok_or(VmError::CodeOutOfBounds)
    }
}

fn pop_array(stack: &mut Vec<Value>) -> Result<Rc<RefCell<Vec<Value>>>, VmError> {
    match stack.pop().ok_or(VmError::StackUnderflow)? {
        Value::Array(a) => Ok(a),
        _ => Err(VmError::TypeMismatch),
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

fn pop_bool_obj(stack: &mut Vec<Value>) -> Result<bool, VmError> {
    match stack.pop().ok_or(VmError::StackUnderflow)? {
        Value::Object { ref fields, .. } => {
            match fields.first().ok_or(VmError::FieldOutOfBounds(0))? {
                Value::Int(n) => Ok(*n != 0),
                _ => Err(VmError::TypeMismatch),
            }
        }
        _ => Err(VmError::TypeMismatch),
    }
}

fn bool_obj(b: bool) -> Value {
    Value::Object {
        type_tag: 0,
        fields: Rc::new(vec![Value::Int(i64::from(b))]),
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
        ConstEntry::String(s) => Value::String(Rc::from(s.as_ref())),
    }
}

#[cfg(test)]
mod tests;
