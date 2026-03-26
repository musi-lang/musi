#![allow(clippy::arithmetic_side_effects, clippy::as_conversions)]

use music_il::format;
use music_il::opcode::Opcode;

use crate::effect::EffectHandler;
use crate::errors::{VmError, VmResult};
use crate::ffi::{self, FfiRuntime};
use crate::frame::CallFrame;
use crate::heap::{Heap, HeapObject};
use crate::module::{ConstantEntry, Module, ENTRY_POINT_NAME};
use crate::value::Value;

const MAX_CALL_DEPTH: usize = 1024;

pub struct Vm {
    module: Module,
    globals: Vec<Value>,
    heap: Heap,
    frames: Vec<CallFrame>,
    effect_handlers: Vec<EffectHandler>,
    resolved_constants: Vec<Value>,
    ffi_runtime: FfiRuntime,
}

impl Vm {
    #[must_use]
    pub fn new(module: Module) -> Self {
        let globals_count = module.globals.len();
        let mut heap = Heap::new();
        let resolved_constants = module
            .constants
            .iter()
            .map(|entry| match entry {
                ConstantEntry::Value(v) => *v,
                ConstantEntry::StringRef(idx) => {
                    let s = module
                        .strings
                        .get(usize::from(*idx))
                        .cloned()
                        .unwrap_or_default();
                    Value::from_ptr(heap.alloc_string(s))
                }
            })
            .collect();
        Self {
            module,
            globals: vec![Value::UNIT; globals_count],
            heap,
            frames: Vec::new(),
            effect_handlers: Vec::new(),
            resolved_constants,
            ffi_runtime: FfiRuntime::new(),
        }
    }

    /// # Errors
    /// Returns a [`VmError`] if no entry point exists, the bytecode is invalid,
    /// a runtime type error occurs, or `Panic` is executed.
    pub fn run(&mut self) -> VmResult<Value> {
        let entry_idx = self
            .module
            .methods
            .iter()
            .position(|m| m.name == ENTRY_POINT_NAME)
            .ok_or(VmError::NoEntryPoint)?;
        let locals_count = usize::from(self.module.methods[entry_idx].locals_count);
        let frame = CallFrame::new_call(
            locals_count,
            0,
            u16::try_from(entry_idx).map_err(|_| VmError::InvalidMethod(entry_idx))?,
            None,
        );
        self.frames.push(frame);
        self.execute()
    }

    fn execute(&mut self) -> VmResult<Value> {
        let mut pc: usize = 0;
        loop {
            let method_idx =
                usize::from(self.frames.last().ok_or(VmError::NoEntryPoint)?.method_idx);
            let byte = *self
                .module
                .methods
                .get(method_idx)
                .ok_or(VmError::InvalidMethod(method_idx))?
                .code
                .get(pc)
                .ok_or(VmError::PcOutOfBounds)?;
            let op = Opcode::from_byte(byte).ok_or(VmError::InvalidOpcode(byte))?;
            pc = pc.wrapping_add(1);

            match op {
                Opcode::Halt => {
                    return Ok(self
                        .frames
                        .last_mut()
                        .ok_or(VmError::StackUnderflow)?
                        .peek_or(Value::UNIT));
                }
                Opcode::Nop => {}
                Opcode::Panic => return Err(VmError::ExplicitPanic),

                Opcode::BrTrue
                | Opcode::BrFalse
                | Opcode::BrJmp
                | Opcode::BrBack
                | Opcode::BrTbl => {
                    self.dispatch_branch(op, method_idx, &mut pc)?;
                }

                Opcode::Ret | Opcode::Call | Opcode::CallTail | Opcode::ClsNew => {
                    if let Some(ret) = self.dispatch_call(op, method_idx, &mut pc)? {
                        return Ok(ret);
                    }
                }

                Opcode::EffPush | Opcode::EffPop | Opcode::EffNeed | Opcode::EffCont => {
                    self.dispatch_effect(op, method_idx, &mut pc)?;
                }

                Opcode::ArrNew
                | Opcode::ArrNewT
                | Opcode::ArrGet
                | Opcode::ArrSet
                | Opcode::ArrGetI
                | Opcode::ArrSetI
                | Opcode::ArrLen
                | Opcode::ArrTag
                | Opcode::ArrCopy
                | Opcode::ArrCaten => {
                    self.dispatch_array(op, method_idx, &mut pc)?;
                }

                Opcode::TyChk | Opcode::TyCast | Opcode::TyTag => {
                    self.dispatch_type_op(op, method_idx, &mut pc)?;
                }

                Opcode::TyclDict | Opcode::TyclCall => {
                    self.dispatch_tycl(op, method_idx, &mut pc)?;
                }

                Opcode::FfiCall => {
                    self.dispatch_ffi_call(method_idx, &mut pc)?;
                }

                Opcode::ArrSlice => {
                    self.exec_arr_slice()?;
                }

                Opcode::ArrFill => {
                    self.exec_arr_fill()?;
                }

                Opcode::CmpEq | Opcode::CmpNeq => {
                    self.dispatch_equality(op)?;
                }

                Opcode::GcPin => {
                    let val = self.pop_stack()?;
                    if val.is_ptr() {
                        self.heap.pin(val.as_ptr_idx());
                    }
                    self.push_stack(val)?;
                }

                Opcode::GcUnpin => {
                    let val = self.pop_stack()?;
                    if val.is_ptr() {
                        self.heap.unpin(val.as_ptr_idx());
                    }
                    self.push_stack(val)?;
                }

                _ => self.dispatch_load_store_stack(op, method_idx, &mut pc)?,
            }
        }
    }

    fn dispatch_branch(&mut self, op: Opcode, method_idx: usize, pc: &mut usize) -> VmResult {
        match op {
            Opcode::BrTrue | Opcode::BrFalse => {
                let offset = self.read_i16(method_idx, pc)?;
                let cond = self
                    .frames
                    .last_mut()
                    .ok_or(VmError::StackUnderflow)?
                    .pop()?;
                if !cond.is_bool() {
                    return Err(VmError::TypeError {
                        expected: "Bool",
                        found: "non-Bool",
                    });
                }
                let take = cond.as_bool() == (op == Opcode::BrTrue);
                if take {
                    *pc = pc.wrapping_add_signed(isize::from(offset));
                }
            }
            Opcode::BrJmp | Opcode::BrBack => {
                let offset = self.read_i16(method_idx, pc)?;
                *pc = pc.wrapping_add_signed(isize::from(offset));
            }
            Opcode::BrTbl => {
                let count = usize::from(self.read_u16(method_idx, pc)?);
                let base_pc = *pc;
                *pc = pc.wrapping_add(count * 2);
                let idx_val = self
                    .frames
                    .last_mut()
                    .ok_or(VmError::StackUnderflow)?
                    .pop()?;
                if !idx_val.is_int() {
                    return Err(VmError::TypeError {
                        expected: "Int",
                        found: "non-Int",
                    });
                }
                let idx = idx_val.as_int();
                if let Ok(i) = usize::try_from(idx) {
                    if i < count {
                        let off_pos = base_pc + i * 2;
                        let code = &self
                            .module
                            .methods
                            .get(method_idx)
                            .ok_or(VmError::InvalidMethod(method_idx))?
                            .code;
                        let lo = *code.get(off_pos).ok_or(VmError::PcOutOfBounds)?;
                        let hi = *code.get(off_pos + 1).ok_or(VmError::PcOutOfBounds)?;
                        let offset = i16::from_le_bytes([lo, hi]);
                        *pc = pc.wrapping_add_signed(isize::from(offset));
                    }
                }
            }
            _ => return Err(VmError::UnsupportedOpcode(op)),
        }
        Ok(())
    }

    /// Returns `Some(value)` when the program exits (top-level `Ret` or `Halt`),
    /// `None` to continue the dispatch loop.
    fn dispatch_call(
        &mut self,
        op: Opcode,
        method_idx: usize,
        pc: &mut usize,
    ) -> VmResult<Option<Value>> {
        match op {
            Opcode::Ret => {
                let ret_val = self
                    .frames
                    .last_mut()
                    .ok_or(VmError::StackUnderflow)?
                    .peek_or(Value::UNIT);
                let frame = self.frames.pop().unwrap();
                if self.frames.is_empty() {
                    return Ok(Some(ret_val));
                }
                *pc = frame.return_pc;
                self.frames.last_mut().unwrap().push(ret_val);
            }

            Opcode::Call => {
                let arity = usize::from(self.read_u8(method_idx, pc)?);
                let (callee, args) = self.pop_call_args(arity)?;
                let (new_method_idx, closure_idx) = self.resolve_callee(callee)?;
                if self.frames.len() >= MAX_CALL_DEPTH {
                    return Err(VmError::CallStackOverflow);
                }
                let mut new_frame = self.make_call_frame(new_method_idx, *pc, closure_idx)?;
                for (i, arg) in args.iter().enumerate() {
                    new_frame.store_local(i, *arg)?;
                }
                self.frames.push(new_frame);
                *pc = 0;
                self.maybe_collect();
            }

            Opcode::CallTail => {
                let arity = usize::from(self.read_u8(method_idx, pc)?);
                let (callee, args) = self.pop_call_args(arity)?;
                let (new_method_idx, closure_idx) = self.resolve_callee(callee)?;
                let return_pc = self.frames.last().ok_or(VmError::StackUnderflow)?.return_pc;
                let mut new_frame = self.make_call_frame(new_method_idx, return_pc, closure_idx)?;
                for (i, arg) in args.iter().enumerate() {
                    new_frame.store_local(i, *arg)?;
                }
                *self.frames.last_mut().unwrap() = new_frame;
                *pc = 0;
            }

            Opcode::ClsNew => {
                let cls_method_idx = self.read_u16(method_idx, pc)?;
                let upval_count = usize::from(self.read_u8(method_idx, pc)?);
                let mut upvalues = Vec::with_capacity(upval_count);
                {
                    let frame = self.frames.last_mut().ok_or(VmError::StackUnderflow)?;
                    for _ in 0..upval_count {
                        upvalues.push(frame.pop()?);
                    }
                }
                upvalues.reverse();
                let heap_idx = self.heap.alloc_closure(cls_method_idx, upvalues);
                self.frames
                    .last_mut()
                    .unwrap()
                    .push(Value::from_ptr(heap_idx));
                self.maybe_collect();
            }

            _ => return Err(VmError::UnsupportedOpcode(op)),
        }
        Ok(None)
    }

    fn dispatch_load_store_stack(
        &mut self,
        op: Opcode,
        method_idx: usize,
        pc: &mut usize,
    ) -> VmResult {
        match op {
            Opcode::LdUnit => self.frames.last_mut().unwrap().push(Value::UNIT),
            Opcode::LdTru => self.frames.last_mut().unwrap().push(Value::TRUE),
            Opcode::LdFls => self.frames.last_mut().unwrap().push(Value::FALSE),
            Opcode::LdNil => self.frames.last_mut().unwrap().push(Value::ZERO),
            Opcode::LdOne => self.frames.last_mut().unwrap().push(Value::ONE),
            Opcode::LdSmi => {
                let v = Value::from_int(i64::from(self.read_i16(method_idx, pc)?));
                self.frames.last_mut().unwrap().push(v);
            }
            Opcode::LdLoc => {
                let idx = usize::from(self.read_u8(method_idx, pc)?);
                let v = self
                    .frames
                    .last_mut()
                    .ok_or(VmError::StackUnderflow)?
                    .load_local(idx)?;
                self.frames.last_mut().unwrap().push(v);
            }
            Opcode::StLoc => {
                let idx = usize::from(self.read_u8(method_idx, pc)?);
                let v = self
                    .frames
                    .last_mut()
                    .ok_or(VmError::StackUnderflow)?
                    .pop()?;
                self.frames.last_mut().unwrap().store_local(idx, v)?;
            }
            Opcode::LdLocW => {
                let idx = usize::from(self.read_u16(method_idx, pc)?);
                let v = self
                    .frames
                    .last_mut()
                    .ok_or(VmError::StackUnderflow)?
                    .load_local(idx)?;
                self.frames.last_mut().unwrap().push(v);
            }
            Opcode::StLocW => {
                let idx = usize::from(self.read_u16(method_idx, pc)?);
                let v = self
                    .frames
                    .last_mut()
                    .ok_or(VmError::StackUnderflow)?
                    .pop()?;
                self.frames.last_mut().unwrap().store_local(idx, v)?;
            }
            Opcode::LdConst => {
                let idx = usize::from(self.read_u16(method_idx, pc)?);
                let v = self
                    .resolved_constants
                    .get(idx)
                    .copied()
                    .ok_or(VmError::InvalidConstant(idx))?;
                self.frames.last_mut().unwrap().push(v);
            }
            Opcode::LdGlob | Opcode::StGlob | Opcode::LdUpv | Opcode::StUpv => {
                self.dispatch_global_upval(op, method_idx, pc)?;
            }
            Opcode::Pop => {
                let _ = self
                    .frames
                    .last_mut()
                    .ok_or(VmError::StackUnderflow)?
                    .pop()?;
            }
            Opcode::Dup => self
                .frames
                .last_mut()
                .ok_or(VmError::StackUnderflow)?
                .dup()?,
            Opcode::Swap => self
                .frames
                .last_mut()
                .ok_or(VmError::StackUnderflow)?
                .swap()?,
            Opcode::Rot => self
                .frames
                .last_mut()
                .ok_or(VmError::StackUnderflow)?
                .rot()?,
            op => {
                let frame = self.frames.last_mut().ok_or(VmError::StackUnderflow)?;
                exec_scalar_op(op, frame)?;
            }
        }
        Ok(())
    }

    fn dispatch_equality(&mut self, op: Opcode) -> VmResult {
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

    /// Structural equality for strings, bitwise equality for everything else.
    fn values_equal(&self, a: Value, b: Value) -> bool {
        if a == b {
            return true;
        }
        if !a.is_ptr() || !b.is_ptr() {
            return false;
        }
        let (Some(obj_a), Some(obj_b)) =
            (self.heap.get(a.as_ptr_idx()), self.heap.get(b.as_ptr_idx()))
        else {
            return false;
        };
        matches!(
            (obj_a, obj_b),
            (HeapObject::String(sa), HeapObject::String(sb)) if sa == sb
        )
    }

    fn dispatch_global_upval(&mut self, op: Opcode, method_idx: usize, pc: &mut usize) -> VmResult {
        match op {
            Opcode::LdGlob => {
                let idx = usize::from(self.read_u16(method_idx, pc)?);
                let v = self
                    .globals
                    .get(idx)
                    .copied()
                    .ok_or(VmError::InvalidGlobal(idx))?;
                self.frames.last_mut().unwrap().push(v);
            }
            Opcode::StGlob => {
                let idx = usize::from(self.read_u16(method_idx, pc)?);
                let v = self
                    .frames
                    .last_mut()
                    .ok_or(VmError::StackUnderflow)?
                    .pop()?;
                *self
                    .globals
                    .get_mut(idx)
                    .ok_or(VmError::InvalidGlobal(idx))? = v;
            }
            Opcode::LdUpv => {
                let slot = usize::from(self.read_u16(method_idx, pc)?);
                let closure_idx = self.current_closure_idx()?;
                let v = match self.heap.get(closure_idx) {
                    Some(HeapObject::Closure(cls)) => cls
                        .upvalues
                        .get(slot)
                        .copied()
                        .ok_or(VmError::InvalidUpvalue(slot))?,
                    _ => return Err(VmError::NoClosureContext),
                };
                self.frames.last_mut().unwrap().push(v);
            }
            Opcode::StUpv => {
                let slot = usize::from(self.read_u16(method_idx, pc)?);
                let v = self
                    .frames
                    .last_mut()
                    .ok_or(VmError::StackUnderflow)?
                    .pop()?;
                let closure_idx = self.current_closure_idx()?;
                match self.heap.get_mut(closure_idx) {
                    Some(HeapObject::Closure(cls)) => {
                        *cls.upvalues
                            .get_mut(slot)
                            .ok_or(VmError::InvalidUpvalue(slot))? = v;
                    }
                    _ => return Err(VmError::NoClosureContext),
                }
                self.write_barrier(closure_idx, v);
            }
            _ => return Err(VmError::UnsupportedOpcode(op)),
        }
        Ok(())
    }

    fn current_closure_idx(&self) -> VmResult<usize> {
        self.frames
            .last()
            .ok_or(VmError::NoClosureContext)?
            .closure
            .ok_or(VmError::NoClosureContext)
    }

    fn pop_call_args(&mut self, arity: usize) -> VmResult<(Value, Vec<Value>)> {
        let frame = self.frames.last_mut().ok_or(VmError::StackUnderflow)?;
        let mut args = Vec::with_capacity(arity);
        for _ in 0..arity {
            args.push(frame.pop()?);
        }
        args.reverse();
        let callee = frame.pop()?;
        Ok((callee, args))
    }

    fn resolve_callee(&self, callee: Value) -> VmResult<(usize, Option<usize>)> {
        if callee.is_ptr() {
            let cls_idx = callee.as_ptr_idx();
            let obj = self.heap.get(cls_idx).ok_or(VmError::NotCallable)?;
            let HeapObject::Closure(cls) = obj else {
                return Err(VmError::NotCallable);
            };
            let method_idx = usize::from(cls.method_idx);
            Ok((method_idx, Some(cls_idx)))
        } else if callee.is_int() {
            let m_idx = usize::try_from(callee.as_int()).map_err(|_| VmError::NotCallable)?;
            Ok((m_idx, None))
        } else {
            Err(VmError::NotCallable)
        }
    }

    fn make_call_frame(
        &self,
        method_idx: usize,
        return_pc: usize,
        closure: Option<usize>,
    ) -> VmResult<CallFrame> {
        let locals_count = usize::from(
            self.module
                .methods
                .get(method_idx)
                .ok_or(VmError::InvalidMethod(method_idx))?
                .locals_count,
        );
        Ok(CallFrame::new_call(
            locals_count,
            return_pc,
            u16::try_from(method_idx).map_err(|_| VmError::InvalidMethod(method_idx))?,
            closure,
        ))
    }

    fn dispatch_array(&mut self, op: Opcode, method_idx: usize, pc: &mut usize) -> VmResult {
        match op {
            Opcode::ArrGetI | Opcode::ArrSetI | Opcode::ArrGet | Opcode::ArrSet => {
                return self.dispatch_arr_rw(op, method_idx, pc);
            }
            Opcode::ArrNew => {
                let len = usize::from(self.read_u16(method_idx, pc)?);
                let heap_idx = self.heap.alloc_array(Value::UNIT, vec![Value::UNIT; len]);
                self.push_stack(Value::from_ptr(heap_idx))?;
                self.maybe_collect();
            }
            Opcode::ArrNewT => {
                let tag_pool_idx = usize::from(self.read_u8(method_idx, pc)?);
                let len = usize::from(self.read_u16(method_idx, pc)?);
                let tag = self
                    .resolved_constants
                    .get(tag_pool_idx)
                    .copied()
                    .ok_or(VmError::InvalidConstant(tag_pool_idx))?;
                let heap_idx = self.heap.alloc_array(tag, vec![Value::UNIT; len]);
                self.push_stack(Value::from_ptr(heap_idx))?;
                self.maybe_collect();
            }
            Opcode::ArrLen => {
                let ptr = self.pop_stack()?;
                if !ptr.is_ptr() {
                    return Err(VmError::NotAnArray);
                }
                let obj = self.heap.get(ptr.as_ptr_idx()).ok_or(VmError::NotAnArray)?;
                let len = match obj {
                    HeapObject::Array(arr) => arr.elements.len(),
                    HeapObject::String(s) => s.len(),
                    HeapObject::Slice(sl) => sl.end - sl.start,
                    _ => return Err(VmError::NotAnArray),
                };
                self.frames
                    .last_mut()
                    .unwrap()
                    .push(Value::from_int(i64::try_from(len).unwrap_or(i64::MAX)));
            }
            Opcode::ArrTag => {
                let ptr = self.pop_stack()?;
                if !ptr.is_ptr() {
                    return Err(VmError::NotAnArray);
                }
                let ptr_idx = ptr.as_ptr_idx();
                let tag = match self.heap.get(ptr_idx).ok_or(VmError::NotAnArray)? {
                    HeapObject::Array(arr) => arr.tag,
                    HeapObject::String(_) => {
                        let str_idx = self.heap.alloc_string("Str".into());
                        Value::from_ptr(str_idx)
                    }
                    HeapObject::Slice(_) => Value::UNIT,
                    _ => return Err(VmError::NotAnArray),
                };
                self.frames.last_mut().unwrap().push(tag);
            }
            Opcode::ArrCopy => {
                self.exec_arr_copy()?;
                self.maybe_collect();
            }
            Opcode::ArrCaten => {
                let b = self.pop_stack()?;
                let a = self.pop_stack()?;
                let result = exec_arr_concat(&mut self.heap, a, b)?;
                self.frames.last_mut().unwrap().push(result);
                self.maybe_collect();
            }
            _ => return Err(VmError::UnsupportedOpcode(op)),
        }
        Ok(())
    }

    fn dispatch_arr_rw(&mut self, op: Opcode, method_idx: usize, pc: &mut usize) -> VmResult {
        match op {
            Opcode::ArrGetI => {
                let elem_idx = usize::from(self.read_u8(method_idx, pc)?);
                let arr_ptr = self.pop_stack()?;
                let val = exec_arr_geti(&self.heap, arr_ptr, elem_idx)?;
                self.frames.last_mut().unwrap().push(val);
            }
            Opcode::ArrSetI => {
                let elem_idx = usize::from(self.read_u8(method_idx, pc)?);
                let val = self.pop_stack()?;
                // Array stays on TOS -- peek without popping
                let arr_ptr = self.peek_stack()?;
                exec_arr_seti(&mut self.heap, arr_ptr, elem_idx, val)?;
                if arr_ptr.is_ptr() {
                    self.write_barrier(arr_ptr.as_ptr_idx(), val);
                }
            }
            Opcode::ArrGet => {
                let idx_val = self.pop_stack()?;
                let arr_ptr = self.pop_stack()?;
                let val = exec_arr_get(&self.heap, arr_ptr, idx_val)?;
                self.frames.last_mut().unwrap().push(val);
            }
            Opcode::ArrSet => {
                let val = self.pop_stack()?;
                let idx_val = self.pop_stack()?;
                let arr_ptr = self.pop_stack()?;
                exec_arr_set(&mut self.heap, arr_ptr, idx_val, val)?;
                if arr_ptr.is_ptr() {
                    self.write_barrier(arr_ptr.as_ptr_idx(), val);
                }
            }
            _ => return Err(VmError::UnsupportedOpcode(op)),
        }
        Ok(())
    }

    fn dispatch_type_op(&mut self, op: Opcode, method_idx: usize, pc: &mut usize) -> VmResult {
        match op {
            Opcode::TyChk => {
                let type_id = self.read_u16(method_idx, pc)?;
                let val = self.pop_stack()?;
                let matches = value_matches_type(val, type_id);
                self.push_stack(Value::from_bool(matches))?;
            }
            Opcode::TyCast => {
                let type_id = self.read_u16(method_idx, pc)?;
                let val = self.pop_stack()?;
                if value_matches_type(val, type_id) {
                    self.push_stack(val)?;
                } else {
                    return Err(VmError::TypeCastFailed);
                }
            }
            Opcode::TyTag => {
                let val = self.pop_stack()?;
                self.push_stack(Value::from_int(i64::from(val.nan_tag())))?;
            }
            _ => return Err(VmError::UnsupportedOpcode(op)),
        }
        Ok(())
    }

    fn dispatch_tycl(&mut self, op: Opcode, method_idx: usize, pc: &mut usize) -> VmResult {
        match op {
            Opcode::TyclDict => {
                let class_id = self.read_u16(method_idx, pc)?;
                let type_id_val = self.pop_stack()?;
                let type_id = if type_id_val.is_int() {
                    u16::try_from(type_id_val.as_int()).unwrap_or(u16::MAX)
                } else {
                    u16::MAX
                };
                let class_idx = usize::from(class_id);
                let class = self
                    .module
                    .classes
                    .get(class_idx)
                    .ok_or(VmError::NoInstance { class_id, type_id })?;
                let inst_idx = class
                    .instances
                    .iter()
                    .position(|inst| inst.type_id == type_id)
                    .ok_or(VmError::NoInstance { class_id, type_id })?;
                let dict = Value::from_int(
                    (i64::from(class_id) << 16) | i64::from(u16::try_from(inst_idx).unwrap_or(0)),
                );
                self.push_stack(dict)?;
            }
            Opcode::TyclCall => {
                let method_idx_op = usize::from(self.read_u8(method_idx, pc)?);
                let dict = self.pop_stack()?;
                if !dict.is_int() {
                    return Err(VmError::InvalidDictionary);
                }
                let packed = dict.as_int();
                let cls_idx =
                    usize::try_from(packed >> 16).map_err(|_| VmError::InvalidDictionary)?;
                let inst_idx =
                    usize::try_from(packed & 0xFFFF).map_err(|_| VmError::InvalidDictionary)?;
                let class = self
                    .module
                    .classes
                    .get(cls_idx)
                    .ok_or(VmError::InvalidDictionary)?;
                let instance = class
                    .instances
                    .get(inst_idx)
                    .ok_or(VmError::InvalidDictionary)?;
                let cm = instance
                    .methods
                    .get(method_idx_op)
                    .ok_or(VmError::InvalidDictionary)?;
                let callee = Value::from_int(i64::from(cm.method_idx));
                self.push_stack(callee)?;
            }
            _ => return Err(VmError::UnsupportedOpcode(op)),
        }
        Ok(())
    }

    fn dispatch_ffi_call(&mut self, method_idx: usize, pc: &mut usize) -> VmResult {
        let ffi_idx = usize::from(self.read_u16(method_idx, pc)?);
        let foreign = self
            .module
            .foreigns
            .get(ffi_idx)
            .ok_or(VmError::FfiForeignIndexOutOfBounds(ffi_idx))?;

        let arity = usize::from(foreign.arity);
        let param_types = foreign.param_types.clone();
        let return_type = foreign.return_type;

        let symbol_name = if foreign.symbol_idx == u32::MAX {
            self.module
                .strings
                .get(usize::try_from(foreign.name_idx).unwrap_or(usize::MAX))
                .cloned()
                .unwrap_or_default()
        } else {
            self.module
                .strings
                .get(usize::try_from(foreign.symbol_idx).unwrap_or(usize::MAX))
                .cloned()
                .unwrap_or_default()
        };

        let lib_name = if foreign.lib_idx == u32::MAX {
            String::new()
        } else {
            self.module
                .strings
                .get(usize::try_from(foreign.lib_idx).unwrap_or(usize::MAX))
                .cloned()
                .unwrap_or_default()
        };

        self.ffi_runtime.load_library(&lib_name)?;
        let fn_ptr = self.ffi_runtime.resolve_symbol(&lib_name, &symbol_name)?;

        let mut args = Vec::with_capacity(arity);
        {
            let frame = self.frames.last_mut().ok_or(VmError::StackUnderflow)?;
            for _ in 0..arity {
                args.push(frame.pop()?);
            }
        }
        args.reverse();

        let result =
            ffi::execute_ffi_call(fn_ptr, &param_types, return_type, &mut args, &mut self.heap)?;
        self.push_stack(result)?;
        self.maybe_collect();
        Ok(())
    }

    fn exec_arr_slice(&mut self) -> VmResult {
        let end_val = self.pop_stack()?;
        let start_val = self.pop_stack()?;
        let arr_ptr = self.pop_stack()?;
        if !arr_ptr.is_ptr() {
            return Err(VmError::NotAnArray);
        }
        if !start_val.is_int() || !end_val.is_int() {
            return Err(VmError::TypeError {
                expected: "`Int`",
                found: "non-`Int`",
            });
        }
        let start = usize::try_from(start_val.as_int()).map_err(|_| VmError::IndexOutOfBounds {
            index: 0,
            length: 0,
        })?;
        let end = usize::try_from(end_val.as_int()).map_err(|_| VmError::IndexOutOfBounds {
            index: 0,
            length: 0,
        })?;
        let source = arr_ptr.as_ptr_idx();
        let arr_len = match self.heap.get(source) {
            Some(HeapObject::Array(arr)) => arr.elements.len(),
            _ => return Err(VmError::NotAnArray),
        };
        if start > end || end > arr_len {
            return Err(VmError::IndexOutOfBounds {
                index: end,
                length: arr_len,
            });
        }
        let slice_idx = self.heap.alloc_slice(source, start, end);
        self.push_stack(Value::from_ptr(slice_idx))?;
        Ok(())
    }

    fn exec_arr_copy(&mut self) -> VmResult {
        let ptr = self.pop_stack()?;
        if !ptr.is_ptr() {
            return Err(VmError::NotAnArray);
        }
        let ptr_idx = ptr.as_ptr_idx();
        // Extract the data we need before mutating the heap.
        let copy_data = match self.heap.get(ptr_idx).ok_or(VmError::NotAnArray)? {
            HeapObject::Array(arr) => CopyData::Array {
                tag: arr.tag,
                elements: arr.elements.clone(),
            },
            HeapObject::String(s) => CopyData::Str(s.clone()),
            HeapObject::Slice(sl) => CopyData::Slice {
                source: sl.source,
                start: sl.start,
                end: sl.end,
            },
            _ => return Err(VmError::NotAnArray),
        };
        let new_idx = match copy_data {
            CopyData::Array { tag, elements } => self.heap.alloc_array(tag, elements),
            CopyData::Str(s) => self.heap.alloc_string(s),
            CopyData::Slice { source, start, end } => {
                let elements = match self.heap.get(source) {
                    Some(HeapObject::Array(arr)) => arr.elements[start..end].to_vec(),
                    _ => return Err(VmError::NotAnArray),
                };
                self.heap.alloc_array(Value::UNIT, elements)
            }
        };
        self.push_stack(Value::from_ptr(new_idx))?;
        Ok(())
    }

    fn exec_arr_fill(&mut self) -> VmResult {
        let len_val = self.pop_stack()?;
        let value = self.pop_stack()?;
        if !len_val.is_int() {
            return Err(VmError::TypeError {
                expected: "`Int`",
                found: "non-`Int`",
            });
        }
        let len = usize::try_from(len_val.as_int()).map_err(|_| VmError::TypeError {
            expected: "non-negative `Int`",
            found: "negative `Int`",
        })?;
        let elements = vec![value; len];
        let heap_idx = self.heap.alloc_array(Value::UNIT, elements);
        self.push_stack(Value::from_ptr(heap_idx))?;
        self.maybe_collect();
        Ok(())
    }

    fn pop_stack(&mut self) -> VmResult<Value> {
        self.frames.last_mut().ok_or(VmError::StackUnderflow)?.pop()
    }

    fn push_stack(&mut self, v: Value) -> VmResult {
        self.frames
            .last_mut()
            .ok_or(VmError::StackUnderflow)?
            .push(v);
        Ok(())
    }

    fn peek_stack(&mut self) -> VmResult<Value> {
        self.frames
            .last_mut()
            .ok_or(VmError::StackUnderflow)?
            .peek()
    }

    fn read_u8(&self, method_idx: usize, pc: &mut usize) -> VmResult<u8> {
        let b = *self
            .module
            .methods
            .get(method_idx)
            .ok_or(VmError::InvalidMethod(method_idx))?
            .code
            .get(*pc)
            .ok_or(VmError::PcOutOfBounds)?;
        *pc = pc.wrapping_add(1);
        Ok(b)
    }

    fn read_u16(&self, method_idx: usize, pc: &mut usize) -> VmResult<u16> {
        let lo = self.read_u8(method_idx, pc)?;
        let hi = self.read_u8(method_idx, pc)?;
        Ok(u16::from_le_bytes([lo, hi]))
    }

    fn read_i16(&self, method_idx: usize, pc: &mut usize) -> VmResult<i16> {
        let lo = self.read_u8(method_idx, pc)?;
        let hi = self.read_u8(method_idx, pc)?;
        Ok(i16::from_le_bytes([lo, hi]))
    }

    fn dispatch_effect(&mut self, op: Opcode, method_idx: usize, pc: &mut usize) -> VmResult {
        match op {
            Opcode::EffPush => {
                let effect_id = self.read_u16(method_idx, pc)?;
                let skip_offset = self.read_i16(method_idx, pc)?;
                let handler_pc = *pc;
                let saved_stack_depth = self
                    .frames
                    .last()
                    .ok_or(VmError::StackUnderflow)?
                    .stack_depth();
                let handler_frame_depth = self.frames.len() - 1;
                self.effect_handlers.push(EffectHandler {
                    effect_id,
                    handler_frame_depth,
                    handler_pc,
                    saved_stack_depth,
                });
                *pc = pc.wrapping_add_signed(isize::from(skip_offset));
            }
            Opcode::EffPop => {
                let _ = self.effect_handlers.pop().ok_or(VmError::NoEffectHandler)?;
            }
            Opcode::EffNeed => {
                let effect_idx = self.read_u16(method_idx, pc)?;
                let handler_pos = self
                    .effect_handlers
                    .iter()
                    .rposition(|h| h.effect_id == effect_idx)
                    .ok_or(VmError::NoEffectHandler)?;
                let handler = self.effect_handlers.remove(handler_pos);
                let captured_handlers: Vec<EffectHandler> =
                    self.effect_handlers.drain(handler_pos..).collect();
                let cont_frames = self.frames[handler.handler_frame_depth + 1..].to_vec();
                let resume_pc = *pc;
                let cont_idx =
                    self.heap
                        .alloc_continuation(cont_frames, resume_pc, captured_handlers);
                self.frames.truncate(handler.handler_frame_depth + 1);
                let frame = self.frames.last_mut().ok_or(VmError::StackUnderflow)?;
                frame.truncate_stack(handler.saved_stack_depth);
                frame.push(Value::from_ptr(cont_idx));
                *pc = handler.handler_pc;
            }
            Opcode::EffCont => {
                let flag = self.read_u8(method_idx, pc)?;
                let frame = self.frames.last_mut().ok_or(VmError::StackUnderflow)?;
                let value = if flag != 0 { frame.pop()? } else { Value::UNIT };
                let cont_ptr = frame.pop()?;
                if !cont_ptr.is_ptr() {
                    return Err(VmError::TypeError {
                        expected: "Continuation",
                        found: "non-ptr",
                    });
                }
                let cont_idx = cont_ptr.as_ptr_idx();
                let obj = self.heap.get(cont_idx).ok_or(VmError::TypeError {
                    expected: "Continuation",
                    found: "non-continuation",
                })?;
                let HeapObject::Continuation(cont) = obj else {
                    return Err(VmError::TypeError {
                        expected: "Continuation",
                        found: "non-continuation",
                    });
                };
                let cont_frames = cont.frames.clone();
                let resume_pc = cont.resume_pc;
                let restored_handlers = cont.captured_handlers.clone();
                self.frames.extend(cont_frames);
                self.effect_handlers.extend(restored_handlers);
                *pc = resume_pc;
                self.frames
                    .last_mut()
                    .ok_or(VmError::StackUnderflow)?
                    .push(value);
            }
            _ => return Err(VmError::UnsupportedOpcode(op)),
        }
        Ok(())
    }

    // ── GC ────────────────────────────────────────────────────────────────────

    fn maybe_collect(&mut self) {
        if self.heap.should_collect() {
            if self.heap.should_major() {
                self.collect_major();
            } else {
                self.collect_minor();
            }
        }
    }

    fn collect_minor(&mut self) {
        let roots = self.gather_roots();
        self.heap.collect_minor(&roots);
        self.heap.reset_threshold();
    }

    fn collect_major(&mut self) {
        let roots = self.gather_roots();
        self.heap.collect_major(&roots);
        self.heap.reset_threshold();
    }

    fn gather_roots(&self) -> Vec<Value> {
        let mut roots = Vec::new();
        roots.extend_from_slice(&self.globals);
        roots.extend_from_slice(&self.resolved_constants);
        for frame in &self.frames {
            roots.extend(frame.locals_iter());
            roots.extend(frame.stack_iter());
            if let Some(idx) = frame.closure {
                roots.push(Value::from_ptr(idx));
            }
        }
        roots
    }

    fn write_barrier(&mut self, target_idx: usize, stored_value: Value) {
        if stored_value.is_ptr()
            && self.heap.is_marked(target_idx)
            && !self.heap.is_marked(stored_value.as_ptr_idx())
        {
            self.heap.remember(target_idx);
        }
    }
}

/// Intermediate data extracted from heap before mutating it (for `arr_copy`).
enum CopyData {
    Array {
        tag: Value,
        elements: Vec<Value>,
    },
    Str(String),
    Slice {
        source: usize,
        start: usize,
        end: usize,
    },
}

/// Format a value for display, resolving heap pointers to their contents.
#[must_use]
pub fn display_value(val: Value, heap: &Heap) -> String {
    if val.is_ptr() {
        if let Some(obj) = heap.get(val.as_ptr_idx()) {
            return match obj {
                HeapObject::String(s) => s.clone(),
                HeapObject::Array(arr) => format!("[Array; len={}]", arr.elements.len()),
                HeapObject::Slice(sl) => format!("[Slice; len={}]", sl.end - sl.start),
                HeapObject::Closure(_) => "<closure>".into(),
                HeapObject::Continuation(_) => "<continuation>".into(),
                HeapObject::CPtr(_) => "<cptr>".into(),
                HeapObject::Cell(v) => format!("<cell:{v:?}>"),
            };
        }
    }
    format!("{val:?}")
}

fn exec_arr_geti(heap: &Heap, arr_ptr: Value, elem_idx: usize) -> VmResult<Value> {
    if !arr_ptr.is_ptr() {
        return Err(VmError::NotAnArray);
    }
    let ptr_idx = arr_ptr.as_ptr_idx();
    let obj = heap
        .get(ptr_idx)
        .ok_or(VmError::InvalidHeapIndex(ptr_idx))?;
    match obj {
        HeapObject::Array(arr) => {
            let len = arr.elements.len();
            arr.elements
                .get(elem_idx)
                .copied()
                .ok_or(VmError::IndexOutOfBounds {
                    index: elem_idx,
                    length: len,
                })
        }
        HeapObject::String(s) => {
            let bytes = s.as_bytes();
            bytes
                .get(elem_idx)
                .map(|&b| Value::from_int(i64::from(b)))
                .ok_or(VmError::IndexOutOfBounds {
                    index: elem_idx,
                    length: bytes.len(),
                })
        }
        HeapObject::Slice(sl) => {
            let real_idx = sl.start + elem_idx;
            let len = sl.end - sl.start;
            if elem_idx >= len {
                return Err(VmError::IndexOutOfBounds {
                    index: elem_idx,
                    length: len,
                });
            }
            let source = sl.source;
            match heap.get(source) {
                Some(HeapObject::Array(arr)) => {
                    arr.elements
                        .get(real_idx)
                        .copied()
                        .ok_or(VmError::IndexOutOfBounds {
                            index: real_idx,
                            length: arr.elements.len(),
                        })
                }
                _ => Err(VmError::NotAnArray),
            }
        }
        _ => Err(VmError::NotAnArray),
    }
}

fn exec_arr_seti(heap: &mut Heap, arr_ptr: Value, elem_idx: usize, val: Value) -> VmResult {
    if !arr_ptr.is_ptr() {
        return Err(VmError::NotAnArray);
    }
    let ptr_idx = arr_ptr.as_ptr_idx();
    match heap
        .get_mut(ptr_idx)
        .ok_or(VmError::InvalidHeapIndex(ptr_idx))?
    {
        HeapObject::Array(arr) => {
            let len = arr.elements.len();
            *arr.elements
                .get_mut(elem_idx)
                .ok_or(VmError::IndexOutOfBounds {
                    index: elem_idx,
                    length: len,
                })? = val;
            Ok(())
        }
        HeapObject::String(_) => Err(VmError::TypeError {
            expected: "Array",
            found: "String",
        }),
        _ => Err(VmError::NotAnArray),
    }
}

fn exec_arr_get(heap: &Heap, arr_ptr: Value, idx_val: Value) -> VmResult<Value> {
    if !idx_val.is_int() {
        return Err(VmError::TypeError {
            expected: "`Int`",
            found: "non-`Int`",
        });
    }
    let raw_idx = idx_val.as_int();
    let elem_idx = usize::try_from(raw_idx).map_err(|_| VmError::IndexOutOfBounds {
        index: 0,
        length: 0,
    })?;
    exec_arr_geti(heap, arr_ptr, elem_idx)
}

fn exec_arr_set(heap: &mut Heap, arr_ptr: Value, idx_val: Value, val: Value) -> VmResult {
    if !idx_val.is_int() {
        return Err(VmError::TypeError {
            expected: "`Int`",
            found: "non-`Int`",
        });
    }
    let raw_idx = idx_val.as_int();
    let elem_idx = usize::try_from(raw_idx).map_err(|_| VmError::IndexOutOfBounds {
        index: 0,
        length: 0,
    })?;
    exec_arr_seti(heap, arr_ptr, elem_idx, val)
}

fn exec_arr_concat(heap: &mut Heap, a: Value, b: Value) -> VmResult<Value> {
    match (a.is_ptr(), b.is_ptr()) {
        (true, true) => {
            let a_idx = a.as_ptr_idx();
            let b_idx = b.as_ptr_idx();
            // Extract data from both objects before mutating heap.
            let concat_data = match (
                heap.get(a_idx).ok_or(VmError::NotAnArray)?,
                heap.get(b_idx).ok_or(VmError::NotAnArray)?,
            ) {
                (HeapObject::String(sa), HeapObject::String(sb)) => {
                    let mut result = sa.clone();
                    result.push_str(sb);
                    ConcatData::Str(result)
                }
                (HeapObject::Array(arr_a), HeapObject::Array(arr_b)) => {
                    let tag = arr_a.tag;
                    let mut elems = arr_a.elements.clone();
                    elems.extend_from_slice(&arr_b.elements);
                    ConcatData::Array { tag, elems }
                }
                _ => {
                    return Err(VmError::TypeError {
                        expected: "matching types",
                        found: "mismatched",
                    });
                }
            };
            match concat_data {
                ConcatData::Str(s) => Ok(Value::from_ptr(heap.alloc_string(s))),
                ConcatData::Array { tag, elems } => {
                    Ok(Value::from_ptr(heap.alloc_array(tag, elems)))
                }
            }
        }
        (true, false) => {
            let a_idx = a.as_ptr_idx();
            let (tag, mut elems) = match heap.get(a_idx).ok_or(VmError::NotAnArray)? {
                HeapObject::Array(arr) => (arr.tag, arr.elements.clone()),
                _ => return Err(VmError::NotAnArray),
            };
            elems.push(b);
            Ok(Value::from_ptr(heap.alloc_array(tag, elems)))
        }
        (false, true) => {
            let b_idx = b.as_ptr_idx();
            let (tag, tail) = match heap.get(b_idx).ok_or(VmError::NotAnArray)? {
                HeapObject::Array(arr) => (arr.tag, arr.elements.clone()),
                _ => return Err(VmError::NotAnArray),
            };
            let mut elems = vec![a];
            elems.extend_from_slice(&tail);
            Ok(Value::from_ptr(heap.alloc_array(tag, elems)))
        }
        (false, false) => Ok(Value::UNIT),
    }
}

enum ConcatData {
    Str(String),
    Array { tag: Value, elems: Vec<Value> },
}

/// Handles scalar arithmetic, bitwise, and comparison opcodes - pure stack
/// operations with no operand bytes and no PC mutation.
fn exec_scalar_op(op: Opcode, frame: &mut CallFrame) -> VmResult {
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

/// Check whether a NaN-boxed value matches a builtin type ID.
const fn value_matches_type(val: Value, type_id: u16) -> bool {
    match type_id {
        format::BUILTIN_TYPE_INT => val.is_int(),
        format::BUILTIN_TYPE_BOOL => val.is_bool(),
        format::BUILTIN_TYPE_UNIT => val.is_unit(),
        format::BUILTIN_TYPE_FLOAT => val.is_float(),
        format::BUILTIN_TYPE_STRING => val.is_ptr(),
        _ => false,
    }
}

#[cfg(test)]
mod tests;
