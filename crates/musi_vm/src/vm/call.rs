use super::scalar::exec_scalar_op;
use super::*;

impl Vm {
    pub(super) fn dispatch_call(
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
                let arity = usize::from(self.read_u8(method_idx, pc));
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
                let arity = usize::from(self.read_u8(method_idx, pc));
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
                let cls_method_idx = self.read_u16(method_idx, pc);
                let upval_count = usize::from(self.read_u8(method_idx, pc));
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

    pub(super) fn dispatch_load_store_stack(
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
                let v = Value::from_int(i64::from(self.read_i16(method_idx, pc)));
                self.frames.last_mut().unwrap().push(v);
            }
            Opcode::LdLoc => {
                let idx = usize::from(self.read_u8(method_idx, pc));
                let frame = self.frames.last_mut().ok_or(VmError::StackUnderflow)?;
                // SAFETY: the emitter counts locals correctly; idx < locals.len()
                let v = unsafe { frame.load_local_unchecked(idx) };
                frame.push(v);
            }
            Opcode::StLoc => {
                let idx = usize::from(self.read_u8(method_idx, pc));
                let frame = self.frames.last_mut().ok_or(VmError::StackUnderflow)?;
                let v = frame.pop()?;
                // SAFETY: the emitter counts locals correctly; idx < locals.len()
                unsafe { frame.store_local_unchecked(idx, v) };
            }
            Opcode::LdLocW => {
                let idx = usize::from(self.read_u16(method_idx, pc));
                let frame = self.frames.last_mut().ok_or(VmError::StackUnderflow)?;
                // SAFETY: the emitter counts locals correctly; idx < locals.len()
                let v = unsafe { frame.load_local_unchecked(idx) };
                frame.push(v);
            }
            Opcode::StLocW => {
                let idx = usize::from(self.read_u16(method_idx, pc));
                let frame = self.frames.last_mut().ok_or(VmError::StackUnderflow)?;
                let v = frame.pop()?;
                // SAFETY: the emitter counts locals correctly; idx < locals.len()
                unsafe { frame.store_local_unchecked(idx, v) };
            }
            Opcode::LdConst => {
                let idx = usize::from(self.read_u16(method_idx, pc));
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

    pub(super) fn dispatch_global_upval(
        &mut self,
        op: Opcode,
        method_idx: usize,
        pc: &mut usize,
    ) -> VmResult {
        match op {
            Opcode::LdGlob => {
                let idx = usize::from(self.read_u16(method_idx, pc));
                let v = self
                    .globals
                    .get(idx)
                    .copied()
                    .ok_or(VmError::InvalidGlobal(idx))?;
                self.frames.last_mut().unwrap().push(v);
            }
            Opcode::StGlob => {
                let idx = usize::from(self.read_u16(method_idx, pc));
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
                let slot = usize::from(self.read_u16(method_idx, pc));
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
                let slot = usize::from(self.read_u16(method_idx, pc));
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

    pub(super) fn current_closure_idx(&self) -> VmResult<usize> {
        self.frames
            .last()
            .ok_or(VmError::NoClosureContext)?
            .closure
            .ok_or(VmError::NoClosureContext)
    }

    pub(super) fn pop_call_args(&mut self, arity: usize) -> VmResult<(Value, Vec<Value>)> {
        let frame = self.frames.last_mut().ok_or(VmError::StackUnderflow)?;
        let mut args = Vec::with_capacity(arity);
        for _ in 0..arity {
            args.push(frame.pop()?);
        }
        args.reverse();
        let callee = frame.pop()?;
        Ok((callee, args))
    }

    pub(super) fn resolve_callee(&self, callee: Value) -> VmResult<(usize, Option<usize>)> {
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

    pub(super) fn make_call_frame(
        &self,
        method_idx: usize,
        return_pc: usize,
        closure: Option<usize>,
    ) -> VmResult<CallFrame> {
        let locals_count = usize::from(
            self.program
                .module()
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

    pub(super) fn pop_stack(&mut self) -> VmResult<Value> {
        self.frames.last_mut().ok_or(VmError::StackUnderflow)?.pop()
    }

    pub(super) fn push_stack(&mut self, v: Value) -> VmResult {
        self.frames
            .last_mut()
            .ok_or(VmError::StackUnderflow)?
            .push(v);
        Ok(())
    }

    pub(super) fn peek_stack(&mut self) -> VmResult<Value> {
        self.frames
            .last_mut()
            .ok_or(VmError::StackUnderflow)?
            .peek()
    }

    pub(super) fn read_u8(&self, method_idx: usize, pc: &mut usize) -> u8 {
        // SAFETY: method_idx and pc are maintained in bounds by the emitter
        let code = unsafe { &self.program.module().methods.get_unchecked(method_idx).code };
        let b = unsafe { *code.get_unchecked(*pc) };
        *pc = pc.wrapping_add(1);
        b
    }

    /// Read a little-endian u16 from bytecode in a single pass.
    pub(super) fn read_u16(&self, method_idx: usize, pc: &mut usize) -> u16 {
        // SAFETY: method_idx and pc are maintained in bounds by the emitter;
        // the emitter always emits complete 2-byte operands.
        let code = unsafe { &self.program.module().methods.get_unchecked(method_idx).code };
        let lo = u16::from(unsafe { *code.get_unchecked(*pc) });
        let hi = u16::from(unsafe { *code.get_unchecked(*pc + 1) });
        *pc += 2;
        lo | (hi << 8)
    }

    pub(super) fn read_i16(&self, method_idx: usize, pc: &mut usize) -> i16 {
        let raw = self.read_u16(method_idx, pc);
        let [lo, hi] = raw.to_le_bytes();
        i16::from_le_bytes([lo, hi])
    }
}
