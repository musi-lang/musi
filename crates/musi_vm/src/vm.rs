#![allow(clippy::arithmetic_side_effects, clippy::as_conversions)]

use music_il::opcode::Opcode;

use crate::errors::VmError;
use crate::frame::CallFrame;
use crate::heap::Heap;
use crate::module::Module;
use crate::value::Value;

const MAX_CALL_DEPTH: usize = 1024;

struct EffectHandler {
    /// Index into `self.frames` for the frame that called `handle`.
    handler_frame_depth: usize,
    /// Byte offset in the method's code where the handler body starts.
    handler_pc: usize,
    /// Operand stack depth of the handler frame at the point `EffPush` executed.
    saved_stack_depth: usize,
}

pub struct Vm {
    module: Module,
    globals: Vec<Value>,
    heap: Heap,
    frames: Vec<CallFrame>,
    effect_handlers: Vec<EffectHandler>,
}

impl Vm {
    #[must_use]
    pub fn new(module: Module) -> Self {
        let globals_count = module.globals.len();
        Self {
            module,
            globals: vec![Value::UNIT; globals_count],
            heap: Heap::new(),
            frames: Vec::new(),
            effect_handlers: Vec::new(),
        }
    }

    /// # Errors
    /// Returns a [`VmError`] if no entry point exists, the bytecode is invalid,
    /// a runtime type error occurs, or `Panic` is executed.
    pub fn run(&mut self) -> Result<Value, VmError> {
        let entry_idx = self
            .module
            .methods
            .iter()
            .position(|m| m.name == u32::MAX)
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

    fn execute(&mut self) -> Result<Value, VmError> {
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

                Opcode::BrTrue | Opcode::BrFalse | Opcode::BrJmp | Opcode::BrBack => {
                    self.dispatch_branch(op, method_idx, &mut pc)?;
                }

                Opcode::Ret | Opcode::Call | Opcode::CallTail | Opcode::ClsNew => {
                    if let Some(ret) = self.dispatch_call(op, method_idx, &mut pc)? {
                        return Ok(ret);
                    }
                }

                Opcode::EffPush | Opcode::EffPop | Opcode::EffNeed | Opcode::EffResume => {
                    self.dispatch_effect(op, method_idx, &mut pc)?;
                }

                _ => self.dispatch_load_store_stack(op, method_idx, &mut pc)?,
            }
        }
    }

    fn dispatch_branch(
        &mut self,
        op: Opcode,
        method_idx: usize,
        pc: &mut usize,
    ) -> Result<(), VmError> {
        match op {
            Opcode::BrTrue => {
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
                if cond.as_bool() {
                    *pc = pc.wrapping_add_signed(isize::from(offset));
                }
            }
            Opcode::BrFalse => {
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
                if !cond.as_bool() {
                    *pc = pc.wrapping_add_signed(isize::from(offset));
                }
            }
            Opcode::BrJmp | Opcode::BrBack => {
                let offset = self.read_i16(method_idx, pc)?;
                *pc = pc.wrapping_add_signed(isize::from(offset));
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
    ) -> Result<Option<Value>, VmError> {
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
    ) -> Result<(), VmError> {
        match op {
            Opcode::LdUnit => self.frames.last_mut().unwrap().push(Value::UNIT),
            Opcode::LdTrue => self.frames.last_mut().unwrap().push(Value::TRUE),
            Opcode::LdFalse => self.frames.last_mut().unwrap().push(Value::FALSE),
            Opcode::LdZero => self.frames.last_mut().unwrap().push(Value::ZERO),
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
            Opcode::LdCst => {
                let idx = usize::from(self.read_u16(method_idx, pc)?);
                let v = self
                    .module
                    .constants
                    .get(idx)
                    .copied()
                    .ok_or(VmError::InvalidConstant(idx))?;
                self.frames.last_mut().unwrap().push(v);
            }
            Opcode::LdGlb | Opcode::StGlb | Opcode::LdUpv | Opcode::StUpv => {
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

    fn dispatch_global_upval(
        &mut self,
        op: Opcode,
        method_idx: usize,
        pc: &mut usize,
    ) -> Result<(), VmError> {
        match op {
            Opcode::LdGlb => {
                let idx = usize::from(self.read_u16(method_idx, pc)?);
                let v = self
                    .globals
                    .get(idx)
                    .copied()
                    .ok_or(VmError::InvalidGlobal(idx))?;
                self.frames.last_mut().unwrap().push(v);
            }
            Opcode::StGlb => {
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
                let closure_idx = self
                    .frames
                    .last()
                    .ok_or(VmError::NoClosureContext)?
                    .closure
                    .ok_or(VmError::NoClosureContext)?;
                let cls = self
                    .heap
                    .get_closure(closure_idx)
                    .ok_or(VmError::InvalidUpvalue(slot))?;
                let borrow = cls.borrow();
                let v = *borrow
                    .upvalues
                    .get(slot)
                    .ok_or(VmError::InvalidUpvalue(slot))?;
                drop(borrow);
                self.frames.last_mut().unwrap().push(v);
            }
            Opcode::StUpv => {
                let slot = usize::from(self.read_u16(method_idx, pc)?);
                let v = self
                    .frames
                    .last_mut()
                    .ok_or(VmError::StackUnderflow)?
                    .pop()?;
                let closure_idx = self
                    .frames
                    .last()
                    .ok_or(VmError::NoClosureContext)?
                    .closure
                    .ok_or(VmError::NoClosureContext)?;
                let cls = self
                    .heap
                    .get_closure(closure_idx)
                    .ok_or(VmError::InvalidUpvalue(slot))?;
                let mut borrow = cls.borrow_mut();
                *borrow
                    .upvalues
                    .get_mut(slot)
                    .ok_or(VmError::InvalidUpvalue(slot))? = v;
            }
            _ => return Err(VmError::UnsupportedOpcode(op)),
        }
        Ok(())
    }

    fn pop_call_args(&mut self, arity: usize) -> Result<(Value, Vec<Value>), VmError> {
        let frame = self.frames.last_mut().ok_or(VmError::StackUnderflow)?;
        let mut args = Vec::with_capacity(arity);
        for _ in 0..arity {
            args.push(frame.pop()?);
        }
        args.reverse();
        let callee = frame.pop()?;
        Ok((callee, args))
    }

    fn resolve_callee(&self, callee: Value) -> Result<(usize, Option<usize>), VmError> {
        if callee.is_ptr() {
            let cls_idx = callee.as_ptr_idx();
            let method_idx = usize::from(
                self.heap
                    .get_closure(cls_idx)
                    .ok_or(VmError::NotCallable)?
                    .borrow()
                    .method_idx,
            );
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
    ) -> Result<CallFrame, VmError> {
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

    fn read_u8(&self, method_idx: usize, pc: &mut usize) -> Result<u8, VmError> {
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

    fn read_u16(&self, method_idx: usize, pc: &mut usize) -> Result<u16, VmError> {
        let lo = self.read_u8(method_idx, pc)?;
        let hi = self.read_u8(method_idx, pc)?;
        Ok(u16::from_le_bytes([lo, hi]))
    }

    fn read_i16(&self, method_idx: usize, pc: &mut usize) -> Result<i16, VmError> {
        let lo = self.read_u8(method_idx, pc)?;
        let hi = self.read_u8(method_idx, pc)?;
        Ok(i16::from_le_bytes([lo, hi]))
    }

    fn dispatch_effect(
        &mut self,
        op: Opcode,
        method_idx: usize,
        pc: &mut usize,
    ) -> Result<(), VmError> {
        match op {
            Opcode::EffPush => {
                let skip_offset = self.read_i16(method_idx, pc)?;
                let handler_pc = *pc;
                let saved_stack_depth = self
                    .frames
                    .last()
                    .ok_or(VmError::StackUnderflow)?
                    .stack_depth();
                let handler_frame_depth = self.frames.len() - 1;
                self.effect_handlers.push(EffectHandler {
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
                let _effect_idx = self.read_u16(method_idx, pc)?;
                let handler = self.effect_handlers.pop().ok_or(VmError::NoEffectHandler)?;
                let cont_frames = self.frames[handler.handler_frame_depth + 1..].to_vec();
                let resume_pc = *pc;
                let cont_idx = self.heap.alloc_continuation(cont_frames, resume_pc);
                self.frames.truncate(handler.handler_frame_depth + 1);
                let frame = self.frames.last_mut().ok_or(VmError::StackUnderflow)?;
                frame.truncate_stack(handler.saved_stack_depth);
                frame.push(Value::from_ptr(cont_idx));
                *pc = handler.handler_pc;
            }
            Opcode::EffResume => {
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
                let (cont_frames, resume_pc) = {
                    let cont = self
                        .heap
                        .get_continuation(cont_idx)
                        .ok_or(VmError::TypeError {
                            expected: "Continuation",
                            found: "non-continuation",
                        })?;
                    (cont.frames.clone(), cont.resume_pc)
                };
                self.frames.extend(cont_frames);
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
}

/// Handles scalar arithmetic, bitwise, and comparison opcodes - pure stack
/// operations with no operand bytes and no PC mutation.
fn exec_scalar_op(op: Opcode, frame: &mut CallFrame) -> Result<(), VmError> {
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
        Opcode::IMod => {
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
        Opcode::CmpEq => {
            let b = frame.pop()?;
            let a = frame.pop()?;
            frame.push(Value::from_bool(a == b));
        }
        Opcode::CmpNeq => {
            let b = frame.pop()?;
            let a = frame.pop()?;
            frame.push(Value::from_bool(a != b));
        }
        Opcode::CmpLt => apply_cmp(frame, i64::lt, f64::lt)?,
        Opcode::CmpGt => apply_cmp(frame, i64::gt, f64::gt)?,
        Opcode::CmpLeq => apply_cmp(frame, i64::le, f64::le)?,
        Opcode::CmpGeq => apply_cmp(frame, i64::ge, f64::ge)?,
        op => return Err(VmError::UnsupportedOpcode(op)),
    }
    Ok(())
}

fn pop_int(frame: &mut CallFrame) -> Result<i64, VmError> {
    let v = frame.pop()?;
    if !v.is_int() {
        return Err(VmError::TypeError {
            expected: "`Int`",
            found: "non-`Int`",
        });
    }
    Ok(v.as_int())
}

fn pop_float(frame: &mut CallFrame) -> Result<f64, VmError> {
    let v = frame.pop()?;
    if !v.is_float() {
        return Err(VmError::TypeError {
            expected: "`Float`",
            found: "non-`Float`",
        });
    }
    Ok(v.as_float())
}

fn pop_two_ints(frame: &mut CallFrame) -> Result<(i64, i64), VmError> {
    let b = pop_int(frame)?;
    let a = pop_int(frame)?;
    Ok((a, b))
}

fn pop_two_floats(frame: &mut CallFrame) -> Result<(f64, f64), VmError> {
    let b = pop_float(frame)?;
    let a = pop_float(frame)?;
    Ok((a, b))
}

fn apply_int_binop(frame: &mut CallFrame, f: impl Fn(i64, i64) -> i64) -> Result<(), VmError> {
    let (a, b) = pop_two_ints(frame)?;
    frame.push(Value::from_int(f(a, b)));
    Ok(())
}

fn apply_float_binop(frame: &mut CallFrame, f: impl Fn(f64, f64) -> f64) -> Result<(), VmError> {
    let (a, b) = pop_two_floats(frame)?;
    frame.push(Value::from_float(f(a, b)));
    Ok(())
}

fn apply_cmp(
    frame: &mut CallFrame,
    int_cmp: impl Fn(&i64, &i64) -> bool,
    float_cmp: impl Fn(&f64, &f64) -> bool,
) -> Result<(), VmError> {
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

#[cfg(test)]
mod tests;
