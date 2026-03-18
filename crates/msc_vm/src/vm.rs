//! Interpreter core: `Vm` struct and the main run loop.

#[cfg(test)]
mod tests;

mod arith;
mod call;
mod continuations;
mod frame;
mod gc;
mod ops;

pub use frame::{ContMarker, Continuation, Frame};

use std::collections::HashMap;

use msc_bc::Opcode;

use crate::error::{VmError, malformed};
use crate::heap::Heap;
use crate::host::HostFunctions;
use crate::loader::LoadedModule;
use crate::value::Value;

const MAX_CALL_DEPTH: usize = 1024;

/// The result of executing a single instruction.
pub enum StepResult {
    /// Execution continues normally.
    Continue,
    /// The VM has returned a final value (call stack is empty).
    Returned(Value),
}

/// The Musi bytecode interpreter.
pub struct Vm {
    module: LoadedModule,
    heap: Heap,
    globals: Vec<Value>,
    call_stack: Vec<Frame>,
    instruction_count: u64,
    instruction_limit: Option<u64>,
    continuations: Vec<Continuation>,
    host: Option<Box<dyn HostFunctions>>,
    /// Maps `(frame_depth, local_slot)` to the heap index of the open upvalue cell.
    open_upvalue_map: HashMap<(usize, usize), usize>,
}

impl Vm {
    /// Create a new VM owning the loaded module.
    ///
    /// Globals are pre-allocated to `Value::UNIT`. Call `init_globals()` to
    /// run any initializer functions declared in the GLOB section.
    #[must_use]
    pub fn new(module: LoadedModule) -> Self {
        let globals = vec![Value::UNIT; module.globals.len()];
        Self {
            module,
            heap: Heap::new(),
            globals,
            call_stack: vec![],
            instruction_count: 0,
            instruction_limit: None,
            continuations: vec![],
            host: None,
            open_upvalue_map: HashMap::new(),
        }
    }

    /// Set the maximum number of instructions the VM will execute before
    /// returning `InstructionLimitExceeded`. Pass `None` to disable.
    pub const fn set_instruction_limit(&mut self, limit: Option<u64>) {
        self.instruction_limit = limit;
    }

    /// Attach a host function provider for `FFI_CALL` dispatch.
    pub fn set_host(&mut self, host: Box<dyn HostFunctions>) {
        self.host = Some(host);
    }

    /// Number of instructions executed since the VM was created.
    #[must_use]
    pub const fn instruction_count(&self) -> u64 {
        self.instruction_count
    }

    #[must_use]
    pub fn frames(&self) -> &[Frame] {
        &self.call_stack
    }

    #[must_use]
    pub const fn heap(&self) -> &Heap {
        &self.heap
    }

    #[must_use]
    pub fn globals(&self) -> &[Value] {
        &self.globals
    }

    #[must_use]
    pub const fn module(&self) -> &LoadedModule {
        &self.module
    }

    #[must_use]
    pub const fn is_running(&self) -> bool {
        !self.call_stack.is_empty()
    }

    /// Initialize globals that have initializer functions (flag bit 0x04).
    ///
    /// Must be called after `new()` and before `run()` for modules that use
    /// global variables with initializers.
    ///
    /// # Errors
    ///
    /// Returns `VmError` if any initializer function fails.
    pub fn init_globals(&mut self) -> Result<(), VmError> {
        for i in 0..self.module.globals.len() {
            let (flags, init_fn_id) = {
                let g = &self.module.globals[i];
                (g.flags, g.initializer)
            };
            if flags & 0x04 != 0 {
                if let Some(fn_id) = init_fn_id {
                    let fn_idx = usize::from(fn_id);
                    self.push_frame(fn_idx, &[])?;
                    let val = self.run_to_completion()?;
                    self.globals[i] = val;
                }
            }
        }
        Ok(())
    }

    /// Push a call frame for `fn_idx` with the given arguments without executing.
    ///
    /// # Errors
    ///
    /// Returns `VmError` if `fn_idx` is out of bounds or the call stack overflows.
    fn push_frame(&mut self, fn_idx: usize, args: &[Value]) -> Result<(), VmError> {
        if self.call_stack.len() >= MAX_CALL_DEPTH {
            return Err(VmError::StackOverflow);
        }
        let func = self
            .module
            .fn_by_index(fn_idx)
            .ok_or_else(|| malformed!("fn_idx {} not found in module", fn_idx))?;
        let local_count = usize::from(func.param_count) + usize::from(func.local_count);
        let max_stack = usize::from(func.max_stack);
        let param_count = usize::from(func.param_count);
        let mut locals = vec![Value::UNIT; local_count];
        for (i, &arg) in args.iter().enumerate().take(param_count) {
            locals[i] = arg;
        }
        self.call_stack.push(Frame {
            fn_idx,
            ip: 0,
            locals,
            stack: Vec::with_capacity(max_stack.max(4)),
            marker_stack: vec![],
            closure_ref: None,
            open_upvalues: vec![],
        });
        Ok(())
    }

    /// Run the module's entry point and return the result value.
    ///
    /// # Errors
    ///
    /// Returns `VmError` on type errors, stack overflows, malformed bytecode,
    /// or unimplemented features.
    pub fn run(&mut self) -> Result<Value, VmError> {
        let entry = self
            .module
            .entry_point
            .ok_or_else(|| malformed!("module has no entry point"))?;
        self.call_fn(entry, &[])
    }

    /// Call a function by `fn_id`, passing `args`, and return its result.
    ///
    /// # Errors
    ///
    /// Returns `VmError` on type errors, stack overflows, malformed bytecode,
    /// or unimplemented features.
    pub fn call_fn(&mut self, fn_id: u32, args: &[Value]) -> Result<Value, VmError> {
        self.setup_call(fn_id, args)?;
        self.run_to_completion()
    }

    /// Push a call frame for the given function without executing it.
    ///
    /// # Errors
    ///
    /// Returns `VmError` if `fn_id` is not found or the call stack overflows.
    pub fn setup_call(&mut self, fn_id: u32, args: &[Value]) -> Result<(), VmError> {
        self.push_frame(usize::try_from(fn_id).unwrap_or(usize::MAX), args)
    }

    /// Run until the call stack returns below its current depth.
    ///
    /// # Errors
    ///
    /// Returns `VmError` on any runtime error.
    pub fn run_to_completion(&mut self) -> Result<Value, VmError> {
        loop {
            match self.step()? {
                StepResult::Continue => {}
                StepResult::Returned(v) => return Ok(v),
            }
        }
    }

    /// Execute a single instruction and return the result.
    ///
    /// # Errors
    ///
    /// Returns `VmError` on any runtime error.
    pub fn step(&mut self) -> Result<StepResult, VmError> {
        if let Some(limit) = self.instruction_limit
            && self.instruction_count >= limit
        {
            return Err(VmError::InstructionLimitExceeded { limit });
        }
        self.instruction_count += 1;

        let (fn_idx, instr_ip) = {
            let frame = self
                .call_stack
                .last()
                .ok_or_else(|| malformed!("empty call stack"))?;
            (frame.fn_idx, frame.ip)
        };

        self.step_inner().map_err(|e| {
            if let Some(frame) = self.call_stack.last() {
                let code = &self.module.functions[frame.fn_idx].code;
                let disasm = msc_bc::disassemble(code);
                eprintln!("[VM ERR] fn_idx={fn_idx} ip={instr_ip}\n{disasm}",);
            }
            VmError::Runtime {
                fn_id: u32::try_from(fn_idx).unwrap_or(u32::MAX),
                ip: instr_ip,
                source: Box::new(e),
            }
        })
    }

    #[expect(
        clippy::too_many_lines,
        reason = "flat opcode dispatch table - splitting would obscure control flow"
    )]
    fn step_inner(&mut self) -> Result<StepResult, VmError> {
        self.maybe_gc();

        // Phase 1: decode instruction, advance IP.
        let (fn_idx, op, operand) = {
            let frame = self
                .call_stack
                .last_mut()
                .ok_or_else(|| malformed!("empty call stack"))?;
            let fn_idx = frame.fn_idx;
            let ip = frame.ip;
            let code = &self.module.functions[fn_idx].code;
            if ip >= code.len() {
                return Err(malformed!("ip past end of bytecode"));
            }
            let decoded = ops::decode_instruction(code, ip);
            frame.ip = ip + decoded.total_len;
            (fn_idx, decoded.op, decoded.operand)
        };

        // Phase 2: arithmetic (most frequent in hot loops).
        {
            let frame = self
                .call_stack
                .last_mut()
                .ok_or_else(|| malformed!("empty call stack"))?;
            if arith::exec(op, frame, &mut self.heap)? {
                return Ok(StepResult::Continue);
            }
        }

        // Phase 3: flat dispatch.
        match op {
            // §4.20 Misc
            Opcode::PANIC => Err(VmError::Halted),

            // §4.2 Stack
            Opcode::DUP => {
                self.current_frame()?.dup()?;
                Ok(StepResult::Continue)
            }
            Opcode::POP => {
                let _ = self.current_frame()?.stack.pop();
                Ok(StepResult::Continue)
            }
            Opcode::SWAP => {
                self.current_frame()?.swp()?;
                Ok(StepResult::Continue)
            }

            // §4.1 Data movement - immediates
            Opcode::LD_UNIT => {
                self.current_frame()?.stack.push(Value::UNIT);
                Ok(StepResult::Continue)
            }
            Opcode::LD_TRUE => {
                self.current_frame()?.stack.push(Value::TRUE);
                Ok(StepResult::Continue)
            }
            Opcode::LD_FALSE => {
                self.current_frame()?.stack.push(Value::FALSE);
                Ok(StepResult::Continue)
            }
            Opcode::LD_NONE => {
                // Push a None optional (represented as empty record tag=0).
                let frame = self
                    .call_stack
                    .last_mut()
                    .ok_or_else(|| malformed!("empty call stack"))?;
                ops::exec_opt_none(frame, &mut self.heap);
                Ok(StepResult::Continue)
            }
            Opcode::LD_SMI => {
                // FI16: signed 16-bit small immediate integer.
                let signed = (u16::try_from(operand & 0xFFFF).unwrap_or(u16::MAX)).cast_signed();
                self.current_frame()?
                    .stack
                    .push(Value::from_int(i64::from(signed)));
                Ok(StepResult::Continue)
            }

            // §4.1 Data movement - locals
            Opcode::LD_LOC => {
                let frame = self.current_frame()?;
                let slot =
                    usize::try_from(operand).map_err(|_| malformed!("ld.loc operand overflow"))?;
                let v = frame.get_local(slot)?;
                frame.stack.push(v);
                Ok(StepResult::Continue)
            }
            Opcode::ST_LOC => {
                let frame = self.current_frame()?;
                let slot =
                    usize::try_from(operand).map_err(|_| malformed!("st.loc operand overflow"))?;
                let v = frame.pop()?;
                frame.set_local(slot, v)?;
                Ok(StepResult::Continue)
            }

            // §4.1 Data movement - const pool
            Opcode::LD_CONST => {
                let frame = self
                    .call_stack
                    .last_mut()
                    .ok_or_else(|| malformed!("empty call stack"))?;
                ops::exec_ld_const(operand, frame, &self.module.consts, &mut self.heap)?;
                Ok(StepResult::Continue)
            }

            // §4.1 Data movement - indirect memory
            Opcode::LD_ADDR | Opcode::REC_ADDR => {
                self.current_frame()?
                    .stack
                    .push(Value::from_int(i64::from(operand)));
                Ok(StepResult::Continue)
            }
            Opcode::LD_IND => {
                let frame = self
                    .call_stack
                    .last_mut()
                    .ok_or_else(|| malformed!("empty call stack"))?;
                ops::exec_ld_ind(frame, &self.heap)?;
                Ok(StepResult::Continue)
            }
            Opcode::ST_IND => {
                let frame = self
                    .call_stack
                    .last_mut()
                    .ok_or_else(|| malformed!("empty call stack"))?;
                ops::exec_st_ind(frame, &mut self.heap)?;
                Ok(StepResult::Continue)
            }

            // §4.9 Record
            Opcode::REC_NEW => {
                let frame = self
                    .call_stack
                    .last_mut()
                    .ok_or_else(|| malformed!("empty call stack"))?;
                ops::exec_rec_new(operand, frame, &mut self.heap)?;
                Ok(StepResult::Continue)
            }
            Opcode::REC_GET => {
                let frame = self
                    .call_stack
                    .last_mut()
                    .ok_or_else(|| malformed!("empty call stack"))?;
                ops::exec_rec_get(operand, frame, &self.heap)?;
                Ok(StepResult::Continue)
            }
            Opcode::REC_SET => {
                let frame = self
                    .call_stack
                    .last_mut()
                    .ok_or_else(|| malformed!("empty call stack"))?;
                ops::exec_rec_set(operand, frame, &mut self.heap)?;
                Ok(StepResult::Continue)
            }
            // §4.10 Array
            Opcode::ARR_NEW => {
                let frame = self
                    .call_stack
                    .last_mut()
                    .ok_or_else(|| malformed!("empty call stack"))?;
                ops::exec_arr_new(operand, frame, &mut self.heap)?;
                Ok(StepResult::Continue)
            }
            Opcode::ARR_GET => {
                let frame = self
                    .call_stack
                    .last_mut()
                    .ok_or_else(|| malformed!("empty call stack"))?;
                ops::exec_arr_get(frame, &self.heap)?;
                Ok(StepResult::Continue)
            }
            Opcode::ARR_SET => {
                let frame = self
                    .call_stack
                    .last_mut()
                    .ok_or_else(|| malformed!("empty call stack"))?;
                ops::exec_arr_set(frame, &mut self.heap)?;
                Ok(StepResult::Continue)
            }
            Opcode::ARR_LEN => {
                let frame = self
                    .call_stack
                    .last_mut()
                    .ok_or_else(|| malformed!("empty call stack"))?;
                ops::exec_arr_len(frame, &self.heap)?;
                Ok(StepResult::Continue)
            }

            // §4.11 Tuple
            Opcode::TUP_NEW => {
                let frame = self
                    .call_stack
                    .last_mut()
                    .ok_or_else(|| malformed!("empty call stack"))?;
                ops::exec_tup_new(operand, frame, &mut self.heap)?;
                Ok(StepResult::Continue)
            }
            Opcode::TUP_GET => {
                let frame = self
                    .call_stack
                    .last_mut()
                    .ok_or_else(|| malformed!("empty call stack"))?;
                ops::exec_tup_get(operand, frame, &self.heap)?;
                Ok(StepResult::Continue)
            }

            // §4.12 Type operations
            Opcode::TY_TEST => {
                let frame = self
                    .call_stack
                    .last_mut()
                    .ok_or_else(|| malformed!("empty call stack"))?;
                ops::exec_ty_test(operand, frame, &self.module.types, &self.heap)?;
                Ok(StepResult::Continue)
            }
            Opcode::TY_OF => {
                let frame = self
                    .call_stack
                    .last_mut()
                    .ok_or_else(|| malformed!("empty call stack"))?;
                ops::exec_ty_of(frame, &self.heap)?;
                Ok(StepResult::Continue)
            }
            Opcode::TY_EQ => {
                // Pop two type ids (nats), push bool(a == b).
                let frame = self.current_frame()?;
                let (b, a) = frame.pop2()?;
                let eq = a.0 == b.0;
                frame.stack.push(Value::from_bool(eq));
                Ok(StepResult::Continue)
            }
            Opcode::TY_CAST => {
                // Identity cast at this level - the emitter already checked types.
                Ok(StepResult::Continue)
            }
            Opcode::TY_DESC => {
                let frame = self
                    .call_stack
                    .last_mut()
                    .ok_or_else(|| malformed!("empty call stack"))?;
                ops::exec_ty_desc(operand, frame, &mut self.heap, &self.module.types);
                Ok(StepResult::Continue)
            }

            // §4.14 Match
            Opcode::MAT_TAG => {
                let frame = self
                    .call_stack
                    .last_mut()
                    .ok_or_else(|| malformed!("empty call stack"))?;
                ops::exec_mat_tag(operand, frame, &self.heap)?;
                Ok(StepResult::Continue)
            }
            Opcode::MAT_DATA => {
                let frame = self
                    .call_stack
                    .last_mut()
                    .ok_or_else(|| malformed!("empty call stack"))?;
                ops::exec_mat_data(operand, frame, &self.heap)?;
                Ok(StepResult::Continue)
            }

            // §4.15 Optional
            Opcode::OPT_SOME => {
                let frame = self
                    .call_stack
                    .last_mut()
                    .ok_or_else(|| malformed!("empty call stack"))?;
                ops::exec_opt_some(frame, &mut self.heap)?;
                Ok(StepResult::Continue)
            }
            Opcode::OPT_NONE => {
                let frame = self
                    .call_stack
                    .last_mut()
                    .ok_or_else(|| malformed!("empty call stack"))?;
                ops::exec_opt_none(frame, &mut self.heap);
                Ok(StepResult::Continue)
            }
            Opcode::OPT_IS => {
                let frame = self
                    .call_stack
                    .last_mut()
                    .ok_or_else(|| malformed!("empty call stack"))?;
                ops::exec_opt_is(frame, &self.heap)?;
                Ok(StepResult::Continue)
            }
            Opcode::OPT_GET => {
                let frame = self
                    .call_stack
                    .last_mut()
                    .ok_or_else(|| malformed!("empty call stack"))?;
                ops::exec_opt_get(frame, &self.heap)?;
                Ok(StepResult::Continue)
            }

            // §4.16 String
            Opcode::STR_CAT => {
                let frame = self
                    .call_stack
                    .last_mut()
                    .ok_or_else(|| malformed!("empty call stack"))?;
                ops::exec_str_cat(frame, &mut self.heap)?;
                Ok(StepResult::Continue)
            }
            Opcode::STR_LEN => {
                let frame = self
                    .call_stack
                    .last_mut()
                    .ok_or_else(|| malformed!("empty call stack"))?;
                ops::exec_str_len(frame, &self.heap)?;
                Ok(StepResult::Continue)
            }

            // §4.8 Closure
            Opcode::CLS_NEW => {
                let frame = self
                    .call_stack
                    .last_mut()
                    .ok_or_else(|| malformed!("empty call stack"))?;
                ops::exec_cls_new(operand, frame, &self.module.functions, &mut self.heap)?;
                Ok(StepResult::Continue)
            }
            Opcode::CLS_UPV => {
                let Self {
                    call_stack,
                    heap,
                    open_upvalue_map,
                    ..
                } = self;
                ops::exec_cls_upv(operand, call_stack, heap, open_upvalue_map)?;
                Ok(StepResult::Continue)
            }
            Opcode::LD_UPV => {
                let val = ops::exec_ld_upv(operand, &self.call_stack, &self.heap)?;
                self.call_stack
                    .last_mut()
                    .ok_or_else(|| malformed!("empty call stack"))?
                    .stack
                    .push(val);
                Ok(StepResult::Continue)
            }
            Opcode::ST_UPV => {
                let new_val = self
                    .call_stack
                    .last_mut()
                    .ok_or_else(|| malformed!("empty call stack"))?
                    .pop()?;
                let Self {
                    call_stack, heap, ..
                } = self;
                ops::exec_st_upv(operand, new_val, call_stack, heap)?;
                Ok(StepResult::Continue)
            }

            // §4.6 Branch - FI16 signed BE relative offsets
            Opcode::BR => {
                let frame = self.current_frame()?;
                let offset = ops::read_i16_operand(operand);
                frame.ip = ops::jump_target(frame.ip, offset)?;
                Ok(StepResult::Continue)
            }
            Opcode::BR_TRUE => {
                let frame = self.current_frame()?;
                let cond = frame.pop()?;
                if cond.as_truthy()? {
                    let offset = ops::read_i16_operand(operand);
                    frame.ip = ops::jump_target(frame.ip, offset)?;
                }
                Ok(StepResult::Continue)
            }
            Opcode::BR_FALSE => {
                let frame = self.current_frame()?;
                let cond = frame.pop()?;
                if !cond.as_truthy()? {
                    let offset = ops::read_i16_operand(operand);
                    frame.ip = ops::jump_target(frame.ip, offset)?;
                }
                Ok(StepResult::Continue)
            }
            Opcode::BR_LONG => {
                let frame = self.current_frame()?;
                let offset = ops::read_i24_operand(operand)?;
                frame.ip = ops::jump_target(frame.ip, offset)?;
                Ok(StepResult::Continue)
            }

            // §4.7 Call/Return
            Opcode::RET => {
                let v = self.current_frame()?.pop()?;
                self.do_return(v)
            }
            Opcode::RET_UNIT => self.do_return(Value::UNIT),

            // CALL arity:u8 - callee is on the stack below the args.
            Opcode::CALL => {
                let arity = u8::try_from(operand & 0xFF).unwrap_or(u8::MAX);
                let call_target = {
                    let frame = self
                        .call_stack
                        .last_mut()
                        .ok_or_else(|| malformed!("empty call stack"))?;
                    ops::resolve_callee(arity, frame, &self.heap)?
                };
                match call_target {
                    ops::CallTarget::Fn(fn_id) => self.do_call_with_stack_args(fn_id, None),
                    ops::CallTarget::Closure { fn_id, closure_ref } => {
                        self.do_call_with_stack_args(fn_id, Some(closure_ref))
                    }
                }
            }
            Opcode::CALL_TAIL => {
                let arity = u8::try_from(operand & 0xFF).unwrap_or(u8::MAX);
                let call_target = {
                    let frame = self
                        .call_stack
                        .last_mut()
                        .ok_or_else(|| malformed!("empty call stack"))?;
                    ops::resolve_callee(arity, frame, &self.heap)?
                };
                let (fn_id, closure_ref) = match call_target {
                    ops::CallTarget::Fn(fn_id) => (fn_id, None),
                    ops::CallTarget::Closure { fn_id, closure_ref } => (fn_id, Some(closure_ref)),
                };
                self.do_tail_call(fn_id, closure_ref)
            }

            // FFI_CALL sym_idx:u8, arity:u8
            Opcode::FFI_CALL => {
                let ffi_id = u32::from(ops::fi8x2_a(operand));
                self.exec_ffi_call(ffi_id)
            }

            // §4.13 Effects
            Opcode::EFF_HDL | Opcode::EFF_POP | Opcode::EFF_NEED | Opcode::EFF_RES => {
                self.step_continuations(op, operand, fn_idx)
            }

            // §4.17 Arena - not yet implemented; error explicitly rather than
            // falling through to the generic "unknown opcode" branch.
            Opcode::AR_NEW => Err(VmError::Unimplemented { desc: "ar.new" }),
            Opcode::AR_ALLOC => Err(VmError::Unimplemented { desc: "ar.alloc" }),
            Opcode::AR_FREE => Err(VmError::Unimplemented { desc: "ar.free" }),

            // §4.18 GC hints and NOP - no-ops
            Opcode::NOP | Opcode::GC_PIN | Opcode::GC_UNPIN => Ok(StepResult::Continue),

            _ => Err(malformed!("unknown opcode {:#04x}", op.0)),
        }
    }

    fn step_continuations(
        &mut self,
        op: Opcode,
        operand: u32,
        fn_idx: usize,
    ) -> Result<StepResult, VmError> {
        let cont_action = {
            let frame = self
                .call_stack
                .last_mut()
                .ok_or_else(|| malformed!("empty call stack"))?;
            let handlers = &self.module.functions[fn_idx].handlers;
            continuations::exec(op, operand, frame, &self.module.effects, handlers)?
        };
        match cont_action {
            continuations::ContAction::NotHandled | continuations::ContAction::Continue => {
                Ok(StepResult::Continue)
            }
            continuations::ContAction::Dispatch { handler_fn_id } => {
                self.do_call_with_stack_args(handler_fn_id, None)
            }
            continuations::ContAction::CrossFrameSearch { effect_id, op_id } => {
                self.exec_cont_save_cross_frame(effect_id, op_id)
            }
            continuations::ContAction::Resume => self.exec_cont_resume(),
        }
    }

    fn exec_ffi_call(&mut self, ffi_id: u32) -> Result<StepResult, VmError> {
        let foreign_idx =
            usize::try_from(ffi_id).map_err(|_| malformed!("foreign fn index overflows usize"))?;
        let foreign_fn = self
            .module
            .foreign_fns
            .get(foreign_idx)
            .ok_or_else(|| malformed!("foreign fn index {} out of bounds", ffi_id))?;
        let param_count = usize::from(foreign_fn.param_count);

        let frame = self
            .call_stack
            .last_mut()
            .ok_or_else(|| malformed!("ffi.call with empty call stack"))?;

        if frame.stack.len() < param_count {
            return Err(malformed!(
                "ffi.call: need {} args, stack has {}",
                param_count,
                frame.stack.len()
            ));
        }
        let start = frame.stack.len() - param_count;
        let args: Vec<Value> = frame.stack.drain(start..).collect();

        let host = self
            .host
            .as_mut()
            .ok_or_else(|| malformed!("ffi.call without a host attached"))?;
        let result = host.call_foreign(ffi_id, &args, &mut self.heap)?;

        self.current_frame()?.stack.push(result);
        Ok(StepResult::Continue)
    }

    fn current_frame(&mut self) -> Result<&mut Frame, VmError> {
        self.call_stack
            .last_mut()
            .ok_or_else(|| malformed!("empty call stack"))
    }
}
