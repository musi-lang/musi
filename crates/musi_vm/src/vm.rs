//! Interpreter core: `Vm` struct and the main run loop.

#[cfg(test)]
mod tests;

mod arith;
mod effects;
mod frame;
mod ops;

pub use frame::{Continuation, EffFrame, Frame};

use std::mem;

use musi_bc::Opcode;

use crate::channel::ChannelTable;
use crate::error::{VmError, malformed};
use crate::heap::Heap;
use crate::host::HostFunctions;
use crate::loader::LoadedModule;
use crate::task::{TaskScheduler, TaskStatus};
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
    scheduler: Option<TaskScheduler>,
    channels: Option<ChannelTable>,
}

impl Vm {
    /// Create a new VM owning the loaded module.
    #[must_use]
    pub const fn new(module: LoadedModule) -> Self {
        Self {
            module,
            heap: Heap::new(),
            globals: vec![],
            call_stack: vec![],
            instruction_count: 0,
            instruction_limit: None,
            continuations: vec![],
            host: None,
            scheduler: None,
            channels: None,
        }
    }

    /// Set the maximum number of instructions the VM will execute before
    /// returning `InstructionLimitExceeded`. Pass `None` to disable.
    pub const fn set_instruction_limit(&mut self, limit: Option<u64>) {
        self.instruction_limit = limit;
    }

    /// Attach a host function provider for `INV_FFI` dispatch.
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
        if self.call_stack.len() >= MAX_CALL_DEPTH {
            return Err(VmError::StackOverflow);
        }
        let (fn_idx, func) = self
            .module
            .fn_by_id(fn_id)
            .ok_or_else(|| malformed!("fn_id {} not found in module", fn_id))?;
        let local_count = usize::from(func.local_count);
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
            eff_stack: vec![],
            closure_ref: None,
        });
        Ok(())
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

        let (fn_id, instr_ip) = {
            let frame = self
                .call_stack
                .last()
                .ok_or_else(|| malformed!("empty call stack"))?;
            (self.module.functions[frame.fn_idx].fn_id, frame.ip)
        };

        self.step_inner().map_err(|e| VmError::Runtime {
            fn_id,
            ip: instr_ip,
            source: Box::new(e),
        })
    }

    fn step_inner(&mut self) -> Result<StepResult, VmError> {
        self.maybe_gc();

        // Phase 1: Read opcode, decode operand, advance IP.
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
            let raw_op = code[ip];
            let (operand, ilen) = ops::decode_operand(code, ip);
            frame.ip = ip + ilen;
            (fn_idx, Opcode(raw_op), operand)
        };

        // Phase 2: Try arithmetic (most frequent in hot loops).
        {
            let frame = self
                .call_stack
                .last_mut()
                .ok_or_else(|| malformed!("empty call stack"))?;
            if arith::exec(op, frame)? {
                return Ok(StepResult::Continue);
            }
        }

        // Phase 3: Flat dispatch.
        match op {
            // §0 Stack manipulation
            Opcode::NOP | Opcode::BRK => Ok(StepResult::Continue),
            Opcode::DUP => {
                self.current_frame()?.dup()?;
                Ok(StepResult::Continue)
            }
            Opcode::POP => {
                let _ = self.current_frame()?.stack.pop();
                Ok(StepResult::Continue)
            }
            Opcode::SWP => {
                self.current_frame()?.swp()?;
                Ok(StepResult::Continue)
            }

            // §5 Locals
            Opcode::LD_LOC | Opcode::LD_LOC_W => {
                let frame = self.current_frame()?;
                let slot = usize::try_from(operand)
                    .map_err(|_| malformed!("ld.loc operand overflow"))?;
                let v = frame.get_local(slot)?;
                frame.stack.push(v);
                Ok(StepResult::Continue)
            }
            Opcode::ST_LOC | Opcode::ST_LOC_W => {
                let frame = self.current_frame()?;
                let slot = usize::try_from(operand)
                    .map_err(|_| malformed!("st.loc operand overflow"))?;
                let v = frame.pop()?;
                frame.set_local(slot, v)?;
                Ok(StepResult::Continue)
            }

            // §5 Constants
            Opcode::LD_CST | Opcode::LD_CST_W => {
                let frame = self.call_stack.last_mut().ok_or_else(|| malformed!("empty call stack"))?;
                ops::exec_ld_cst(operand, frame, &self.module.consts, &mut self.heap)?;
                Ok(StepResult::Continue)
            }

            // §5 Struct / variant
            Opcode::MK_PRD => {
                let frame = self.call_stack.last_mut().ok_or_else(|| malformed!("empty call stack"))?;
                ops::exec_mk_prd(operand, frame, &mut self.heap)?;
                Ok(StepResult::Continue)
            }
            Opcode::LD_FLD => {
                let frame = self.call_stack.last_mut().ok_or_else(|| malformed!("empty call stack"))?;
                ops::exec_ld_fld(operand, frame, &self.heap)?;
                Ok(StepResult::Continue)
            }
            Opcode::MK_VAR | Opcode::MK_VAR_W => {
                let frame = self.call_stack.last_mut().ok_or_else(|| malformed!("empty call stack"))?;
                ops::exec_mk_var(operand, frame, &mut self.heap)?;
                Ok(StepResult::Continue)
            }
            Opcode::LD_PAY => {
                let frame = self.call_stack.last_mut().ok_or_else(|| malformed!("empty call stack"))?;
                ops::exec_ld_pay(operand, frame, &self.heap)?;
                Ok(StepResult::Continue)
            }
            Opcode::CMP_TAG | Opcode::CMP_TAG_W => {
                let frame = self.call_stack.last_mut().ok_or_else(|| malformed!("empty call stack"))?;
                ops::exec_cmp_tag(operand, frame, &self.heap)?;
                Ok(StepResult::Continue)
            }

            // §9 Array / heap
            Opcode::LD_TAG => {
                let frame = self.call_stack.last_mut().ok_or_else(|| malformed!("empty call stack"))?;
                ops::exec_ld_tag(frame, &self.heap)?;
                Ok(StepResult::Continue)
            }
            Opcode::LD_LEN => {
                let frame = self.call_stack.last_mut().ok_or_else(|| malformed!("empty call stack"))?;
                ops::exec_ld_len(frame, &self.heap)?;
                Ok(StepResult::Continue)
            }
            Opcode::LD_IDX => {
                let frame = self.call_stack.last_mut().ok_or_else(|| malformed!("empty call stack"))?;
                ops::exec_ld_idx(frame, &self.heap)?;
                Ok(StepResult::Continue)
            }
            Opcode::ST_IDX => {
                let frame = self.call_stack.last_mut().ok_or_else(|| malformed!("empty call stack"))?;
                ops::exec_st_idx(frame, &mut self.heap)?;
                Ok(StepResult::Continue)
            }
            Opcode::FRE => {
                let ptr = self.current_frame()?.pop()?.as_ref()?;
                self.heap.free(ptr)?;
                Ok(StepResult::Continue)
            }
            Opcode::MK_ARR => {
                let frame = self.call_stack.last_mut().ok_or_else(|| malformed!("empty call stack"))?;
                ops::exec_mk_arr(operand, frame, &mut self.heap)?;
                Ok(StepResult::Continue)
            }
            Opcode::ALC_REF => {
                let frame = self.call_stack.last_mut().ok_or_else(|| malformed!("empty call stack"))?;
                let initial = frame.pop()?;
                let ptr = self.heap.alloc(operand, vec![initial]);
                frame.stack.push(Value::from_ref(ptr));
                Ok(StepResult::Continue)
            }
            Opcode::ALC_ARN => {
                let ptr = self.heap.alloc(operand, vec![]);
                self.current_frame()?.stack.push(Value::from_ref(ptr));
                Ok(StepResult::Continue)
            }
            Opcode::ST_FLD => {
                let frame = self.call_stack.last_mut().ok_or_else(|| malformed!("empty call stack"))?;
                ops::exec_st_fld(operand, frame, &mut self.heap)?;
                Ok(StepResult::Continue)
            }

            // §12 Globals
            Opcode::LD_GLB => {
                let frame = self.call_stack.last_mut().ok_or_else(|| malformed!("empty call stack"))?;
                ops::exec_ld_glb(operand, frame, &mut self.globals)?;
                Ok(StepResult::Continue)
            }
            Opcode::ST_GLB => {
                let frame = self.call_stack.last_mut().ok_or_else(|| malformed!("empty call stack"))?;
                ops::exec_st_glb(operand, frame, &mut self.globals)?;
                Ok(StepResult::Continue)
            }

            // §16 Type check
            Opcode::TYP_CHK => {
                let frame = self.call_stack.last_mut().ok_or_else(|| malformed!("empty call stack"))?;
                ops::exec_type_chk(operand, frame, &self.module.types, &self.heap)?;
                Ok(StepResult::Continue)
            }

            // §17 Closures
            Opcode::MK_CLO => {
                let frame = self.call_stack.last_mut().ok_or_else(|| malformed!("empty call stack"))?;
                ops::exec_mk_clo(operand, frame, &self.module.functions, &mut self.heap)?;
                Ok(StepResult::Continue)
            }
            Opcode::LD_UPV => {
                let frame = self.call_stack.last_mut().ok_or_else(|| malformed!("empty call stack"))?;
                ops::exec_ld_upv(operand, frame, &self.heap)?;
                Ok(StepResult::Continue)
            }

            // §11 Control — jumps (only need frame)
            Opcode::JMP_W => {
                let frame = self.current_frame()?;
                let target = ops::jump_target(frame.ip, ops::read_i32_operand(operand))?;
                frame.ip = target;
                Ok(StepResult::Continue)
            }
            Opcode::JMP_T_W => {
                let frame = self.current_frame()?;
                let cond = frame.pop()?;
                if cond.as_bool()? {
                    frame.ip = ops::jump_target(frame.ip, ops::read_i32_operand(operand))?;
                }
                Ok(StepResult::Continue)
            }
            Opcode::JMP_F_W => {
                let frame = self.current_frame()?;
                let cond = frame.pop()?;
                if !cond.as_bool()? {
                    frame.ip = ops::jump_target(frame.ip, ops::read_i32_operand(operand))?;
                }
                Ok(StepResult::Continue)
            }

            // §11 Control — call / return / halt
            Opcode::HLT => Err(VmError::Halted),
            Opcode::UNR => Err(malformed!("unr (unreachable) reached at runtime")),
            Opcode::RET => {
                let v = self.current_frame()?.pop()?;
                self.do_return(v)
            }
            Opcode::RET_U => self.do_return(Value::UNIT),
            Opcode::INV | Opcode::INV_EFF => self.do_call_with_stack_args(operand),
            Opcode::INV_TAL | Opcode::INV_TAL_EFF => self.do_tail_call(operand),
            Opcode::INV_DYN => {
                let dyn_call = {
                    let frame = self.call_stack.last_mut().ok_or_else(|| malformed!("empty call stack"))?;
                    ops::resolve_inv_dyn(operand, frame, &self.heap)?
                };
                match dyn_call {
                    ops::DynCall::Fn(fn_id) => self.do_call_with_stack_args(fn_id),
                    ops::DynCall::Closure { fn_id, closure_ref } => {
                        let result = self.do_call_with_stack_args(fn_id)?;
                        if let Some(frame) = self.call_stack.last_mut() {
                            frame.closure_ref = Some(closure_ref);
                        }
                        Ok(result)
                    }
                }
            }
            Opcode::INV_FFI => self.exec_inv_ffi(operand),

            // §13 Effects
            Opcode::EFF_PSH | Opcode::EFF_POP | Opcode::EFF_DO | Opcode::EFF_RES
            | Opcode::EFF_RES_C | Opcode::EFF_ABT => {
                let eff_action = {
                    let frame = self
                        .call_stack
                        .last_mut()
                        .ok_or_else(|| malformed!("empty call stack"))?;
                    let handlers = &self.module.functions[fn_idx].handlers;
                    effects::exec(op, operand, frame, &self.module.effects, handlers)?
                };
                match eff_action {
                    effects::EffectAction::NotHandled | effects::EffectAction::Continue => {
                        Ok(StepResult::Continue)
                    }
                    effects::EffectAction::Abort => Err(VmError::EffectAborted),
                    effects::EffectAction::DoEffect {
                        handler_fn_id,
                        op_id: _,
                    } => self.do_call_with_stack_args(handler_fn_id),
                    effects::EffectAction::CrossFrameSearch { effect_id, op_id } => {
                        self.exec_eff_do_cross_frame(effect_id, op_id)
                    }
                    effects::EffectAction::Resume => self.exec_eff_res(),
                }
            }

            // §14 Concurrency
            Opcode::TSK_SPN => self.exec_tsk_spn(operand),
            Opcode::TSK_AWT => self.exec_tsk_awt(),
            Opcode::TSK_CMK => self.exec_tsk_cmk(),
            Opcode::TSK_CHS => self.exec_tsk_chs(),
            Opcode::TSK_CHR => self.exec_tsk_chr(),

            _ => Err(malformed!("unknown opcode {:#04x}", op.0)),
        }
    }

    // ── Call / return / tail-call ─────────────────────────────────────

    fn do_return(&mut self, value: Value) -> Result<StepResult, VmError> {
        let _ = self.call_stack.pop();
        if let Some(caller) = self.call_stack.last_mut() {
            caller.stack.push(value);
            Ok(StepResult::Continue)
        } else if self.scheduler.is_some() {
            self.complete_current_task(value)
        } else {
            Ok(StepResult::Returned(value))
        }
    }

    fn do_call_with_stack_args(&mut self, fn_id: u32) -> Result<StepResult, VmError> {
        if self.call_stack.len() >= MAX_CALL_DEPTH {
            return Err(VmError::StackOverflow);
        }

        let (fn_idx, func) = self
            .module
            .fn_by_id(fn_id)
            .ok_or_else(|| malformed!("call to unknown fn_id {}", fn_id))?;
        let param_count = usize::from(func.param_count);
        let local_count = usize::from(func.local_count);
        let max_stack = usize::from(func.max_stack);

        let caller_stack_len = self.call_stack.last().map_or(0, |f| f.stack.len());
        if caller_stack_len < param_count {
            return Err(malformed!(
                "call to fn {}: need {} args, stack has {}",
                fn_id,
                param_count,
                caller_stack_len
            ));
        }
        let start = caller_stack_len - param_count;
        let args: Vec<Value> = self
            .call_stack
            .last_mut()
            .ok_or_else(|| malformed!("empty call stack"))?
            .stack
            .drain(start..)
            .collect();

        let mut locals = vec![Value::UNIT; local_count];
        for (i, v) in args.into_iter().enumerate() {
            locals[i] = v;
        }

        self.call_stack.push(Frame {
            fn_idx,
            ip: 0,
            locals,
            stack: Vec::with_capacity(max_stack.max(4)),
            eff_stack: vec![],
            closure_ref: None,
        });
        Ok(StepResult::Continue)
    }

    fn do_tail_call(&mut self, fn_id: u32) -> Result<StepResult, VmError> {
        let (fn_idx, func) = self
            .module
            .fn_by_id(fn_id)
            .ok_or_else(|| malformed!("tail call to unknown fn_id {}", fn_id))?;
        let param_count = usize::from(func.param_count);
        let local_count = usize::from(func.local_count);

        let frame = self.current_frame()?;
        let stack_len = frame.stack.len();
        if stack_len < param_count {
            return Err(malformed!(
                "tail call to fn {}: need {} args, stack has {}",
                fn_id,
                param_count,
                stack_len
            ));
        }
        let start = stack_len - param_count;
        let args: Vec<Value> = frame.stack.drain(start..).collect();

        frame.fn_idx = fn_idx;
        frame.ip = 0;
        frame.stack.clear();
        frame.eff_stack.clear();
        frame.closure_ref = None;

        frame.locals.resize(local_count, Value::UNIT);
        for v in &mut frame.locals {
            *v = Value::UNIT;
        }
        for (i, v) in args.into_iter().enumerate() {
            frame.locals[i] = v;
        }

        Ok(StepResult::Continue)
    }

    // ── Effects ──────────────────────────────────────────────────────

    fn exec_eff_do_cross_frame(
        &mut self,
        effect_id: u8,
        op_id: u32,
    ) -> Result<StepResult, VmError> {
        let handler_idx = self
            .call_stack
            .iter()
            .enumerate()
            .rev()
            .skip(1)
            .find_map(|(idx, frame)| {
                frame
                    .eff_stack
                    .iter()
                    .rev()
                    .find(|f| f.effect_id == effect_id)
                    .map(|_| idx)
            });

        let h_idx = handler_idx.ok_or(VmError::NoHandler { effect_id })?;

        let handler_fn_id = self.call_stack[h_idx]
            .eff_stack
            .iter()
            .rev()
            .find(|f| f.effect_id == effect_id)
            .ok_or_else(|| malformed!("handler not found"))?
            .handler_fn_id;

        let captured: Vec<Frame> = self.call_stack.drain(h_idx + 1..).collect();
        self.continuations
            .push(Continuation { frames: captured, op_id });

        self.do_call_with_stack_args(handler_fn_id)
    }

    fn exec_eff_res(&mut self) -> Result<StepResult, VmError> {
        let resume_value = self
            .call_stack
            .last_mut()
            .ok_or_else(|| malformed!("eff.res with empty call stack"))?
            .stack
            .pop()
            .ok_or_else(|| malformed!("eff.res on empty operand stack"))?;

        let cont = self
            .continuations
            .pop()
            .ok_or_else(|| malformed!("eff.res with no captured continuation"))?;

        let op_is_fatal = self
            .module
            .effects
            .iter()
            .flat_map(|eff| eff.ops.iter())
            .any(|op| op.id == cont.op_id && op.fatal);
        if op_is_fatal {
            return Err(VmError::FatalEffectResumed { op_id: cont.op_id });
        }

        let _ = self.call_stack.pop();
        self.call_stack.extend(cont.frames);

        let top = self
            .call_stack
            .last_mut()
            .ok_or_else(|| malformed!("eff.res: no frame after restoring continuation"))?;
        top.stack.push(resume_value);

        Ok(StepResult::Continue)
    }

    // ── FFI ──────────────────────────────────────────────────────────

    fn exec_inv_ffi(&mut self, operand: u32) -> Result<StepResult, VmError> {
        let foreign_idx =
            usize::try_from(operand).map_err(|_| malformed!("foreign fn index overflows usize"))?;
        let foreign_fn = self
            .module
            .foreign_fns
            .get(foreign_idx)
            .ok_or_else(|| malformed!("foreign fn index {} out of bounds", operand))?;
        let param_count = usize::from(foreign_fn.param_count);

        let frame = self
            .call_stack
            .last_mut()
            .ok_or_else(|| malformed!("inv.ffi with empty call stack"))?;

        if frame.stack.len() < param_count {
            return Err(malformed!(
                "inv.ffi: need {} args, stack has {}",
                param_count,
                frame.stack.len()
            ));
        }
        let start = frame.stack.len() - param_count;
        let args: Vec<Value> = frame.stack.drain(start..).collect();

        let host = self
            .host
            .as_mut()
            .ok_or_else(|| malformed!("inv.ffi without a host attached"))?;
        let result = host.call_foreign(operand, &args, &mut self.heap)?;

        self.current_frame()?.stack.push(result);
        Ok(StepResult::Continue)
    }

    // ── Concurrency ──────────────────────────────────────────────────

    fn ensure_scheduler(&mut self) {
        if self.scheduler.is_none() {
            let mut sched = TaskScheduler::new();
            let _ = sched.init_main_task(Vec::new(), Vec::new());
            self.scheduler = Some(sched);
        }
        if self.channels.is_none() {
            self.channels = Some(ChannelTable::new());
        }
    }

    fn save_current_task(&mut self) {
        if let Some(sched) = &mut self.scheduler
            && let Some(cur_id) = sched.current_task_id()
            && let Some(task) = sched.get_mut(cur_id)
        {
            task.call_stack = mem::take(&mut self.call_stack);
            task.continuations = mem::take(&mut self.continuations);
        }
    }

    fn load_task(&mut self, task_id: u32) {
        if let Some(sched) = &mut self.scheduler
            && let Some(task) = sched.get_mut(task_id)
        {
            self.call_stack = mem::take(&mut task.call_stack);
            self.continuations = mem::take(&mut task.continuations);
        }
    }

    fn exec_tsk_spn(&mut self, fn_id: u32) -> Result<StepResult, VmError> {
        self.ensure_scheduler();

        let (fn_idx, func) = self
            .module
            .fn_by_id(fn_id)
            .ok_or_else(|| malformed!("tsk.spn: unknown fn_id {}", fn_id))?;
        let param_count = usize::from(func.param_count);
        let local_count = usize::from(func.local_count);
        let max_stack = usize::from(func.max_stack);

        let frame = self
            .call_stack
            .last_mut()
            .ok_or_else(|| malformed!("tsk.spn with empty call stack"))?;

        if frame.stack.len() < param_count {
            return Err(malformed!(
                "tsk.spn fn {}: need {} args, stack has {}",
                fn_id,
                param_count,
                frame.stack.len()
            ));
        }
        let start = frame.stack.len() - param_count;
        let args: Vec<Value> = frame.stack.drain(start..).collect();

        let mut locals = vec![Value::UNIT; local_count];
        for (i, v) in args.into_iter().enumerate() {
            locals[i] = v;
        }

        let new_frame = Frame {
            fn_idx,
            ip: 0,
            locals,
            stack: Vec::with_capacity(max_stack.max(4)),
            eff_stack: vec![],
            closure_ref: None,
        };

        let task_id = self.scheduler_mut()?.spawn(new_frame);
        self.current_frame()?.stack.push(Value::from_task(task_id));
        Ok(StepResult::Continue)
    }

    fn exec_tsk_awt(&mut self) -> Result<StepResult, VmError> {
        let frame = self
            .call_stack
            .last_mut()
            .ok_or_else(|| malformed!("tsk.awt with empty call stack"))?;
        let task_handle = frame
            .stack
            .pop()
            .ok_or_else(|| malformed!("tsk.awt: stack underflow"))?;
        let task_id = task_handle.as_task_id()?;

        let sched = self
            .scheduler
            .as_mut()
            .ok_or_else(|| malformed!("tsk.awt without scheduler"))?;
        let task = sched.get(task_id).ok_or(VmError::UnknownTask { task_id })?;

        if let TaskStatus::Completed(v) = task.status {
            self.current_frame()?.stack.push(v);
            return Ok(StepResult::Continue);
        }

        sched.suspend_awaiting_task(task_id);
        self.save_current_task();
        self.switch_to_next_task()
    }

    fn exec_tsk_cmk(&mut self) -> Result<StepResult, VmError> {
        self.ensure_scheduler();

        let chan_id = self
            .channels
            .as_mut()
            .ok_or_else(|| malformed!("channels not initialized"))?
            .create();

        self.current_frame()?.stack.push(Value::from_chan(chan_id));
        Ok(StepResult::Continue)
    }

    fn exec_tsk_chs(&mut self) -> Result<StepResult, VmError> {
        let frame = self
            .call_stack
            .last_mut()
            .ok_or_else(|| malformed!("tsk.chs with empty call stack"))?;
        let value = frame
            .stack
            .pop()
            .ok_or_else(|| malformed!("tsk.chs: stack underflow (value)"))?;
        let chan_handle = frame
            .stack
            .pop()
            .ok_or_else(|| malformed!("tsk.chs: stack underflow (chan)"))?;
        let chan_id = chan_handle.as_chan_id()?;

        let channels = self
            .channels
            .as_mut()
            .ok_or_else(|| malformed!("tsk.chs without channel table"))?;
        let sched = self
            .scheduler
            .as_mut()
            .ok_or_else(|| malformed!("tsk.chs without scheduler"))?;
        if !sched.wake_one_receiver(chan_id, value) {
            channels.send(chan_id, value)?;
        }

        self.current_frame()?.stack.push(Value::UNIT);
        Ok(StepResult::Continue)
    }

    fn exec_tsk_chr(&mut self) -> Result<StepResult, VmError> {
        let frame = self
            .call_stack
            .last_mut()
            .ok_or_else(|| malformed!("tsk.chr with empty call stack"))?;
        let chan_handle = frame
            .stack
            .pop()
            .ok_or_else(|| malformed!("tsk.chr: stack underflow"))?;
        let chan_id = chan_handle.as_chan_id()?;

        let channels = self
            .channels
            .as_mut()
            .ok_or_else(|| malformed!("tsk.chr without channel table"))?;

        if let Some(value) = channels.try_recv(chan_id)? {
            self.current_frame()?.stack.push(value);
            return Ok(StepResult::Continue);
        }

        let sched = self
            .scheduler
            .as_mut()
            .ok_or_else(|| malformed!("tsk.chr without scheduler"))?;
        sched.suspend_awaiting_recv(chan_id);
        self.save_current_task();
        self.switch_to_next_task()
    }

    fn complete_current_task(&mut self, value: Value) -> Result<StepResult, VmError> {
        let sched = self.scheduler_mut()?;
        let next_id = sched.complete_current(value);
        let has_live = sched.has_live_tasks();
        let main_result = sched
            .get(0)
            .and_then(|t| match t.status {
                TaskStatus::Completed(v) => Some(v),
                _ => None,
            })
            .unwrap_or(value);

        match next_id {
            Some(id) => {
                self.load_task(id);
                Ok(StepResult::Continue)
            }
            None if has_live => Err(VmError::Deadlock),
            None => Ok(StepResult::Returned(main_result)),
        }
    }

    fn switch_to_next_task(&mut self) -> Result<StepResult, VmError> {
        let sched = self.scheduler_mut()?;
        let next_id = sched.schedule_next();
        let has_live = sched.has_live_tasks();

        match next_id {
            Some(id) => {
                self.load_task(id);
                Ok(StepResult::Continue)
            }
            None if has_live => Err(VmError::Deadlock),
            None => Ok(StepResult::Returned(Value::UNIT)),
        }
    }

    fn current_frame(&mut self) -> Result<&mut Frame, VmError> {
        self.call_stack
            .last_mut()
            .ok_or_else(|| malformed!("empty call stack"))
    }

    fn scheduler_mut(&mut self) -> Result<&mut TaskScheduler, VmError> {
        self.scheduler
            .as_mut()
            .ok_or_else(|| malformed!("scheduler not initialized"))
    }

    // ── Garbage collection ───────────────────────────────────────────

    /// Run a mark-sweep garbage collection cycle.
    pub fn collect_garbage(&mut self) -> usize {
        let roots = self.collect_roots();
        self.heap.mark_reachable(&roots);
        let freed = self.heap.sweep();
        self.heap.reset_gc_counter();
        freed
    }

    /// Run GC if the heap's allocation counter has reached its threshold.
    fn maybe_gc(&mut self) {
        if self.heap.needs_gc() {
            let _ = self.collect_garbage();
        }
    }

    fn collect_roots(&self) -> Vec<Value> {
        let mut roots = Vec::new();
        for frame in &self.call_stack {
            roots.extend_from_slice(&frame.locals);
            roots.extend_from_slice(&frame.stack);
            if let Some(cr) = frame.closure_ref {
                roots.push(cr);
            }
        }
        for cont in &self.continuations {
            for frame in &cont.frames {
                roots.extend_from_slice(&frame.locals);
                roots.extend_from_slice(&frame.stack);
                if let Some(cr) = frame.closure_ref {
                    roots.push(cr);
                }
            }
        }
        roots.extend_from_slice(&self.globals);

        if let Some(sched) = &self.scheduler {
            for task in sched.tasks() {
                for frame in &task.call_stack {
                    roots.extend_from_slice(&frame.locals);
                    roots.extend_from_slice(&frame.stack);
                    if let Some(cr) = frame.closure_ref {
                        roots.push(cr);
                    }
                }
                for cont in &task.continuations {
                    for frame in &cont.frames {
                        roots.extend_from_slice(&frame.locals);
                        roots.extend_from_slice(&frame.stack);
                        if let Some(cr) = frame.closure_ref {
                            roots.push(cr);
                        }
                    }
                }
                if let TaskStatus::Completed(v) = task.status {
                    roots.push(v);
                }
            }
        }
        if let Some(chans) = &self.channels {
            for chan in chans.channels() {
                for val in &chan.buffer {
                    roots.push(*val);
                }
            }
        }
        roots
    }
}
