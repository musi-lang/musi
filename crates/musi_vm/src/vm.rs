//! Interpreter core: `Vm` struct and the main run loop.

#[cfg(test)]
mod tests;

mod arith;
mod control;
mod effects;
mod structural;

use musi_bc::Opcode;

use crate::error::VmError;
use crate::heap::Heap;
use crate::host::HostFunctions;
use crate::loader::LoadedModule;
use crate::value::Value;

const MAX_CALL_DEPTH: usize = 1024;

/// An activation record for a single function invocation.
#[derive(Clone)]
pub struct Frame {
    /// Index into `module.functions`.
    pub fn_idx: usize,
    /// Byte offset of the *next* instruction to execute.
    pub ip: usize,
    /// Local variable slots (pre-zeroed, size = `local_count`).
    pub locals: Vec<Value>,
    /// Operand stack.
    pub stack: Vec<Value>,
    /// Active effect frames (innermost last).
    pub eff_stack: Vec<EffFrame>,
}

/// An active effect handler frame.
#[derive(Clone, Copy, Debug)]
pub struct EffFrame {
    pub effect_id: u8,
    pub handler_fn_id: u32,
}

/// A captured one-shot continuation (frames between handler and `EFF_DO` site).
struct Continuation {
    frames: Vec<Frame>,
}

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
        let entry = self.module.entry_point.ok_or_else(|| VmError::Malformed {
            desc: "module has no entry point".into(),
        })?;
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
    /// Use [`step`](Self::step) to advance execution one instruction at a time,
    /// or [`run_to_completion`](Self::run_to_completion) to run until the
    /// call stack returns to the current depth.
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
            .ok_or_else(|| VmError::Malformed {
                desc: format!("fn_id {fn_id} not found in module").into_boxed_str(),
            })?;
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
    /// Returns `StepResult::Continue` if execution should continue, or
    /// `StepResult::Returned(value)` if the call stack is empty and the
    /// VM has produced a final result.
    ///
    /// # Errors
    ///
    /// Returns `VmError` on any runtime error. Errors include context
    /// (`fn_id` and bytecode offset) via `VmError::Runtime`.
    pub fn step(&mut self) -> Result<StepResult, VmError> {
        // Check instruction limit.
        if let Some(limit) = self.instruction_limit
            && self.instruction_count >= limit
        {
            return Err(VmError::InstructionLimitExceeded { limit });
        }
        self.instruction_count += 1;

        // Capture context for error wrapping.
        let (fn_id, instr_ip) = {
            let frame = self.call_stack.last().ok_or_else(|| VmError::Malformed {
                desc: "empty call stack".into(),
            })?;
            let fn_idx = frame.fn_idx;
            (self.module.functions[fn_idx].fn_id, frame.ip)
        };

        self.step_inner().map_err(|e| VmError::Runtime {
            fn_id,
            ip: instr_ip,
            source: Box::new(e),
        })
    }

    fn step_inner(&mut self) -> Result<StepResult, VmError> {
        let frame = self
            .call_stack
            .last_mut()
            .ok_or_else(|| VmError::Malformed {
                desc: "empty call stack".into(),
            })?;

        let fn_idx = frame.fn_idx;
        let ip = frame.ip;
        let code_len = self.module.functions[fn_idx].code.len();

        if ip >= code_len {
            return Err(VmError::Malformed {
                desc: "ip past end of bytecode".into(),
            });
        }

        let raw_op = self.module.functions[fn_idx].code[ip];
        let op = Opcode(raw_op);
        let (operand, instr_len) = control::decode_operand(&self.module.functions[fn_idx].code, ip);

        frame.ip = ip + instr_len;

        if arith::exec(op, frame)? {
            return Ok(StepResult::Continue);
        }

        if structural::exec(
            op,
            operand,
            frame,
            &self.module.consts,
            &mut self.heap,
            &mut self.globals,
        )? {
            return Ok(StepResult::Continue);
        }

        let handlers = &self.module.functions[fn_idx].handlers;
        let eff_action = effects::exec(op, operand, frame, &self.module.effects, handlers)?;
        match eff_action {
            effects::EffectAction::NotHandled => {}
            effects::EffectAction::Continue => return Ok(StepResult::Continue),
            effects::EffectAction::Abort => {
                return Err(VmError::EffectAborted);
            }
            effects::EffectAction::DoEffect { handler_fn_id } => {
                return self.do_call_with_stack_args(handler_fn_id);
            }
            effects::EffectAction::CrossFrameSearch { effect_id } => {
                return self.exec_eff_do_cross_frame(effect_id);
            }
            effects::EffectAction::Resume => {
                return self.exec_eff_res();
            }
        }

        if op == Opcode::INV_FFI {
            return self.exec_inv_ffi(operand);
        }

        let cf = control::exec(
            op,
            operand,
            self.call_stack.last_mut().expect("frame exists"),
        )?;
        self.handle_control(cf)
    }

    fn handle_control(&mut self, cf: control::ControlFlow) -> Result<StepResult, VmError> {
        match cf {
            control::ControlFlow::Continue => Ok(StepResult::Continue),
            control::ControlFlow::Jump { ip } => {
                let frame = self.call_stack.last_mut().expect("frame exists");
                frame.ip = ip;
                Ok(StepResult::Continue)
            }
            control::ControlFlow::Return { value } => {
                let _ = self.call_stack.pop();
                if let Some(caller) = self.call_stack.last_mut() {
                    caller.stack.push(value);
                    Ok(StepResult::Continue)
                } else {
                    Ok(StepResult::Returned(value))
                }
            }
            control::ControlFlow::Call { fn_id } => self.do_call_with_stack_args(fn_id),
            control::ControlFlow::TailCall { fn_id } => self.do_tail_call(fn_id),
            control::ControlFlow::Halt => Err(VmError::Halted),
        }
    }

    fn do_call_with_stack_args(&mut self, fn_id: u32) -> Result<StepResult, VmError> {
        if self.call_stack.len() >= MAX_CALL_DEPTH {
            return Err(VmError::StackOverflow);
        }

        let (fn_idx, func) = self
            .module
            .fn_by_id(fn_id)
            .ok_or_else(|| VmError::Malformed {
                desc: format!("call to unknown fn_id {fn_id}").into_boxed_str(),
            })?;
        let param_count = usize::from(func.param_count);
        let local_count = usize::from(func.local_count);
        let max_stack = usize::from(func.max_stack);

        // Pop args off caller's stack (first arg deepest).
        let caller_stack_len = self.call_stack.last().map_or(0, |f| f.stack.len());
        if caller_stack_len < param_count {
            return Err(VmError::Malformed {
                desc: format!(
                    "call to fn {fn_id}: need {param_count} args, stack has {caller_stack_len}"
                )
                .into_boxed_str(),
            });
        }
        let start = caller_stack_len - param_count;
        let args: Vec<Value> = self
            .call_stack
            .last_mut()
            .expect("caller frame exists")
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
        });
        Ok(StepResult::Continue)
    }

    fn do_tail_call(&mut self, fn_id: u32) -> Result<StepResult, VmError> {
        let (fn_idx, func) = self
            .module
            .fn_by_id(fn_id)
            .ok_or_else(|| VmError::Malformed {
                desc: format!("tail call to unknown fn_id {fn_id}").into_boxed_str(),
            })?;
        let param_count = usize::from(func.param_count);
        let local_count = usize::from(func.local_count);

        // Pop args from current frame's stack.
        let frame = self.call_stack.last_mut().expect("frame exists");
        let stack_len = frame.stack.len();
        if stack_len < param_count {
            return Err(VmError::Malformed {
                desc: format!(
                    "tail call to fn {fn_id}: need {param_count} args, stack has {stack_len}"
                )
                .into_boxed_str(),
            });
        }
        let start = stack_len - param_count;
        let args: Vec<Value> = frame.stack.drain(start..).collect();

        // Reuse the current frame.
        frame.fn_idx = fn_idx;
        frame.ip = 0;
        frame.stack.clear();
        frame.eff_stack.clear();

        // Resize locals if needed.
        frame.locals.resize(local_count, Value::UNIT);
        // Zero all locals then fill params.
        for v in &mut frame.locals {
            *v = Value::UNIT;
        }
        for (i, v) in args.into_iter().enumerate() {
            frame.locals[i] = v;
        }

        Ok(StepResult::Continue)
    }

    /// Cross-frame `EFF_DO`: search the entire call stack top-to-bottom for a
    /// handler matching `effect_id`. Captures frames between handler and
    /// current frame as a continuation, then calls the handler function.
    fn exec_eff_do_cross_frame(&mut self, effect_id: u8) -> Result<StepResult, VmError> {
        // Search call stack from top-1 to bottom for a frame with matching handler.
        let handler_idx = self
            .call_stack
            .iter()
            .enumerate()
            .rev()
            .skip(1) // skip current frame (already searched)
            .find_map(|(idx, frame)| {
                frame
                    .eff_stack
                    .iter()
                    .rev()
                    .find(|f| f.effect_id == effect_id)
                    .map(|_| idx)
            });

        let h_idx = handler_idx.ok_or(VmError::NoHandler { effect_id })?;

        // Find the handler fn_id from the handler frame's eff_stack.
        let handler_fn_id = self.call_stack[h_idx]
            .eff_stack
            .iter()
            .rev()
            .find(|f| f.effect_id == effect_id)
            .expect("handler exists")
            .handler_fn_id;

        // Capture frames above the handler as a continuation.
        let captured: Vec<Frame> = self.call_stack.drain(h_idx + 1..).collect();
        self.continuations.push(Continuation { frames: captured });

        // Call the handler function (it will push a new frame onto call_stack).
        self.do_call_with_stack_args(handler_fn_id)
    }

    /// `EFF_RES`: handler-side resume. Pop resume value, restore continuation
    /// frames, push resume value onto the restored topmost frame.
    fn exec_eff_res(&mut self) -> Result<StepResult, VmError> {
        // Pop resume value from handler's stack.
        let resume_value = self
            .call_stack
            .last_mut()
            .ok_or_else(|| VmError::Malformed {
                desc: "eff.res with empty call stack".into(),
            })?
            .stack
            .pop()
            .ok_or_else(|| VmError::Malformed {
                desc: "eff.res on empty operand stack".into(),
            })?;

        // Pop the most recent continuation.
        let cont = self.continuations.pop().ok_or_else(|| VmError::Malformed {
            desc: "eff.res with no captured continuation".into(),
        })?;

        // Pop the handler frame.
        let _ = self.call_stack.pop();

        // Restore continuation frames.
        self.call_stack.extend(cont.frames);

        // Push resume value onto the topmost restored frame.
        let top = self
            .call_stack
            .last_mut()
            .ok_or_else(|| VmError::Malformed {
                desc: "eff.res: no frame after restoring continuation".into(),
            })?;
        top.stack.push(resume_value);

        Ok(StepResult::Continue)
    }

    /// Dispatch an `INV_FFI` opcode: pop args, call the host, push result.
    fn exec_inv_ffi(&mut self, operand: u32) -> Result<StepResult, VmError> {
        let foreign_idx = usize::try_from(operand).map_err(|_| VmError::Malformed {
            desc: "foreign fn index overflows usize".into(),
        })?;
        let foreign_fn =
            self.module
                .foreign_fns
                .get(foreign_idx)
                .ok_or_else(|| VmError::Malformed {
                    desc: format!("foreign fn index {operand} out of bounds").into_boxed_str(),
                })?;
        let param_count = usize::from(foreign_fn.param_count);

        let frame = self
            .call_stack
            .last_mut()
            .ok_or_else(|| VmError::Malformed {
                desc: "inv.ffi with empty call stack".into(),
            })?;

        if frame.stack.len() < param_count {
            return Err(VmError::Malformed {
                desc: format!(
                    "inv.ffi: need {param_count} args, stack has {}",
                    frame.stack.len()
                )
                .into_boxed_str(),
            });
        }
        let start = frame.stack.len() - param_count;
        let args: Vec<Value> = frame.stack.drain(start..).collect();

        let host = self.host.as_mut().ok_or_else(|| VmError::Malformed {
            desc: "inv.ffi without a host attached".into(),
        })?;
        let result = host.call_foreign(operand, &args, &self.heap)?;

        let frame = self.call_stack.last_mut().expect("frame exists");
        frame.stack.push(result);

        Ok(StepResult::Continue)
    }

    /// Run a mark-sweep garbage collection cycle.
    ///
    /// Marks all objects reachable from the call stack and global table,
    /// then sweeps unreachable objects. Returns the number of objects freed.
    pub fn collect_garbage(&mut self) -> usize {
        let roots = self.collect_roots();
        self.heap.mark_reachable(&roots);
        self.heap.sweep()
    }

    /// Gather all root values from the call stack, continuations, and globals.
    fn collect_roots(&self) -> Vec<Value> {
        let mut roots = Vec::new();
        for frame in &self.call_stack {
            roots.extend_from_slice(&frame.locals);
            roots.extend_from_slice(&frame.stack);
        }
        for cont in &self.continuations {
            for frame in &cont.frames {
                roots.extend_from_slice(&frame.locals);
                roots.extend_from_slice(&frame.stack);
            }
        }
        roots.extend_from_slice(&self.globals);
        roots
    }
}
