//! Interpreter core: `Vm` struct and the main run loop.

#[cfg(test)]
mod tests;

mod arith;
mod control;
mod effects;
mod structural;

use crate::error::VmError;
use crate::heap::Heap;
use crate::loader::LoadedModule;
use crate::value::Value;

const MAX_CALL_DEPTH: usize = 1024;

/// An activation record for a single function invocation.
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
        }
    }

    // ── Configuration ───────────────────────────────────────────────────────

    /// Set the maximum number of instructions the VM will execute before
    /// returning `InstructionLimitExceeded`. Pass `None` to disable.
    pub const fn set_instruction_limit(&mut self, limit: Option<u64>) {
        self.instruction_limit = limit;
    }

    /// Number of instructions executed since the VM was created.
    #[must_use]
    pub const fn instruction_count(&self) -> u64 {
        self.instruction_count
    }

    // ── Introspection ───────────────────────────────────────────────────────

    /// Borrow the current call stack (read-only).
    #[must_use]
    pub fn frames(&self) -> &[Frame] {
        &self.call_stack
    }

    /// Borrow the heap (read-only).
    #[must_use]
    pub const fn heap(&self) -> &Heap {
        &self.heap
    }

    /// Borrow the global variable table (read-only).
    #[must_use]
    pub fn globals(&self) -> &[Value] {
        &self.globals
    }

    /// Borrow the loaded module (read-only).
    #[must_use]
    pub const fn module(&self) -> &LoadedModule {
        &self.module
    }

    /// Returns `true` if the call stack is non-empty (a function is running).
    #[must_use]
    pub const fn is_running(&self) -> bool {
        !self.call_stack.is_empty()
    }

    // ── Execution ───────────────────────────────────────────────────────────

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

        let op = self.module.functions[fn_idx].code[ip];
        let (operand, instr_len) = control::decode_operand(&self.module.functions[fn_idx].code, ip);

        // Advance ip past the instruction before dispatch (jumps overwrite it).
        frame.ip = ip + instr_len;

        // Try arith first (no-operand, frequent).
        if arith::exec(op, frame)? {
            return Ok(StepResult::Continue);
        }

        // Structural/locals/array/alloc/globals.
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

        // Effect opcodes.
        let handlers = &self.module.functions[fn_idx].handlers;
        let eff_action = effects::exec(op, operand, frame, &self.module.effects, handlers)?;
        match eff_action {
            effects::EffectAction::NotHandled => {}
            effects::EffectAction::Continue => return Ok(StepResult::Continue),
            effects::EffectAction::Abort => {
                return Err(VmError::EffectAborted);
            }
            effects::EffectAction::DoEffect { handler_fn_id } => {
                return self.do_call(handler_fn_id);
            }
        }

        // Control flow (jumps, calls, returns).
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

    fn do_call(&mut self, fn_id: u32) -> Result<StepResult, VmError> {
        self.do_call_with_stack_args(fn_id)
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

    // ── Garbage collection ──────────────────────────────────────────────────

    /// Run a mark-sweep garbage collection cycle.
    ///
    /// Marks all objects reachable from the call stack and global table,
    /// then sweeps unreachable objects. Returns the number of objects freed.
    pub fn collect_garbage(&mut self) -> usize {
        let roots = self.collect_roots();
        self.heap.mark_reachable(&roots);
        self.heap.sweep()
    }

    /// Gather all root values from the call stack and globals.
    fn collect_roots(&self) -> Vec<Value> {
        let mut roots = Vec::new();
        for frame in &self.call_stack {
            roots.extend_from_slice(&frame.locals);
            roots.extend_from_slice(&frame.stack);
        }
        roots.extend_from_slice(&self.globals);
        roots
    }
}
