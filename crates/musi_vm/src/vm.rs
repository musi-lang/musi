use std::collections::HashMap;

use super::gc::{HeapCollectionStats, HeapOptions, RuntimeHeap};
pub use super::host::{EffectCall, ForeignCall};
pub use super::loader::{RejectingLoader, VmLoader};
use super::opcode::{VmOpcodeFamily, classify_opcode};
pub use super::value::{
    ContinuationFrame, ContinuationHandler, ContinuationValuePtr, DataValuePtr, ForeignValue,
    SeqValuePtr, SyntaxView, ValueList,
};
pub use super::{
    OperandShape, Program, RecordView, RejectingHost, SeqView, StringView, Value, ValueView,
    VmError, VmErrorKind, VmHost, VmResult, VmValueKind,
};

mod dispatch;
mod module;
mod ops;
mod stack;
mod state;
mod value_support;

use self::state::{
    CallFrameList, EffectHandlerList, LoadedModule, LoadedModuleList, ModuleSlotMap, ResumeList,
};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct VmOptions {
    pub heap_limit_bytes: Option<usize>,
    pub max_object_bytes: Option<usize>,
    pub stack_frame_limit: Option<usize>,
    pub instruction_budget: Option<u64>,
    pub gc_stress: bool,
}

impl Default for VmOptions {
    fn default() -> Self {
        Self::DEFAULT
    }
}

impl VmOptions {
    pub const DEFAULT: Self = Self {
        heap_limit_bytes: None,
        max_object_bytes: None,
        stack_frame_limit: None,
        instruction_budget: None,
        gc_stress: false,
    };

    #[must_use]
    pub const fn with_heap_limit_bytes(mut self, heap_limit_bytes: usize) -> Self {
        self.heap_limit_bytes = Some(heap_limit_bytes);
        self
    }

    #[must_use]
    pub const fn with_max_object_bytes(mut self, max_object_bytes: usize) -> Self {
        self.max_object_bytes = Some(max_object_bytes);
        self
    }

    #[must_use]
    pub const fn with_stack_frame_limit(mut self, stack_frame_limit: usize) -> Self {
        self.stack_frame_limit = Some(stack_frame_limit);
        self
    }

    #[must_use]
    pub const fn with_instruction_budget(mut self, instruction_budget: u64) -> Self {
        self.instruction_budget = Some(instruction_budget);
        self
    }

    #[must_use]
    pub const fn with_gc_stress(mut self, gc_stress: bool) -> Self {
        self.gc_stress = gc_stress;
        self
    }
}

#[allow(non_upper_case_globals)]
pub const VmOptions: VmOptions = VmOptions::DEFAULT;

pub struct Vm {
    loaded_modules: LoadedModuleList,
    module_slots: ModuleSlotMap,
    loader: Box<dyn VmLoader>,
    host: Box<dyn VmHost>,
    options: VmOptions,
    frames: CallFrameList,
    handlers: EffectHandlerList,
    active_resumes: ResumeList,
    next_handler_id: u64,
    continuation_target_handler: Option<u64>,
    heap: RuntimeHeap,
    executed_instructions: u64,
    external_roots: Vec<Value>,
}

impl Vm {
    #[must_use]
    pub fn new(
        program: Program,
        loader: impl VmLoader + 'static,
        host: impl VmHost + 'static,
        options: VmOptions,
    ) -> Self {
        let root_module = LoadedModule::new("<root>", program);
        Self {
            loaded_modules: vec![root_module],
            module_slots: HashMap::new(),
            loader: Box::new(loader),
            host: Box::new(host),
            options,
            frames: Vec::new(),
            handlers: Vec::new(),
            active_resumes: Vec::new(),
            next_handler_id: 0,
            continuation_target_handler: None,
            heap: RuntimeHeap::default(),
            executed_instructions: 0,
            external_roots: Vec::new(),
        }
    }

    #[must_use]
    pub fn with_rejecting_host(program: Program, options: VmOptions) -> Self {
        Self::new(program, RejectingLoader, RejectingHost, options)
    }

    /// Runs synthesized module/program initialization exactly once.
    ///
    /// # Errors
    ///
    /// Returns [`VmError`] if entry execution fails.
    pub fn initialize(&mut self) -> VmResult {
        let _ = &self.options;
        self.initialize_slot(0)
    }

    /// Resolves one export by source name after initialization.
    ///
    /// # Errors
    ///
    /// Returns [`VmError`] if initialization is missing or export is absent.
    pub fn lookup_export(&mut self, name: &str) -> VmResult<Value> {
        self.ensure_initialized()?;
        let value = self.lookup_export_in_slot(0, name)?;
        self.retain_external_value(&value)?;
        Ok(value)
    }

    /// Resolves one export from one loaded module handle.
    ///
    /// # Errors
    ///
    /// Returns [`VmError`] if handle is not one module value or export is absent.
    pub fn lookup_module_export(&mut self, module: &Value, name: &str) -> VmResult<Value> {
        self.ensure_initialized()?;
        let slot = Self::expect_module_slot(module)?;
        let value = self.lookup_export_in_slot(slot, name)?;
        self.retain_external_value(&value)?;
        Ok(value)
    }

    /// Calls one export with runtime values.
    ///
    /// # Errors
    ///
    /// Returns [`VmError`] if export resolution or invocation fails.
    pub fn call_export(&mut self, name: &str, args: &[Value]) -> VmResult<Value> {
        let value = self.lookup_export(name)?;
        self.call_value(value, args)
    }

    /// Calls one export from one loaded module handle.
    ///
    /// # Errors
    ///
    /// Returns [`VmError`] if module export lookup or invocation fails.
    pub fn call_module_export(
        &mut self,
        module: &Value,
        name: &str,
        args: &[Value],
    ) -> VmResult<Value> {
        let value = self.lookup_module_export(module, name)?;
        self.call_value(value, args)
    }

    /// Loads one loaded module through host boundary and returns one initialized module handle.
    ///
    /// # Errors
    ///
    /// Returns [`VmError`] if initialization is missing, host loading fails, or module init fails.
    pub fn load_module(&mut self, spec: &str) -> VmResult<Value> {
        self.ensure_initialized()?;
        let slot = self.load_dynamic_module(spec)?;
        let module = Value::module(spec, slot);
        self.retain_external_value(&module)?;
        Ok(module)
    }

    #[must_use]
    pub fn module_spec(&self, slot: usize) -> Option<&str> {
        self.loaded_modules
            .get(slot)
            .map(|module| module.spec.as_ref())
    }

    #[must_use]
    pub fn module_program(&self, slot: usize) -> Option<&Program> {
        self.loaded_modules.get(slot).map(|module| &module.program)
    }

    #[must_use]
    pub const fn heap_allocated_bytes(&self) -> usize {
        self.heap.allocated_bytes()
    }

    #[must_use]
    pub const fn executed_instructions(&self) -> u64 {
        self.executed_instructions
    }

    pub fn collect_garbage(&mut self) -> HeapCollectionStats {
        let roots = self.heap_roots();
        self.heap.collect_from_roots(roots.iter())
    }

    /// Calls one runtime value if it is callable.
    ///
    /// # Errors
    ///
    /// Returns [`VmError`] if initialization is missing or value is not callable.
    pub fn call_value(&mut self, value: Value, args: &[Value]) -> VmResult<Value> {
        self.ensure_initialized()?;
        let base_depth = self.frames.len();
        let result = match value {
            Value::Closure(closure) => {
                let closure = closure.borrow();
                let mut full_args = closure.captures.clone();
                full_args.extend(args.iter().cloned());
                if base_depth == 0 {
                    self.invoke_procedure(closure.module_slot, closure.procedure, full_args)
                } else {
                    self.invoke_procedure_in_context(
                        closure.module_slot,
                        closure.procedure,
                        full_args,
                        base_depth,
                    )
                }
            }
            Value::Continuation(continuation) => {
                let [value] = args else {
                    return Err(VmError::new(VmErrorKind::CallArityMismatch {
                        callee: "continuation".into(),
                        expected: 1,
                        found: args.len(),
                    }));
                };
                self.invoke_continuation(&continuation, value.clone())
            }
            Value::Foreign(foreign_value) => {
                let ForeignValue {
                    module_slot,
                    foreign,
                    type_args,
                } = foreign_value;
                let call = self.foreign_call(module_slot, foreign);
                let call = Self::specialize_foreign_call(call, &type_args);
                self.call_musi_intrinsic(module_slot, &call, args)
                    .unwrap_or_else(|| self.host.call_foreign(&call, args))
            }
            _ => Err(VmError::new(VmErrorKind::NonCallableValue {
                found: value.kind(),
            })),
        }?;
        self.retain_external_value(&result)?;
        Ok(result)
    }

    #[must_use]
    pub fn inspect<'a>(&self, value: &'a Value) -> ValueView<'a> {
        match value {
            Value::Unit => ValueView::Unit,
            Value::Int(value) => ValueView::Int(*value),
            Value::Nat(value) => ValueView::Nat(*value),
            Value::Float(value) => ValueView::Float(*value),
            Value::String(text) => ValueView::String(StringView::new(text)),
            Value::CPtr(address) => ValueView::CPtr(*address),
            Value::Syntax(term) => ValueView::Syntax(SyntaxView::new(term.as_ref())),
            Value::Seq(seq) => ValueView::Seq(SeqView::new(seq.borrow())),
            Value::Data(data) => {
                let inner = data.borrow();
                if inner.fields.is_empty() && self.is_named_type(inner.ty, "Bool") {
                    ValueView::Bool(inner.tag != 0)
                } else if inner.tag == 0 {
                    ValueView::Record(RecordView::new(inner))
                } else {
                    ValueView::Data(RecordView::new(inner))
                }
            }
            Value::Closure(_) => ValueView::Closure,
            Value::Continuation(_) => ValueView::Continuation,
            Value::Type(ty) => ValueView::Type(*ty),
            Value::Module(module) => ValueView::Module(&module.spec),
            Value::Foreign(foreign) => ValueView::Foreign(foreign.foreign),
            Value::Effect(effect) => ValueView::Effect(*effect),
            Value::Class(class) => ValueView::Class(*class),
        }
    }

    pub(crate) fn observe_heap_value(&mut self, value: &Value) -> VmResult {
        self.heap.observe_value(value, &self.heap_options())
    }

    fn retain_external_value(&mut self, value: &Value) -> VmResult {
        if !is_heap_value(value) {
            return Ok(());
        }
        self.observe_heap_value(value)?;
        self.external_roots.push(value.clone());
        self.after_value_mutation()
    }

    pub(crate) fn after_value_mutation(&mut self) -> VmResult {
        if self.options.gc_stress {
            let _ = self.collect_garbage();
        }
        self.enforce_heap_limit()
    }

    pub(crate) fn before_instruction(&mut self) -> VmResult {
        if self
            .options
            .instruction_budget
            .is_some_and(|budget| self.executed_instructions >= budget)
        {
            return Err(VmError::new(VmErrorKind::InstructionBudgetExhausted {
                budget: self.options.instruction_budget.unwrap_or_default(),
            }));
        }
        self.executed_instructions = self.executed_instructions.saturating_add(1);
        Ok(())
    }

    fn enforce_heap_limit(&mut self) -> VmResult {
        let Some(limit) = self.options.heap_limit_bytes else {
            return Ok(());
        };
        if self.heap.allocated_bytes() <= limit {
            return Ok(());
        }
        let _ = self.collect_garbage();
        let allocated = self.heap.allocated_bytes();
        if allocated <= limit {
            Ok(())
        } else {
            Err(VmError::new(VmErrorKind::HeapLimitExceeded {
                allocated,
                limit,
            }))
        }
    }

    const fn heap_options(&self) -> HeapOptions {
        HeapOptions {
            max_object_bytes: self.options.max_object_bytes,
        }
    }

    fn heap_roots(&self) -> Vec<Value> {
        let module_globals = self
            .loaded_modules
            .iter()
            .flat_map(|module| module.globals.iter().cloned());
        let frame_values = self.frames.iter().flat_map(|frame| {
            frame
                .locals
                .iter()
                .chain(frame.stack.iter())
                .cloned()
                .collect::<Vec<_>>()
        });
        let handler_values = self.handlers.iter().map(|handler| handler.handler.clone());
        let resume_values = self.active_resumes.iter().cloned().map(Value::Continuation);
        let external_values = self.external_roots.iter().cloned();
        module_globals
            .chain(frame_values)
            .chain(handler_values)
            .chain(resume_values)
            .chain(external_values)
            .collect()
    }
}

const fn is_heap_value(value: &Value) -> bool {
    matches!(
        value,
        Value::String(_)
            | Value::Syntax(_)
            | Value::Seq(_)
            | Value::Data(_)
            | Value::Closure(_)
            | Value::Continuation(_)
            | Value::Module(_)
    )
}
