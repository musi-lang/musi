use std::collections::HashMap;

use music_seam::{ProcedureId, TypeId};
use music_term::SyntaxTerm;

use super::gc::{HeapCollectionStats, HeapOptions, RuntimeHeap};
pub use super::host::{EffectCall, ForeignCall, VmHostContext};
pub use super::loader::{RejectingLoader, VmLoader};
use super::opcode::{VmOpcodeFamily, classify_opcode};
pub use super::value::{
    ClosureValue, ClosureView, ContinuationFrame, ContinuationHandler, ContinuationValue,
    DataValue, ForeignValue, GcRef, ModuleValue, ModuleView, SequenceValue, SyntaxView, ValueList,
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
        let slot = self.expect_module_slot(module)?;
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
        let module = self.alloc_module(spec, slot)?;
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
                let closure = self.heap.closure(closure)?.clone();
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
                self.invoke_continuation(continuation, value.clone())
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
                    .unwrap_or_else(|| self.call_host_foreign(&call, args))
            }
            _ => Err(VmError::new(VmErrorKind::NonCallableValue {
                found: value.kind(),
            })),
        }?;
        self.retain_external_value(&result)?;
        if base_depth == 0 {
            let _ = self.collect_garbage();
        }
        Ok(result)
    }

    /// Inspects one value through this VM heap.
    ///
    /// # Panics
    ///
    /// Panics when given a stale heap reference that did not come from this VM or was collected.
    #[must_use]
    pub fn inspect<'a>(&'a self, value: &'a Value) -> ValueView<'a> {
        match value {
            Value::Unit => ValueView::Unit,
            Value::Int(value) => ValueView::Int(*value),
            Value::Nat(value) => ValueView::Nat(*value),
            Value::Float(value) => ValueView::Float(*value),
            Value::String(text) => ValueView::String(StringView::new(
                self.heap.string(*text).expect("live string"),
            )),
            Value::CPtr(address) => ValueView::CPtr(*address),
            Value::Syntax(term) => ValueView::Syntax(SyntaxView::new(
                self.heap.syntax(*term).expect("live syntax"),
            )),
            Value::Seq(seq) => ValueView::Seq(SeqView::new(
                self.heap.sequence(*seq).expect("live sequence"),
            )),
            Value::Data(data) => {
                let inner = self.heap.data(*data).expect("live data");
                if inner.fields.is_empty() && self.is_named_type(inner.ty, "Bool") {
                    ValueView::Bool(inner.tag != 0)
                } else if inner.tag == 0 {
                    ValueView::Record(RecordView::new(inner))
                } else {
                    ValueView::Data(RecordView::new(inner))
                }
            }
            Value::Closure(closure) => ValueView::Closure(ClosureView::new(
                self.heap.closure(*closure).expect("live closure"),
            )),
            Value::Continuation(_) => ValueView::Continuation,
            Value::Type(ty) => ValueView::Type(*ty),
            Value::Module(module) => {
                let module = self.heap.module(*module).expect("live module");
                ValueView::Module(ModuleView::new(&module.spec, module.slot))
            }
            Value::Foreign(foreign) => ValueView::Foreign(foreign.foreign),
            Value::Effect(effect) => ValueView::Effect(*effect),
            Value::Class(class) => ValueView::Class(*class),
        }
    }

    pub(crate) fn observe_heap_value(&self, value: &Value) -> VmResult {
        if let Some(reference) = value.gc_ref() {
            if !self.heap.contains(reference) {
                return Err(VmError::new(VmErrorKind::InvalidProgramShape {
                    detail: "stale heap reference".into(),
                }));
            }
        }
        Ok(())
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

    pub(crate) fn call_host_foreign(
        &mut self,
        foreign: &ForeignCall,
        args: &[Value],
    ) -> VmResult<Value> {
        let options = self.heap_options();
        let mut ctx = VmHostContext::new(&mut self.heap, options);
        self.host.call_foreign(&mut ctx, foreign, args)
    }

    pub(crate) fn call_host_effect(
        &mut self,
        effect: &EffectCall,
        args: &[Value],
    ) -> VmResult<Value> {
        let options = self.heap_options();
        let mut ctx = VmHostContext::new(&mut self.heap, options);
        self.host.handle_effect(&mut ctx, effect, args)
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
        let resume_values = self.active_resumes.iter().copied().map(Value::Continuation);
        let external_values = self.external_roots.iter().cloned();
        module_globals
            .chain(frame_values)
            .chain(handler_values)
            .chain(resume_values)
            .chain(external_values)
            .collect()
    }
}

impl Vm {
    /// Allocates one VM-owned string.
    ///
    /// # Errors
    ///
    /// Returns [`VmError`] when heap limits reject the allocation.
    pub fn alloc_string(&mut self, text: impl Into<Box<str>>) -> VmResult<Value> {
        self.heap.alloc_string(text, &self.heap_options())
    }

    /// Allocates one VM-owned syntax term.
    ///
    /// # Errors
    ///
    /// Returns [`VmError`] when heap limits reject the allocation.
    pub fn alloc_syntax(&mut self, term: SyntaxTerm) -> VmResult<Value> {
        self.heap.alloc_syntax(term, &self.heap_options())
    }

    /// Allocates one VM-owned sequence.
    ///
    /// # Errors
    ///
    /// Returns [`VmError`] when heap limits reject the allocation.
    pub fn alloc_sequence<Items>(&mut self, ty: TypeId, items: Items) -> VmResult<Value>
    where
        Items: IntoIterator<Item = Value>,
    {
        self.heap.alloc_sequence(
            SequenceValue::new(ty, items.into_iter().collect()),
            &self.heap_options(),
        )
    }

    /// Allocates one VM-owned data value.
    ///
    /// # Errors
    ///
    /// Returns [`VmError`] when heap limits reject the allocation.
    pub fn alloc_data<Fields>(&mut self, ty: TypeId, tag: i64, fields: Fields) -> VmResult<Value>
    where
        Fields: IntoIterator<Item = Value>,
    {
        self.heap.alloc_data(
            DataValue::new(ty, tag, fields.into_iter().collect()),
            &self.heap_options(),
        )
    }

    pub(crate) fn alloc_closure<Captures>(
        &mut self,
        module_slot: usize,
        procedure: ProcedureId,
        captures: Captures,
    ) -> VmResult<Value>
    where
        Captures: IntoIterator<Item = Value>,
    {
        self.heap.alloc_closure(
            ClosureValue::new(module_slot, procedure, captures.into_iter().collect()),
            &self.heap_options(),
        )
    }

    pub(crate) fn alloc_module(
        &mut self,
        spec: impl Into<Box<str>>,
        slot: usize,
    ) -> VmResult<Value> {
        self.heap
            .alloc_module(ModuleValue::new(spec, slot), &self.heap_options())
    }

    pub(crate) fn alloc_continuation(
        &mut self,
        frames: super::value::ContinuationFrameList,
        handlers: super::value::ContinuationHandlerList,
    ) -> VmResult<Value> {
        self.heap.alloc_continuation(
            ContinuationValue::new(frames, handlers),
            &self.heap_options(),
        )
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
