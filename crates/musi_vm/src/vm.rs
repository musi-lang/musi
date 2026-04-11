use std::collections::HashMap;

pub use super::host::{EffectCall, ForeignCall};
pub use super::loader::{RejectingLoader, VmLoader};
use super::opcode::{VmOpcodeFamily, classify_opcode};
pub use super::value::{
    ClosureValuePtr, ContinuationFrame, ContinuationHandler, ContinuationValuePtr, DataValuePtr,
    ForeignValue, SeqValuePtr, SyntaxView, ValueList,
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

#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct VmOptions;

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
        self.lookup_export_in_slot(0, name)
    }

    /// Resolves one export from one loaded module handle.
    ///
    /// # Errors
    ///
    /// Returns [`VmError`] if handle is not one module value or export is absent.
    pub fn lookup_module_export(&mut self, module: &Value, name: &str) -> VmResult<Value> {
        self.ensure_initialized()?;
        let slot = Self::expect_module_slot(module)?;
        self.lookup_export_in_slot(slot, name)
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

    /// Loads one dynamic module through host boundary and returns one initialized module handle.
    ///
    /// # Errors
    ///
    /// Returns [`VmError`] if initialization is missing, host loading fails, or module init fails.
    pub fn load_module(&mut self, spec: &str) -> VmResult<Value> {
        self.ensure_initialized()?;
        let slot = self.load_dynamic_module(spec)?;
        Ok(Value::module(spec, slot))
    }

    /// Calls one runtime value if it is callable.
    ///
    /// # Errors
    ///
    /// Returns [`VmError`] if initialization is missing or value is not callable.
    pub fn call_value(&mut self, value: Value, args: &[Value]) -> VmResult<Value> {
        self.ensure_initialized()?;
        let base_depth = self.frames.len();
        match value {
            Value::Closure(closure) => {
                let closure = closure.borrow();
                let mut full_args = closure.captures.clone();
                full_args.extend(args.iter().cloned());
                if base_depth == 0 {
                    self.invoke_method(closure.module_slot, closure.method, full_args)
                } else {
                    self.invoke_method_in_context(
                        closure.module_slot,
                        closure.method,
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
            Value::Foreign(ForeignValue {
                module_slot,
                foreign,
            }) => {
                let call = self.foreign_call(module_slot, foreign);
                self.host.call_foreign(&call, args)
            }
            _ => Err(VmError::new(VmErrorKind::NonCallableValue {
                found: value.kind(),
            })),
        }
    }

    #[must_use]
    pub fn inspect<'a>(&self, value: &'a Value) -> ValueView<'a> {
        match value {
            Value::Unit => ValueView::Unit,
            Value::Int(value) => ValueView::Int(*value),
            Value::Float(value) => ValueView::Float(*value),
            Value::Bool(value) => ValueView::Bool(*value),
            Value::String(text) => ValueView::String(StringView { text }),
            Value::Syntax(term) => ValueView::Syntax(SyntaxView {
                inner: term.as_ref(),
            }),
            Value::Seq(seq) => ValueView::Seq(SeqView {
                inner: seq.borrow(),
            }),
            Value::Data(data) => {
                let inner = data.borrow();
                if inner.tag == 0 {
                    ValueView::Record(RecordView { inner })
                } else {
                    ValueView::Data(RecordView { inner })
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
}
