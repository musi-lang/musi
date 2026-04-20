use super::boundary::{HostState, LoaderState};
use super::state::{LoadedModule, LoadedModuleList};
use super::{
    HeapCollectionStats, Program, RejectingHost, RejectingLoader, RuntimeHeap, Value, Vm, VmHost,
    VmLoader, VmOptions, VmResult,
};
use crate::value::IsolateId;

impl Vm {
    #[must_use]
    pub fn new(
        program: Program,
        loader: impl VmLoader + 'static,
        host: impl VmHost + 'static,
        options: VmOptions,
    ) -> Self {
        let root_module = LoadedModule::new("<root>", program);
        let mut loaded_modules = LoadedModuleList::new();
        loaded_modules.push(root_module);
        Self {
            loaded_modules,
            module_slots: None,
            loader: LoaderState::Custom(Box::new(loader)),
            host: HostState::Custom(Box::new(host)),
            options,
            frames: Vec::new(),
            spare_frames: Vec::new(),
            handlers: Vec::new(),
            active_resumes: Vec::new(),
            next_handler_id: 0,
            continuation_target_handler: None,
            return_depth: None,
            heap: RuntimeHeap::new(),
            heap_dirty: false,
            executed_instructions: 0,
            external_roots: Vec::new(),
        }
    }

    #[must_use]
    pub fn with_rejecting_host(program: Program, options: VmOptions) -> Self {
        let root_module = LoadedModule::new("<root>", program);
        let mut loaded_modules = LoadedModuleList::new();
        loaded_modules.push(root_module);
        Self {
            loaded_modules,
            module_slots: None,
            loader: LoaderState::Rejecting(RejectingLoader),
            host: HostState::Rejecting(RejectingHost),
            options,
            frames: Vec::new(),
            spare_frames: Vec::new(),
            handlers: Vec::new(),
            active_resumes: Vec::new(),
            next_handler_id: 0,
            continuation_target_handler: None,
            return_depth: None,
            heap: RuntimeHeap::new(),
            heap_dirty: false,
            executed_instructions: 0,
            external_roots: Vec::new(),
        }
    }

    /// Runs synthesized module/program initialization exactly once.
    ///
    /// # Errors
    ///
    /// Returns [`VmError`] if entry execution fails.
    pub fn initialize(&mut self) -> VmResult {
        if self
            .loaded_modules
            .first()
            .is_some_and(LoadedModule::is_initialized)
        {
            return Ok(());
        }
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
    pub const fn isolate_id(&self) -> IsolateId {
        self.heap.isolate()
    }

    #[must_use]
    pub const fn executed_instructions(&self) -> u64 {
        self.executed_instructions
    }

    pub fn collect_garbage(&mut self) -> HeapCollectionStats {
        self.collect_garbage_with_extra(None)
    }
}
