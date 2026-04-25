use crate::{VmIndexSpace, VmStackKind};
use music_seam::descriptor::ExportTarget;
use music_seam::{ProcedureId, TypeId};

use super::{
    RuntimeKernel, Seq8ExportCache, Value, ValueList, VmError, VmErrorKind, VmResult, VmValueKind,
};

use super::Vm;
use super::state::{LoadedModule, ModuleState};

impl Vm {
    pub(crate) fn ensure_initialized(&self) -> VmResult {
        if self
            .loaded_modules
            .first()
            .is_some_and(LoadedModule::is_initialized)
        {
            Ok(())
        } else {
            Err(VmError::new(VmErrorKind::VmInitializationRequired))
        }
    }

    pub(crate) fn initialize_slot(&mut self, slot: usize) -> VmResult {
        let state = self.module(slot)?.state;
        if matches!(state, ModuleState::Initialized) {
            if slot == 0 {
                self.root_initialized = true;
            }
            return Ok(());
        }
        if matches!(state, ModuleState::Initializing) {
            let spec: Box<str> = self.module(slot)?.spec.as_ref().into();
            return Err(VmError::new(VmErrorKind::ModuleInitCycle { spec }));
        }
        let entry = self.module(slot)?.program.entry_procedure();

        self.module_mut(slot)?.state = ModuleState::Initializing;
        let result = entry.map_or(Ok(()), |entry| self.initialize_entry(slot, entry));
        match result {
            Ok(()) => {
                self.module_mut(slot)?.state = ModuleState::Initialized;
                if slot == 0 {
                    self.root_initialized = true;
                }
                Ok(())
            }
            Err(error) => {
                self.module_mut(slot)?.state = ModuleState::Uninitialized;
                if slot == 0 {
                    self.root_initialized = false;
                }
                Err(error)
            }
        }
    }

    fn initialize_entry(&mut self, slot: usize, entry: ProcedureId) -> VmResult {
        let base_depth = self.frames.len();
        self.invoke_procedure_in_context(slot, entry, ValueList::new(), base_depth)
            .map(|_| ())
    }

    pub(crate) fn lookup_export_in_slot(&mut self, slot: usize, name: &str) -> VmResult<Value> {
        self.initialize_slot(slot)?;
        let (target, opaque) = {
            let module = self.module(slot)?;
            let Some((target, opaque)) = module.program.export_target_with_opaque(name) else {
                return Err(VmError::new(VmErrorKind::ExportNotFound {
                    module: module.spec.as_ref().into(),
                    export: name.into(),
                }));
            };
            (target, opaque)
        };
        if opaque {
            let module_name: Box<str> = self.module(slot)?.spec.as_ref().into();
            return Err(VmError::new(VmErrorKind::OpaqueExport {
                module: module_name,
                export: name.into(),
            }));
        }
        self.export_value(slot, name, target)
    }

    pub(crate) fn export_value(
        &self,
        slot: usize,
        _name: &str,
        target: ExportTarget,
    ) -> VmResult<Value> {
        let module_name: Box<str> = self.module(slot)?.spec.as_ref().into();
        match target {
            ExportTarget::Procedure(procedure) => {
                let loaded = self.module(slot)?.program.loaded_procedure(procedure)?;
                Ok(Value::procedure(
                    slot,
                    procedure,
                    loaded.params,
                    loaded.locals,
                ))
            }
            ExportTarget::Global(global) => {
                let globals = &self.module(slot)?.globals;
                let raw_slot = usize::try_from(global.raw()).unwrap_or(usize::MAX);
                globals.get(raw_slot).cloned().ok_or_else(|| {
                    VmError::new(VmErrorKind::IndexOutOfBounds {
                        space: VmIndexSpace::Global,
                        owner: Some(module_name),
                        index: i64::try_from(raw_slot).unwrap_or(i64::MAX),
                        len: globals.len(),
                    })
                })
            }
            ExportTarget::Foreign(foreign) => Ok(Value::foreign(slot, foreign)),
            ExportTarget::Type(ty) => Ok(Value::Type(ty)),
            ExportTarget::Effect(effect) => Ok(Value::Effect(effect)),
            ExportTarget::Shape(shape) => Ok(Value::Shape(shape)),
        }
    }

    pub(crate) fn try_call_export_kernel_fast(
        &mut self,
        slot: usize,
        name: &str,
        args: &[Value],
    ) -> VmResult<Option<Value>> {
        self.initialize_slot(slot)?;
        let (target, opaque) = {
            let module = self.module(slot)?;
            let Some((target, opaque)) = module.program.export_target_with_opaque(name) else {
                return Err(VmError::new(VmErrorKind::ExportNotFound {
                    module: module.spec.as_ref().into(),
                    export: name.into(),
                }));
            };
            (target, opaque)
        };
        if opaque {
            let module_name: Box<str> = self.module(slot)?.spec.as_ref().into();
            return Err(VmError::new(VmErrorKind::OpaqueExport {
                module: module_name,
                export: name.into(),
            }));
        }
        let ExportTarget::Procedure(procedure) = target else {
            return Ok(None);
        };
        self.try_invoke_kernel_from_args(slot, procedure, args)
    }

    pub(crate) fn cached_seq8_export(
        &self,
        slot: usize,
        name: &str,
    ) -> Option<(TypeId, super::GcRef)> {
        self.seq8_export_cache
            .iter()
            .find(|cache| cache.module_slot == slot && cache.name.as_ref() == name)
            .map(|cache| (cache.ty, cache.buffer))
    }

    pub(crate) fn cache_seq8_export(
        &mut self,
        slot: usize,
        name: &str,
    ) -> VmResult<Option<(TypeId, super::GcRef)>> {
        let Some((ty, cells)) = self.seq8_export_kernel(slot, name)? else {
            return Ok(None);
        };
        let (prototype, buffer) = self.alloc_shared_i64_array8_sequence(ty, cells)?;
        self.retain_external_value(&prototype)?;
        self.seq8_export_cache
            .push(Seq8ExportCache::new(slot, name, ty, buffer));
        Ok(Some((ty, buffer)))
    }

    /// Prepares known export fast paths without invoking the export.
    ///
    /// # Errors
    ///
    /// Returns [`VmError`] if VM initialization fails or fast-path prototype allocation fails.
    pub fn prewarm_export_fast_path(&mut self, name: &str) -> VmResult {
        self.ensure_initialized()?;
        let _ = self.cache_seq8_export(0, name)?;
        Ok(())
    }

    fn seq8_export_kernel(&self, slot: usize, name: &str) -> VmResult<Option<(TypeId, [i64; 8])>> {
        let module = self.module(slot)?;
        let Some((ExportTarget::Procedure(procedure), false)) =
            module.program.export_target_with_opaque(name)
        else {
            return Ok(None);
        };
        let Some(RuntimeKernel::ConstI64Array8Return { ty, cells }) =
            module.program.loaded_procedure(procedure)?.runtime_kernel()
        else {
            return Ok(None);
        };
        Ok(Some((ty, cells)))
    }

    pub(crate) fn expect_module_slot(&self, module: &Value) -> VmResult<usize> {
        match module {
            Value::Module(module) => Ok(self.heap.module(*module)?.slot),
            _ => Err(VmError::new(VmErrorKind::InvalidValueKind {
                expected: VmValueKind::Module,
                found: module.kind(),
            })),
        }
    }

    pub(crate) fn module(&self, slot: usize) -> VmResult<&LoadedModule> {
        self.loaded_modules.get(slot).ok_or_else(|| {
            VmError::new(VmErrorKind::IndexOutOfBounds {
                space: VmIndexSpace::ModuleSlot,
                owner: None,
                index: i64::try_from(slot).unwrap_or(i64::MAX),
                len: self.loaded_modules.len(),
            })
        })
    }

    pub(crate) fn module_mut(&mut self, slot: usize) -> VmResult<&mut LoadedModule> {
        let len = self.loaded_modules.len();
        self.loaded_modules.get_mut(slot).ok_or_else(|| {
            VmError::new(VmErrorKind::IndexOutOfBounds {
                space: VmIndexSpace::ModuleSlot,
                owner: None,
                index: i64::try_from(slot).unwrap_or(i64::MAX),
                len,
            })
        })
    }

    pub(crate) fn current_module_slot(&self) -> VmResult<usize> {
        self.frames
            .last()
            .map(|frame| frame.module_slot)
            .ok_or_else(|| {
                VmError::new(VmErrorKind::StackEmpty {
                    stack: VmStackKind::CallFrame,
                })
            })
    }

    pub(crate) fn load_dynamic_module(&mut self, spec: &str) -> VmResult<usize> {
        if let Some(slot) = self
            .module_slots
            .as_ref()
            .and_then(|slots| slots.get(spec).copied())
        {
            self.initialize_slot(slot)?;
            return Ok(slot);
        }
        let program = self.loader.load_program(spec)?;
        let slot = self.loaded_modules.len();
        self.loaded_modules
            .push(LoadedModule::new(spec.to_owned(), program));
        let _ = self
            .module_slots
            .get_or_insert_with(Default::default)
            .insert(spec.into(), slot);
        self.initialize_slot(slot)?;
        Ok(slot)
    }
}
