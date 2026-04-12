use music_seam::descriptor::ExportTarget;

use super::{Value, ValueList, VmError, VmErrorKind, VmResult, VmValueKind};

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
        let (spec, state, entry) = {
            let module = self.module(slot)?;
            (
                module.spec.clone(),
                module.state,
                module.program.entry_method(),
            )
        };
        match state {
            ModuleState::Initialized => return Ok(()),
            ModuleState::Initializing => {
                return Err(VmError::new(VmErrorKind::ModuleInitCycle { spec }));
            }
            ModuleState::Uninitialized => {}
        }

        self.module_mut(slot)?.state = ModuleState::Initializing;
        let base_depth = self.frames.len();
        let result = entry.map_or(Ok(()), |entry| {
            self.invoke_method_in_context(slot, entry, ValueList::new(), base_depth)
                .map(|_| ())
        });
        match result {
            Ok(()) => {
                self.module_mut(slot)?.state = ModuleState::Initialized;
                Ok(())
            }
            Err(error) => {
                self.module_mut(slot)?.state = ModuleState::Uninitialized;
                Err(error)
            }
        }
    }

    pub(crate) fn lookup_export_in_slot(&mut self, slot: usize, name: &str) -> VmResult<Value> {
        self.initialize_slot(slot)?;
        let module_name = self.module(slot)?.spec.clone();
        if self
            .module(slot)?
            .program
            .is_export_opaque(name)
            .unwrap_or(false)
        {
            return Err(VmError::new(VmErrorKind::OpaqueExport {
                module: module_name,
                export: name.into(),
            }));
        }
        let target = self
            .module(slot)?
            .program
            .export_target(name)
            .ok_or_else(|| {
                VmError::new(VmErrorKind::ExportNotFound {
                    module: module_name.clone(),
                    export: name.into(),
                })
            })?;
        self.export_value(slot, name, target)
    }

    pub(crate) fn export_value(
        &self,
        slot: usize,
        _name: &str,
        target: ExportTarget,
    ) -> VmResult<Value> {
        let module_name = self.module(slot)?.spec.clone();
        match target {
            ExportTarget::Method(method) => Ok(Value::closure(slot, method, Vec::new())),
            ExportTarget::Global(global) => {
                let globals = &self.module(slot)?.globals;
                let raw_slot = usize::try_from(global.raw()).unwrap_or(usize::MAX);
                globals.get(raw_slot).cloned().ok_or_else(|| {
                    VmError::new(VmErrorKind::GlobalOutOfBounds {
                        module: module_name,
                        slot: raw_slot,
                        len: globals.len(),
                    })
                })
            }
            ExportTarget::Foreign(foreign) => Ok(Value::foreign(slot, foreign)),
            ExportTarget::Type(ty) => Ok(Value::Type(ty)),
            ExportTarget::Effect(effect) => Ok(Value::Effect(effect)),
            ExportTarget::Class(class) => Ok(Value::Class(class)),
        }
    }

    pub(crate) fn expect_module_slot(module: &Value) -> VmResult<usize> {
        match module {
            Value::Module(module) => Ok(module.slot),
            _ => Err(VmError::new(VmErrorKind::InvalidValueKind {
                expected: VmValueKind::Module,
                found: module.kind(),
            })),
        }
    }

    pub(crate) fn module(&self, slot: usize) -> VmResult<&LoadedModule> {
        self.loaded_modules.get(slot).ok_or_else(|| {
            VmError::new(VmErrorKind::ModuleSlotOutOfBounds {
                slot,
                len: self.loaded_modules.len(),
            })
        })
    }

    pub(crate) fn module_mut(&mut self, slot: usize) -> VmResult<&mut LoadedModule> {
        let len = self.loaded_modules.len();
        self.loaded_modules
            .get_mut(slot)
            .ok_or_else(|| VmError::new(VmErrorKind::ModuleSlotOutOfBounds { slot, len }))
    }

    pub(crate) fn current_module_slot(&self) -> VmResult<usize> {
        self.frames
            .last()
            .map(|frame| frame.module_slot)
            .ok_or_else(|| VmError::new(VmErrorKind::EmptyCallFrameStack))
    }

    pub(crate) fn load_dynamic_module(&mut self, spec: &str) -> VmResult<usize> {
        if let Some(slot) = self.module_slots.get(spec).copied() {
            self.initialize_slot(slot)?;
            return Ok(slot);
        }
        let program = self.loader.load_program(spec)?;
        let slot = self.loaded_modules.len();
        self.loaded_modules.push(LoadedModule::new(spec, program));
        let _ = self.module_slots.insert(spec.into(), slot);
        self.initialize_slot(slot)?;
        Ok(slot)
    }
}
