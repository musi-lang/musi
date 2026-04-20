use crate::{VmIndexSpace, VmStackKind};
use music_seam::descriptor::ExportTarget;
use music_seam::{GlobalId, Instruction, Opcode, Operand, ProcedureId};

use super::{Program, Value, ValueList, VmError, VmErrorKind, VmResult, VmValueKind};

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
                module.program.entry_procedure(),
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
        let result = entry.map_or(Ok(()), |entry| self.initialize_entry(slot, entry));
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

    fn initialize_entry(&mut self, slot: usize, entry: ProcedureId) -> VmResult {
        if self.try_initialize_simple_globals(slot, entry)? {
            return Ok(());
        }
        let base_depth = self.frames.len();
        self.invoke_procedure_in_context(slot, entry, ValueList::new(), base_depth)
            .map(|_| ())
    }

    fn try_initialize_simple_globals(&mut self, slot: usize, entry: ProcedureId) -> VmResult<bool> {
        let assignments = {
            let program = &self.module(slot)?.program;
            simple_global_assignments(program, entry)
        };
        let Some(assignments) = assignments else {
            return Ok(false);
        };
        let globals = &mut self.module_mut(slot)?.globals;
        for (global, value) in assignments {
            let raw_slot = usize::try_from(global.raw()).unwrap_or(usize::MAX);
            let len = globals.len();
            let Some(global) = globals.get_mut(raw_slot) else {
                return Err(VmError::new(VmErrorKind::IndexOutOfBounds {
                    space: VmIndexSpace::Global,
                    owner: None,
                    index: i64::try_from(raw_slot).unwrap_or(i64::MAX),
                    len,
                }));
            };
            *global = Value::Int(value);
        }
        Ok(true)
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
            ExportTarget::Class(class) => Ok(Value::Class(class)),
        }
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
        self.loaded_modules.push(LoadedModule::new(spec, program));
        let _ = self
            .module_slots
            .get_or_insert_with(Default::default)
            .insert(spec.into(), slot);
        self.initialize_slot(slot)?;
        Ok(slot)
    }
}

fn simple_global_assignments(
    program: &Program,
    procedure: ProcedureId,
) -> Option<Vec<(GlobalId, i64)>> {
    let procedure = program.loaded_procedure(procedure).ok()?;
    if let Some(assignment) = simple_global_init_assignment(&procedure.instructions) {
        return Some(vec![assignment]);
    }
    let mut assignments = Vec::new();
    let mut chunks = procedure.instructions.chunks_exact(2);
    for chunk in &mut chunks {
        let [call, store] = chunk else {
            return None;
        };
        let (Opcode::Call, Operand::Procedure(callee)) = (call.opcode, &call.operand) else {
            return None;
        };
        if !matches!(
            (store.opcode, &store.operand),
            (Opcode::StLoc, Operand::Local(_))
        ) {
            return None;
        }
        assignments.extend(simple_global_assignments(program, *callee)?);
    }
    let [ret] = chunks.remainder() else {
        return None;
    };
    (ret.opcode == Opcode::Ret).then_some(assignments)
}

fn simple_global_init_assignment(instructions: &[Instruction]) -> Option<(GlobalId, i64)> {
    let [load, store, unit, ret] = instructions else {
        return None;
    };
    let (Opcode::LdSmi, Operand::I16(value)) = (load.opcode, &load.operand) else {
        return None;
    };
    let (Opcode::StGlob, Operand::Global(global)) = (store.opcode, &store.operand) else {
        return None;
    };
    if matches!(
        (unit.opcode, &unit.operand),
        (Opcode::LdSmi, Operand::I16(0))
    ) && ret.opcode == Opcode::Ret
    {
        Some((*global, i64::from(*value)))
    } else {
        None
    }
}
