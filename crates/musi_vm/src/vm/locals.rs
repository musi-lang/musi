use crate::{VmIndexSpace, VmStackKind};

use super::{Value, Vm, VmError, VmErrorKind, VmResult};

impl Vm {
    fn checked_local_slot(&self, slot: u16) -> VmResult<usize> {
        let Some(frame) = self.frames.last() else {
            return Err(VmError::new(VmErrorKind::StackEmpty {
                stack: VmStackKind::CallFrame,
            }));
        };
        let index = usize::from(slot);
        if index < frame.locals.len() {
            Ok(index)
        } else {
            Err(VmError::new(VmErrorKind::IndexOutOfBounds {
                space: VmIndexSpace::Local,
                owner: None,
                index: i64::from(slot),
                len: frame.locals.len(),
            }))
        }
    }

    pub(crate) fn local(&self, slot: u16) -> VmResult<&Value> {
        let index = self.checked_local_slot(slot)?;
        Ok(&self.frames.last().expect("checked local frame").locals[index])
    }

    pub(crate) fn local_mut(&mut self, slot: u16) -> VmResult<&mut Value> {
        let index = self.checked_local_slot(slot)?;
        Ok(&mut self.frames.last_mut().expect("checked local frame").locals[index])
    }

    pub(crate) fn jump_to(&mut self, label: u16) -> VmResult {
        let (module_slot, procedure) = {
            let frame = self.frames.last().ok_or_else(|| {
                VmError::new(VmErrorKind::StackEmpty {
                    stack: VmStackKind::CallFrame,
                })
            })?;
            (frame.module_slot, frame.procedure)
        };
        let loaded_procedure = self
            .module(module_slot)?
            .program
            .loaded_procedure(procedure)?;
        let ip = *loaded_procedure.labels.get(&label).ok_or_else(|| {
            VmError::new(VmErrorKind::InvalidBranchTarget {
                procedure: loaded_procedure.name.clone(),
                label: Some(label),
                index: None,
                len: None,
            })
        })?;
        let frame = self.frames.last_mut().ok_or_else(|| {
            VmError::new(VmErrorKind::StackEmpty {
                stack: VmStackKind::CallFrame,
            })
        })?;
        frame.ip = ip;
        Ok(())
    }
}
