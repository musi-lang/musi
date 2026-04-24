use crate::VmStackKind;
use music_seam::{Instruction, ProcedureId};

use super::state::CallFrame;
use super::{
    RuntimeInstruction, RuntimeInstructionList, ValueList, Vm, VmError, VmErrorKind, VmResult,
};

impl Vm {
    pub(crate) fn next_runtime_instruction(&mut self) -> VmResult<RuntimeInstruction> {
        self.before_instruction()?;
        self.next_runtime_instruction_inner()
    }

    pub(crate) fn next_runtime_instruction_unbudgeted(&mut self) -> VmResult<RuntimeInstruction> {
        self.count_instruction();
        self.next_runtime_instruction_inner()
    }

    fn next_runtime_instruction_inner(&mut self) -> VmResult<RuntimeInstruction> {
        let frame = self.frames.last_mut().ok_or_else(|| {
            VmError::new(VmErrorKind::StackEmpty {
                stack: VmStackKind::CallFrame,
            })
        })?;
        let Some(instruction) = frame.next_runtime_instruction_cached() else {
            return Err(VmError::new(VmErrorKind::InvalidBranchTarget {
                procedure: Box::from("<runtime>"),
                label: Some(u16::MAX),
                index: None,
                len: None,
            }));
        };
        Ok(instruction)
    }

    pub(crate) fn jump_to_ip(&mut self, ip: usize) -> VmResult {
        let frame = self.frames.last_mut().ok_or_else(|| {
            VmError::new(VmErrorKind::StackEmpty {
                stack: VmStackKind::CallFrame,
            })
        })?;
        frame.set_ip(ip);
        Ok(())
    }

    pub(crate) fn skip_next_instruction(&mut self) -> VmResult {
        let frame = self.frames.last_mut().ok_or_else(|| {
            VmError::new(VmErrorKind::StackEmpty {
                stack: VmStackKind::CallFrame,
            })
        })?;
        frame.advance_ip();
        Ok(())
    }

    pub(crate) fn current_raw_instruction(&self, raw_index: usize) -> VmResult<Instruction> {
        let frame = self.frames.last().ok_or_else(|| {
            VmError::new(VmErrorKind::StackEmpty {
                stack: VmStackKind::CallFrame,
            })
        })?;
        let loaded_procedure = self
            .module(frame.module_slot)?
            .program
            .loaded_procedure(frame.procedure)?;
        loaded_procedure
            .instructions
            .get(raw_index)
            .cloned()
            .ok_or_else(|| {
                VmError::new(VmErrorKind::InvalidBranchTarget {
                    procedure: loaded_procedure.name.clone(),
                    label: Some(u16::MAX),
                    index: None,
                    len: None,
                })
            })
    }

    fn runtime_code(
        &self,
        module_slot: usize,
        procedure: ProcedureId,
    ) -> VmResult<RuntimeInstructionList> {
        self.module(module_slot)?
            .program
            .loaded_runtime_code(procedure)
    }

    pub(crate) fn refresh_frame_runtime_codes(&mut self) -> VmResult {
        let codes = self
            .frames
            .iter()
            .map(|frame| self.runtime_code(frame.module_slot, frame.procedure))
            .collect::<VmResult<Vec<_>>>()?;
        for (frame, code) in self.frames.iter_mut().zip(codes) {
            frame.set_runtime_code(code);
        }
        Ok(())
    }

    pub(super) fn call_frame(
        &mut self,
        module_slot: usize,
        procedure: ProcedureId,
        locals: ValueList,
        stack: ValueList,
    ) -> VmResult<CallFrame> {
        let mut frame = self.empty_call_frame(module_slot, procedure)?;
        frame.locals.extend(locals);
        frame.stack.extend(stack);
        Ok(frame)
    }

    pub(super) fn empty_call_frame(
        &mut self,
        module_slot: usize,
        procedure: ProcedureId,
    ) -> VmResult<CallFrame> {
        let code = self.runtime_code(module_slot, procedure)?;
        if let Some(mut frame) = self.spare_frames.pop() {
            frame.reset_empty(module_slot, procedure, code);
            Ok(frame)
        } else {
            Ok(
                CallFrame::new(module_slot, procedure, ValueList::new(), ValueList::new())
                    .with_runtime_code(code),
            )
        }
    }
}
