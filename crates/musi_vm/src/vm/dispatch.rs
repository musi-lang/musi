use std::mem;

use music_seam::{Instruction, ProcedureId};

use super::state::{CallFrame, CallFrameList, EffectHandler, EffectHandlerList, StepOutcome};
use super::{
    ContinuationValuePtr, OperandShape, Value, ValueList, VmError, VmErrorKind, VmOpcodeFamily,
    VmResult, classify_opcode,
};

use super::Vm;

impl Vm {
    pub(crate) fn invoke_procedure(
        &mut self,
        module_slot: usize,
        procedure: ProcedureId,
        args: ValueList,
    ) -> VmResult<Value> {
        self.push_frame(module_slot, procedure, args)?;
        self.run_current_state()
    }

    pub(crate) fn invoke_procedure_in_context(
        &mut self,
        module_slot: usize,
        procedure: ProcedureId,
        args: ValueList,
        base_depth: usize,
    ) -> VmResult<Value> {
        self.push_frame(module_slot, procedure, args)?;
        loop {
            let instruction = self.next_instruction()?;
            match self.execute_instr(&instruction)? {
                StepOutcome::Return(value) => return Ok(value),
                StepOutcome::Continue if self.frames.len() == base_depth => {
                    return self.pop_value();
                }
                StepOutcome::Continue => {}
            }
        }
    }

    pub(crate) fn invoke_continuation(
        &mut self,
        continuation: &ContinuationValuePtr,
        value: Value,
    ) -> VmResult<Value> {
        let saved_frames = mem::take(&mut self.frames);
        let saved_handlers = mem::take(&mut self.handlers);
        let saved_active_resumes = mem::take(&mut self.active_resumes);
        let saved_target = self.continuation_target_handler;

        let frames = continuation
            .frames
            .iter()
            .cloned()
            .map(CallFrame::from)
            .collect::<CallFrameList>();
        let handlers = continuation
            .handlers
            .iter()
            .cloned()
            .map(EffectHandler::from)
            .collect::<EffectHandlerList>();
        let target_handler = continuation
            .handlers
            .first()
            .map(|handler| handler.handler_id);

        self.frames = frames;
        self.handlers = handlers;
        self.active_resumes = Vec::new();
        self.continuation_target_handler = target_handler;
        let result = match self.frames.last_mut() {
            Some(frame) => {
                frame.stack.push(value);
                self.run_current_state()
            }
            None => Err(VmError::new(VmErrorKind::InvalidProgramShape {
                detail: "continuation frame list is empty".into(),
            })),
        };

        self.frames = saved_frames;
        self.handlers = saved_handlers;
        self.active_resumes = saved_active_resumes;
        self.continuation_target_handler = saved_target;
        result
    }

    pub(crate) fn run_current_state(&mut self) -> VmResult<Value> {
        loop {
            let instruction = self.next_instruction()?;
            match self.execute_instr(&instruction)? {
                StepOutcome::Continue => {}
                StepOutcome::Return(value) => return Ok(value),
            }
        }
    }

    pub(crate) fn execute_instr(&mut self, instruction: &Instruction) -> VmResult<StepOutcome> {
        match classify_opcode(instruction.opcode) {
            VmOpcodeFamily::LoadStore => self.exec_load_store(instruction),
            VmOpcodeFamily::Scalar => self.exec_scalar(instruction),
            VmOpcodeFamily::Branch => self.exec_branch(instruction),
            VmOpcodeFamily::Call => self.exec_call(instruction),
            VmOpcodeFamily::Sequence => self.exec_seq(instruction),
            VmOpcodeFamily::Data => self.exec_data(instruction),
            VmOpcodeFamily::Types => self.exec_type(instruction),
            VmOpcodeFamily::Effects => self.exec_effect(instruction),
            VmOpcodeFamily::Host => self.exec_host_edge(instruction),
        }
    }

    pub(crate) fn invalid_operand(instruction: &Instruction) -> VmError {
        VmError::new(VmErrorKind::InvalidOperandForOpcode {
            opcode: instruction.opcode,
            found: OperandShape::from(&instruction.operand),
        })
    }

    pub(crate) fn invalid_dispatch(instruction: &Instruction, family: &str) -> VmError {
        VmError::new(VmErrorKind::InvalidProgramShape {
            detail: format!(
                "opcode `{}` reached `{family}` executor",
                instruction.opcode.mnemonic()
            )
            .into(),
        })
    }
}
