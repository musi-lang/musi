use std::mem;

use music_seam::{Instruction, Opcode, ProcedureId};

use super::state::{CallFrame, CallFrameList, EffectHandler, EffectHandlerList, StepOutcome};
use super::{GcRef, OperandShape, Value, ValueList, VmError, VmErrorKind, VmResult};

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
        continuation: GcRef,
        value: Value,
    ) -> VmResult<Value> {
        let saved_frames = mem::take(&mut self.frames);
        let saved_handlers = mem::take(&mut self.handlers);
        let saved_active_resumes = mem::take(&mut self.active_resumes);
        let saved_target = self.continuation_target_handler;

        let continuation = self.heap.continuation(continuation)?.clone();
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
        match instruction.opcode {
            Opcode::LdLoc
            | Opcode::StLoc
            | Opcode::LdGlob
            | Opcode::StGlob
            | Opcode::LdConst
            | Opcode::LdSmi
            | Opcode::LdStr => self.exec_load_store(instruction),
            Opcode::IAdd
            | Opcode::ISub
            | Opcode::IMul
            | Opcode::IDiv
            | Opcode::IRem
            | Opcode::FAdd
            | Opcode::FSub
            | Opcode::FMul
            | Opcode::FDiv
            | Opcode::FRem
            | Opcode::StrCat
            | Opcode::CmpEq
            | Opcode::CmpNe
            | Opcode::CmpLt
            | Opcode::CmpGt
            | Opcode::CmpLe
            | Opcode::CmpGe => self.exec_scalar(instruction),
            Opcode::Br | Opcode::BrFalse | Opcode::BrTbl => self.exec_branch(instruction),
            Opcode::Call
            | Opcode::CallSeq
            | Opcode::CallCls
            | Opcode::CallClsSeq
            | Opcode::CallTail
            | Opcode::Ret
            | Opcode::ClsNew => self.exec_call(instruction),
            Opcode::SeqNew
            | Opcode::SeqGet
            | Opcode::SeqGetN
            | Opcode::SeqSet
            | Opcode::SeqSetN
            | Opcode::SeqCat
            | Opcode::SeqLen
            | Opcode::RangeNew
            | Opcode::RangeContains
            | Opcode::RangeMaterialize
            | Opcode::SeqHas => self.exec_seq(instruction),
            Opcode::DataNew | Opcode::DataTag | Opcode::DataGet | Opcode::DataSet => {
                self.exec_data(instruction)
            }
            Opcode::TyId | Opcode::TyApply | Opcode::TyChk | Opcode::TyCast => {
                self.exec_type(instruction)
            }
            Opcode::HdlPush
            | Opcode::HdlPop
            | Opcode::EffInvk
            | Opcode::EffInvkSeq
            | Opcode::EffResume => self.exec_effect(instruction),
            Opcode::FfiCall
            | Opcode::FfiCallSeq
            | Opcode::FfiRef
            | Opcode::ModLoad
            | Opcode::ModGet => self.exec_host_edge(instruction),
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
