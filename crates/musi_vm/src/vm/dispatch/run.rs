use super::*;

impl Vm {
    pub(crate) fn run_current_state(&mut self) -> VmResult<Value> {
        if self.options.instruction_budget.is_some() {
            self.run_current_state_budgeted()
        } else {
            self.run_current_state_unbudgeted()
        }
    }

    fn run_current_state_budgeted(&mut self) -> VmResult<Value> {
        loop {
            let instruction = self.next_runtime_instruction()?;
            match self.execute_runtime_instr(&instruction)? {
                StepOutcome::Continue => {}
                StepOutcome::Return(value) => return Ok(value),
            }
        }
    }

    fn run_current_state_unbudgeted(&mut self) -> VmResult<Value> {
        loop {
            let instruction = self.next_runtime_instruction_unbudgeted()?;
            match self.execute_runtime_instr(&instruction)? {
                StepOutcome::Continue => {}
                StepOutcome::Return(value) => return Ok(value),
            }
        }
    }

    pub(crate) fn execute_runtime_instr(
        &mut self,
        runtime: &RuntimeInstruction,
    ) -> VmResult<StepOutcome> {
        if self.optimized_dispatch_enabled() {
            if let Some(fused) = runtime.fused {
                return self.exec_fused(fused);
            }
            if let Some((compare, target)) = runtime.compare_branch {
                return self.exec_compare_branch(compare, target);
            }
        }
        match runtime.opcode {
            Opcode::LdLoc => self.exec_fast_ldloc(runtime),
            Opcode::StLoc => self.exec_fast_stloc(runtime),
            Opcode::LdGlob => self.exec_fast_ldglob(runtime),
            Opcode::StGlob => self.exec_fast_stglob(runtime),
            Opcode::LdC => self.exec_fast_ldconst(runtime),
            Opcode::LdCI4 => self.exec_fast_ldsmi(runtime),
            Opcode::Add => self.exec_fast_int_op(runtime, i64::checked_add),
            Opcode::Sub => self.exec_fast_int_op(runtime, i64::checked_sub),
            Opcode::Mul => self.exec_fast_int_op(runtime, i64::checked_mul),
            Opcode::DivS => self.exec_fast_int_op(runtime, i64::checked_div),
            Opcode::RemS => self.exec_fast_int_op(runtime, i64::checked_rem),
            Opcode::Br => self.exec_fast_br(runtime),
            Opcode::BrFalse => self.exec_fast_brfalse(runtime),
            Opcode::Call => self.execute_runtime_call(runtime),
            Opcode::TailCall => self.exec_fast_tail_call(runtime),
            Opcode::CallInd | Opcode::NewFn => self.exec_fast_call_edge(runtime),
            Opcode::Ret => self.return_from_frame(),
            Opcode::NewArr | Opcode::LdElem | Opcode::StElem => self.exec_fast_seq(runtime),
            Opcode::NewObj | Opcode::LdFld | Opcode::StFld => self.exec_fast_data(runtime),
            Opcode::HdlPush | Opcode::HdlPop | Opcode::Raise | Opcode::Resume => {
                self.exec_fast_effect(runtime)
            }
            _ => {
                let instruction =
                    if let Some(instruction) = runtime.operand.to_instruction(runtime.opcode) {
                        instruction
                    } else {
                        self.current_raw_instruction(runtime.raw_index)?
                    };
                self.execute_instr(&instruction)
            }
        }
    }

    fn execute_runtime_call(&mut self, runtime: &RuntimeInstruction) -> VmResult<StepOutcome> {
        if let RuntimeOperand::Procedure(_) = runtime.operand {
            self.exec_fast_call(runtime)
        } else {
            let instruction = self.current_raw_instruction(runtime.raw_index)?;
            self.execute_instr(&instruction)
        }
    }

    pub(crate) fn execute_instr(&mut self, instruction: &Instruction) -> VmResult<StepOutcome> {
        match instruction.opcode {
            Opcode::LdLoc
            | Opcode::StLoc
            | Opcode::LdGlob
            | Opcode::StGlob
            | Opcode::LdC
            | Opcode::LdCI4
            | Opcode::LdStr => self.exec_load_store(instruction),
            Opcode::Add
            | Opcode::Sub
            | Opcode::Mul
            | Opcode::DivS
            | Opcode::RemS
            | Opcode::Ceq
            | Opcode::Cne
            | Opcode::CltS
            | Opcode::CgtS
            | Opcode::CleS
            | Opcode::CgeS
            | Opcode::And
            | Opcode::Or
            | Opcode::Xor
            | Opcode::Not => self.exec_scalar(instruction),
            Opcode::Br | Opcode::BrFalse | Opcode::BrTbl => self.exec_branch(instruction),
            Opcode::Call if matches!(instruction.operand, music_seam::Operand::I16(_)) => {
                self.exec_type(instruction)
            }
            Opcode::Call | Opcode::CallInd | Opcode::TailCall | Opcode::Ret | Opcode::NewFn => {
                self.exec_call(instruction)
            }
            Opcode::NewArr | Opcode::LdElem | Opcode::StElem | Opcode::LdLen => {
                self.exec_seq(instruction)
            }
            Opcode::NewObj | Opcode::LdFld | Opcode::StFld => self.exec_data(instruction),
            Opcode::LdType | Opcode::IsInst | Opcode::Cast => self.exec_type(instruction),
            Opcode::HdlPush | Opcode::HdlPop | Opcode::Raise | Opcode::Resume => {
                self.exec_effect(instruction)
            }
            Opcode::CallFfi | Opcode::LdFfi | Opcode::MdlLoad | Opcode::MdlGet => {
                self.exec_host_edge(instruction)
            }
            _ => Err(Self::invalid_dispatch(instruction, "general")),
        }
    }

    const fn optimized_dispatch_enabled(&self) -> bool {
        self.options.features.has_fused_dispatch()
            && !matches!(self.options.mode, MvmMode::DebugInterpreter)
    }
}
