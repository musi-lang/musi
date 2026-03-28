use super::*;

impl Vm {
    pub(super) fn dispatch_branch(
        &mut self,
        op: Opcode,
        method_idx: usize,
        pc: &mut usize,
    ) -> VmResult {
        match op {
            Opcode::BrTrue | Opcode::BrFalse => {
                let offset = self.read_i16(method_idx, pc);
                let cond = self
                    .frames
                    .last_mut()
                    .ok_or(VmError::StackUnderflow)?
                    .pop()?;
                if !cond.is_bool() {
                    return Err(VmError::TypeError {
                        expected: "Bool",
                        found: "non-Bool",
                    });
                }
                let take = cond.as_bool() == (op == Opcode::BrTrue);
                if take {
                    *pc = pc.wrapping_add_signed(isize::from(offset));
                }
            }
            Opcode::BrJmp | Opcode::BrBack => {
                let offset = self.read_i16(method_idx, pc);
                *pc = pc.wrapping_add_signed(isize::from(offset));
            }
            Opcode::BrTbl => {
                let count = usize::from(self.read_u16(method_idx, pc));
                let base_pc = *pc;
                *pc = pc.wrapping_add(count * 2);
                let idx_val = self
                    .frames
                    .last_mut()
                    .ok_or(VmError::StackUnderflow)?
                    .pop()?;
                if !idx_val.is_int() {
                    return Err(VmError::TypeError {
                        expected: "Int",
                        found: "non-Int",
                    });
                }
                let idx = idx_val.as_int();
                if let Ok(i) = usize::try_from(idx) {
                    if i < count {
                        let off_pos = base_pc + i * 2;
                        let code = unsafe {
                            &self.program.module().methods.get_unchecked(method_idx).code
                        };
                        let lo = unsafe { *code.get_unchecked(off_pos) };
                        let hi = unsafe { *code.get_unchecked(off_pos + 1) };
                        let offset = i16::from_le_bytes([lo, hi]);
                        *pc = pc.wrapping_add_signed(isize::from(offset));
                    }
                }
            }
            _ => return Err(VmError::UnsupportedOpcode(op)),
        }
        Ok(())
    }
}
