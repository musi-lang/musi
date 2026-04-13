use super::super::*;
use crate::EmitDiagKind;

impl MethodEmitter<'_, '_> {
    pub(super) fn compile_let(
        &mut self,
        binding: Option<NameBindingId>,
        _name: &str,
        value: &IrExpr,
        diags: &mut EmitDiagList,
    ) {
        self.compile_expr(value, true, diags);
        if let Some(binding) = binding {
            let slot = self.ensure_local_slot(binding);
            self.code.push(CodeEntry::Instruction(Instruction::new(
                Opcode::StLoc,
                Operand::Local(slot),
            )));
        } else {
            let slot = Self::scratch_slot(self);
            self.code.push(CodeEntry::Instruction(Instruction::new(
                Opcode::StLoc,
                Operand::Local(slot),
            )));
        }
        emit_zero(self);
    }

    pub(super) fn compile_temp(&mut self, temp: IrTempId) {
        let slot = self.ensure_temp_slot(temp);
        self.code.push(CodeEntry::Instruction(Instruction::new(
            Opcode::LdLoc,
            Operand::Local(slot),
        )));
    }

    pub(super) fn compile_temp_let(
        &mut self,
        temp: IrTempId,
        value: &IrExpr,
        diags: &mut EmitDiagList,
    ) {
        self.compile_expr(value, true, diags);
        let slot = self.ensure_temp_slot(temp);
        self.code.push(CodeEntry::Instruction(Instruction::new(
            Opcode::StLoc,
            Operand::Local(slot),
        )));
        emit_zero(self);
    }

    pub(super) fn compile_assign(
        &mut self,
        target: &IrAssignTarget,
        value: &IrExpr,
        diags: &mut EmitDiagList,
    ) {
        match target {
            IrAssignTarget::Binding {
                binding,
                name,
                module_target,
            } => {
                self.compile_expr(value, true, diags);
                if let Some(binding) = binding
                    && let Some(slot) = self.locals.get(binding).copied()
                {
                    self.code.push(CodeEntry::Instruction(Instruction::new(
                        Opcode::StLoc,
                        Operand::Local(slot),
                    )));
                    emit_zero(self);
                    return;
                }
                if let Some(slot) = self.synthetic_locals.get(name).copied() {
                    self.code.push(CodeEntry::Instruction(Instruction::new(
                        Opcode::StLoc,
                        Operand::Local(slot),
                    )));
                    emit_zero(self);
                    return;
                }
                if let Some(global) = self.resolve_global(*binding, name, module_target.as_ref()) {
                    self.code.push(CodeEntry::Instruction(Instruction::new(
                        Opcode::StGlob,
                        Operand::Global(global),
                    )));
                    emit_zero(self);
                    return;
                }
                super::support::push_expr_diag(
                    diags,
                    self.module_key,
                    &value.origin,
                    EmitDiagKind::UnsupportedAssignTarget,
                    format!("assignment target `{name}` has no emitted form"),
                );
                emit_zero(self);
            }
            IrAssignTarget::RecordField { base, index } => {
                self.compile_expr(base, true, diags);
                self.compile_i64(i64::from(*index));
                self.compile_expr(value, true, diags);
                self.code.push(CodeEntry::Instruction(Instruction::new(
                    Opcode::DataSet,
                    Operand::None,
                )));
                emit_zero(self);
            }
            IrAssignTarget::Index { base, indices } => {
                self.compile_expr(base, true, diags);
                for index in indices {
                    self.compile_expr(index, true, diags);
                }
                self.compile_expr(value, true, diags);
                let instruction = match indices.as_ref() {
                    [_] => Instruction::new(Opcode::SeqSet, Operand::None),
                    _ => Instruction::new(
                        Opcode::SeqSetN,
                        Operand::I16(i16::try_from(indices.len()).unwrap_or(i16::MAX)),
                    ),
                };
                self.code.push(CodeEntry::Instruction(instruction));
                emit_zero(self);
            }
        }
    }
}
