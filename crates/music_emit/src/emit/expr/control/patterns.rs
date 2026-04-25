use crate::api::EmitDiagList;
use crate::emit::ProcedureEmitter;
use music_base::{SourceId, Span};
use music_ir::{DefinitionKey, IrCasePattern, IrCaseRecordField, IrLit, IrOrigin};
use music_names::NameBindingId;
use music_seam::{CodeEntry, Instruction, Opcode, Operand};

impl ProcedureEmitter<'_, '_> {
    pub(super) fn compile_case_pattern(
        &mut self,
        pattern: &IrCasePattern,
        scrutinee_slot: u16,
        next_label: u16,
        diags: &mut EmitDiagList,
    ) -> bool {
        match pattern {
            IrCasePattern::Wildcard => true,
            IrCasePattern::Bind { binding, .. } => {
                self.store_binding_value(scrutinee_slot, *binding);
                true
            }
            IrCasePattern::Lit(lit) => {
                self.compile_case_lit(scrutinee_slot, lit, next_label, diags)
            }
            IrCasePattern::Tuple { items } | IrCasePattern::Array { items } => {
                self.compile_sequence_patterns(scrutinee_slot, items, next_label, diags)
            }
            IrCasePattern::Record { fields } => {
                self.compile_record_patterns(scrutinee_slot, fields, next_label, diags)
            }
            IrCasePattern::Variant {
                data_key,
                tag_value,
                args,
                ..
            } => self.compile_case_variant_pattern(
                scrutinee_slot,
                data_key,
                *tag_value,
                args,
                next_label,
                diags,
            ),
            IrCasePattern::As { pat, binding, .. } => {
                if !self.compile_case_pattern(pat, scrutinee_slot, next_label, diags) {
                    return false;
                }
                self.store_binding_value(scrutinee_slot, *binding);
                true
            }
        }
    }
}

impl ProcedureEmitter<'_, '_> {
    fn compile_case_lit(
        &mut self,
        scrutinee_slot: u16,
        lit: &IrLit,
        next_label: u16,
        diags: &mut EmitDiagList,
    ) -> bool {
        self.code.push(CodeEntry::Instruction(Instruction::new(
            Opcode::LdLoc,
            Operand::Local(scrutinee_slot),
        )));
        self.compile_lit(
            lit,
            &IrOrigin {
                source_id: SourceId::from_raw(0),
                span: Span::new(0, 0),
            },
            diags,
        );
        self.code.push(CodeEntry::Instruction(Instruction::new(
            Opcode::Ceq,
            Operand::None,
        )));
        self.code.push(CodeEntry::Instruction(Instruction::new(
            Opcode::BrFalse,
            Operand::Label(next_label),
        )));
        true
    }
}

impl ProcedureEmitter<'_, '_> {
    fn compile_case_variant_pattern(
        &mut self,
        scrutinee_slot: u16,
        data_key: &DefinitionKey,
        tag_value: i64,
        args: &[IrCasePattern],
        next_label: u16,
        diags: &mut EmitDiagList,
    ) -> bool {
        let Some(data_ty) = self.resolve_variant_data_ty(
            data_key,
            &IrOrigin {
                source_id: SourceId::from_raw(0),
                span: Span::new(0, 0),
            },
            diags,
        ) else {
            return false;
        };
        self.compile_variant_tag_match(scrutinee_slot, data_ty, tag_value, next_label);
        self.compile_variant_payload_patterns(scrutinee_slot, args, None, next_label, diags)
    }
}

impl ProcedureEmitter<'_, '_> {
    fn compile_indexed_item(&mut self, scrutinee_slot: u16, index: usize, opcode: Opcode) -> u16 {
        self.code.push(CodeEntry::Instruction(Instruction::new(
            Opcode::LdLoc,
            Operand::Local(scrutinee_slot),
        )));
        self.compile_i64(i64::try_from(index).unwrap_or(i64::MAX));
        self.code.push(CodeEntry::Instruction(Instruction::new(
            opcode,
            Operand::None,
        )));
        let item_slot = Self::reserve_temp_slot(self);
        self.code.push(CodeEntry::Instruction(Instruction::new(
            Opcode::StLoc,
            Operand::Local(item_slot),
        )));
        item_slot
    }
}

impl ProcedureEmitter<'_, '_> {
    fn compile_sequence_patterns(
        &mut self,
        scrutinee_slot: u16,
        items: &[IrCasePattern],
        next_label: u16,
        diags: &mut EmitDiagList,
    ) -> bool {
        self.code.push(CodeEntry::Instruction(Instruction::new(
            Opcode::LdLoc,
            Operand::Local(scrutinee_slot),
        )));
        self.code.push(CodeEntry::Instruction(Instruction::new(
            Opcode::LdLen,
            Operand::None,
        )));
        self.compile_i64(i64::try_from(items.len()).unwrap_or(i64::MAX));
        self.code.push(CodeEntry::Instruction(Instruction::new(
            Opcode::Ceq,
            Operand::None,
        )));
        self.code.push(CodeEntry::Instruction(Instruction::new(
            Opcode::BrFalse,
            Operand::Label(next_label),
        )));
        self.compile_projected_patterns(scrutinee_slot, items, Opcode::LdElem, next_label, diags)
    }
}

impl ProcedureEmitter<'_, '_> {
    fn compile_projected_patterns(
        &mut self,
        scrutinee_slot: u16,
        items: &[IrCasePattern],
        opcode: Opcode,
        next_label: u16,
        diags: &mut EmitDiagList,
    ) -> bool {
        for (idx, item) in items.iter().enumerate() {
            let item_slot = self.compile_indexed_item(scrutinee_slot, idx, opcode);
            if !self.compile_case_pattern(item, item_slot, next_label, diags) {
                return false;
            }
        }
        true
    }
}

impl ProcedureEmitter<'_, '_> {
    fn compile_record_patterns(
        &mut self,
        scrutinee_slot: u16,
        fields: &[IrCaseRecordField],
        next_label: u16,
        diags: &mut EmitDiagList,
    ) -> bool {
        for field in fields {
            let item_slot = self.compile_record_item(scrutinee_slot, field.index);
            if !self.compile_case_pattern(&field.pat, item_slot, next_label, diags) {
                return false;
            }
        }
        true
    }
}

impl ProcedureEmitter<'_, '_> {
    fn compile_record_item(&mut self, scrutinee_slot: u16, index: u16) -> u16 {
        self.code.push(CodeEntry::Instruction(Instruction::new(
            Opcode::LdLoc,
            Operand::Local(scrutinee_slot),
        )));
        self.compile_i64(i64::from(index));
        self.code.push(CodeEntry::Instruction(Instruction::new(
            Opcode::LdFld,
            Operand::None,
        )));
        let item_slot = Self::reserve_temp_slot(self);
        self.code.push(CodeEntry::Instruction(Instruction::new(
            Opcode::StLoc,
            Operand::Local(item_slot),
        )));
        item_slot
    }
}

impl ProcedureEmitter<'_, '_> {
    pub(super) fn compile_variant_payload_patterns(
        &mut self,
        scrutinee_slot: u16,
        args: &[IrCasePattern],
        as_binding: Option<(NameBindingId, &str)>,
        next_label: u16,
        diags: &mut EmitDiagList,
    ) -> bool {
        if !self.compile_projected_patterns(scrutinee_slot, args, Opcode::LdFld, next_label, diags)
        {
            return false;
        }
        if let Some((binding, _name)) = as_binding {
            self.store_binding_value(scrutinee_slot, binding);
        }
        true
    }
}

impl ProcedureEmitter<'_, '_> {
    fn store_binding_value(&mut self, scrutinee_slot: u16, binding: NameBindingId) {
        let slot = self.ensure_local_slot(binding);
        self.code.push(CodeEntry::Instruction(Instruction::new(
            Opcode::LdLoc,
            Operand::Local(scrutinee_slot),
        )));
        self.code.push(CodeEntry::Instruction(Instruction::new(
            Opcode::StLoc,
            Operand::Local(slot),
        )));
    }
}
