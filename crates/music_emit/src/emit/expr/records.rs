use std::collections::HashMap;

use super::super::*;
use crate::EmitDiagKind;
use music_base::diag::DiagContext;

impl ProcedureEmitter<'_, '_> {
    pub(super) fn compile_record_literal(
        &mut self,
        ty_name: &str,
        field_count: u16,
        fields: &[IrRecordField],
        diags: &mut EmitDiagList,
    ) {
        let field_count_usize = usize::from(field_count);
        let mut field_slots = vec![None::<u16>; field_count_usize];
        let missing_origin = fields.first().map_or_else(
            || IrOrigin {
                source_id: SourceId::from_raw(0),
                span: Span::new(0, 0),
            },
            |field| field.expr.origin,
        );

        for field in fields {
            self.compile_expr(&field.expr, true, diags);
            let slot = Self::reserve_temp_slot(self);
            self.code.push(CodeEntry::Instruction(Instruction::new(
                Opcode::StLoc,
                Operand::Local(slot),
            )));
            if let Some(entry) = field_slots.get_mut(usize::from(field.index)) {
                *entry = Some(slot);
            }
        }

        let Some(ty) = self.layout.types.get(ty_name).copied() else {
            super::support::push_expr_diag_with(
                diags,
                self.module_key,
                &missing_origin,
                EmitDiagKind::UnknownRecordType,
                DiagContext::new().with("type", ty_name),
            );
            emit_zero(self);
            return;
        };

        for (index, slot) in field_slots.iter().enumerate() {
            let Some(slot) = slot else {
                super::support::push_expr_diag_with(
                    diags,
                    self.module_key,
                    &missing_origin,
                    EmitDiagKind::RecordLiteralMissingFieldValue,
                    DiagContext::new().with("field", index),
                );
                emit_zero(self);
                continue;
            };
            self.code.push(CodeEntry::Instruction(Instruction::new(
                Opcode::LdLoc,
                Operand::Local(*slot),
            )));
        }

        self.compile_i64(0);
        self.code.push(CodeEntry::Instruction(Instruction::new(
            Opcode::NewObj,
            Operand::TypeLen {
                ty,
                len: field_count,
            },
        )));
    }

    pub(super) fn compile_record_get(
        &mut self,
        base: &IrExpr,
        index: u16,
        diags: &mut EmitDiagList,
    ) {
        self.compile_expr(base, true, diags);
        self.compile_i64(i64::from(index));
        self.code.push(CodeEntry::Instruction(Instruction::new(
            Opcode::LdFld,
            Operand::None,
        )));
    }

    pub(super) fn compile_record_update(
        &mut self,
        input: RecordUpdateInput<'_>,
        diags: &mut EmitDiagList,
    ) {
        let Some(ty) = self.layout.types.get(input.ty_name).copied() else {
            super::support::push_expr_diag_with(
                diags,
                self.module_key,
                &input.base.origin,
                EmitDiagKind::UnknownRecordType,
                DiagContext::new().with("type", input.ty_name),
            );
            emit_zero(self);
            return;
        };

        let base_slot = Self::reserve_temp_slot(self);
        self.compile_expr(input.base, true, diags);
        self.code.push(CodeEntry::Instruction(Instruction::new(
            Opcode::StLoc,
            Operand::Local(base_slot),
        )));

        let field_count_usize = usize::from(input.field_count);
        let mut update_slots = vec![None::<u16>; field_count_usize];
        for update in input.updates {
            self.compile_expr(&update.expr, true, diags);
            let slot = Self::reserve_temp_slot(self);
            self.code.push(CodeEntry::Instruction(Instruction::new(
                Opcode::StLoc,
                Operand::Local(slot),
            )));
            if let Some(entry) = update_slots.get_mut(usize::from(update.index)) {
                *entry = Some(slot);
            }
        }

        let mut base_index_by_name = HashMap::<&str, u16>::new();
        for field in input.base_fields {
            let _ = base_index_by_name.insert(field.name.as_ref(), field.index);
        }

        for field in input.result_fields {
            if let Some(slot) = update_slots
                .get(usize::from(field.index))
                .and_then(|slot| *slot)
            {
                self.code.push(CodeEntry::Instruction(Instruction::new(
                    Opcode::LdLoc,
                    Operand::Local(slot),
                )));
                continue;
            }
            let Some(base_index) = base_index_by_name.get(field.name.as_ref()).copied() else {
                super::support::push_expr_diag_with(
                    diags,
                    self.module_key,
                    &input.base.origin,
                    EmitDiagKind::RecordUpdateMissingFieldValue,
                    DiagContext::new().with("field", field.name.as_ref()),
                );
                emit_zero(self);
                continue;
            };
            self.code.push(CodeEntry::Instruction(Instruction::new(
                Opcode::LdLoc,
                Operand::Local(base_slot),
            )));
            self.compile_i64(i64::from(base_index));
            self.code.push(CodeEntry::Instruction(Instruction::new(
                Opcode::LdFld,
                Operand::None,
            )));
        }

        self.compile_i64(0);
        self.code.push(CodeEntry::Instruction(Instruction::new(
            Opcode::NewObj,
            Operand::TypeLen {
                ty,
                len: input.field_count,
            },
        )));
    }
}
