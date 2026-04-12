use std::collections::HashMap;

use super::super::*;
use crate::EmitDiagKind;

use super::literals::compile_i64;
use super::support::{push_expr_diag, reserve_temp_slot};

pub(super) fn compile_record_literal(
    emitter: ExprEmitterMut<'_, '_, '_>,
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
        super::compile_expr(emitter, &field.expr, true, diags);
        let slot = reserve_temp_slot(emitter);
        emitter.code.push(CodeEntry::Instruction(Instruction::new(
            Opcode::StLoc,
            Operand::Local(slot),
        )));
        if let Some(entry) = field_slots.get_mut(usize::from(field.index)) {
            *entry = Some(slot);
        }
    }

    let Some(ty) = emitter.layout.types.get(ty_name).copied() else {
        push_expr_diag(
            diags,
            emitter.module_key,
            &missing_origin,
            &EmitDiagKind::UnknownRecordType,
            format!("unknown emitted record type `{ty_name}`"),
        );
        emit_zero(emitter);
        return;
    };

    for slot in &field_slots {
        let Some(slot) = slot else {
            push_expr_diag(
                diags,
                emitter.module_key,
                &missing_origin,
                &EmitDiagKind::RecordLiteralMissingFieldValue,
                "record literal missing field value",
            );
            emit_zero(emitter);
            continue;
        };
        emitter.code.push(CodeEntry::Instruction(Instruction::new(
            Opcode::LdLoc,
            Operand::Local(*slot),
        )));
    }

    compile_i64(emitter, 0);
    emitter.code.push(CodeEntry::Instruction(Instruction::new(
        Opcode::DataNew,
        Operand::TypeLen {
            ty,
            len: field_count,
        },
    )));
}

pub(super) fn compile_record_get(
    emitter: ExprEmitterMut<'_, '_, '_>,
    base: &IrExpr,
    index: u16,
    diags: &mut EmitDiagList,
) {
    super::compile_expr(emitter, base, true, diags);
    compile_i64(emitter, i64::from(index));
    emitter.code.push(CodeEntry::Instruction(Instruction::new(
        Opcode::DataGet,
        Operand::None,
    )));
}

pub(super) fn compile_record_update(
    emitter: ExprEmitterMut<'_, '_, '_>,
    input: RecordUpdateInput<'_>,
    diags: &mut EmitDiagList,
) {
    let Some(ty) = emitter.layout.types.get(input.ty_name).copied() else {
        push_expr_diag(
            diags,
            emitter.module_key,
            &input.base.origin,
            &EmitDiagKind::UnknownRecordType,
            format!("unknown emitted record type `{}`", input.ty_name),
        );
        emit_zero(emitter);
        return;
    };

    let base_slot = reserve_temp_slot(emitter);
    super::compile_expr(emitter, input.base, true, diags);
    emitter.code.push(CodeEntry::Instruction(Instruction::new(
        Opcode::StLoc,
        Operand::Local(base_slot),
    )));

    let field_count_usize = usize::from(input.field_count);
    let mut update_slots = vec![None::<u16>; field_count_usize];
    for update in input.updates {
        super::compile_expr(emitter, &update.expr, true, diags);
        let slot = reserve_temp_slot(emitter);
        emitter.code.push(CodeEntry::Instruction(Instruction::new(
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
            emitter.code.push(CodeEntry::Instruction(Instruction::new(
                Opcode::LdLoc,
                Operand::Local(slot),
            )));
            continue;
        }
        let Some(base_index) = base_index_by_name.get(field.name.as_ref()).copied() else {
            push_expr_diag(
                diags,
                emitter.module_key,
                &input.base.origin,
                &EmitDiagKind::RecordUpdateMissingFieldValue,
                "record update missing field value",
            );
            emit_zero(emitter);
            continue;
        };
        emitter.code.push(CodeEntry::Instruction(Instruction::new(
            Opcode::LdLoc,
            Operand::Local(base_slot),
        )));
        compile_i64(emitter, i64::from(base_index));
        emitter.code.push(CodeEntry::Instruction(Instruction::new(
            Opcode::DataGet,
            Operand::None,
        )));
    }

    compile_i64(emitter, 0);
    emitter.code.push(CodeEntry::Instruction(Instruction::new(
        Opcode::DataNew,
        Operand::TypeLen {
            ty,
            len: input.field_count,
        },
    )));
}
