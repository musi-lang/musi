use super::*;

mod callable;
mod control;
mod effects;
mod literals;
mod names;
mod records;
mod sequence;
mod storage;
mod support;

use callable::{compile_call, compile_call_seq, compile_closure_new, compile_variant_new};
use control::{compile_binary, compile_case};
use effects::{compile_handle, compile_perform, compile_perform_seq, compile_resume};
use names::compile_name;
use records::{compile_record_get, compile_record_literal, compile_record_update};
use sequence::{compile_array_cat, compile_index, compile_sequence, compile_sequence_literal};
use storage::{compile_assign, compile_let, compile_temp, compile_temp_let};

pub(super) fn compile_expr(
    emitter: &mut MethodEmitter<'_, '_>,
    expr: &IrExpr,
    keep_result: bool,
    diags: &mut EmitDiagList,
) {
    match &expr.kind {
        IrExprKind::Unit => emit_zero(emitter),
        IrExprKind::Name {
            binding,
            name,
            module_target,
            ..
        } => compile_name(emitter, *binding, name, module_target.as_ref(), expr, diags),
        IrExprKind::Temp { temp } => compile_temp(emitter, *temp),
        IrExprKind::Lit(lit) => literals::compile_lit(emitter, lit, &expr.origin, diags),
        IrExprKind::Sequence { exprs } => compile_sequence(emitter, exprs, diags),
        IrExprKind::Tuple { ty_name, items } | IrExprKind::Array { ty_name, items } => {
            compile_sequence_literal(emitter, ty_name, items, diags);
        }
        IrExprKind::ArrayCat { ty_name, parts } => compile_array_cat(emitter, ty_name, parts, diags),
        IrExprKind::Record {
            ty_name,
            field_count,
            fields,
        } => compile_record_literal(emitter, ty_name, *field_count, fields, diags),
        IrExprKind::RecordGet { base, index } => compile_record_get(emitter, base, *index, diags),
        IrExprKind::RecordUpdate {
            ty_name,
            field_count,
            base,
            base_fields,
            result_fields,
            updates,
        } => compile_record_update(
            emitter,
            RecordUpdateInput {
                ty_name,
                field_count: *field_count,
                base,
                base_fields,
                result_fields,
                updates,
            },
            diags,
        ),
        IrExprKind::VariantNew {
            data_key,
            tag_index,
            field_count,
            args,
        } => compile_variant_new(emitter, data_key, *tag_index, *field_count, args, diags),
        IrExprKind::Let {
            binding,
            name,
            value,
            ..
        } => compile_let(emitter, *binding, name, value, diags),
        IrExprKind::TempLet { temp, value } => compile_temp_let(emitter, *temp, value, diags),
        IrExprKind::Assign { target, value } => compile_assign(emitter, target, value, diags),
        IrExprKind::Index { base, index } => compile_index(emitter, base, index, diags),
        IrExprKind::ClosureNew { callee, captures } => {
            compile_closure_new(emitter, callee, captures, diags);
        }
        IrExprKind::Binary { op, left, right } => {
            compile_binary(emitter, op, left, right, diags);
        }
        IrExprKind::Not { expr: inner } => {
            compile_expr(emitter, inner, true, diags);
            emit_zero(emitter);
            emitter.code.push(CodeEntry::Instruction(Instruction::new(
                Opcode::CmpEq,
                Operand::None,
            )));
        }
        IrExprKind::TyTest { base, ty_name } => {
            compile_expr(emitter, base, true, diags);
            let Some(ty) = emitter.layout.types.get(ty_name.as_ref()).copied() else {
                support::push_expr_diag(
                    diags,
                    emitter.module_key,
                    &expr.origin,
                    format!("unknown type name `{ty_name}` for `:?`"),
                );
                emit_zero(emitter);
                return;
            };
            emitter.code.push(CodeEntry::Instruction(Instruction::new(
                Opcode::TyChk,
                Operand::Type(ty),
            )));
        }
        IrExprKind::TyCast { base, ty_name } => {
            compile_expr(emitter, base, true, diags);
            let Some(ty) = emitter.layout.types.get(ty_name.as_ref()).copied() else {
                support::push_expr_diag(
                    diags,
                    emitter.module_key,
                    &expr.origin,
                    format!("unknown type name `{ty_name}` for `:?>`"),
                );
                emit_zero(emitter);
                return;
            };
            emitter.code.push(CodeEntry::Instruction(Instruction::new(
                Opcode::TyCast,
                Operand::Type(ty),
            )));
        }
        IrExprKind::Case { scrutinee, arms } => compile_case(emitter, scrutinee, arms, diags),
        IrExprKind::Call { callee, args } => compile_call(emitter, callee, args, diags),
        IrExprKind::CallSeq { callee, args } => compile_call_seq(emitter, callee, args, diags),
        IrExprKind::Perform {
            effect_key,
            op_index,
            args,
        } => compile_perform(emitter, effect_key, *op_index, args, diags),
        IrExprKind::PerformSeq {
            effect_key,
            op_index,
            args,
        } => compile_perform_seq(emitter, effect_key, *op_index, args, diags),
        IrExprKind::Handle {
            effect_key,
            value,
            ops,
            body,
        } => compile_handle(emitter, effect_key, value, ops, body, diags),
        IrExprKind::Resume { expr } => compile_resume(emitter, expr.as_deref(), diags),
        IrExprKind::Unsupported { description } => {
            support::push_expr_diag(
                diags,
                emitter.module_key,
                &expr.origin,
                format!("unsupported emitted expression `{description}`"),
            );
            emit_zero(emitter);
        }
    }
    if !keep_result {
        let slot = support::scratch_slot(emitter);
        emitter.code.push(CodeEntry::Instruction(Instruction::new(
            Opcode::StLoc,
            Operand::Local(slot),
        )));
    }
}
