use super::*;
use crate::EmitDiagKind;

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
use literals::compile_string_constant;
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
    let matched = compile_expr_literal(emitter, expr, diags)
        || compile_expr_sequence_and_data(emitter, expr, diags)
        || compile_expr_storage_ops(emitter, expr, diags)
        || compile_expr_control_ops(emitter, expr, diags)
        || compile_expr_effect_ops(emitter, expr, diags)
        || compile_expr_type_ops(emitter, expr, diags);
    debug_assert!(matched);
    if !keep_result {
        let slot = support::scratch_slot(emitter);
        emitter.code.push(CodeEntry::Instruction(Instruction::new(
            Opcode::StLoc,
            Operand::Local(slot),
        )));
    }
}

fn compile_expr_literal(
    emitter: &mut MethodEmitter<'_, '_>,
    expr: &IrExpr,
    diags: &mut EmitDiagList,
) -> bool {
    match &expr.kind {
        IrExprKind::Unit => {
            emit_zero(emitter);
            true
        }
        IrExprKind::Name {
            binding,
            name,
            module_target,
            ..
        } => {
            compile_name(emitter, *binding, name, module_target.as_ref(), expr, diags);
            true
        }
        IrExprKind::Temp { temp } => {
            compile_temp(emitter, *temp);
            true
        }
        IrExprKind::Lit(lit) => {
            literals::compile_lit(emitter, lit, &expr.origin, diags);
            true
        }
        _ => false,
    }
}

fn compile_expr_sequence_and_data(
    emitter: &mut MethodEmitter<'_, '_>,
    expr: &IrExpr,
    diags: &mut EmitDiagList,
) -> bool {
    match &expr.kind {
        IrExprKind::Sequence { exprs } => {
            compile_sequence(emitter, exprs, diags);
            true
        }
        IrExprKind::Tuple { ty_name, items } | IrExprKind::Array { ty_name, items } => {
            compile_sequence_literal(emitter, ty_name, items, diags);
            true
        }
        IrExprKind::ArrayCat { ty_name, parts } => {
            compile_array_cat(emitter, ty_name, parts, diags);
            true
        }
        IrExprKind::Record {
            ty_name,
            field_count,
            fields,
        } => {
            compile_record_literal(emitter, ty_name, *field_count, fields, diags);
            true
        }
        IrExprKind::RecordGet { base, index } => {
            compile_record_get(emitter, base, *index, diags);
            true
        }
        IrExprKind::RecordUpdate {
            ty_name,
            field_count,
            base,
            base_fields,
            result_fields,
            updates,
        } => {
            compile_record_update(
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
            );
            true
        }
        IrExprKind::VariantNew {
            data_key,
            tag_index,
            field_count,
            args,
        } => {
            compile_variant_new(emitter, data_key, *tag_index, *field_count, args, diags);
            true
        }
        IrExprKind::Index { base, indices } => {
            compile_index(emitter, base, indices, diags);
            true
        }
        IrExprKind::DynamicImport { spec } => {
            compile_expr(emitter, spec, true, diags);
            emitter.code.push(CodeEntry::Instruction(Instruction::new(
                Opcode::ModLoad,
                Operand::None,
            )));
            true
        }
        IrExprKind::TypeValue { ty_name } => {
            let Some(ty) = emitter.layout.types.get(ty_name).copied() else {
                support::push_expr_diag(
                    diags,
                    emitter.module_key,
                    &expr.origin,
                    &EmitDiagKind::UnknownTypeValue(ty_name.clone()),
                );
                emit_zero(emitter);
                return true;
            };
            emitter.code.push(CodeEntry::Instruction(Instruction::new(
                Opcode::TyId,
                Operand::Type(ty),
            )));
            true
        }
        IrExprKind::SyntaxValue { raw } => {
            compile_string_constant(emitter, raw);
            true
        }
        _ => false,
    }
}

fn compile_expr_storage_ops(
    emitter: &mut MethodEmitter<'_, '_>,
    expr: &IrExpr,
    diags: &mut EmitDiagList,
) -> bool {
    match &expr.kind {
        IrExprKind::Let {
            binding,
            name,
            value,
            ..
        } => {
            compile_let(emitter, *binding, name, value, diags);
            true
        }
        IrExprKind::TempLet { temp, value } => {
            compile_temp_let(emitter, *temp, value, diags);
            true
        }
        IrExprKind::Assign { target, value } => {
            compile_assign(emitter, target, value, diags);
            true
        }
        _ => false,
    }
}

fn compile_expr_control_ops(
    emitter: &mut MethodEmitter<'_, '_>,
    expr: &IrExpr,
    diags: &mut EmitDiagList,
) -> bool {
    match &expr.kind {
        IrExprKind::ClosureNew { callee, captures } => {
            compile_closure_new(emitter, callee, captures, diags);
            true
        }
        IrExprKind::Binary { op, left, right } => {
            compile_binary(emitter, op, left, right, diags);
            true
        }
        IrExprKind::Not { expr: inner } => {
            compile_expr(emitter, inner, true, diags);
            emit_zero(emitter);
            emitter.code.push(CodeEntry::Instruction(Instruction::new(
                Opcode::CmpEq,
                Operand::None,
            )));
            true
        }
        IrExprKind::Case { scrutinee, arms } => {
            compile_case(emitter, scrutinee, arms, diags);
            true
        }
        IrExprKind::Call { callee, args } => {
            compile_call(emitter, callee, args, diags);
            true
        }
        IrExprKind::CallSeq { callee, args } => {
            compile_call_seq(emitter, callee, args, diags);
            true
        }
        _ => false,
    }
}

fn compile_expr_effect_ops(
    emitter: &mut MethodEmitter<'_, '_>,
    expr: &IrExpr,
    diags: &mut EmitDiagList,
) -> bool {
    match &expr.kind {
        IrExprKind::Perform {
            effect_key,
            op_index,
            args,
        } => {
            compile_perform(emitter, effect_key, *op_index, args, diags);
            true
        }
        IrExprKind::PerformSeq {
            effect_key,
            op_index,
            args,
        } => {
            compile_perform_seq(emitter, effect_key, *op_index, args, diags);
            true
        }
        IrExprKind::Handle {
            effect_key,
            value,
            ops,
            body,
        } => {
            compile_handle(emitter, effect_key, value, ops, body, diags);
            true
        }
        IrExprKind::Resume { expr } => {
            compile_resume(emitter, expr.as_deref(), diags);
            true
        }
        _ => false,
    }
}

fn compile_expr_type_ops(
    emitter: &mut MethodEmitter<'_, '_>,
    expr: &IrExpr,
    diags: &mut EmitDiagList,
) -> bool {
    match &expr.kind {
        IrExprKind::TyTest { base, ty_name } => {
            compile_type_op_by_name(
                emitter,
                &expr.origin,
                base,
                ty_name,
                Opcode::TyChk,
                ":?",
                diags,
            );
            true
        }
        IrExprKind::TyCast { base, ty_name } => {
            compile_type_op_by_name(
                emitter,
                &expr.origin,
                base,
                ty_name,
                Opcode::TyCast,
                ":?>",
                diags,
            );
            true
        }
        _ => false,
    }
}

fn compile_type_op_by_name(
    emitter: &mut MethodEmitter<'_, '_>,
    origin: &IrOrigin,
    base: &IrExpr,
    ty_name: &str,
    opcode: Opcode,
    op_text: &str,
    diags: &mut EmitDiagList,
) {
    compile_expr(emitter, base, true, diags);
    let Some(ty) = emitter.layout.types.get(ty_name).copied() else {
        support::push_expr_diag(
            diags,
            emitter.module_key,
            origin,
            &EmitDiagKind::UnknownTypeNameForOp {
                ty_name: ty_name.into(),
                op_text: op_text.into(),
            },
        );
        emit_zero(emitter);
        return;
    };
    emitter.code.push(CodeEntry::Instruction(Instruction::new(
        opcode,
        Operand::Type(ty),
    )));
}
