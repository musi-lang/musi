use super::super::*;
use crate::EmitDiagKind;

use super::literals::compile_i64;
use super::support::push_expr_diag;

pub(super) fn compile_perform(
    emitter: ExprEmitterMut<'_, '_, '_>,
    effect_key: &DefinitionKey,
    op_index: u16,
    args: &[IrExpr],
    diags: &mut EmitDiagList,
) {
    for arg in args {
        super::compile_expr(emitter, arg, true, diags);
    }
    let Some(effect) = emitter.layout.effects.get(effect_key).copied() else {
        let origin = args.first().map_or_else(
            || IrOrigin {
                source_id: SourceId::from_raw(0),
                span: Span::new(0, 0),
            },
            |expr| expr.origin,
        );
        push_expr_diag(
            diags,
            emitter.module_key,
            &origin,
            &EmitDiagKind::UnknownEffect,
            "unknown emitted effect",
        );
        emit_zero(emitter);
        return;
    };
    emitter.code.push(CodeEntry::Instruction(Instruction::new(
        Opcode::EffInvk,
        Operand::Effect {
            effect,
            op: op_index,
        },
    )));
}

pub(super) fn compile_handle(
    emitter: ExprEmitterMut<'_, '_, '_>,
    effect_key: &DefinitionKey,
    value: &IrExpr,
    ops: &[IrHandleOp],
    body: &IrExpr,
    diags: &mut EmitDiagList,
) {
    super::compile_expr(emitter, value, true, diags);
    for op in ops {
        super::compile_expr(emitter, &op.closure, true, diags);
    }

    let handler_ty_name = handler_type_name(effect_key);
    let Some(handler_ty) = emitter.layout.types.get(handler_ty_name.as_ref()).copied() else {
        push_expr_diag(
            diags,
            emitter.module_key,
            &body.origin,
            &EmitDiagKind::UnknownHandlerType,
            format!("unknown emitted handler type `{handler_ty_name}`"),
        );
        emit_zero(emitter);
        return;
    };
    let field_count = u16::try_from(ops.len().saturating_add(1)).unwrap_or(u16::MAX);
    compile_i64(emitter, 0);
    emitter.code.push(CodeEntry::Instruction(Instruction::new(
        Opcode::DataNew,
        Operand::TypeLen {
            ty: handler_ty,
            len: field_count,
        },
    )));

    let Some(effect) = emitter.layout.effects.get(effect_key).copied() else {
        push_expr_diag(
            diags,
            emitter.module_key,
            &body.origin,
            &EmitDiagKind::UnknownEffect,
            "unknown emitted effect",
        );
        emit_zero(emitter);
        return;
    };
    emitter.code.push(CodeEntry::Instruction(Instruction::new(
        Opcode::HdlPush,
        Operand::EffectId(effect),
    )));

    super::compile_expr(emitter, body, true, diags);

    emitter.code.push(CodeEntry::Instruction(Instruction::new(
        Opcode::HdlPop,
        Operand::None,
    )));
}

pub(super) fn compile_resume(
    emitter: ExprEmitterMut<'_, '_, '_>,
    expr: Option<&IrExpr>,
    diags: &mut EmitDiagList,
) {
    if let Some(expr) = expr {
        super::compile_expr(emitter, expr, true, diags);
    } else {
        emit_zero(emitter);
    }
    emitter.code.push(CodeEntry::Instruction(Instruction::new(
        Opcode::EffResume,
        Operand::None,
    )));
}

pub(super) fn compile_perform_seq(
    emitter: ExprEmitterMut<'_, '_, '_>,
    effect_key: &DefinitionKey,
    op_index: u16,
    args: &[IrSeqPart],
    diags: &mut EmitDiagList,
) {
    super::sequence::compile_seq_parts_any(emitter, args, diags);
    let Some(effect) = emitter.layout.effects.get(effect_key).copied() else {
        push_expr_diag(
            diags,
            emitter.module_key,
            &IrOrigin {
                source_id: SourceId::from_raw(0),
                span: Span::new(0, 0),
            },
            &EmitDiagKind::UnknownEffect,
            "unknown emitted effect",
        );
        emit_zero(emitter);
        return;
    };
    emitter.code.push(CodeEntry::Instruction(Instruction::new(
        Opcode::EffInvkSeq,
        Operand::Effect {
            effect,
            op: op_index,
        },
    )));
}
