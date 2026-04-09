use super::super::*;

use super::support::push_expr_diag;

pub(super) fn compile_sequence(
    emitter: &mut MethodEmitter<'_, '_>,
    exprs: &[IrExpr],
    diags: &mut EmitDiagList,
) {
    for (index, expr) in exprs.iter().enumerate() {
        let keep_result = index + 1 == exprs.len();
        super::compile_expr(emitter, expr, keep_result, diags);
    }
}

pub(super) fn compile_sequence_literal(
    emitter: &mut MethodEmitter<'_, '_>,
    ty_name: &str,
    items: &[IrExpr],
    diags: &mut EmitDiagList,
) {
    for item in items {
        super::compile_expr(emitter, item, true, diags);
    }
    let Some(ty) = emitter.layout.types.get(ty_name).copied() else {
        let missing_origin = items.first().map_or_else(
            || IrOrigin {
                source_id: SourceId::from_raw(0),
                span: Span::new(0, 0),
            },
            |expr| expr.origin,
        );
        push_expr_diag(
            diags,
            emitter.module_key,
            &missing_origin,
            format!("unknown emitted sequence type `{ty_name}`"),
        );
        emit_zero(emitter);
        return;
    };
    emitter.code.push(CodeEntry::Instruction(Instruction::new(
        Opcode::SeqNew,
        Operand::TypeLen {
            ty,
            len: u16::try_from(items.len()).unwrap_or(u16::MAX),
        },
    )));
}

pub(super) fn compile_array_cat(
    emitter: &mut MethodEmitter<'_, '_>,
    ty_name: &str,
    parts: &[IrSeqPart],
    diags: &mut EmitDiagList,
) {
    let Some(ty) = emitter.layout.types.get(ty_name).copied() else {
        push_expr_diag(
            diags,
            emitter.module_key,
            &IrOrigin {
                source_id: SourceId::from_raw(0),
                span: Span::new(0, 0),
            },
            format!("unknown emitted sequence type `{ty_name}`"),
        );
        emit_zero(emitter);
        return;
    };

    for (index, part) in parts.iter().enumerate() {
        match part {
            IrSeqPart::Expr(expr) => {
                super::compile_expr(emitter, expr, true, diags);
                emitter.code.push(CodeEntry::Instruction(Instruction::new(
                    Opcode::SeqNew,
                    Operand::TypeLen { ty, len: 1 },
                )));
            }
            IrSeqPart::Spread(expr) => {
                super::compile_expr(emitter, expr, true, diags);
            }
        }
        if index != 0 {
            emitter.code.push(CodeEntry::Instruction(Instruction::new(
                Opcode::SeqCat,
                Operand::None,
            )));
        }
    }
}

pub(super) fn compile_seq_parts_any(
    emitter: &mut MethodEmitter<'_, '_>,
    parts: &[IrSeqPart],
    diags: &mut EmitDiagList,
) {
    // Runtime spread lowers via `IrSeqPart::Spread` and uses a sequence runtime contract.
    // The SEAM metadata type chosen here is an emission detail.
    let ty_name = "[]Any";
    let Some(ty) = emitter.layout.types.get(ty_name).copied() else {
        push_expr_diag(
            diags,
            emitter.module_key,
            &IrOrigin {
                source_id: SourceId::from_raw(0),
                span: Span::new(0, 0),
            },
            format!("unknown emitted sequence type `{ty_name}`"),
        );
        emit_zero(emitter);
        return;
    };

    emitter.code.push(CodeEntry::Instruction(Instruction::new(
        Opcode::SeqNew,
        Operand::TypeLen { ty, len: 0 },
    )));
    compile_seq_parts_any_append(emitter, ty, parts, diags);
}

fn compile_seq_parts_any_append(
    emitter: &mut MethodEmitter<'_, '_>,
    ty: TypeId,
    parts: &[IrSeqPart],
    diags: &mut EmitDiagList,
) {
    for part in parts {
        match part {
            IrSeqPart::Expr(expr) => {
                super::compile_expr(emitter, expr, true, diags);
                emitter.code.push(CodeEntry::Instruction(Instruction::new(
                    Opcode::SeqNew,
                    Operand::TypeLen { ty, len: 1 },
                )));
                emitter.code.push(CodeEntry::Instruction(Instruction::new(
                    Opcode::SeqCat,
                    Operand::None,
                )));
            }
            IrSeqPart::Spread(expr) => {
                super::compile_expr(emitter, expr, true, diags);
                emitter.code.push(CodeEntry::Instruction(Instruction::new(
                    Opcode::SeqCat,
                    Operand::None,
                )));
            }
        }
    }
}

pub(super) fn compile_index(
    emitter: &mut MethodEmitter<'_, '_>,
    base: &IrExpr,
    index: &IrExpr,
    diags: &mut EmitDiagList,
) {
    super::compile_expr(emitter, base, true, diags);
    super::compile_expr(emitter, index, true, diags);
    emitter.code.push(CodeEntry::Instruction(Instruction::new(
        Opcode::SeqGet,
        Operand::None,
    )));
}
