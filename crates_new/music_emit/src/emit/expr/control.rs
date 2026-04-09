use super::super::*;

use super::compile_expr;
use super::literals::{compile_i64, compile_lit};
use super::support::{alloc_label, ensure_local_slot, push_expr_diag, reserve_temp_slot};

pub(super) fn compile_binary(
    emitter: &mut MethodEmitter<'_, '_>,
    op: &IrBinaryOp,
    left: &IrExpr,
    right: &IrExpr,
    diags: &mut EmitDiagList,
) {
    compile_expr(emitter, left, true, diags);
    compile_expr(emitter, right, true, diags);
    let opcode = match op {
        IrBinaryOp::IAdd => Opcode::IAdd,
        IrBinaryOp::ISub => Opcode::ISub,
        IrBinaryOp::IMul => Opcode::IMul,
        IrBinaryOp::IDiv => Opcode::IDiv,
        IrBinaryOp::IRem => Opcode::IRem,
        IrBinaryOp::FAdd => Opcode::FAdd,
        IrBinaryOp::FSub => Opcode::FSub,
        IrBinaryOp::FMul => Opcode::FMul,
        IrBinaryOp::FDiv => Opcode::FDiv,
        IrBinaryOp::FRem => Opcode::FRem,
        IrBinaryOp::StrCat => Opcode::StrCat,
        IrBinaryOp::Eq => Opcode::CmpEq,
        IrBinaryOp::Ne => Opcode::CmpNe,
        IrBinaryOp::Lt => Opcode::CmpLt,
        IrBinaryOp::Gt => Opcode::CmpGt,
        IrBinaryOp::Le => Opcode::CmpLe,
        IrBinaryOp::Ge => Opcode::CmpGe,
        IrBinaryOp::Other(name) => {
            push_expr_diag(
                diags,
                emitter.module_key,
                &left.origin,
                format!("unsupported emitted binary operator `{name}`"),
            );
            Opcode::CmpEq
        }
    };
    emitter.code.push(CodeEntry::Instruction(Instruction::new(
        opcode,
        Operand::None,
    )));
}

pub(super) fn compile_case(
    emitter: &mut MethodEmitter<'_, '_>,
    scrutinee: &IrExpr,
    arms: &[IrCaseArm],
    diags: &mut EmitDiagList,
) {
    let scrutinee_slot = reserve_temp_slot(emitter);
    let end_label = alloc_label(emitter);
    compile_expr(emitter, scrutinee, true, diags);
    emitter.code.push(CodeEntry::Instruction(Instruction::new(
        Opcode::StLoc,
        Operand::Local(scrutinee_slot),
    )));

    let (variant_start, variant_end) = variant_dispatch_span(arms);
    if let Some((variant_start, variant_end)) = variant_start.zip(variant_end)
        && variant_start <= variant_end
        && arms[variant_start..=variant_end]
            .iter()
            .all(|arm| pattern_variantish(&arm.pattern).is_some())
    {
        for arm in &arms[0..variant_start] {
            let next_label = alloc_label(emitter);
            if !compile_case_pattern(emitter, &arm.pattern, scrutinee_slot, next_label, diags) {
                continue;
            }
            if let Some(guard) = &arm.guard {
                compile_expr(emitter, guard, true, diags);
                emitter.code.push(CodeEntry::Instruction(Instruction::new(
                    Opcode::BrFalse,
                    Operand::Label(next_label),
                )));
            }
            compile_expr(emitter, &arm.expr, true, diags);
            emitter.code.push(CodeEntry::Instruction(Instruction::new(
                Opcode::Br,
                Operand::Label(end_label),
            )));
            emitter.code.push(CodeEntry::Label(Label { id: next_label }));
        }

        compile_case_variant_dispatch(
            emitter,
            scrutinee_slot,
            &arms[variant_start..=variant_end],
            &arms[(variant_end + 1)..],
            end_label,
            diags,
        );
        emitter.code.push(CodeEntry::Label(Label { id: end_label }));
        return;
    }

    for arm in arms {
        let next_label = alloc_label(emitter);
        if !compile_case_pattern(emitter, &arm.pattern, scrutinee_slot, next_label, diags) {
            continue;
        }
        if let Some(guard) = &arm.guard {
            compile_expr(emitter, guard, true, diags);
            emitter.code.push(CodeEntry::Instruction(Instruction::new(
                Opcode::BrFalse,
                Operand::Label(next_label),
            )));
        }
        compile_expr(emitter, &arm.expr, true, diags);
        emitter.code.push(CodeEntry::Instruction(Instruction::new(
            Opcode::Br,
            Operand::Label(end_label),
        )));
        emitter.code.push(CodeEntry::Label(Label { id: next_label }));
    }

    emit_zero(emitter);
    emitter.code.push(CodeEntry::Label(Label { id: end_label }));
}

fn variant_dispatch_span(arms: &[IrCaseArm]) -> (Option<usize>, Option<usize>) {
    let mut first = None::<usize>;
    let mut last = None::<usize>;
    for (idx, arm) in arms.iter().enumerate() {
        if pattern_variantish(&arm.pattern).is_some() {
            if first.is_none() {
                first = Some(idx);
            }
            last = Some(idx);
        }
    }
    (first, last)
}

struct Variantish<'a> {
    data_key: &'a DefinitionKey,
    variant_count: u16,
    tag_index: u16,
    args: &'a [IrCasePattern],
    as_binding: Option<(NameBindingId, &'a str)>,
}

fn pattern_variantish(pattern: &IrCasePattern) -> Option<Variantish<'_>> {
    match pattern {
        IrCasePattern::Variant {
            data_key,
            variant_count,
            tag_index,
            args,
        } => Some(Variantish {
            data_key,
            variant_count: *variant_count,
            tag_index: *tag_index,
            args,
            as_binding: None,
        }),
        IrCasePattern::As { pat, binding, name } => match pat.as_ref() {
            IrCasePattern::Variant {
                data_key,
                variant_count,
                tag_index,
                args,
            } => Some(Variantish {
                data_key,
                variant_count: *variant_count,
                tag_index: *tag_index,
                args,
                as_binding: Some((*binding, name.as_ref())),
            }),
            _ => None,
        },
        _ => None,
    }
}

fn compile_case_variant_dispatch(
    emitter: &mut MethodEmitter<'_, '_>,
    scrutinee_slot: u16,
    arms: &[IrCaseArm],
    tail: &[IrCaseArm],
    end_label: u16,
    diags: &mut EmitDiagList,
) {
    let Some(first) = arms.first().and_then(|arm| pattern_variantish(&arm.pattern)) else {
        return;
    };
    let origin = case_dispatch_origin(arms);
    if arms
        .iter()
        .filter_map(|arm| pattern_variantish(&arm.pattern))
        .any(|info| info.data_key != first.data_key || info.variant_count != first.variant_count)
    {
        push_expr_diag(
            diags,
            emitter.module_key,
            &origin,
            "case variant dispatch requires a single data type".into(),
        );
        emit_zero(emitter);
        return;
    }

    let data_ty_name = qualified_name(&first.data_key.module, &first.data_key.name);
    let Some(data_ty) = emitter.layout.types.get(data_ty_name.as_ref()).copied() else {
        push_expr_diag(
            diags,
            emitter.module_key,
            &origin,
            format!("unknown emitted data type `{data_ty_name}`"),
        );
        emit_zero(emitter);
        return;
    };

    let default_label = alloc_label(emitter);
    let tag_labels = (0..usize::from(first.variant_count))
        .map(|_| alloc_label(emitter))
        .collect::<Vec<_>>();
    emit_variant_dispatch_table(emitter, scrutinee_slot, data_ty, &tag_labels, default_label);

    let arms_by_tag = group_variant_arms_by_tag(arms, first.variant_count);
    emit_variant_dispatch_tag_blocks(
        emitter,
        scrutinee_slot,
        &tag_labels,
        &arms_by_tag,
        default_label,
        end_label,
        diags,
    );

    emitter.code.push(CodeEntry::Label(Label { id: default_label }));
    emit_case_arms(
        emitter,
        scrutinee_slot,
        tail,
        Some(end_label),
        diags,
    );
    emit_zero(emitter);
}

fn case_dispatch_origin(arms: &[IrCaseArm]) -> IrOrigin {
    arms.first().map_or_else(
        || IrOrigin {
            source_id: SourceId::from_raw(0),
            span: Span::new(0, 0),
        },
        |arm| arm.expr.origin,
    )
}

fn emit_variant_dispatch_table(
    emitter: &mut MethodEmitter<'_, '_>,
    scrutinee_slot: u16,
    data_ty: TypeId,
    tag_labels: &[u16],
    default_label: u16,
) {
    let mut table = tag_labels.to_vec();
    table.push(default_label);
    emitter.code.push(CodeEntry::Instruction(Instruction::new(
        Opcode::LdLoc,
        Operand::Local(scrutinee_slot),
    )));
    emitter.code.push(CodeEntry::Instruction(Instruction::new(
        Opcode::DataTag,
        Operand::Type(data_ty),
    )));
    emitter.code.push(CodeEntry::Instruction(Instruction::new(
        Opcode::BrTbl,
        Operand::BranchTable(table.into_boxed_slice()),
    )));
}

fn group_variant_arms_by_tag(arms: &[IrCaseArm], variant_count: u16) -> Vec<Vec<&IrCaseArm>> {
    let mut out = vec![Vec::<&IrCaseArm>::new(); usize::from(variant_count)];
    for arm in arms {
        if let Some(info) = pattern_variantish(&arm.pattern) {
            let idx = usize::from(info.tag_index);
            if idx < out.len() {
                out[idx].push(arm);
            }
        }
    }
    out
}

fn emit_variant_dispatch_tag_blocks(
    emitter: &mut MethodEmitter<'_, '_>,
    scrutinee_slot: u16,
    tag_labels: &[u16],
    arms_by_tag: &[Vec<&IrCaseArm>],
    default_label: u16,
    end_label: u16,
    diags: &mut EmitDiagList,
) {
    for (tag_index, label) in tag_labels.iter().copied().enumerate() {
        emitter.code.push(CodeEntry::Label(Label { id: label }));
        for arm in &arms_by_tag[tag_index] {
            let Some(info) = pattern_variantish(&arm.pattern) else {
                continue;
            };
            let next_label = alloc_label(emitter);
            if !compile_variant_arm_payload(
                emitter,
                scrutinee_slot,
                info.args,
                info.as_binding,
                next_label,
                diags,
            ) {
                continue;
            }
            if let Some(guard) = &arm.guard {
                compile_expr(emitter, guard, true, diags);
                emitter.code.push(CodeEntry::Instruction(Instruction::new(
                    Opcode::BrFalse,
                    Operand::Label(next_label),
                )));
            }
            compile_expr(emitter, &arm.expr, true, diags);
            emitter.code.push(CodeEntry::Instruction(Instruction::new(
                Opcode::Br,
                Operand::Label(end_label),
            )));
            emitter.code.push(CodeEntry::Label(Label { id: next_label }));
        }
        emitter.code.push(CodeEntry::Instruction(Instruction::new(
            Opcode::Br,
            Operand::Label(default_label),
        )));
    }
}

fn emit_case_arms(
    emitter: &mut MethodEmitter<'_, '_>,
    scrutinee_slot: u16,
    arms: &[IrCaseArm],
    end_label: Option<u16>,
    diags: &mut EmitDiagList,
) {
    for arm in arms {
        let next_label = alloc_label(emitter);
        if !compile_case_pattern(emitter, &arm.pattern, scrutinee_slot, next_label, diags) {
            continue;
        }
        if let Some(guard) = &arm.guard {
            compile_expr(emitter, guard, true, diags);
            emitter.code.push(CodeEntry::Instruction(Instruction::new(
                Opcode::BrFalse,
                Operand::Label(next_label),
            )));
        }
        compile_expr(emitter, &arm.expr, true, diags);
        if let Some(end_label) = end_label {
            emitter.code.push(CodeEntry::Instruction(Instruction::new(
                Opcode::Br,
                Operand::Label(end_label),
            )));
        }
        emitter.code.push(CodeEntry::Label(Label { id: next_label }));
    }
}

fn compile_variant_arm_payload(
    emitter: &mut MethodEmitter<'_, '_>,
    scrutinee_slot: u16,
    args: &[IrCasePattern],
    as_binding: Option<(NameBindingId, &str)>,
    next_label: u16,
    diags: &mut EmitDiagList,
) -> bool {
    for (idx, pat) in args.iter().enumerate() {
        emitter.code.push(CodeEntry::Instruction(Instruction::new(
            Opcode::LdLoc,
            Operand::Local(scrutinee_slot),
        )));
        compile_i64(emitter, i64::try_from(idx).unwrap_or(i64::MAX));
        emitter.code.push(CodeEntry::Instruction(Instruction::new(
            Opcode::DataGet,
            Operand::None,
        )));
        let item_slot = reserve_temp_slot(emitter);
        emitter.code.push(CodeEntry::Instruction(Instruction::new(
            Opcode::StLoc,
            Operand::Local(item_slot),
        )));
        if !compile_case_pattern(emitter, pat, item_slot, next_label, diags) {
            return false;
        }
    }
    if let Some((binding, _name)) = as_binding {
        let slot = ensure_local_slot(emitter, binding);
        emitter.code.push(CodeEntry::Instruction(Instruction::new(
            Opcode::LdLoc,
            Operand::Local(scrutinee_slot),
        )));
        emitter.code.push(CodeEntry::Instruction(Instruction::new(
            Opcode::StLoc,
            Operand::Local(slot),
        )));
    }
    true
}

fn compile_case_pattern(
    emitter: &mut MethodEmitter<'_, '_>,
    pattern: &IrCasePattern,
    scrutinee_slot: u16,
    next_label: u16,
    diags: &mut EmitDiagList,
) -> bool {
    match pattern {
        IrCasePattern::Wildcard => true,
        IrCasePattern::Bind { binding, .. } => compile_case_bind(emitter, scrutinee_slot, *binding),
        IrCasePattern::Lit(lit) => compile_case_lit(emitter, scrutinee_slot, lit, next_label, diags),
        IrCasePattern::Tuple { items } | IrCasePattern::Array { items } => {
            compile_case_seq_pattern(emitter, scrutinee_slot, items, next_label, diags)
        }
        IrCasePattern::Variant {
            data_key,
            tag_index,
            args,
            ..
        } => compile_case_variant_pattern(
            emitter,
            scrutinee_slot,
            data_key,
            *tag_index,
            args,
            next_label,
            diags,
        ),
        IrCasePattern::As { pat, binding, .. } => {
            compile_case_as_pattern(emitter, scrutinee_slot, pat, *binding, next_label, diags)
        }
    }
}

fn compile_case_bind(emitter: &mut MethodEmitter<'_, '_>, scrutinee_slot: u16, binding: NameBindingId) -> bool {
    let slot = ensure_local_slot(emitter, binding);
    emitter.code.push(CodeEntry::Instruction(Instruction::new(
        Opcode::LdLoc,
        Operand::Local(scrutinee_slot),
    )));
    emitter.code.push(CodeEntry::Instruction(Instruction::new(
        Opcode::StLoc,
        Operand::Local(slot),
    )));
    true
}

fn compile_case_lit(
    emitter: &mut MethodEmitter<'_, '_>,
    scrutinee_slot: u16,
    lit: &IrLit,
    next_label: u16,
    diags: &mut EmitDiagList,
) -> bool {
    emitter.code.push(CodeEntry::Instruction(Instruction::new(
        Opcode::LdLoc,
        Operand::Local(scrutinee_slot),
    )));
    compile_lit(
        emitter,
        lit,
        &IrOrigin {
            source_id: SourceId::from_raw(0),
            span: Span::new(0, 0),
        },
        diags,
    );
    emitter.code.push(CodeEntry::Instruction(Instruction::new(
        Opcode::CmpEq,
        Operand::None,
    )));
    emitter.code.push(CodeEntry::Instruction(Instruction::new(
        Opcode::BrFalse,
        Operand::Label(next_label),
    )));
    true
}

fn compile_case_seq_pattern(
    emitter: &mut MethodEmitter<'_, '_>,
    scrutinee_slot: u16,
    items: &[IrCasePattern],
    next_label: u16,
    diags: &mut EmitDiagList,
) -> bool {
    for (idx, item) in items.iter().enumerate() {
        emitter.code.push(CodeEntry::Instruction(Instruction::new(
            Opcode::LdLoc,
            Operand::Local(scrutinee_slot),
        )));
        let idx = i64::try_from(idx).unwrap_or(i64::MAX);
        compile_i64(emitter, idx);
        emitter.code.push(CodeEntry::Instruction(Instruction::new(
            Opcode::SeqGet,
            Operand::None,
        )));
        let item_slot = reserve_temp_slot(emitter);
        emitter.code.push(CodeEntry::Instruction(Instruction::new(
            Opcode::StLoc,
            Operand::Local(item_slot),
        )));
        if !compile_case_pattern(emitter, item, item_slot, next_label, diags) {
            return false;
        }
    }
    true
}

fn compile_case_variant_pattern(
    emitter: &mut MethodEmitter<'_, '_>,
    scrutinee_slot: u16,
    data_key: &DefinitionKey,
    tag_index: u16,
    args: &[IrCasePattern],
    next_label: u16,
    diags: &mut EmitDiagList,
) -> bool {
    let data_ty_name = qualified_name(&data_key.module, &data_key.name);
    let Some(data_ty) = emitter.layout.types.get(data_ty_name.as_ref()).copied() else {
        push_expr_diag(
            diags,
            emitter.module_key,
            &IrOrigin {
                source_id: SourceId::from_raw(0),
                span: Span::new(0, 0),
            },
            format!("unknown emitted data type `{data_ty_name}`"),
        );
        return false;
    };
    emitter.code.push(CodeEntry::Instruction(Instruction::new(
        Opcode::LdLoc,
        Operand::Local(scrutinee_slot),
    )));
    emitter.code.push(CodeEntry::Instruction(Instruction::new(
        Opcode::DataTag,
        Operand::Type(data_ty),
    )));
    compile_i64(emitter, i64::from(tag_index));
    emitter.code.push(CodeEntry::Instruction(Instruction::new(
        Opcode::CmpEq,
        Operand::None,
    )));
    emitter.code.push(CodeEntry::Instruction(Instruction::new(
        Opcode::BrFalse,
        Operand::Label(next_label),
    )));
    for (idx, item) in args.iter().enumerate() {
        emitter.code.push(CodeEntry::Instruction(Instruction::new(
            Opcode::LdLoc,
            Operand::Local(scrutinee_slot),
        )));
        compile_i64(emitter, i64::try_from(idx).unwrap_or(i64::MAX));
        emitter.code.push(CodeEntry::Instruction(Instruction::new(
            Opcode::DataGet,
            Operand::None,
        )));
        let item_slot = reserve_temp_slot(emitter);
        emitter.code.push(CodeEntry::Instruction(Instruction::new(
            Opcode::StLoc,
            Operand::Local(item_slot),
        )));
        if !compile_case_pattern(emitter, item, item_slot, next_label, diags) {
            return false;
        }
    }
    true
}

fn compile_case_as_pattern(
    emitter: &mut MethodEmitter<'_, '_>,
    scrutinee_slot: u16,
    pat: &IrCasePattern,
    binding: NameBindingId,
    next_label: u16,
    diags: &mut EmitDiagList,
) -> bool {
    if !compile_case_pattern(emitter, pat, scrutinee_slot, next_label, diags) {
        return false;
    }
    let slot = ensure_local_slot(emitter, binding);
    emitter.code.push(CodeEntry::Instruction(Instruction::new(
        Opcode::LdLoc,
        Operand::Local(scrutinee_slot),
    )));
    emitter.code.push(CodeEntry::Instruction(Instruction::new(
        Opcode::StLoc,
        Operand::Local(slot),
    )));
    true
}
