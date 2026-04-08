use super::*;

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
        IrExprKind::Lit(lit) => compile_lit(emitter, lit, &expr.origin, diags),
        IrExprKind::Sequence { exprs } => compile_sequence(emitter, exprs, diags),
        IrExprKind::Tuple { ty_name, items } | IrExprKind::Array { ty_name, items } => {
            compile_sequence_literal(emitter, ty_name, items, diags);
        }
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
        IrExprKind::Case { scrutinee, arms } => compile_case(emitter, scrutinee, arms, diags),
        IrExprKind::Call { callee, args } => compile_call(emitter, callee, args, diags),
        IrExprKind::Perform {
            effect_key,
            op_index,
            args,
        } => compile_perform(emitter, effect_key, *op_index, args, diags),
        IrExprKind::Handle {
            effect_key,
            value,
            ops,
            body,
        } => compile_handle(emitter, effect_key, value, ops, body, diags),
        IrExprKind::Resume { expr } => compile_resume(emitter, expr.as_deref(), diags),
        IrExprKind::Unsupported { description } => {
            push_expr_diag(
                diags,
                emitter.module_key,
                &expr.origin,
                format!("unsupported emitted expression `{description}`"),
            );
            emit_zero(emitter);
        }
    }
    if !keep_result {
        let slot = scratch_slot(emitter);
        emitter.code.push(CodeEntry::Instruction(Instruction::new(
            Opcode::StLoc,
            Operand::Local(slot),
        )));
    }
}

fn compile_lit(
    emitter: &mut MethodEmitter<'_, '_>,
    lit: &IrLit,
    origin: &IrOrigin,
    diags: &mut EmitDiagList,
) {
    match lit {
        IrLit::Int { raw } => compile_int_literal(emitter, raw, origin, diags),
        IrLit::Float { raw } => compile_float_literal(emitter, raw, origin, diags),
        IrLit::String { value } => compile_string_literal(emitter, value),
        IrLit::Rune { value } => compile_i64(emitter, i64::from(*value)),
    }
}

fn compile_int_literal(
    emitter: &mut MethodEmitter<'_, '_>,
    raw: &str,
    origin: &IrOrigin,
    diags: &mut EmitDiagList,
) {
    if let Some(value) = parse_int_literal(raw) {
        compile_i64(emitter, value);
    } else {
        push_expr_diag(
            diags,
            emitter.module_key,
            origin,
            format!("invalid integer literal `{raw}`"),
        );
        emit_zero(emitter);
    }
}

fn compile_string_literal(emitter: &mut MethodEmitter<'_, '_>, value: &str) {
    let string_id = emitter.artifact.intern_string(value);
    let const_name = format!("const:string:{}", emitter.artifact.constants.len());
    let name_id = emitter.artifact.intern_string(&const_name);
    let constant_id = emitter.artifact.constants.alloc(ConstantDescriptor {
        name: name_id,
        value: ConstantValue::String(string_id),
    });
    emitter.code.push(CodeEntry::Instruction(Instruction::new(
        Opcode::LdConst,
        Operand::Constant(constant_id),
    )));
}

fn compile_float_literal(
    emitter: &mut MethodEmitter<'_, '_>,
    raw: &str,
    origin: &IrOrigin,
    diags: &mut EmitDiagList,
) {
    let compact = raw.replace('_', "");
    let Ok(value) = compact.parse::<f64>() else {
        push_expr_diag(
            diags,
            emitter.module_key,
            origin,
            format!("invalid float literal `{raw}`"),
        );
        emit_zero(emitter);
        return;
    };
    let const_name = format!("const:float:{}", emitter.artifact.constants.len());
    let name_id = emitter.artifact.intern_string(&const_name);
    let constant_id = emitter.artifact.constants.alloc(ConstantDescriptor {
        name: name_id,
        value: ConstantValue::Float(value),
    });
    emitter.code.push(CodeEntry::Instruction(Instruction::new(
        Opcode::LdConst,
        Operand::Constant(constant_id),
    )));
}

fn compile_i64(emitter: &mut MethodEmitter<'_, '_>, value: i64) {
    if let Ok(short) = i16::try_from(value) {
        emitter.code.push(CodeEntry::Instruction(Instruction::new(
            Opcode::LdSmi,
            Operand::I16(short),
        )));
        return;
    }
    let const_name = format!("const:int:{}", emitter.artifact.constants.len());
    let name_id = emitter.artifact.intern_string(&const_name);
    let constant_id = emitter.artifact.constants.alloc(ConstantDescriptor {
        name: name_id,
        value: ConstantValue::Int(value),
    });
    emitter.code.push(CodeEntry::Instruction(Instruction::new(
        Opcode::LdConst,
        Operand::Constant(constant_id),
    )));
}

fn compile_name(
    emitter: &mut MethodEmitter<'_, '_>,
    binding: Option<NameBindingId>,
    name: &str,
    module_target: Option<&ModuleKey>,
    expr: &IrExpr,
    diags: &mut EmitDiagList,
) {
    if let Some(binding) = binding
        && let Some(slot) = emitter.locals.get(&binding).copied()
    {
        emitter.code.push(CodeEntry::Instruction(Instruction::new(
            Opcode::LdLoc,
            Operand::Local(slot),
        )));
        return;
    }
    if let Some(global) = resolve_global(emitter, binding, name, module_target) {
        emitter.code.push(CodeEntry::Instruction(Instruction::new(
            Opcode::LdGlob,
            Operand::Global(global),
        )));
        return;
    }
    if let Some(method) = resolve_method(emitter, binding, name, module_target) {
        emitter.code.push(CodeEntry::Instruction(Instruction::new(
            Opcode::ClsNew,
            Operand::WideMethodCaptures {
                method,
                captures: 0,
            },
        )));
        return;
    }
    push_expr_diag(
        diags,
        emitter.module_key,
        &expr.origin,
        format!("unsupported emitted name reference `{name}`"),
    );
    emit_zero(emitter);
}

fn compile_sequence(
    emitter: &mut MethodEmitter<'_, '_>,
    exprs: &[IrExpr],
    diags: &mut EmitDiagList,
) {
    for (index, expr) in exprs.iter().enumerate() {
        let keep_result = index + 1 == exprs.len();
        compile_expr(emitter, expr, keep_result, diags);
    }
}

fn compile_sequence_literal(
    emitter: &mut MethodEmitter<'_, '_>,
    ty_name: &str,
    items: &[IrExpr],
    diags: &mut EmitDiagList,
) {
    for item in items {
        compile_expr(emitter, item, true, diags);
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

fn compile_record_literal(
    emitter: &mut MethodEmitter<'_, '_>,
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
        compile_expr(emitter, &field.expr, true, diags);
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
                "record literal missing field value".into(),
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
        Operand::TypeLen { ty, len: field_count },
    )));
}

fn compile_record_get(
    emitter: &mut MethodEmitter<'_, '_>,
    base: &IrExpr,
    index: u16,
    diags: &mut EmitDiagList,
) {
    compile_expr(emitter, base, true, diags);
    compile_i64(emitter, i64::from(index));
    emitter.code.push(CodeEntry::Instruction(Instruction::new(
        Opcode::DataGet,
        Operand::None,
    )));
}

fn compile_record_update(
    emitter: &mut MethodEmitter<'_, '_>,
    input: RecordUpdateInput<'_>,
    diags: &mut EmitDiagList,
) {
    let Some(ty) = emitter.layout.types.get(input.ty_name).copied() else {
        push_expr_diag(
            diags,
            emitter.module_key,
            &input.base.origin,
            format!("unknown emitted record type `{}`", input.ty_name),
        );
        emit_zero(emitter);
        return;
    };

    let base_slot = reserve_temp_slot(emitter);
    compile_expr(emitter, input.base, true, diags);
    emitter.code.push(CodeEntry::Instruction(Instruction::new(
        Opcode::StLoc,
        Operand::Local(base_slot),
    )));

    let field_count_usize = usize::from(input.field_count);
    let mut update_slots = vec![None::<u16>; field_count_usize];
    for update in input.updates {
        compile_expr(emitter, &update.expr, true, diags);
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
                "record update missing field value".into(),
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

fn compile_variant_new(
    emitter: &mut MethodEmitter<'_, '_>,
    data_key: &DefinitionKey,
    tag_index: u16,
    field_count: u16,
    args: &[IrExpr],
    diags: &mut EmitDiagList,
) {
    for arg in args {
        compile_expr(emitter, arg, true, diags);
    }
    compile_i64(emitter, i64::from(tag_index));

    let ty_name = qualified_name(&data_key.module, &data_key.name);
    let Some(ty) = emitter.layout.types.get(ty_name.as_ref()).copied() else {
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
            format!("unknown emitted data type `{ty_name}`"),
        );
        emit_zero(emitter);
        return;
    };

    emitter.code.push(CodeEntry::Instruction(Instruction::new(
        Opcode::DataNew,
        Operand::TypeLen { ty, len: field_count },
    )));
}

fn compile_perform(
    emitter: &mut MethodEmitter<'_, '_>,
    effect_key: &DefinitionKey,
    op_index: u16,
    args: &[IrExpr],
    diags: &mut EmitDiagList,
) {
    for arg in args {
        compile_expr(emitter, arg, true, diags);
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
            "unknown emitted effect".into(),
        );
        emit_zero(emitter);
        return;
    };
    emitter.code.push(CodeEntry::Instruction(Instruction::new(
        Opcode::EffInvk,
        Operand::Effect { effect, op: op_index },
    )));
}

fn compile_handle(
    emitter: &mut MethodEmitter<'_, '_>,
    effect_key: &DefinitionKey,
    value: &IrExpr,
    ops: &[IrHandleOp],
    body: &IrExpr,
    diags: &mut EmitDiagList,
) {
    compile_expr(emitter, value, true, diags);
    for op in ops {
        compile_expr(emitter, &op.closure, true, diags);
    }

    let handler_ty_name = handler_type_name(effect_key);
    let Some(handler_ty) = emitter.layout.types.get(handler_ty_name.as_ref()).copied() else {
        push_expr_diag(
            diags,
            emitter.module_key,
            &body.origin,
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
            "unknown emitted effect".into(),
        );
        emit_zero(emitter);
        return;
    };
    emitter.code.push(CodeEntry::Instruction(Instruction::new(
        Opcode::HdlPush,
        Operand::EffectId(effect),
    )));

    compile_expr(emitter, body, true, diags);

    emitter.code.push(CodeEntry::Instruction(Instruction::new(
        Opcode::HdlPop,
        Operand::None,
    )));
}

fn compile_resume(emitter: &mut MethodEmitter<'_, '_>, expr: Option<&IrExpr>, diags: &mut EmitDiagList) {
    if let Some(expr) = expr {
        compile_expr(emitter, expr, true, diags);
    } else {
        emit_zero(emitter);
    }
    emitter.code.push(CodeEntry::Instruction(Instruction::new(
        Opcode::EffResume,
        Operand::None,
    )));
}

fn compile_let(
    emitter: &mut MethodEmitter<'_, '_>,
    binding: Option<NameBindingId>,
    _name: &str,
    value: &IrExpr,
    diags: &mut EmitDiagList,
) {
    compile_expr(emitter, value, true, diags);
    if let Some(binding) = binding {
        let slot = ensure_local_slot(emitter, binding);
        emitter.code.push(CodeEntry::Instruction(Instruction::new(
            Opcode::StLoc,
            Operand::Local(slot),
        )));
    } else {
        let slot = scratch_slot(emitter);
        emitter.code.push(CodeEntry::Instruction(Instruction::new(
            Opcode::StLoc,
            Operand::Local(slot),
        )));
    }
    emit_zero(emitter);
}

fn compile_temp(emitter: &mut MethodEmitter<'_, '_>, temp: IrTempId) {
    let slot = ensure_temp_slot(emitter, temp);
    emitter.code.push(CodeEntry::Instruction(Instruction::new(
        Opcode::LdLoc,
        Operand::Local(slot),
    )));
}

fn compile_temp_let(
    emitter: &mut MethodEmitter<'_, '_>,
    temp: IrTempId,
    value: &IrExpr,
    diags: &mut EmitDiagList,
) {
    compile_expr(emitter, value, true, diags);
    let slot = ensure_temp_slot(emitter, temp);
    emitter.code.push(CodeEntry::Instruction(Instruction::new(
        Opcode::StLoc,
        Operand::Local(slot),
    )));
    emit_zero(emitter);
}

fn compile_assign(
    emitter: &mut MethodEmitter<'_, '_>,
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
            compile_expr(emitter, value, true, diags);
            if let Some(binding) = binding
                && let Some(slot) = emitter.locals.get(binding).copied()
            {
                emitter.code.push(CodeEntry::Instruction(Instruction::new(
                    Opcode::StLoc,
                    Operand::Local(slot),
                )));
                emit_zero(emitter);
                return;
            }
            if let Some(global) = resolve_global(emitter, *binding, name, module_target.as_ref()) {
                emitter.code.push(CodeEntry::Instruction(Instruction::new(
                    Opcode::StGlob,
                    Operand::Global(global),
                )));
                emit_zero(emitter);
                return;
            }
            push_expr_diag(
                diags,
                emitter.module_key,
                &value.origin,
                format!("unsupported emitted assignment target `{name}`"),
            );
            emit_zero(emitter);
        }
        IrAssignTarget::Index { base, index } => {
            compile_expr(emitter, base, true, diags);
            compile_expr(emitter, index, true, diags);
            compile_expr(emitter, value, true, diags);
            emitter.code.push(CodeEntry::Instruction(Instruction::new(
                Opcode::SeqSet,
                Operand::None,
            )));
            emit_zero(emitter);
        }
    }
}

fn compile_index(
    emitter: &mut MethodEmitter<'_, '_>,
    base: &IrExpr,
    index: &IrExpr,
    diags: &mut EmitDiagList,
) {
    compile_expr(emitter, base, true, diags);
    compile_expr(emitter, index, true, diags);
    emitter.code.push(CodeEntry::Instruction(Instruction::new(
        Opcode::SeqGet,
        Operand::None,
    )));
}

fn compile_binary(
    emitter: &mut MethodEmitter<'_, '_>,
    op: &IrBinaryOp,
    left: &IrExpr,
    right: &IrExpr,
    diags: &mut EmitDiagList,
) {
    compile_expr(emitter, left, true, diags);
    compile_expr(emitter, right, true, diags);
    let opcode = match op {
        IrBinaryOp::Add => Opcode::IAdd,
        IrBinaryOp::Sub => Opcode::ISub,
        IrBinaryOp::Mul => Opcode::IMul,
        IrBinaryOp::Div => Opcode::IDiv,
        IrBinaryOp::Rem => Opcode::IRem,
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

fn compile_case(
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

fn compile_call(
    emitter: &mut MethodEmitter<'_, '_>,
    callee: &IrExpr,
    args: &[IrArg],
    diags: &mut EmitDiagList,
) {
    for arg in args {
        if arg.spread {
            push_expr_diag(
                diags,
                emitter.module_key,
                &arg.expr.origin,
                "spread call arguments are not yet emitted".into(),
            );
        }
        compile_expr(emitter, &arg.expr, true, diags);
    }
    if let IrExprKind::Name {
        binding,
        name,
        module_target,
        ..
    } = &callee.kind
    {
        if let Some(binding) = binding
            && let Some(slot) = emitter.locals.get(binding).copied()
        {
            emitter.code.push(CodeEntry::Instruction(Instruction::new(
                Opcode::LdLoc,
                Operand::Local(slot),
            )));
            emitter.code.push(CodeEntry::Instruction(Instruction::new(
                Opcode::CallCls,
                Operand::None,
            )));
            return;
        }
        if let Some(method) = resolve_method(emitter, *binding, name, module_target.as_ref()) {
            emitter.code.push(CodeEntry::Instruction(Instruction::new(
                Opcode::Call,
                Operand::Method(method),
            )));
            return;
        }
        if let Some(foreign) = resolve_foreign(emitter, *binding, name, module_target.as_ref()) {
            emitter.code.push(CodeEntry::Instruction(Instruction::new(
                Opcode::FfiCall,
                Operand::Foreign(foreign),
            )));
            return;
        }
    }

    compile_expr(emitter, callee, true, diags);
    emitter.code.push(CodeEntry::Instruction(Instruction::new(
        Opcode::CallCls,
        Operand::None,
    )));
}

fn compile_closure_new(
    emitter: &mut MethodEmitter<'_, '_>,
    callee: &IrNameRef,
    captures: &[IrExpr],
    diags: &mut EmitDiagList,
) {
    for cap in captures {
        compile_expr(emitter, cap, true, diags);
    }
    let Some(method) = resolve_method(
        emitter,
        callee.binding,
        callee.name.as_ref(),
        callee.module_target.as_ref(),
    ) else {
        let origin = captures.first().map_or_else(|| IrOrigin {
            source_id: SourceId::from_raw(0),
            span: Span::new(0, 0),
        }, |expr| expr.origin);
        push_expr_diag(
            diags,
            emitter.module_key,
            &origin,
            format!("unknown emitted closure target `{}`", callee.name),
        );
        emit_zero(emitter);
        return;
    };
    let captures = u8::try_from(captures.len()).unwrap_or(u8::MAX);
    emitter.code.push(CodeEntry::Instruction(Instruction::new(
        Opcode::ClsNew,
        Operand::WideMethodCaptures { method, captures },
    )));
}

fn resolve_method(
    emitter: &MethodEmitter<'_, '_>,
    binding: Option<NameBindingId>,
    name: &str,
    module_target: Option<&ModuleKey>,
) -> Option<MethodId> {
    if module_target.is_none() || module_target.is_some_and(|target| target == emitter.module_key) {
        if let Some(method) = emitter.layout.callables_by_name.get(name).copied() {
            return Some(method);
        }
    }
    binding
        .and_then(|binding| emitter.layout.callables.get(&binding).copied())
        .or_else(|| {
            module_target.and_then(|target| {
                emitter
                    .qualified_methods
                    .get(&(target.clone(), name.into()))
                    .copied()
            })
        })
        .or_else(|| emitter.unique_methods.get(name).copied())
}

fn resolve_foreign(
    emitter: &MethodEmitter<'_, '_>,
    binding: Option<NameBindingId>,
    name: &str,
    module_target: Option<&ModuleKey>,
) -> Option<ForeignId> {
    binding
        .and_then(|binding| emitter.layout.foreigns.get(&binding).copied())
        .or_else(|| {
            module_target.and_then(|target| {
                emitter
                    .qualified_foreigns
                    .get(&(target.clone(), name.into()))
                    .copied()
            })
        })
        .or_else(|| emitter.unique_foreigns.get(name).copied())
}

fn resolve_global(
    emitter: &MethodEmitter<'_, '_>,
    binding: Option<NameBindingId>,
    name: &str,
    module_target: Option<&ModuleKey>,
) -> Option<GlobalId> {
    binding
        .and_then(|binding| emitter.layout.globals.get(&binding).copied())
        .or_else(|| {
            module_target.and_then(|target| {
                emitter
                    .qualified_globals
                    .get(&(target.clone(), name.into()))
                    .copied()
            })
        })
        .or_else(|| emitter.unique_globals.get(name).copied())
}

fn ensure_local_slot(emitter: &mut MethodEmitter<'_, '_>, binding: NameBindingId) -> u16 {
    if let Some(slot) = emitter.locals.get(&binding).copied() {
        return slot;
    }
    let slot = reserve_temp_slot(emitter);
    let _ = emitter.locals.insert(binding, slot);
    slot
}

fn ensure_temp_slot(emitter: &mut MethodEmitter<'_, '_>, temp: IrTempId) -> u16 {
    if let Some(slot) = emitter.temps.get(&temp).copied() {
        return slot;
    }
    let slot = reserve_temp_slot(emitter);
    let _ = emitter.temps.insert(temp, slot);
    slot
}

const fn reserve_temp_slot(emitter: &mut MethodEmitter<'_, '_>) -> u16 {
    let slot = emitter.next_local;
    emitter.next_local = emitter.next_local.saturating_add(1);
    slot
}

const fn scratch_slot(emitter: &MethodEmitter<'_, '_>) -> u16 {
    emitter.next_local
}

fn alloc_label(emitter: &mut MethodEmitter<'_, '_>) -> u16 {
    let id = u16::try_from(emitter.labels.len()).unwrap_or(u16::MAX);
    let name = format!("L{id}");
    emitter.labels.push(emitter.artifact.intern_string(&name));
    id
}

fn parse_int_literal(raw: &str) -> Option<i64> {
    let compact = raw.replace('_', "");
    let (sign, digits) = compact
        .strip_prefix('-')
        .map_or((1_i64, compact.as_str()), |rest| (-1_i64, rest));
    let (radix, digits) = digits
        .strip_prefix("0x")
        .or_else(|| digits.strip_prefix("0X"))
        .map_or_else(
            || {
                digits
                    .strip_prefix("0o")
                    .or_else(|| digits.strip_prefix("0O"))
                    .map_or_else(
                        || {
                            digits
                                .strip_prefix("0b")
                                .or_else(|| digits.strip_prefix("0B"))
                                .map_or((10, digits), |rest| (2, rest))
                        },
                        |rest| (8, rest),
                    )
            },
            |rest| (16, rest),
        );
    i64::from_str_radix(digits, radix)
        .ok()
        .map(|value| value * sign)
}

fn push_expr_diag(
    diags: &mut EmitDiagList,
    module_key: &ModuleKey,
    origin: &IrOrigin,
    message: String,
) {
    push_span_diag(diags, module_key, origin.source_id, origin.span, message);
}

fn push_span_diag(
    diags: &mut EmitDiagList,
    module_key: &ModuleKey,
    source_id: SourceId,
    span: Span,
    message: String,
) {
    diags.push(Diag::error(message).with_label(span, source_id, module_key.as_str()));
}
