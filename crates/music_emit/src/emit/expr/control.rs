use super::super::*;
use crate::EmitDiagKind;
use music_base::diag::DiagContext;

impl ProcedureEmitter<'_, '_> {
    pub(super) fn compile_binary(
        &mut self,
        op: &IrBinaryOp,
        left: &IrExpr,
        right: &IrExpr,
        diags: &mut EmitDiagList,
    ) {
        self.compile_expr(left, true, diags);
        self.compile_expr(right, true, diags);
        let opcode = match op {
            IrBinaryOp::IAdd | IrBinaryOp::FAdd => Opcode::Add,
            IrBinaryOp::ISub | IrBinaryOp::FSub => Opcode::Sub,
            IrBinaryOp::IMul | IrBinaryOp::FMul => Opcode::Mul,
            IrBinaryOp::IDiv | IrBinaryOp::FDiv => Opcode::DivS,
            IrBinaryOp::IRem | IrBinaryOp::FRem => Opcode::RemS,
            IrBinaryOp::StrCat => Opcode::CallInd,
            IrBinaryOp::Eq => Opcode::Ceq,
            IrBinaryOp::Ne => Opcode::Cne,
            IrBinaryOp::Lt => Opcode::CltS,
            IrBinaryOp::Gt => Opcode::CgtS,
            IrBinaryOp::Le => Opcode::CleS,
            IrBinaryOp::Ge => Opcode::CgeS,
            IrBinaryOp::Other(name) => {
                super::support::push_expr_diag_with(
                    diags,
                    self.module_key,
                    &left.origin,
                    EmitDiagKind::UnsupportedBinaryOperator,
                    DiagContext::new().with("operator", name),
                );
                Opcode::Ceq
            }
        };
        self.code.push(CodeEntry::Instruction(Instruction::new(
            opcode,
            Operand::None,
        )));
    }

    pub(super) fn compile_case(
        &mut self,
        scrutinee: &IrExpr,
        arms: &[IrMatchArm],
        diags: &mut EmitDiagList,
    ) {
        let scrutinee_slot = Self::reserve_temp_slot(self);
        let end_label = self.alloc_label();
        self.compile_expr(scrutinee, true, diags);
        self.code.push(CodeEntry::Instruction(Instruction::new(
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
            self.emit_match_arms(
                scrutinee_slot,
                &arms[0..variant_start],
                Some(end_label),
                diags,
            );

            self.compile_case_variant_dispatch(
                scrutinee_slot,
                &arms[variant_start..=variant_end],
                &arms[(variant_end + 1)..],
                end_label,
                diags,
            );
            self.code.push(CodeEntry::Label(Label { id: end_label }));
            return;
        }

        self.emit_match_arms(scrutinee_slot, arms, Some(end_label), diags);

        emit_zero(self);
        self.code.push(CodeEntry::Label(Label { id: end_label }));
    }
}

fn variant_dispatch_span(arms: &[IrMatchArm]) -> (Option<usize>, Option<usize>) {
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
    tag_value: i64,
    args: &'a [IrCasePattern],
    as_binding: Option<(NameBindingId, &'a str)>,
}

fn pattern_variantish(pattern: &IrCasePattern) -> Option<Variantish<'_>> {
    match pattern {
        IrCasePattern::Variant {
            data_key,
            variant_count,
            tag_value,
            args,
            ..
        } => Some(Variantish {
            data_key,
            variant_count: *variant_count,
            tag_value: *tag_value,
            args,
            as_binding: None,
        }),
        IrCasePattern::As { pat, binding, name } => match pat.as_ref() {
            IrCasePattern::Variant {
                data_key,
                variant_count,
                tag_value,
                args,
                ..
            } => Some(Variantish {
                data_key,
                variant_count: *variant_count,
                tag_value: *tag_value,
                args,
                as_binding: Some((*binding, name.as_ref())),
            }),
            _ => None,
        },
        _ => None,
    }
}

impl ProcedureEmitter<'_, '_> {
    fn compile_case_variant_dispatch(
        &mut self,
        scrutinee_slot: u16,
        arms: &[IrMatchArm],
        tail: &[IrMatchArm],
        end_label: u16,
        diags: &mut EmitDiagList,
    ) {
        let Some(first) = arms
            .first()
            .and_then(|arm| pattern_variantish(&arm.pattern))
        else {
            return;
        };
        let origin = case_dispatch_origin(arms);
        if arms
            .iter()
            .filter_map(|arm| pattern_variantish(&arm.pattern))
            .any(|info| {
                info.data_key != first.data_key || info.variant_count != first.variant_count
            })
        {
            super::support::push_expr_diag_with(
                diags,
                self.module_key,
                &origin,
                EmitDiagKind::CaseVariantDispatchRequiresSingleDataType,
                DiagContext::new(),
            );
            emit_zero(self);
            return;
        }

        let Some(data_ty) = self.resolve_variant_data_ty(first.data_key, &origin, diags) else {
            emit_zero(self);
            return;
        };

        if !variant_tags_are_dense(arms) {
            self.emit_sparse_variant_dispatch(
                scrutinee_slot,
                arms,
                tail,
                data_ty,
                end_label,
                diags,
            );
            return;
        }

        let default_label = self.alloc_label();
        let tag_labels = (0..usize::from(first.variant_count))
            .map(|_| self.alloc_label())
            .collect::<Vec<_>>();
        self.emit_variant_dispatch_table(scrutinee_slot, data_ty, &tag_labels, default_label);

        let arms_by_tag = group_variant_arms_by_tag(arms, first.variant_count);
        self.emit_variant_dispatch_tag_blocks(
            scrutinee_slot,
            &tag_labels,
            &arms_by_tag,
            default_label,
            end_label,
            diags,
        );

        self.code
            .push(CodeEntry::Label(Label { id: default_label }));
        self.emit_match_arms(scrutinee_slot, tail, Some(end_label), diags);
        emit_zero(self);
    }
}

fn variant_tags_are_dense(arms: &[IrMatchArm]) -> bool {
    arms.iter()
        .filter_map(|arm| pattern_variantish(&arm.pattern))
        .all(|info| {
            info.tag_value >= 0
                && u16::try_from(info.tag_value)
                    .ok()
                    .is_some_and(|tag| tag < info.variant_count)
        })
}

impl ProcedureEmitter<'_, '_> {
    fn emit_sparse_variant_dispatch(
        &mut self,
        scrutinee_slot: u16,
        arms: &[IrMatchArm],
        tail: &[IrMatchArm],
        data_ty: TypeId,
        end_label: u16,
        diags: &mut EmitDiagList,
    ) {
        for arm in arms {
            let Some(info) = pattern_variantish(&arm.pattern) else {
                continue;
            };
            let next_label = self.alloc_label();
            self.emit_case_arm(
                next_label,
                arm.guard.as_ref(),
                &arm.expr,
                Some(end_label),
                diags,
                |emitter, next_label, diags| {
                    emitter.compile_variant_tag_match(
                        scrutinee_slot,
                        data_ty,
                        info.tag_value,
                        next_label,
                    );
                    emitter.compile_variant_payload_patterns(
                        scrutinee_slot,
                        info.args,
                        info.as_binding,
                        next_label,
                        diags,
                    )
                },
            );
        }
        self.emit_match_arms(scrutinee_slot, tail, Some(end_label), diags);
        emit_zero(self);
    }
}

fn case_dispatch_origin(arms: &[IrMatchArm]) -> IrOrigin {
    arms.first().map_or_else(
        || IrOrigin {
            source_id: SourceId::from_raw(0),
            span: Span::new(0, 0),
        },
        |arm| arm.expr.origin,
    )
}

impl ProcedureEmitter<'_, '_> {
    fn emit_variant_dispatch_table(
        &mut self,
        scrutinee_slot: u16,
        data_ty: TypeId,
        tag_labels: &[u16],
        default_label: u16,
    ) {
        let mut table = tag_labels.to_vec();
        table.push(default_label);
        self.code.push(CodeEntry::Instruction(Instruction::new(
            Opcode::LdLoc,
            Operand::Local(scrutinee_slot),
        )));
        self.code.push(CodeEntry::Instruction(Instruction::new(
            Opcode::LdFld,
            Operand::Type(data_ty),
        )));
        self.code.push(CodeEntry::Instruction(Instruction::new(
            Opcode::BrTbl,
            Operand::BranchTable(table.into_boxed_slice()),
        )));
    }
}

fn group_variant_arms_by_tag(arms: &[IrMatchArm], variant_count: u16) -> Vec<Vec<&IrMatchArm>> {
    let mut out = vec![Vec::<&IrMatchArm>::new(); usize::from(variant_count)];
    for arm in arms {
        if let Some(info) = pattern_variantish(&arm.pattern) {
            if let Ok(idx) = usize::try_from(info.tag_value)
                && idx < out.len()
            {
                out[idx].push(arm);
            }
        }
    }
    out
}

impl ProcedureEmitter<'_, '_> {
    fn emit_variant_dispatch_tag_blocks(
        &mut self,
        scrutinee_slot: u16,
        tag_labels: &[u16],
        arms_by_tag: &[Vec<&IrMatchArm>],
        default_label: u16,
        end_label: u16,
        diags: &mut EmitDiagList,
    ) {
        for (tag_index, label) in tag_labels.iter().copied().enumerate() {
            self.code.push(CodeEntry::Label(Label { id: label }));
            for arm in &arms_by_tag[tag_index] {
                let Some(info) = pattern_variantish(&arm.pattern) else {
                    continue;
                };
                let next_label = self.alloc_label();
                self.emit_case_arm(
                    next_label,
                    arm.guard.as_ref(),
                    &arm.expr,
                    Some(end_label),
                    diags,
                    |emitter, next_label, diags| {
                        emitter.compile_variant_payload_patterns(
                            scrutinee_slot,
                            info.args,
                            info.as_binding,
                            next_label,
                            diags,
                        )
                    },
                );
            }
            self.code.push(CodeEntry::Instruction(Instruction::new(
                Opcode::Br,
                Operand::Label(default_label),
            )));
        }
    }
}

impl ProcedureEmitter<'_, '_> {
    fn emit_match_arms(
        &mut self,
        scrutinee_slot: u16,
        arms: &[IrMatchArm],
        end_label: Option<u16>,
        diags: &mut EmitDiagList,
    ) {
        for arm in arms {
            let next_label = self.alloc_label();
            self.emit_case_arm(
                next_label,
                arm.guard.as_ref(),
                &arm.expr,
                end_label,
                diags,
                |emitter, next_label, diags| {
                    emitter.compile_case_pattern(&arm.pattern, scrutinee_slot, next_label, diags)
                },
            );
        }
    }
}

impl ProcedureEmitter<'_, '_> {
    fn emit_case_arm<F>(
        &mut self,
        next_label: u16,
        guard: Option<&IrExpr>,
        expr: &IrExpr,
        end_label: Option<u16>,
        diags: &mut EmitDiagList,
        mut compile_pattern: F,
    ) where
        F: FnMut(&mut ProcedureEmitter<'_, '_>, u16, &mut EmitDiagList) -> bool,
    {
        if !compile_pattern(self, next_label, diags) {
            return;
        }
        self.emit_case_arm_body(next_label, guard, expr, end_label, diags);
    }
}

impl ProcedureEmitter<'_, '_> {
    fn emit_case_arm_body(
        &mut self,
        next_label: u16,
        guard: Option<&IrExpr>,
        expr: &IrExpr,
        end_label: Option<u16>,
        diags: &mut EmitDiagList,
    ) {
        if let Some(guard) = guard {
            self.compile_expr(guard, true, diags);
            self.code.push(CodeEntry::Instruction(Instruction::new(
                Opcode::BrFalse,
                Operand::Label(next_label),
            )));
        }
        self.compile_expr(expr, true, diags);
        if let Some(end_label) = end_label {
            self.code.push(CodeEntry::Instruction(Instruction::new(
                Opcode::Br,
                Operand::Label(end_label),
            )));
        }
        self.code.push(CodeEntry::Label(Label { id: next_label }));
    }
}

impl ProcedureEmitter<'_, '_> {
    fn compile_case_pattern(
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
    fn compile_variant_payload_patterns(
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
    fn compile_variant_tag_match(
        &mut self,
        scrutinee_slot: u16,
        data_ty: TypeId,
        tag_value: i64,
        next_label: u16,
    ) {
        self.code.push(CodeEntry::Instruction(Instruction::new(
            Opcode::LdLoc,
            Operand::Local(scrutinee_slot),
        )));
        self.code.push(CodeEntry::Instruction(Instruction::new(
            Opcode::LdFld,
            Operand::Type(data_ty),
        )));
        self.compile_i64(tag_value);
        self.code.push(CodeEntry::Instruction(Instruction::new(
            Opcode::Ceq,
            Operand::None,
        )));
        self.code.push(CodeEntry::Instruction(Instruction::new(
            Opcode::BrFalse,
            Operand::Label(next_label),
        )));
    }
}

impl ProcedureEmitter<'_, '_> {
    fn resolve_variant_data_ty(
        &self,
        data_key: &DefinitionKey,
        origin: &IrOrigin,
        diags: &mut EmitDiagList,
    ) -> Option<TypeId> {
        let data_ty_name = qualified_name(&data_key.module, &data_key.name);
        let Some(data_ty) = self.layout.types.get(data_ty_name.as_ref()).copied() else {
            super::support::push_expr_diag_with(
                diags,
                self.module_key,
                origin,
                EmitDiagKind::UnknownDataType,
                DiagContext::new().with("type", data_ty_name),
            );
            return None;
        };
        Some(data_ty)
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
