use crate::api::EmitDiagList;
use crate::diag::EmitDiagKind;
use crate::emit::{ProcedureEmitter, emit_zero, qualified_name};
use music_base::{SourceId, Span, diag::DiagContext};
use music_ir::{DefinitionKey, IrCasePattern, IrMatchArm, IrOrigin};
use music_names::NameBindingId;
use music_seam::{CodeEntry, Instruction, Label, Opcode, Operand, TypeId};

pub(super) fn variant_dispatch_span(arms: &[IrMatchArm]) -> (Option<usize>, Option<usize>) {
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

pub(super) struct Variantish<'a> {
    pub(super) data_key: &'a DefinitionKey,
    pub(super) variant_count: u16,
    pub(super) tag_value: i64,
    pub(super) args: &'a [IrCasePattern],
    pub(super) as_binding: Option<(NameBindingId, &'a str)>,
}

pub(super) fn pattern_variantish(pattern: &IrCasePattern) -> Option<Variantish<'_>> {
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
    pub(super) fn compile_case_variant_dispatch(
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
            super::super::support::push_expr_diag_with(
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

pub(super) fn variant_tags_are_dense(arms: &[IrMatchArm]) -> bool {
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
    pub(super) fn emit_sparse_variant_dispatch(
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

pub(super) fn case_dispatch_origin(arms: &[IrMatchArm]) -> IrOrigin {
    arms.first().map_or_else(
        || IrOrigin {
            source_id: SourceId::from_raw(0),
            span: Span::new(0, 0),
        },
        |arm| arm.expr.origin,
    )
}

impl ProcedureEmitter<'_, '_> {
    pub(super) fn emit_variant_dispatch_table(
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

pub(super) fn group_variant_arms_by_tag(
    arms: &[IrMatchArm],
    variant_count: u16,
) -> Vec<Vec<&IrMatchArm>> {
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
    pub(super) fn emit_variant_dispatch_tag_blocks(
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
    pub(super) fn compile_variant_tag_match(
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
    pub(super) fn resolve_variant_data_ty(
        &self,
        data_key: &DefinitionKey,
        origin: &IrOrigin,
        diags: &mut EmitDiagList,
    ) -> Option<TypeId> {
        let data_ty_name = qualified_name(&data_key.module, &data_key.name);
        let Some(data_ty) = self.layout.types.get(data_ty_name.as_ref()).copied() else {
            super::super::support::push_expr_diag_with(
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
