use super::*;
use crate::EmitDiagKind;
use music_base::diag::DiagContext;

mod callable;
mod control;
mod effects;
mod literals;
mod names;
mod records;
mod sequence;
mod storage;
mod support;

pub(super) fn compile_expr(
    emitter: &mut ProcedureEmitter<'_, '_>,
    expr: &IrExpr,
    keep_result: bool,
    diags: &mut EmitDiagList,
) {
    emitter.compile_expr(expr, keep_result, diags);
}

impl ProcedureEmitter<'_, '_> {
    pub(super) fn compile_expr(
        &mut self,
        expr: &IrExpr,
        keep_result: bool,
        diags: &mut EmitDiagList,
    ) {
        let matched = self.compile_expr_literal(expr, diags)
            || self.compile_expr_sequence_and_data(expr, diags)
            || self.compile_expr_storage_ops(expr, diags)
            || self.compile_expr_control_ops(expr, diags)
            || self.compile_expr_effect_ops(expr, diags)
            || self.compile_expr_type_ops(expr, diags);
        if !matched {
            support::push_expr_diag_with(
                diags,
                self.module_key,
                &expr.origin,
                EmitDiagKind::EmitInvariantViolated,
                DiagContext::new().with(
                    "detail",
                    format!("expr `{:?}` has no emitted form", expr.kind),
                ),
            );
            return;
        }
        if !keep_result {
            let slot = Self::scratch_slot(self);
            self.code.push(CodeEntry::Instruction(Instruction::new(
                Opcode::StLoc,
                Operand::Local(slot),
            )));
        }
    }

    fn compile_expr_literal(&mut self, expr: &IrExpr, diags: &mut EmitDiagList) -> bool {
        match &expr.kind {
            IrExprKind::Unit => {
                emit_zero(self);
                true
            }
            IrExprKind::Name {
                binding,
                name,
                import_record_target,
                ..
            } => {
                self.compile_name(*binding, name, import_record_target.as_ref(), expr, diags);
                true
            }
            IrExprKind::Temp { temp } => {
                self.compile_temp(*temp);
                true
            }
            IrExprKind::Lit(lit) => {
                self.compile_lit(lit, &expr.origin, diags);
                true
            }
            _ => false,
        }
    }

    fn compile_expr_sequence_and_data(&mut self, expr: &IrExpr, diags: &mut EmitDiagList) -> bool {
        match &expr.kind {
            IrExprKind::Sequence { exprs } => {
                self.compile_sequence(exprs, diags);
                true
            }
            IrExprKind::Tuple { ty_name, items } | IrExprKind::Array { ty_name, items } => {
                self.compile_sequence_literal(ty_name, items, diags);
                true
            }
            IrExprKind::ArrayCat { ty_name, parts } => {
                self.compile_array_cat(ty_name, parts, diags);
                true
            }
            IrExprKind::Record {
                ty_name,
                field_count,
                fields,
            } => {
                self.compile_record_literal(ty_name, *field_count, fields, diags);
                true
            }
            IrExprKind::RecordGet { base, index } => {
                self.compile_record_get(base, *index, diags);
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
                self.compile_record_update(
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
                tag_index: _,
                tag_value,
                field_count,
                args,
            } => {
                self.compile_variant_new(data_key, *tag_value, *field_count, args, diags);
                true
            }
            IrExprKind::AnswerLit {
                effect_key,
                value,
                ops,
            } => {
                self.compile_answer_lit(effect_key, value, ops, &expr.origin, diags);
                true
            }
            IrExprKind::Index { base, indices } => {
                self.compile_index(base, indices, diags);
                true
            }
            _ => {
                self.compile_expr_range_ops(expr, diags)
                    || self.compile_expr_module_and_type_ops(expr, diags)
            }
        }
    }

    fn compile_expr_range_ops(&mut self, expr: &IrExpr, diags: &mut EmitDiagList) -> bool {
        match &expr.kind {
            IrExprKind::Range {
                ty_name,
                kind,
                lower,
                upper,
                bounds_evidence,
            } => {
                self.compile_range(
                    ty_name,
                    *kind,
                    lower,
                    upper,
                    bounds_evidence.as_deref(),
                    diags,
                );
                true
            }
            IrExprKind::RangeContains {
                value,
                range,
                evidence,
            } => {
                self.compile_range_contains(value, range, evidence, diags);
                true
            }
            IrExprKind::RangeMaterialize {
                range,
                evidence,
                result_ty_name,
            } => {
                self.compile_range_materialize(range, evidence, result_ty_name, diags);
                true
            }
            _ => false,
        }
    }

    fn compile_expr_module_and_type_ops(
        &mut self,
        expr: &IrExpr,
        diags: &mut EmitDiagList,
    ) -> bool {
        match &expr.kind {
            IrExprKind::ModuleLoad { spec } => {
                self.compile_expr(spec, true, diags);
                self.code.push(CodeEntry::Instruction(Instruction::new(
                    Opcode::MdlLoad,
                    Operand::None,
                )));
                true
            }
            IrExprKind::ModuleGet { base, name } => {
                self.compile_expr(base, true, diags);
                let name = self.artifact.intern_string(name);
                self.code.push(CodeEntry::Instruction(Instruction::new(
                    Opcode::MdlGet,
                    Operand::String(name),
                )));
                true
            }
            IrExprKind::TypeValue { ty_name } => {
                let Some(ty) = self.layout.types.get(ty_name).copied() else {
                    support::push_expr_diag_with(
                        diags,
                        self.module_key,
                        &expr.origin,
                        EmitDiagKind::UnknownTypeValue,
                        DiagContext::new().with("type", ty_name),
                    );
                    emit_zero(self);
                    return true;
                };
                self.code.push(CodeEntry::Instruction(Instruction::new(
                    Opcode::LdType,
                    Operand::Type(ty),
                )));
                true
            }
            IrExprKind::TypeApply { callee, type_args } => {
                self.compile_expr(callee, true, diags);
                for ty_name in type_args {
                    let Some(ty) = self.layout.types.get(ty_name).copied() else {
                        support::push_expr_diag_with(
                            diags,
                            self.module_key,
                            &expr.origin,
                            EmitDiagKind::UnknownTypeValue,
                            DiagContext::new().with("type", ty_name),
                        );
                        emit_zero(self);
                        return true;
                    };
                    self.code.push(CodeEntry::Instruction(Instruction::new(
                        Opcode::LdType,
                        Operand::Type(ty),
                    )));
                }
                let count = i16::try_from(type_args.len()).unwrap_or(i16::MAX);
                self.code.push(CodeEntry::Instruction(Instruction::new(
                    Opcode::Call,
                    Operand::I16(count),
                )));
                true
            }
            IrExprKind::SyntaxValue { raw } => {
                self.compile_syntax_constant(raw, &expr.origin, diags);
                true
            }
            _ => false,
        }
    }

    fn compile_expr_storage_ops(&mut self, expr: &IrExpr, diags: &mut EmitDiagList) -> bool {
        match &expr.kind {
            IrExprKind::Let {
                binding,
                name,
                value,
                ..
            } => {
                self.compile_let(*binding, name, value, diags);
                true
            }
            IrExprKind::TempLet { temp, value } => {
                self.compile_temp_let(*temp, value, diags);
                true
            }
            IrExprKind::Assign { target, value } => {
                self.compile_assign(target, value, diags);
                true
            }
            _ => false,
        }
    }

    fn compile_expr_control_ops(&mut self, expr: &IrExpr, diags: &mut EmitDiagList) -> bool {
        match &expr.kind {
            IrExprKind::ClosureNew { callee, captures } => {
                self.compile_closure_new(callee, captures, diags);
                true
            }
            IrExprKind::Binary { op, left, right } => {
                self.compile_binary(op, left, right, diags);
                true
            }
            IrExprKind::Not { expr: inner } => {
                self.compile_expr(inner, true, diags);
                emit_zero(self);
                self.code.push(CodeEntry::Instruction(Instruction::new(
                    Opcode::Ceq,
                    Operand::None,
                )));
                true
            }
            IrExprKind::Match { scrutinee, arms } => {
                self.compile_case(scrutinee, arms, diags);
                true
            }
            IrExprKind::Call { callee, args } => {
                self.compile_call(callee, args, diags);
                true
            }
            IrExprKind::IntrinsicCall {
                symbol,
                param_tys,
                result_ty,
                args,
                ..
            } => {
                self.compile_intrinsic_call(symbol, param_tys, result_ty, args, diags);
                true
            }
            IrExprKind::CallParts { callee, args } => {
                self.compile_call_parts(callee, args, diags);
                true
            }
            _ => false,
        }
    }

    fn compile_expr_effect_ops(&mut self, expr: &IrExpr, diags: &mut EmitDiagList) -> bool {
        match &expr.kind {
            IrExprKind::Request {
                effect_key,
                op_index,
                args,
            } => {
                self.compile_perform(effect_key, *op_index, args, diags);
                true
            }
            IrExprKind::RequestSeq {
                effect_key,
                op_index,
                args,
            } => {
                self.compile_perform_seq(effect_key, *op_index, args, diags);
                true
            }
            IrExprKind::Handle {
                effect_key,
                answer,
                body,
            } => {
                self.compile_handle(effect_key, answer, body, diags);
                true
            }
            IrExprKind::Resume { expr } => {
                self.compile_resume(expr.as_deref(), diags);
                true
            }
            _ => false,
        }
    }

    fn compile_expr_type_ops(&mut self, expr: &IrExpr, diags: &mut EmitDiagList) -> bool {
        match &expr.kind {
            IrExprKind::TyTest { base, ty_name } => {
                self.compile_type_op_by_name(
                    &expr.origin,
                    base,
                    ty_name,
                    Opcode::IsInst,
                    ":?",
                    diags,
                );
                true
            }
            IrExprKind::TyCast { base, ty_name } => {
                self.compile_type_op_by_name(
                    &expr.origin,
                    base,
                    ty_name,
                    Opcode::Cast,
                    ":?>",
                    diags,
                );
                true
            }
            _ => false,
        }
    }

    fn compile_type_op_by_name(
        &mut self,
        origin: &IrOrigin,
        base: &IrExpr,
        ty_name: &str,
        opcode: Opcode,
        op_text: &str,
        diags: &mut EmitDiagList,
    ) {
        self.compile_expr(base, true, diags);
        let Some(ty) = self.layout.types.get(ty_name).copied() else {
            support::push_expr_diag_with(
                diags,
                self.module_key,
                origin,
                EmitDiagKind::UnknownTypeNameForOp,
                DiagContext::new()
                    .with("type", ty_name)
                    .with("operation", op_text),
            );
            emit_zero(self);
            return;
        };
        self.code.push(CodeEntry::Instruction(Instruction::new(
            opcode,
            Operand::Type(ty),
        )));
    }
}
