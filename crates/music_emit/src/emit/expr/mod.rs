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

pub(super) fn compile_expr(
    emitter: &mut MethodEmitter<'_, '_>,
    expr: &IrExpr,
    keep_result: bool,
    diags: &mut EmitDiagList,
) {
    emitter.compile_expr(expr, keep_result, diags);
}

impl MethodEmitter<'_, '_> {
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
            support::push_expr_diag(
                diags,
                self.module_key,
                &expr.origin,
                EmitDiagKind::EmitInvariantViolated,
                format!("expr `{:?}` has no emitted form", expr.kind),
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
                module_target,
                ..
            } => {
                self.compile_name(*binding, name, module_target.as_ref(), expr, diags);
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
            IrExprKind::Range {
                ty_name,
                start,
                end,
                end_bound,
            } => {
                self.compile_range(ty_name, start, end, *end_bound, diags);
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
                tag_index,
                field_count,
                args,
            } => {
                self.compile_variant_new(data_key, *tag_index, *field_count, args, diags);
                true
            }
            IrExprKind::HandlerLit {
                effect_key,
                value,
                ops,
            } => {
                self.compile_handler_lit(effect_key, value, ops, &expr.origin, diags);
                true
            }
            IrExprKind::Index { base, indices } => {
                self.compile_index(base, indices, diags);
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
            IrExprKind::RangeMaterialize { range, evidence } => {
                self.compile_range_materialize(range, evidence, diags);
                true
            }
            _ => self.compile_expr_module_and_type_ops(expr, diags),
        }
    }

    fn compile_expr_module_and_type_ops(
        &mut self,
        expr: &IrExpr,
        diags: &mut EmitDiagList,
    ) -> bool {
        match &expr.kind {
            IrExprKind::DynamicImport { spec } => {
                self.compile_expr(spec, true, diags);
                self.code.push(CodeEntry::Instruction(Instruction::new(
                    Opcode::ModLoad,
                    Operand::None,
                )));
                true
            }
            IrExprKind::ModuleGet { base, name } => {
                self.compile_expr(base, true, diags);
                let name = self.artifact.intern_string(name);
                self.code.push(CodeEntry::Instruction(Instruction::new(
                    Opcode::ModGet,
                    Operand::String(name),
                )));
                true
            }
            IrExprKind::TypeValue { ty_name } => {
                let Some(ty) = self.layout.types.get(ty_name).copied() else {
                    support::push_expr_diag(
                        diags,
                        self.module_key,
                        &expr.origin,
                        EmitDiagKind::UnknownTypeValue,
                        format!("unknown emitted type value `{ty_name}`"),
                    );
                    emit_zero(self);
                    return true;
                };
                self.code.push(CodeEntry::Instruction(Instruction::new(
                    Opcode::TyId,
                    Operand::Type(ty),
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
                    Opcode::CmpEq,
                    Operand::None,
                )));
                true
            }
            IrExprKind::Case { scrutinee, arms } => {
                self.compile_case(scrutinee, arms, diags);
                true
            }
            IrExprKind::Call { callee, args } => {
                self.compile_call(callee, args, diags);
                true
            }
            IrExprKind::CallSeq { callee, args } => {
                self.compile_call_seq(callee, args, diags);
                true
            }
            _ => false,
        }
    }

    fn compile_expr_effect_ops(&mut self, expr: &IrExpr, diags: &mut EmitDiagList) -> bool {
        match &expr.kind {
            IrExprKind::Perform {
                effect_key,
                op_index,
                args,
            } => {
                self.compile_perform(effect_key, *op_index, args, diags);
                true
            }
            IrExprKind::PerformSeq {
                effect_key,
                op_index,
                args,
            } => {
                self.compile_perform_seq(effect_key, *op_index, args, diags);
                true
            }
            IrExprKind::Handle {
                effect_key,
                handler,
                body,
            } => {
                self.compile_handle(effect_key, handler, body, diags);
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
                    Opcode::TyChk,
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
            support::push_expr_diag(
                diags,
                self.module_key,
                origin,
                EmitDiagKind::UnknownTypeNameForOp,
                format!("unknown type name `{ty_name}` for `{op_text}`"),
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
