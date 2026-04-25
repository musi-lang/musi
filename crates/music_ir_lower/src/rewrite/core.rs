use super::super::closures::lower_binding_capture_exprs;
use super::super::{
    DefinitionKey, IrArg, IrBinaryOp, IrExpr, IrExprKind, IrHandleOp, IrIntrinsicKind, IrNameRef,
    IrOrigin, IrRecordField, IrRecordLayoutField, IrSeqPart, LowerCtx, NameBindingId,
};
use super::parts::{
    rewrite_array_cat_kind, rewrite_array_kind, rewrite_assign_kind, rewrite_call_args,
    rewrite_call_kind, rewrite_call_parts_kind, rewrite_case_kind, rewrite_expr_slice,
    rewrite_handle_ops, rewrite_index_kind, rewrite_module_get_kind, rewrite_module_load_kind,
    rewrite_not_kind, rewrite_record_fields, rewrite_record_get_kind, rewrite_resume_kind,
    rewrite_seq_parts, rewrite_sequence_kind, rewrite_temp_let_kind, rewrite_tuple_kind,
    rewrite_ty_cast_kind, rewrite_ty_test_kind,
};
use music_ir::IrRangeKind;

struct RecursiveBindingInput<'a, 'b> {
    ctx: &'a LowerCtx<'b>,
    origin: IrOrigin,
    binding: NameBindingId,
    callable_name: &'a str,
    captures: &'a [NameBindingId],
}

impl RecursiveBindingInput<'_, '_> {
    fn rewrite_refs(&self, expr: IrExpr) -> IrExpr {
        let origin_expr = expr.origin;
        let kind = self.rewrite_kind(expr.kind);
        IrExpr::new(origin_expr, kind)
    }

    fn rewrite_kind(&self, kind: IrExprKind) -> IrExprKind {
        match kind {
            IrExprKind::Name {
                binding: Some(found),
                import_record_target,
                ..
            } if found == self.binding && import_record_target.is_none() => {
                IrExprKind::ClosureNew {
                    callee: IrNameRef::new(self.callable_name)
                        .with_binding(self.binding)
                        .with_import_record_target(self.ctx.module_key.clone()),
                    captures: lower_binding_capture_exprs(self.ctx, self.origin, self.captures),
                }
            }
            IrExprKind::Sequence { exprs } => rewrite_sequence_kind(
                self.ctx,
                self.origin,
                exprs,
                self.binding,
                self.callable_name,
                self.captures,
            ),
            IrExprKind::Tuple { ty_name, items } => rewrite_tuple_kind(
                self.ctx,
                self.origin,
                ty_name,
                items,
                self.binding,
                self.callable_name,
                self.captures,
            ),
            IrExprKind::Array { ty_name, items } => rewrite_array_kind(
                self.ctx,
                self.origin,
                ty_name,
                items,
                self.binding,
                self.callable_name,
                self.captures,
            ),
            IrExprKind::ArrayCat { ty_name, parts } => rewrite_array_cat_kind(
                self.ctx,
                self.origin,
                ty_name,
                parts,
                self.binding,
                self.callable_name,
                self.captures,
            ),
            IrExprKind::Record {
                ty_name,
                field_count,
                fields,
            } => self.rewrite_record_kind(ty_name, field_count, fields),
            IrExprKind::RecordGet { base, index } => rewrite_record_get_kind(
                self.ctx,
                self.origin,
                *base,
                index,
                self.binding,
                self.callable_name,
                self.captures,
            ),
            IrExprKind::RecordUpdate {
                ty_name,
                field_count,
                base,
                base_fields,
                result_fields,
                updates,
            } => self.rewrite_record_update_kind(RecordUpdateRewriteInput {
                ty_name,
                field_count,
                base: *base,
                base_fields,
                result_fields,
                updates,
            }),
            other => self.rewrite_storage_kind(other),
        }
    }

    fn rewrite_storage_kind(&self, kind: IrExprKind) -> IrExprKind {
        match kind {
            IrExprKind::Let {
                binding: local_binding,
                name,
                value,
            } => self.rewrite_let_kind(local_binding, name, *value),
            IrExprKind::TempLet { temp, value } => rewrite_temp_let_kind(
                self.ctx,
                self.origin,
                temp,
                *value,
                self.binding,
                self.callable_name,
                self.captures,
            ),
            IrExprKind::Assign { target, value } => rewrite_assign_kind(
                self.ctx,
                self.origin,
                *target,
                *value,
                self.binding,
                self.callable_name,
                self.captures,
            ),
            IrExprKind::Index { base, indices } => rewrite_index_kind(
                self.ctx,
                self.origin,
                *base,
                indices,
                self.binding,
                self.callable_name,
                self.captures,
            ),
            IrExprKind::ModuleLoad { spec } => rewrite_module_load_kind(
                self.ctx,
                self.origin,
                *spec,
                self.binding,
                self.callable_name,
                self.captures,
            ),
            IrExprKind::ModuleGet { base, name } => rewrite_module_get_kind(
                self.ctx,
                self.origin,
                *base,
                name,
                self.binding,
                self.callable_name,
                self.captures,
            ),
            other => self.rewrite_compute_kind(other),
        }
    }

    fn rewrite_compute_kind(&self, kind: IrExprKind) -> IrExprKind {
        match kind {
            IrExprKind::TypeApply { callee, type_args } => {
                self.rewrite_type_apply_kind(*callee, type_args)
            }
            IrExprKind::ClosureNew { callee, captures } => {
                self.rewrite_closure_new_kind(callee, captures)
            }
            IrExprKind::Binary { op, left, right } => self.rewrite_binary_kind(op, *left, *right),
            IrExprKind::BoolAnd { left, right } => self.rewrite_bool_and_kind(*left, *right),
            IrExprKind::BoolOr { left, right } => self.rewrite_bool_or_kind(*left, *right),
            IrExprKind::Range {
                ty_name,
                kind,
                lower,
                upper,
                bounds_evidence,
            } => self.rewrite_range_kind(ty_name, kind, *lower, *upper, bounds_evidence),
            IrExprKind::RangeContains {
                value,
                range,
                evidence,
            } => self.rewrite_range_contains_kind(*value, *range, *evidence),
            IrExprKind::RangeMaterialize {
                range,
                evidence,
                result_ty_name,
            } => self.rewrite_range_materialize_kind(*range, *evidence, result_ty_name),
            IrExprKind::Not { expr } => rewrite_not_kind(
                self.ctx,
                self.origin,
                *expr,
                self.binding,
                self.callable_name,
                self.captures,
            ),
            IrExprKind::TyTest { base, ty_name } => rewrite_ty_test_kind(
                self.ctx,
                self.origin,
                *base,
                ty_name,
                self.binding,
                self.callable_name,
                self.captures,
            ),
            IrExprKind::TyCast { base, ty_name } => rewrite_ty_cast_kind(
                self.ctx,
                self.origin,
                *base,
                ty_name,
                self.binding,
                self.callable_name,
                self.captures,
            ),
            IrExprKind::Match { scrutinee, arms } => rewrite_case_kind(
                self.ctx,
                self.origin,
                *scrutinee,
                arms,
                self.binding,
                self.callable_name,
                self.captures,
            ),
            IrExprKind::VariantNew {
                data_key,
                tag_index,
                tag_value,
                field_count,
                args,
            } => self.rewrite_variant_kind(data_key, tag_index, tag_value, field_count, args),
            IrExprKind::Call { callee, args } => rewrite_call_kind(
                self.ctx,
                self.origin,
                *callee,
                args,
                self.binding,
                self.callable_name,
                self.captures,
            ),
            IrExprKind::IntrinsicCall {
                kind,
                symbol,
                param_tys,
                result_ty,
                args,
            } => self.rewrite_intrinsic_call_kind(kind, symbol, param_tys, result_ty, args),
            IrExprKind::CallParts { callee, args } => rewrite_call_parts_kind(
                self.ctx,
                self.origin,
                *callee,
                args,
                self.binding,
                self.callable_name,
                self.captures,
            ),
            other => self.rewrite_effect_kind(other),
        }
    }

    fn rewrite_effect_kind(&self, kind: IrExprKind) -> IrExprKind {
        match kind {
            IrExprKind::Request {
                effect_key,
                op_index,
                args,
            } => self.rewrite_perform_kind(effect_key, op_index, args),
            IrExprKind::RequestSeq {
                effect_key,
                op_index,
                args,
            } => self.rewrite_perform_seq_kind(effect_key, op_index, args),
            IrExprKind::AnswerLit {
                effect_key,
                value,
                ops,
            } => self.rewrite_answer_lit_kind(effect_key, *value, ops),
            IrExprKind::Handle {
                effect_key,
                answer,
                body,
            } => self.rewrite_handle_kind(effect_key, *answer, *body),
            IrExprKind::Resume { expr } => rewrite_resume_kind(
                self.ctx,
                self.origin,
                expr.map(|expr| *expr),
                self.binding,
                self.callable_name,
                self.captures,
            ),
            other => other,
        }
    }

    fn rewrite_record_kind(
        &self,
        ty_name: Box<str>,
        field_count: u16,
        fields: Box<[IrRecordField]>,
    ) -> IrExprKind {
        IrExprKind::Record {
            ty_name,
            field_count,
            fields: rewrite_record_fields(
                self.ctx,
                self.origin,
                fields,
                self.binding,
                self.callable_name,
                self.captures,
            ),
        }
    }

    fn rewrite_record_update_kind(&self, update: RecordUpdateRewriteInput) -> IrExprKind {
        IrExprKind::RecordUpdate {
            ty_name: update.ty_name,
            field_count: update.field_count,
            base: Box::new(rewrite_recursive_binding_refs(
                self.ctx,
                self.origin,
                update.base,
                self.binding,
                self.callable_name,
                self.captures,
            )),
            base_fields: update.base_fields,
            result_fields: update.result_fields,
            updates: rewrite_record_fields(
                self.ctx,
                self.origin,
                update.updates,
                self.binding,
                self.callable_name,
                self.captures,
            ),
        }
    }

    fn rewrite_let_kind(
        &self,
        local_binding: Option<NameBindingId>,
        name: Box<str>,
        value: IrExpr,
    ) -> IrExprKind {
        IrExprKind::Let {
            binding: local_binding,
            name,
            value: Box::new(rewrite_recursive_binding_refs(
                self.ctx,
                self.origin,
                value,
                self.binding,
                self.callable_name,
                self.captures,
            )),
        }
    }

    fn rewrite_type_apply_kind(&self, callee: IrExpr, type_args: Box<[Box<str>]>) -> IrExprKind {
        IrExprKind::TypeApply {
            callee: Box::new(rewrite_recursive_binding_refs(
                self.ctx,
                self.origin,
                callee,
                self.binding,
                self.callable_name,
                self.captures,
            )),
            type_args,
        }
    }

    fn rewrite_closure_new_kind(&self, callee: IrNameRef, captures: Box<[IrExpr]>) -> IrExprKind {
        IrExprKind::ClosureNew {
            callee,
            captures: rewrite_expr_slice(
                self.ctx,
                self.origin,
                captures,
                self.binding,
                self.callable_name,
                self.captures,
            ),
        }
    }

    fn rewrite_binary_kind(&self, op: IrBinaryOp, left: IrExpr, right: IrExpr) -> IrExprKind {
        IrExprKind::Binary {
            op,
            left: Box::new(rewrite_recursive_binding_refs(
                self.ctx,
                self.origin,
                left,
                self.binding,
                self.callable_name,
                self.captures,
            )),
            right: Box::new(rewrite_recursive_binding_refs(
                self.ctx,
                self.origin,
                right,
                self.binding,
                self.callable_name,
                self.captures,
            )),
        }
    }

    fn rewrite_bool_and_kind(&self, left: IrExpr, right: IrExpr) -> IrExprKind {
        IrExprKind::BoolAnd {
            left: Box::new(rewrite_recursive_binding_refs(
                self.ctx,
                self.origin,
                left,
                self.binding,
                self.callable_name,
                self.captures,
            )),
            right: Box::new(rewrite_recursive_binding_refs(
                self.ctx,
                self.origin,
                right,
                self.binding,
                self.callable_name,
                self.captures,
            )),
        }
    }

    fn rewrite_bool_or_kind(&self, left: IrExpr, right: IrExpr) -> IrExprKind {
        IrExprKind::BoolOr {
            left: Box::new(rewrite_recursive_binding_refs(
                self.ctx,
                self.origin,
                left,
                self.binding,
                self.callable_name,
                self.captures,
            )),
            right: Box::new(rewrite_recursive_binding_refs(
                self.ctx,
                self.origin,
                right,
                self.binding,
                self.callable_name,
                self.captures,
            )),
        }
    }

    fn rewrite_intrinsic_call_kind(
        &self,
        kind: IrIntrinsicKind,
        symbol: Box<str>,
        param_tys: Box<[Box<str>]>,
        result_ty: Box<str>,
        args: Box<[IrArg]>,
    ) -> IrExprKind {
        IrExprKind::IntrinsicCall {
            kind,
            symbol,
            param_tys,
            result_ty,
            args: rewrite_call_args(
                self.ctx,
                self.origin,
                args,
                self.binding,
                self.callable_name,
                self.captures,
            ),
        }
    }

    fn rewrite_range_kind(
        &self,
        ty_name: Box<str>,
        kind: IrRangeKind,
        lower: IrExpr,
        upper: IrExpr,
        bounds_evidence: Option<Box<IrExpr>>,
    ) -> IrExprKind {
        IrExprKind::Range {
            ty_name,
            kind,
            lower: Box::new(rewrite_recursive_binding_refs(
                self.ctx,
                self.origin,
                lower,
                self.binding,
                self.callable_name,
                self.captures,
            )),
            upper: Box::new(rewrite_recursive_binding_refs(
                self.ctx,
                self.origin,
                upper,
                self.binding,
                self.callable_name,
                self.captures,
            )),
            bounds_evidence: bounds_evidence.map(|expr| {
                Box::new(rewrite_recursive_binding_refs(
                    self.ctx,
                    self.origin,
                    *expr,
                    self.binding,
                    self.callable_name,
                    self.captures,
                ))
            }),
        }
    }

    fn rewrite_range_contains_kind(
        &self,
        value: IrExpr,
        range: IrExpr,
        evidence: IrExpr,
    ) -> IrExprKind {
        IrExprKind::RangeContains {
            value: Box::new(rewrite_recursive_binding_refs(
                self.ctx,
                self.origin,
                value,
                self.binding,
                self.callable_name,
                self.captures,
            )),
            range: Box::new(rewrite_recursive_binding_refs(
                self.ctx,
                self.origin,
                range,
                self.binding,
                self.callable_name,
                self.captures,
            )),
            evidence: Box::new(rewrite_recursive_binding_refs(
                self.ctx,
                self.origin,
                evidence,
                self.binding,
                self.callable_name,
                self.captures,
            )),
        }
    }

    fn rewrite_range_materialize_kind(
        &self,
        range: IrExpr,
        evidence: IrExpr,
        result_ty_name: Box<str>,
    ) -> IrExprKind {
        IrExprKind::RangeMaterialize {
            range: Box::new(rewrite_recursive_binding_refs(
                self.ctx,
                self.origin,
                range,
                self.binding,
                self.callable_name,
                self.captures,
            )),
            evidence: Box::new(rewrite_recursive_binding_refs(
                self.ctx,
                self.origin,
                evidence,
                self.binding,
                self.callable_name,
                self.captures,
            )),
            result_ty_name,
        }
    }

    fn rewrite_variant_kind(
        &self,
        data_key: DefinitionKey,
        tag_index: u16,
        tag_value: i64,
        field_count: u16,
        args: Box<[IrExpr]>,
    ) -> IrExprKind {
        IrExprKind::VariantNew {
            data_key,
            tag_index,
            tag_value,
            field_count,
            args: rewrite_expr_slice(
                self.ctx,
                self.origin,
                args,
                self.binding,
                self.callable_name,
                self.captures,
            ),
        }
    }

    fn rewrite_perform_kind(
        &self,
        effect_key: DefinitionKey,
        op_index: u16,
        args: Box<[IrExpr]>,
    ) -> IrExprKind {
        IrExprKind::Request {
            effect_key,
            op_index,
            args: rewrite_expr_slice(
                self.ctx,
                self.origin,
                args,
                self.binding,
                self.callable_name,
                self.captures,
            ),
        }
    }

    fn rewrite_perform_seq_kind(
        &self,
        effect_key: DefinitionKey,
        op_index: u16,
        args: Box<[IrSeqPart]>,
    ) -> IrExprKind {
        IrExprKind::RequestSeq {
            effect_key,
            op_index,
            args: rewrite_seq_parts(
                self.ctx,
                self.origin,
                args,
                self.binding,
                self.callable_name,
                self.captures,
            ),
        }
    }

    fn rewrite_answer_lit_kind(
        &self,
        effect_key: DefinitionKey,
        value: IrExpr,
        ops: Box<[IrHandleOp]>,
    ) -> IrExprKind {
        IrExprKind::AnswerLit {
            effect_key,
            value: Box::new(rewrite_recursive_binding_refs(
                self.ctx,
                self.origin,
                value,
                self.binding,
                self.callable_name,
                self.captures,
            )),
            ops: rewrite_handle_ops(
                self.ctx,
                self.origin,
                ops,
                self.binding,
                self.callable_name,
                self.captures,
            ),
        }
    }

    fn rewrite_handle_kind(
        &self,
        effect_key: DefinitionKey,
        answer: IrExpr,
        body: IrExpr,
    ) -> IrExprKind {
        IrExprKind::Handle {
            effect_key,
            answer: Box::new(rewrite_recursive_binding_refs(
                self.ctx,
                self.origin,
                answer,
                self.binding,
                self.callable_name,
                self.captures,
            )),
            body: Box::new(rewrite_recursive_binding_refs(
                self.ctx,
                self.origin,
                body,
                self.binding,
                self.callable_name,
                self.captures,
            )),
        }
    }
}

struct RecordUpdateRewriteInput {
    ty_name: Box<str>,
    field_count: u16,
    base: IrExpr,
    base_fields: Box<[IrRecordLayoutField]>,
    result_fields: Box<[IrRecordLayoutField]>,
    updates: Box<[IrRecordField]>,
}

pub(super) fn rewrite_recursive_binding_refs(
    ctx: &LowerCtx<'_>,
    origin: IrOrigin,
    expr: IrExpr,
    binding: NameBindingId,
    callable_name: &str,
    captures: &[NameBindingId],
) -> IrExpr {
    let input = RecursiveBindingInput {
        ctx,
        origin,
        binding,
        callable_name,
        captures,
    };
    input.rewrite_refs(expr)
}
