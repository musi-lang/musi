use super::closures::lower_binding_capture_exprs;
use super::*;
use crate::api::IrRangeKind;

pub(super) struct RecursiveBindingInput<'a, 'b> {
    pub(super) ctx: &'a LowerCtx<'b>,
    pub(super) origin: IrOrigin,
    pub(super) binding: NameBindingId,
    pub(super) callable_name: &'a str,
    pub(super) captures: &'a [NameBindingId],
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
                module_target,
                ..
            } if found == self.binding && module_target.is_none() => IrExprKind::ClosureNew {
                callee: IrNameRef::new(self.callable_name)
                    .with_binding(self.binding)
                    .with_module_target(self.ctx.module_key.clone()),
                captures: lower_binding_capture_exprs(self.ctx, self.origin, self.captures),
            },
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
            IrExprKind::DynamicImport { spec } => rewrite_dynamic_import_kind(
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
            IrExprKind::Binary { op, left, right } => self.rewrite_binary_kind(op, *left, *right),
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
            IrExprKind::RangeMaterialize { range, evidence } => {
                self.rewrite_range_materialize_kind(*range, *evidence)
            }
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
            IrExprKind::CallSeq { callee, args } => rewrite_call_seq_kind(
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
            IrExprKind::HandlerLit {
                effect_key,
                value,
                ops,
            } => self.rewrite_handler_lit_kind(effect_key, *value, ops),
            IrExprKind::Handle {
                effect_key,
                handler,
                body,
            } => self.rewrite_handle_kind(effect_key, *handler, *body),
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

    fn rewrite_range_materialize_kind(&self, range: IrExpr, evidence: IrExpr) -> IrExprKind {
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

    fn rewrite_handler_lit_kind(
        &self,
        effect_key: DefinitionKey,
        value: IrExpr,
        ops: Box<[IrHandleOp]>,
    ) -> IrExprKind {
        IrExprKind::HandlerLit {
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
        handler: IrExpr,
        body: IrExpr,
    ) -> IrExprKind {
        IrExprKind::Handle {
            effect_key,
            handler: Box::new(rewrite_recursive_binding_refs(
                self.ctx,
                self.origin,
                handler,
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

pub(super) struct RecordUpdateRewriteInput {
    pub(super) ty_name: Box<str>,
    pub(super) field_count: u16,
    pub(super) base: IrExpr,
    pub(super) base_fields: Box<[IrRecordLayoutField]>,
    pub(super) result_fields: Box<[IrRecordLayoutField]>,
    pub(super) updates: Box<[IrRecordField]>,
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

fn rewrite_seq_parts(
    ctx: &LowerCtx<'_>,
    origin: IrOrigin,
    parts: Box<[IrSeqPart]>,
    binding: NameBindingId,
    callable_name: &str,
    captures: &[NameBindingId],
) -> Box<[IrSeqPart]> {
    parts
        .into_vec()
        .into_iter()
        .map(|part| match part {
            IrSeqPart::Expr(expr) => IrSeqPart::Expr(rewrite_recursive_binding_refs(
                ctx,
                origin,
                expr,
                binding,
                callable_name,
                captures,
            )),
            IrSeqPart::Spread(expr) => IrSeqPart::Spread(rewrite_recursive_binding_refs(
                ctx,
                origin,
                expr,
                binding,
                callable_name,
                captures,
            )),
        })
        .collect::<Vec<_>>()
        .into_boxed_slice()
}

fn rewrite_sequence_kind(
    ctx: &LowerCtx<'_>,
    origin: IrOrigin,
    exprs: Box<[IrExpr]>,
    binding: NameBindingId,
    callable_name: &str,
    captures: &[NameBindingId],
) -> IrExprKind {
    IrExprKind::Sequence {
        exprs: exprs
            .into_vec()
            .into_iter()
            .map(|item| {
                rewrite_recursive_binding_refs(ctx, origin, item, binding, callable_name, captures)
            })
            .collect::<Vec<_>>()
            .into_boxed_slice(),
    }
}

fn rewrite_tuple_kind(
    ctx: &LowerCtx<'_>,
    origin: IrOrigin,
    ty_name: Box<str>,
    items: Box<[IrExpr]>,
    binding: NameBindingId,
    callable_name: &str,
    captures: &[NameBindingId],
) -> IrExprKind {
    IrExprKind::Tuple {
        ty_name,
        items: rewrite_expr_slice(ctx, origin, items, binding, callable_name, captures),
    }
}

fn rewrite_array_kind(
    ctx: &LowerCtx<'_>,
    origin: IrOrigin,
    ty_name: Box<str>,
    items: Box<[IrExpr]>,
    binding: NameBindingId,
    callable_name: &str,
    captures: &[NameBindingId],
) -> IrExprKind {
    IrExprKind::Array {
        ty_name,
        items: rewrite_expr_slice(ctx, origin, items, binding, callable_name, captures),
    }
}

fn rewrite_array_cat_kind(
    ctx: &LowerCtx<'_>,
    origin: IrOrigin,
    ty_name: Box<str>,
    parts: Box<[IrSeqPart]>,
    binding: NameBindingId,
    callable_name: &str,
    captures: &[NameBindingId],
) -> IrExprKind {
    IrExprKind::ArrayCat {
        ty_name,
        parts: rewrite_seq_parts(ctx, origin, parts, binding, callable_name, captures),
    }
}

fn rewrite_record_get_kind(
    ctx: &LowerCtx<'_>,
    origin: IrOrigin,
    base: IrExpr,
    index: u16,
    binding: NameBindingId,
    callable_name: &str,
    captures: &[NameBindingId],
) -> IrExprKind {
    IrExprKind::RecordGet {
        base: Box::new(rewrite_recursive_binding_refs(
            ctx,
            origin,
            base,
            binding,
            callable_name,
            captures,
        )),
        index,
    }
}

fn rewrite_temp_let_kind(
    ctx: &LowerCtx<'_>,
    origin: IrOrigin,
    temp: IrTempId,
    value: IrExpr,
    binding: NameBindingId,
    callable_name: &str,
    captures: &[NameBindingId],
) -> IrExprKind {
    IrExprKind::TempLet {
        temp,
        value: Box::new(rewrite_recursive_binding_refs(
            ctx,
            origin,
            value,
            binding,
            callable_name,
            captures,
        )),
    }
}

fn rewrite_assign_kind(
    ctx: &LowerCtx<'_>,
    origin: IrOrigin,
    target: IrAssignTarget,
    value: IrExpr,
    binding: NameBindingId,
    callable_name: &str,
    captures: &[NameBindingId],
) -> IrExprKind {
    IrExprKind::Assign {
        target: Box::new(rewrite_assign_target(
            ctx,
            origin,
            target,
            binding,
            callable_name,
            captures,
        )),
        value: Box::new(rewrite_recursive_binding_refs(
            ctx,
            origin,
            value,
            binding,
            callable_name,
            captures,
        )),
    }
}

fn rewrite_index_kind(
    ctx: &LowerCtx<'_>,
    origin: IrOrigin,
    base: IrExpr,
    indices: Box<[IrExpr]>,
    binding: NameBindingId,
    callable_name: &str,
    captures: &[NameBindingId],
) -> IrExprKind {
    IrExprKind::Index {
        base: Box::new(rewrite_recursive_binding_refs(
            ctx,
            origin,
            base,
            binding,
            callable_name,
            captures,
        )),
        indices: rewrite_expr_slice(ctx, origin, indices, binding, callable_name, captures),
    }
}

fn rewrite_dynamic_import_kind(
    ctx: &LowerCtx<'_>,
    origin: IrOrigin,
    spec: IrExpr,
    binding: NameBindingId,
    callable_name: &str,
    captures: &[NameBindingId],
) -> IrExprKind {
    IrExprKind::DynamicImport {
        spec: Box::new(rewrite_recursive_binding_refs(
            ctx,
            origin,
            spec,
            binding,
            callable_name,
            captures,
        )),
    }
}

fn rewrite_module_get_kind(
    ctx: &LowerCtx<'_>,
    origin: IrOrigin,
    base: IrExpr,
    name: Box<str>,
    binding: NameBindingId,
    callable_name: &str,
    captures: &[NameBindingId],
) -> IrExprKind {
    IrExprKind::ModuleGet {
        base: Box::new(rewrite_recursive_binding_refs(
            ctx,
            origin,
            base,
            binding,
            callable_name,
            captures,
        )),
        name,
    }
}

fn rewrite_not_kind(
    ctx: &LowerCtx<'_>,
    origin: IrOrigin,
    expr: IrExpr,
    binding: NameBindingId,
    callable_name: &str,
    captures: &[NameBindingId],
) -> IrExprKind {
    IrExprKind::Not {
        expr: Box::new(rewrite_recursive_binding_refs(
            ctx,
            origin,
            expr,
            binding,
            callable_name,
            captures,
        )),
    }
}

fn rewrite_ty_test_kind(
    ctx: &LowerCtx<'_>,
    origin: IrOrigin,
    base: IrExpr,
    ty_name: Box<str>,
    binding: NameBindingId,
    callable_name: &str,
    captures: &[NameBindingId],
) -> IrExprKind {
    IrExprKind::TyTest {
        base: Box::new(rewrite_recursive_binding_refs(
            ctx,
            origin,
            base,
            binding,
            callable_name,
            captures,
        )),
        ty_name,
    }
}

fn rewrite_ty_cast_kind(
    ctx: &LowerCtx<'_>,
    origin: IrOrigin,
    base: IrExpr,
    ty_name: Box<str>,
    binding: NameBindingId,
    callable_name: &str,
    captures: &[NameBindingId],
) -> IrExprKind {
    IrExprKind::TyCast {
        base: Box::new(rewrite_recursive_binding_refs(
            ctx,
            origin,
            base,
            binding,
            callable_name,
            captures,
        )),
        ty_name,
    }
}

fn rewrite_case_kind(
    ctx: &LowerCtx<'_>,
    origin: IrOrigin,
    scrutinee: IrExpr,
    arms: LoweredMatchArmList,
    binding: NameBindingId,
    callable_name: &str,
    captures: &[NameBindingId],
) -> IrExprKind {
    IrExprKind::Match {
        scrutinee: Box::new(rewrite_recursive_binding_refs(
            ctx,
            origin,
            scrutinee,
            binding,
            callable_name,
            captures,
        )),
        arms: rewrite_match_arms(ctx, origin, arms, binding, callable_name, captures),
    }
}

fn rewrite_call_kind(
    ctx: &LowerCtx<'_>,
    origin: IrOrigin,
    callee: IrExpr,
    args: Box<[IrArg]>,
    binding: NameBindingId,
    callable_name: &str,
    captures: &[NameBindingId],
) -> IrExprKind {
    IrExprKind::Call {
        callee: Box::new(rewrite_recursive_binding_refs(
            ctx,
            origin,
            callee,
            binding,
            callable_name,
            captures,
        )),
        args: rewrite_call_args(ctx, origin, args, binding, callable_name, captures),
    }
}

fn rewrite_call_seq_kind(
    ctx: &LowerCtx<'_>,
    origin: IrOrigin,
    callee: IrExpr,
    args: Box<[IrSeqPart]>,
    binding: NameBindingId,
    callable_name: &str,
    captures: &[NameBindingId],
) -> IrExprKind {
    IrExprKind::CallSeq {
        callee: Box::new(rewrite_recursive_binding_refs(
            ctx,
            origin,
            callee,
            binding,
            callable_name,
            captures,
        )),
        args: rewrite_seq_parts(ctx, origin, args, binding, callable_name, captures),
    }
}

fn rewrite_resume_kind(
    ctx: &LowerCtx<'_>,
    origin: IrOrigin,
    expr: Option<IrExpr>,
    binding: NameBindingId,
    callable_name: &str,
    captures: &[NameBindingId],
) -> IrExprKind {
    IrExprKind::Resume {
        expr: expr.map(|expr| {
            Box::new(rewrite_recursive_binding_refs(
                ctx,
                origin,
                expr,
                binding,
                callable_name,
                captures,
            ))
        }),
    }
}

fn rewrite_record_fields(
    ctx: &LowerCtx<'_>,
    origin: IrOrigin,
    fields: Box<[IrRecordField]>,
    binding: NameBindingId,
    callable_name: &str,
    captures: &[NameBindingId],
) -> Box<[IrRecordField]> {
    fields
        .into_vec()
        .into_iter()
        .map(|field| {
            IrRecordField::new(
                field.name,
                field.index,
                rewrite_recursive_binding_refs(
                    ctx,
                    origin,
                    field.expr,
                    binding,
                    callable_name,
                    captures,
                ),
            )
        })
        .collect::<Vec<_>>()
        .into_boxed_slice()
}

fn rewrite_call_args(
    ctx: &LowerCtx<'_>,
    origin: IrOrigin,
    args: Box<[IrArg]>,
    binding: NameBindingId,
    callable_name: &str,
    captures: &[NameBindingId],
) -> Box<[IrArg]> {
    args.into_vec()
        .into_iter()
        .map(|arg| {
            IrArg::new(
                arg.spread,
                rewrite_recursive_binding_refs(
                    ctx,
                    origin,
                    arg.expr,
                    binding,
                    callable_name,
                    captures,
                ),
            )
        })
        .collect::<Vec<_>>()
        .into_boxed_slice()
}

fn rewrite_match_arms(
    ctx: &LowerCtx<'_>,
    origin: IrOrigin,
    arms: LoweredMatchArmList,
    binding: NameBindingId,
    callable_name: &str,
    captures: &[NameBindingId],
) -> LoweredMatchArmList {
    arms.into_vec()
        .into_iter()
        .map(|arm| IrLoweredMatchArm {
            guard: arm.guard.map(|guard| {
                rewrite_recursive_binding_refs(ctx, origin, guard, binding, callable_name, captures)
            }),
            expr: rewrite_recursive_binding_refs(
                ctx,
                origin,
                arm.expr,
                binding,
                callable_name,
                captures,
            ),
            ..arm
        })
        .collect::<Vec<_>>()
        .into_boxed_slice()
}

fn rewrite_handle_ops(
    ctx: &LowerCtx<'_>,
    origin: IrOrigin,
    ops: Box<[IrHandleOp]>,
    binding: NameBindingId,
    callable_name: &str,
    captures: &[NameBindingId],
) -> Box<[IrHandleOp]> {
    ops.into_vec()
        .into_iter()
        .map(|op| {
            IrHandleOp::new(
                op.op_index,
                op.name,
                rewrite_recursive_binding_refs(
                    ctx,
                    origin,
                    op.closure,
                    binding,
                    callable_name,
                    captures,
                ),
            )
        })
        .collect::<Vec<_>>()
        .into_boxed_slice()
}

fn rewrite_expr_slice(
    ctx: &LowerCtx<'_>,
    origin: IrOrigin,
    exprs: Box<[IrExpr]>,
    binding: NameBindingId,
    callable_name: &str,
    captures: &[NameBindingId],
) -> Box<[IrExpr]> {
    exprs
        .into_vec()
        .into_iter()
        .map(|expr| {
            rewrite_recursive_binding_refs(ctx, origin, expr, binding, callable_name, captures)
        })
        .collect::<Vec<_>>()
        .into_boxed_slice()
}

fn rewrite_assign_target(
    ctx: &LowerCtx<'_>,
    origin: IrOrigin,
    target: IrAssignTarget,
    binding: NameBindingId,
    callable_name: &str,
    captures: &[NameBindingId],
) -> IrAssignTarget {
    match target {
        IrAssignTarget::Binding {
            binding,
            name,
            module_target,
        } => IrAssignTarget::Binding {
            binding,
            name,
            module_target,
        },
        IrAssignTarget::Index { base, indices } => IrAssignTarget::Index {
            base: Box::new(rewrite_recursive_binding_refs(
                ctx,
                origin,
                *base,
                binding,
                callable_name,
                captures,
            )),
            indices: rewrite_expr_slice(ctx, origin, indices, binding, callable_name, captures),
        },
        IrAssignTarget::RecordField { base, index } => IrAssignTarget::RecordField {
            base: Box::new(rewrite_recursive_binding_refs(
                ctx,
                origin,
                *base,
                binding,
                callable_name,
                captures,
            )),
            index,
        },
    }
}
