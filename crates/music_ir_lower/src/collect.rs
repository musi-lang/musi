use super::{
    HashSet, IrArg, IrAssignTarget, IrExpr, IrExprKind, IrLoweredMatchArm, IrRecordField,
    IrSeqPart, NameBindingId, collect_pattern_bindings,
};

pub(crate) type BoundNameSetMut<'a> = &'a mut HashSet<NameBindingId>;
pub(crate) type BindingCollector = fn(&IrExpr, BoundNameSetMut<'_>);
pub(crate) type AssignBindingCollector = fn(&IrExpr, &IrAssignTarget, BoundNameSetMut<'_>);
pub(crate) type MatchBindingCollector = fn(&IrExpr, &[IrLoweredMatchArm], BoundNameSetMut<'_>);
pub(crate) struct SyntheticNameSetMut<'a> {
    names: &'a mut HashSet<Box<str>>,
}

impl<'a> SyntheticNameSetMut<'a> {
    const fn new(names: &'a mut HashSet<Box<str>>) -> Self {
        Self { names }
    }

    fn insert(&mut self, name: Box<str>) {
        let _ = self.names.insert(name);
    }

    fn collect_used(&mut self, expr: &IrExpr) {
        match &expr.kind {
            IrExprKind::Name {
                binding: None,
                name,
                import_record_target: None,
            } => {
                self.insert(name.clone());
            }
            IrExprKind::Unit
            | IrExprKind::Temp { .. }
            | IrExprKind::Lit(_)
            | IrExprKind::Name { .. }
            | IrExprKind::TypeValue { .. }
            | IrExprKind::SyntaxValue { .. } => {}
            _ => self.collect_used_nested(expr),
        }
    }

    fn collect_used_nested(&mut self, expr: &IrExpr) {
        match &expr.kind {
            IrExprKind::Let { value, .. } | IrExprKind::TempLet { value, .. } => {
                self.collect_used(value);
            }
            IrExprKind::Assign { target, value } => {
                self.collect_used(value);
                self.collect_in_assign_target(target);
            }
            IrExprKind::Binary { left, right, .. }
            | IrExprKind::BoolAnd { left, right }
            | IrExprKind::BoolOr { left, right }
            | IrExprKind::Range {
                lower: left,
                upper: right,
                ..
            } => {
                self.collect_used(left);
                self.collect_used(right);
            }
            IrExprKind::RangeContains {
                value,
                range,
                evidence,
            } => {
                self.collect_used(value);
                self.collect_used(range);
                self.collect_used(evidence);
            }
            IrExprKind::RangeMaterialize {
                range, evidence, ..
            } => {
                self.collect_used(range);
                self.collect_used(evidence);
            }
            IrExprKind::Index { base, indices } => {
                self.collect_used(base);
                self.collect_expr_slice(indices);
            }
            IrExprKind::ModuleGet { base, .. }
            | IrExprKind::RecordGet { base, .. }
            | IrExprKind::TypeApply { callee: base, .. }
            | IrExprKind::TyTest { base, .. }
            | IrExprKind::TyCast { base, .. }
            | IrExprKind::Not { expr: base }
            | IrExprKind::ModuleLoad { spec: base } => self.collect_used(base),
            IrExprKind::Sequence { exprs }
            | IrExprKind::Tuple { items: exprs, .. }
            | IrExprKind::Array { items: exprs, .. }
            | IrExprKind::VariantNew { args: exprs, .. }
            | IrExprKind::Request { args: exprs, .. }
            | IrExprKind::ClosureNew {
                captures: exprs, ..
            } => self.collect_expr_slice(exprs),
            IrExprKind::ArrayCat { parts, .. }
            | IrExprKind::CallParts { args: parts, .. }
            | IrExprKind::RequestSeq { args: parts, .. } => self.collect_seq_part_exprs(parts),
            IrExprKind::Record { fields, .. } => self.collect_record_field_exprs(fields),
            IrExprKind::RecordUpdate { base, updates, .. } => {
                self.collect_used(base);
                self.collect_record_field_exprs(updates);
            }
            IrExprKind::Match { scrutinee, arms } => {
                self.collect_used(scrutinee);
                self.collect_used_in_match_arms(arms);
            }
            IrExprKind::Call { callee, args } => {
                self.collect_used(callee);
                for arg in args {
                    self.collect_used(&arg.expr);
                }
            }
            IrExprKind::IntrinsicCall { args, .. } => {
                for arg in args {
                    self.collect_used(&arg.expr);
                }
            }
            IrExprKind::AnswerLit { value, ops, .. } => {
                self.collect_used(value);
                for op in ops {
                    self.collect_used(&op.closure);
                }
            }
            IrExprKind::Handle { answer, body, .. } => {
                self.collect_used(answer);
                self.collect_used(body);
            }
            IrExprKind::Resume { expr } => {
                if let Some(expr) = expr {
                    self.collect_used(expr);
                }
            }
            IrExprKind::Unit
            | IrExprKind::Temp { .. }
            | IrExprKind::Lit(_)
            | IrExprKind::Name { .. }
            | IrExprKind::TypeValue { .. }
            | IrExprKind::SyntaxValue { .. } => {}
        }
    }

    fn collect_expr_slice(&mut self, exprs: &[IrExpr]) {
        for expr in exprs {
            self.collect_used(expr);
        }
    }

    fn collect_used_in_match_arms(&mut self, arms: &[IrLoweredMatchArm]) {
        for arm in arms {
            if let Some(guard) = &arm.guard {
                self.collect_used(guard);
            }
            self.collect_used(&arm.expr);
        }
    }

    fn collect_seq_part_exprs(&mut self, parts: &[IrSeqPart]) {
        for_each_seq_part_expr(parts, |expr| self.collect_used(expr));
    }

    fn collect_record_field_exprs(&mut self, fields: &[IrRecordField]) {
        for field in fields {
            self.collect_used(&field.expr);
        }
    }

    fn collect_in_assign_target(&mut self, target: &IrAssignTarget) {
        match target {
            IrAssignTarget::Binding { .. } => {}
            IrAssignTarget::Index { base, indices } => {
                self.collect_used(base);
                self.collect_expr_slice(indices);
            }
            IrAssignTarget::RecordField { base, .. } => self.collect_used(base),
        }
    }
}

pub(crate) fn collect_used_bindings(expr: &IrExpr, out: BoundNameSetMut<'_>) {
    match &expr.kind {
        IrExprKind::Name {
            binding: Some(binding),
            ..
        } => {
            let _ = out.insert(*binding);
        }
        IrExprKind::Unit
        | IrExprKind::Temp { .. }
        | IrExprKind::Lit(_)
        | IrExprKind::Name { binding: None, .. }
        | IrExprKind::TypeValue { .. }
        | IrExprKind::SyntaxValue { .. } => {}
        _ => collect_used_bindings_nested(expr, out),
    }
}

pub(crate) fn collect_local_decl_bindings(expr: &IrExpr, out: BoundNameSetMut<'_>) {
    match &expr.kind {
        IrExprKind::Let {
            binding: Some(binding),
            value,
            ..
        } => {
            let _ = out.insert(*binding);
            collect_local_decl_bindings(value, out);
        }
        IrExprKind::Unit
        | IrExprKind::Name { .. }
        | IrExprKind::Temp { .. }
        | IrExprKind::Lit(_)
        | IrExprKind::TypeValue { .. }
        | IrExprKind::SyntaxValue { .. } => {}
        _ => collect_local_decl_bindings_nested(expr, out),
    }
}

pub(crate) fn collect_used_synthetic_names(expr: &IrExpr, out: &mut HashSet<Box<str>>) {
    let mut out = SyntheticNameSetMut::new(out);
    out.collect_used(expr);
}

pub(crate) fn collect_used_bindings_nested(expr: &IrExpr, out: BoundNameSetMut<'_>) {
    collect_nested_bindings(
        expr,
        out,
        collect_used_bindings,
        collect_used_assign_target,
        collect_used_match_exprs,
    );
}

pub(crate) fn collect_local_decl_bindings_nested(expr: &IrExpr, out: BoundNameSetMut<'_>) {
    collect_nested_bindings(
        expr,
        out,
        collect_local_decl_bindings,
        collect_local_assign_target,
        collect_local_match_exprs,
    );
}

pub(crate) fn collect_nested_bindings(
    expr: &IrExpr,
    out: BoundNameSetMut<'_>,
    collect: BindingCollector,
    collect_assign: AssignBindingCollector,
    collect_match: MatchBindingCollector,
) {
    match &expr.kind {
        IrExprKind::Let { value, .. } | IrExprKind::TempLet { value, .. } => {
            collect(value, out);
        }
        IrExprKind::Assign { target, value } => collect_assign(value, target, out),
        IrExprKind::Binary { left, right, .. }
        | IrExprKind::BoolAnd { left, right }
        | IrExprKind::BoolOr { left, right }
        | IrExprKind::Range {
            lower: left,
            upper: right,
            ..
        } => {
            collect(left, out);
            collect(right, out);
        }
        IrExprKind::RangeContains {
            value,
            range,
            evidence,
        } => {
            collect(value, out);
            collect(range, out);
            collect(evidence, out);
        }
        IrExprKind::RangeMaterialize {
            range, evidence, ..
        } => {
            collect(range, out);
            collect(evidence, out);
        }
        IrExprKind::Index { base, indices } => {
            collect(base, out);
            collect_expr_slice(indices, out, collect);
        }
        IrExprKind::ModuleGet { base, .. }
        | IrExprKind::RecordGet { base, .. }
        | IrExprKind::TypeApply { callee: base, .. }
        | IrExprKind::TyTest { base, .. }
        | IrExprKind::TyCast { base, .. }
        | IrExprKind::Not { expr: base }
        | IrExprKind::ModuleLoad { spec: base } => collect(base, out),
        IrExprKind::Sequence { exprs }
        | IrExprKind::Tuple { items: exprs, .. }
        | IrExprKind::Array { items: exprs, .. }
        | IrExprKind::VariantNew { args: exprs, .. }
        | IrExprKind::Request { args: exprs, .. }
        | IrExprKind::ClosureNew {
            captures: exprs, ..
        } => collect_expr_slice(exprs, out, collect),
        IrExprKind::ArrayCat { parts, .. }
        | IrExprKind::CallParts { args: parts, .. }
        | IrExprKind::RequestSeq { args: parts, .. } => {
            collect_seq_part_exprs(parts, out, collect);
        }
        IrExprKind::Record { fields, .. } => {
            collect_record_field_exprs(fields, out, collect);
        }
        IrExprKind::RecordUpdate { base, updates, .. } => {
            collect(base, out);
            collect_record_field_exprs(updates, out, collect);
        }
        IrExprKind::Match { scrutinee, arms } => collect_match(scrutinee, arms, out),
        IrExprKind::Call { callee, args } => {
            collect(callee, out);
            collect_call_arg_exprs(args, out, collect);
        }
        IrExprKind::IntrinsicCall { args, .. } => {
            collect_call_arg_exprs(args, out, collect);
        }
        IrExprKind::AnswerLit { value, ops, .. } => {
            collect(value, out);
            for op in ops {
                collect(&op.closure, out);
            }
        }
        IrExprKind::Handle { answer, body, .. } => {
            collect(answer, out);
            collect(body, out);
        }
        IrExprKind::Resume { expr } => {
            if let Some(expr) = expr {
                collect(expr, out);
            }
        }
        IrExprKind::Unit
        | IrExprKind::Temp { .. }
        | IrExprKind::Lit(_)
        | IrExprKind::Name { .. }
        | IrExprKind::TypeValue { .. }
        | IrExprKind::SyntaxValue { .. } => {}
    }
}

pub(crate) fn collect_used_assign_target(
    value: &IrExpr,
    target: &IrAssignTarget,
    out: BoundNameSetMut<'_>,
) {
    collect_used_in_assign_target(value, target, out);
}

pub(crate) fn collect_local_assign_target(
    value: &IrExpr,
    _target: &IrAssignTarget,
    out: BoundNameSetMut<'_>,
) {
    collect_local_decl_bindings(value, out);
}

pub(crate) fn collect_used_match_exprs(
    scrutinee: &IrExpr,
    arms: &[IrLoweredMatchArm],
    out: BoundNameSetMut<'_>,
) {
    collect_used_bindings(scrutinee, out);
    collect_used_in_match_arms(arms, out);
}

pub(crate) fn collect_local_match_exprs(
    scrutinee: &IrExpr,
    arms: &[IrLoweredMatchArm],
    out: BoundNameSetMut<'_>,
) {
    collect_local_case_arm_bindings(scrutinee, arms, out);
}

pub(crate) fn collect_local_case_arm_bindings(
    scrutinee: &IrExpr,
    arms: &[IrLoweredMatchArm],
    out: BoundNameSetMut<'_>,
) {
    collect_local_decl_bindings(scrutinee, out);
    for arm in arms {
        collect_pattern_bindings(&arm.pattern, out);
        if let Some(guard) = &arm.guard {
            collect_local_decl_bindings(guard, out);
        }
        collect_local_decl_bindings(&arm.expr, out);
    }
}

pub(crate) fn collect_used_in_assign_target(
    value: &IrExpr,
    target: &IrAssignTarget,
    out: BoundNameSetMut<'_>,
) {
    collect_used_bindings(value, out);
    match target {
        IrAssignTarget::Binding { .. } => {}
        IrAssignTarget::Index { base, indices } => {
            collect_used_bindings(base, out);
            collect_expr_slice(indices, out, collect_used_bindings);
        }
        IrAssignTarget::RecordField { base, .. } => collect_used_bindings(base, out),
    }
}

pub(crate) fn collect_expr_slice(
    exprs: &[IrExpr],
    out: BoundNameSetMut<'_>,
    collect: BindingCollector,
) {
    for expr in exprs {
        collect(expr, out);
    }
}

pub(crate) fn for_each_seq_part_expr(parts: &[IrSeqPart], mut f: impl FnMut(&IrExpr)) {
    for part in parts {
        match part {
            IrSeqPart::Expr(expr) | IrSeqPart::Spread(expr) => f(expr),
        }
    }
}

pub(crate) fn collect_seq_part_exprs(
    parts: &[IrSeqPart],
    out: BoundNameSetMut<'_>,
    collect: BindingCollector,
) {
    for_each_seq_part_expr(parts, |expr| collect(expr, out));
}

pub(crate) fn collect_record_field_exprs(
    fields: &[IrRecordField],
    out: BoundNameSetMut<'_>,
    collect: BindingCollector,
) {
    for field in fields {
        collect(&field.expr, out);
    }
}

pub(crate) fn collect_call_arg_exprs(
    args: &[IrArg],
    out: BoundNameSetMut<'_>,
    collect: BindingCollector,
) {
    for arg in args {
        collect(&arg.expr, out);
    }
}

pub(crate) fn collect_used_in_match_arms(arms: &[IrLoweredMatchArm], out: BoundNameSetMut<'_>) {
    for arm in arms {
        if let Some(guard) = &arm.guard {
            collect_used_bindings(guard, out);
        }
        collect_used_bindings(&arm.expr, out);
    }
}
