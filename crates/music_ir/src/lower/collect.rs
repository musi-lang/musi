use super::*;

type BoundNameSetMut<'a> = &'a mut HashSet<NameBindingId>;
type BindingCollector = fn(&IrExpr, BoundNameSetMut<'_>);
struct SyntheticNameSetMut<'a> {
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
                for arm in arms {
                    if let Some(guard) = &arm.guard {
                        self.collect_used(guard);
                    }
                    self.collect_used(&arm.expr);
                }
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

pub(super) fn collect_used_bindings(expr: &IrExpr, out: BoundNameSetMut<'_>) {
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

pub(super) fn collect_local_decl_bindings(expr: &IrExpr, out: BoundNameSetMut<'_>) {
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

pub(super) fn collect_used_synthetic_names(expr: &IrExpr, out: &mut HashSet<Box<str>>) {
    let mut out = SyntheticNameSetMut::new(out);
    out.collect_used(expr);
}

fn collect_used_bindings_nested(expr: &IrExpr, out: BoundNameSetMut<'_>) {
    match &expr.kind {
        IrExprKind::Let { value, .. } | IrExprKind::TempLet { value, .. } => {
            collect_used_bindings(value, out);
        }
        IrExprKind::Assign { target, value } => collect_used_in_assign_target(value, target, out),
        IrExprKind::Binary { left, right, .. }
        | IrExprKind::Range {
            lower: left,
            upper: right,
            ..
        } => {
            collect_used_bindings(left, out);
            collect_used_bindings(right, out);
        }
        IrExprKind::RangeContains {
            value,
            range,
            evidence,
        } => {
            collect_used_bindings(value, out);
            collect_used_bindings(range, out);
            collect_used_bindings(evidence, out);
        }
        IrExprKind::RangeMaterialize {
            range, evidence, ..
        } => {
            collect_used_bindings(range, out);
            collect_used_bindings(evidence, out);
        }
        IrExprKind::Index { base, indices } => {
            collect_used_bindings(base, out);
            collect_expr_slice(indices, out, collect_used_bindings);
        }
        IrExprKind::ModuleGet { base, .. }
        | IrExprKind::RecordGet { base, .. }
        | IrExprKind::TypeApply { callee: base, .. }
        | IrExprKind::TyTest { base, .. }
        | IrExprKind::TyCast { base, .. }
        | IrExprKind::Not { expr: base }
        | IrExprKind::ModuleLoad { spec: base } => collect_used_bindings(base, out),
        IrExprKind::Sequence { exprs }
        | IrExprKind::Tuple { items: exprs, .. }
        | IrExprKind::Array { items: exprs, .. }
        | IrExprKind::VariantNew { args: exprs, .. }
        | IrExprKind::Request { args: exprs, .. }
        | IrExprKind::ClosureNew {
            captures: exprs, ..
        } => collect_expr_slice(exprs, out, collect_used_bindings),
        IrExprKind::ArrayCat { parts, .. }
        | IrExprKind::CallParts { args: parts, .. }
        | IrExprKind::RequestSeq { args: parts, .. } => {
            collect_seq_part_exprs(parts, out, collect_used_bindings);
        }
        IrExprKind::Record { fields, .. } => {
            collect_record_field_exprs(fields, out, collect_used_bindings);
        }
        IrExprKind::RecordUpdate { base, updates, .. } => {
            collect_used_bindings(base, out);
            collect_record_field_exprs(updates, out, collect_used_bindings);
        }
        IrExprKind::Match { scrutinee, arms } => {
            collect_used_bindings(scrutinee, out);
            collect_used_in_match_arms(arms, out);
        }
        IrExprKind::Call { callee, args } => {
            collect_used_bindings(callee, out);
            collect_call_arg_exprs(args, out, collect_used_bindings);
        }
        IrExprKind::IntrinsicCall { args, .. } => {
            collect_call_arg_exprs(args, out, collect_used_bindings);
        }
        IrExprKind::AnswerLit { value, ops, .. } => {
            collect_used_bindings(value, out);
            for op in ops {
                collect_used_bindings(&op.closure, out);
            }
        }
        IrExprKind::Handle { answer, body, .. } => {
            collect_used_bindings(answer, out);
            collect_used_bindings(body, out);
        }
        IrExprKind::Resume { expr } => {
            if let Some(expr) = expr {
                collect_used_bindings(expr, out);
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

fn collect_local_decl_bindings_nested(expr: &IrExpr, out: BoundNameSetMut<'_>) {
    match &expr.kind {
        IrExprKind::Let { value, .. }
        | IrExprKind::TempLet { value, .. }
        | IrExprKind::Assign { value, .. } => collect_local_decl_bindings(value, out),
        IrExprKind::Binary { left, right, .. }
        | IrExprKind::Range {
            lower: left,
            upper: right,
            ..
        } => {
            collect_local_decl_bindings(left, out);
            collect_local_decl_bindings(right, out);
        }
        IrExprKind::RangeContains {
            value,
            range,
            evidence,
        } => {
            collect_local_decl_bindings(value, out);
            collect_local_decl_bindings(range, out);
            collect_local_decl_bindings(evidence, out);
        }
        IrExprKind::RangeMaterialize {
            range, evidence, ..
        } => {
            collect_local_decl_bindings(range, out);
            collect_local_decl_bindings(evidence, out);
        }
        IrExprKind::Index { base, indices } => {
            collect_local_decl_bindings(base, out);
            collect_expr_slice(indices, out, collect_local_decl_bindings);
        }
        IrExprKind::ModuleGet { base, .. }
        | IrExprKind::RecordGet { base, .. }
        | IrExprKind::TypeApply { callee: base, .. }
        | IrExprKind::TyTest { base, .. }
        | IrExprKind::TyCast { base, .. }
        | IrExprKind::Not { expr: base }
        | IrExprKind::ModuleLoad { spec: base } => collect_local_decl_bindings(base, out),
        IrExprKind::Sequence { exprs }
        | IrExprKind::Tuple { items: exprs, .. }
        | IrExprKind::Array { items: exprs, .. }
        | IrExprKind::VariantNew { args: exprs, .. }
        | IrExprKind::Request { args: exprs, .. }
        | IrExprKind::ClosureNew {
            captures: exprs, ..
        } => {
            collect_expr_slice(exprs, out, collect_local_decl_bindings);
        }
        IrExprKind::ArrayCat { parts, .. }
        | IrExprKind::CallParts { args: parts, .. }
        | IrExprKind::RequestSeq { args: parts, .. } => {
            collect_seq_part_exprs(parts, out, collect_local_decl_bindings);
        }
        IrExprKind::Record { fields, .. } => {
            collect_record_field_exprs(fields, out, collect_local_decl_bindings);
        }
        IrExprKind::RecordUpdate { base, updates, .. } => {
            collect_local_decl_bindings(base, out);
            collect_record_field_exprs(updates, out, collect_local_decl_bindings);
        }
        IrExprKind::Match { scrutinee, arms } => {
            collect_local_case_arm_bindings(scrutinee, arms, out);
        }
        IrExprKind::Call { callee, args } => {
            collect_local_decl_bindings(callee, out);
            collect_call_arg_exprs(args, out, collect_local_decl_bindings);
        }
        IrExprKind::IntrinsicCall { args, .. } => {
            collect_call_arg_exprs(args, out, collect_local_decl_bindings);
        }
        IrExprKind::AnswerLit { value, ops, .. } => {
            collect_local_decl_bindings(value, out);
            for op in ops {
                collect_local_decl_bindings(&op.closure, out);
            }
        }
        IrExprKind::Handle { answer, body, .. } => {
            collect_local_decl_bindings(answer, out);
            collect_local_decl_bindings(body, out);
        }
        IrExprKind::Resume { expr } => {
            if let Some(expr) = expr {
                collect_local_decl_bindings(expr, out);
            }
        }
        IrExprKind::Unit
        | IrExprKind::Name { .. }
        | IrExprKind::Temp { .. }
        | IrExprKind::Lit(_)
        | IrExprKind::TypeValue { .. }
        | IrExprKind::SyntaxValue { .. } => {}
    }
}

fn collect_local_case_arm_bindings(
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

fn collect_used_in_assign_target(
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

fn collect_expr_slice(exprs: &[IrExpr], out: BoundNameSetMut<'_>, collect: BindingCollector) {
    for expr in exprs {
        collect(expr, out);
    }
}

fn for_each_seq_part_expr(parts: &[IrSeqPart], mut f: impl FnMut(&IrExpr)) {
    for part in parts {
        match part {
            IrSeqPart::Expr(expr) | IrSeqPart::Spread(expr) => f(expr),
        }
    }
}

fn collect_seq_part_exprs(
    parts: &[IrSeqPart],
    out: BoundNameSetMut<'_>,
    collect: BindingCollector,
) {
    for_each_seq_part_expr(parts, |expr| collect(expr, out));
}

fn collect_record_field_exprs(
    fields: &[IrRecordField],
    out: BoundNameSetMut<'_>,
    collect: BindingCollector,
) {
    for field in fields {
        collect(&field.expr, out);
    }
}

fn collect_call_arg_exprs(args: &[IrArg], out: BoundNameSetMut<'_>, collect: BindingCollector) {
    for arg in args {
        collect(&arg.expr, out);
    }
}

fn collect_used_in_match_arms(arms: &[IrLoweredMatchArm], out: BoundNameSetMut<'_>) {
    for arm in arms {
        if let Some(guard) = &arm.guard {
            collect_used_bindings(guard, out);
        }
        collect_used_bindings(&arm.expr, out);
    }
}
