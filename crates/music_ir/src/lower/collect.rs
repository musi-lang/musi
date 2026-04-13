use super::*;

type BoundNameSetMut<'a> = &'a mut HashSet<NameBindingId>;
type BindingCollector = fn(&IrExpr, BoundNameSetMut<'_>);
type SyntheticNameSetMut<'a> = &'a mut HashSet<Box<str>>;
type SyntheticCollector = fn(&IrExpr, SyntheticNameSetMut<'_>);

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

pub(super) fn collect_used_synthetic_names(expr: &IrExpr, out: SyntheticNameSetMut<'_>) {
    match &expr.kind {
        IrExprKind::Name {
            binding: None,
            name,
            module_target: None,
        } => {
            let _ = out.insert(name.clone());
        }
        IrExprKind::Unit
        | IrExprKind::Temp { .. }
        | IrExprKind::Lit(_)
        | IrExprKind::Name { .. }
        | IrExprKind::TypeValue { .. }
        | IrExprKind::SyntaxValue { .. } => {}
        _ => collect_used_synthetic_names_nested(expr, out),
    }
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
        IrExprKind::RangeMaterialize { range, evidence } => {
            collect_used_bindings(range, out);
            collect_used_bindings(evidence, out);
        }
        IrExprKind::Index { base, indices } => {
            collect_used_bindings(base, out);
            collect_expr_slice(indices, out, collect_used_bindings);
        }
        IrExprKind::ModuleGet { base, .. }
        | IrExprKind::RecordGet { base, .. }
        | IrExprKind::TyTest { base, .. }
        | IrExprKind::TyCast { base, .. }
        | IrExprKind::Not { expr: base }
        | IrExprKind::DynamicImport { spec: base } => collect_used_bindings(base, out),
        IrExprKind::Sequence { exprs }
        | IrExprKind::Tuple { items: exprs, .. }
        | IrExprKind::Array { items: exprs, .. }
        | IrExprKind::VariantNew { args: exprs, .. }
        | IrExprKind::Perform { args: exprs, .. }
        | IrExprKind::ClosureNew {
            captures: exprs, ..
        } => collect_expr_slice(exprs, out, collect_used_bindings),
        IrExprKind::ArrayCat { parts, .. }
        | IrExprKind::CallSeq { args: parts, .. }
        | IrExprKind::PerformSeq { args: parts, .. } => {
            collect_seq_part_exprs(parts, out, collect_used_bindings);
        }
        IrExprKind::Record { fields, .. } => {
            collect_record_field_exprs(fields, out, collect_used_bindings);
        }
        IrExprKind::RecordUpdate { base, updates, .. } => {
            collect_used_bindings(base, out);
            collect_record_field_exprs(updates, out, collect_used_bindings);
        }
        IrExprKind::Case { scrutinee, arms } => {
            collect_used_bindings(scrutinee, out);
            collect_used_in_case_arms(arms, out);
        }
        IrExprKind::Call { callee, args } => {
            collect_used_bindings(callee, out);
            collect_call_arg_exprs(args, out, collect_used_bindings);
        }
        IrExprKind::HandlerLit { value, ops, .. } => {
            collect_used_bindings(value, out);
            for op in ops {
                collect_used_bindings(&op.closure, out);
            }
        }
        IrExprKind::Handle { handler, body, .. } => {
            collect_used_bindings(handler, out);
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

fn collect_used_synthetic_names_nested(expr: &IrExpr, out: SyntheticNameSetMut<'_>) {
    match &expr.kind {
        IrExprKind::Let { value, .. } | IrExprKind::TempLet { value, .. } => {
            collect_used_synthetic_names(value, out);
        }
        IrExprKind::Assign { target, value } => {
            collect_used_synthetic_names(value, out);
            collect_used_synthetic_names_in_target(target, out);
        }
        IrExprKind::Binary { left, right, .. }
        | IrExprKind::Range {
            lower: left,
            upper: right,
            ..
        } => {
            collect_used_synthetic_names(left, out);
            collect_used_synthetic_names(right, out);
        }
        IrExprKind::RangeContains {
            value,
            range,
            evidence,
        } => {
            collect_used_synthetic_names(value, out);
            collect_used_synthetic_names(range, out);
            collect_used_synthetic_names(evidence, out);
        }
        IrExprKind::RangeMaterialize { range, evidence } => {
            collect_used_synthetic_names(range, out);
            collect_used_synthetic_names(evidence, out);
        }
        IrExprKind::Index { base, indices } => {
            collect_used_synthetic_names(base, out);
            collect_synthetic_expr_slice(indices, out, collect_used_synthetic_names);
        }
        IrExprKind::ModuleGet { base, .. }
        | IrExprKind::RecordGet { base, .. }
        | IrExprKind::TyTest { base, .. }
        | IrExprKind::TyCast { base, .. }
        | IrExprKind::Not { expr: base }
        | IrExprKind::DynamicImport { spec: base } => collect_used_synthetic_names(base, out),
        IrExprKind::Sequence { exprs }
        | IrExprKind::Tuple { items: exprs, .. }
        | IrExprKind::Array { items: exprs, .. }
        | IrExprKind::VariantNew { args: exprs, .. }
        | IrExprKind::Perform { args: exprs, .. }
        | IrExprKind::ClosureNew {
            captures: exprs, ..
        } => collect_synthetic_expr_slice(exprs, out, collect_used_synthetic_names),
        IrExprKind::ArrayCat { parts, .. }
        | IrExprKind::CallSeq { args: parts, .. }
        | IrExprKind::PerformSeq { args: parts, .. } => {
            collect_synthetic_seq_part_exprs(parts, out, collect_used_synthetic_names);
        }
        IrExprKind::Record { fields, .. } => {
            collect_synthetic_record_field_exprs(fields, out, collect_used_synthetic_names);
        }
        IrExprKind::RecordUpdate { base, updates, .. } => {
            collect_used_synthetic_names(base, out);
            collect_synthetic_record_field_exprs(updates, out, collect_used_synthetic_names);
        }
        IrExprKind::Case { scrutinee, arms } => {
            collect_used_synthetic_names(scrutinee, out);
            for arm in arms {
                if let Some(guard) = &arm.guard {
                    collect_used_synthetic_names(guard, out);
                }
                collect_used_synthetic_names(&arm.expr, out);
            }
        }
        IrExprKind::Call { callee, args } => {
            collect_used_synthetic_names(callee, out);
            for arg in args {
                collect_used_synthetic_names(&arg.expr, out);
            }
        }
        IrExprKind::HandlerLit { value, ops, .. } => {
            collect_used_synthetic_names(value, out);
            for op in ops {
                collect_used_synthetic_names(&op.closure, out);
            }
        }
        IrExprKind::Handle { handler, body, .. } => {
            collect_used_synthetic_names(handler, out);
            collect_used_synthetic_names(body, out);
        }
        IrExprKind::Resume { expr } => {
            if let Some(expr) = expr {
                collect_used_synthetic_names(expr, out);
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
        IrExprKind::RangeMaterialize { range, evidence } => {
            collect_local_decl_bindings(range, out);
            collect_local_decl_bindings(evidence, out);
        }
        IrExprKind::Index { base, indices } => {
            collect_local_decl_bindings(base, out);
            collect_expr_slice(indices, out, collect_local_decl_bindings);
        }
        IrExprKind::ModuleGet { base, .. }
        | IrExprKind::RecordGet { base, .. }
        | IrExprKind::TyTest { base, .. }
        | IrExprKind::TyCast { base, .. }
        | IrExprKind::Not { expr: base }
        | IrExprKind::DynamicImport { spec: base } => collect_local_decl_bindings(base, out),
        IrExprKind::Sequence { exprs }
        | IrExprKind::Tuple { items: exprs, .. }
        | IrExprKind::Array { items: exprs, .. }
        | IrExprKind::VariantNew { args: exprs, .. }
        | IrExprKind::Perform { args: exprs, .. }
        | IrExprKind::ClosureNew {
            captures: exprs, ..
        } => {
            collect_expr_slice(exprs, out, collect_local_decl_bindings);
        }
        IrExprKind::ArrayCat { parts, .. }
        | IrExprKind::CallSeq { args: parts, .. }
        | IrExprKind::PerformSeq { args: parts, .. } => {
            collect_seq_part_exprs(parts, out, collect_local_decl_bindings);
        }
        IrExprKind::Record { fields, .. } => {
            collect_record_field_exprs(fields, out, collect_local_decl_bindings);
        }
        IrExprKind::RecordUpdate { base, updates, .. } => {
            collect_local_decl_bindings(base, out);
            collect_record_field_exprs(updates, out, collect_local_decl_bindings);
        }
        IrExprKind::Case { scrutinee, arms } => {
            collect_local_case_arm_bindings(scrutinee, arms, out);
        }
        IrExprKind::Call { callee, args } => {
            collect_local_decl_bindings(callee, out);
            collect_call_arg_exprs(args, out, collect_local_decl_bindings);
        }
        IrExprKind::HandlerLit { value, ops, .. } => {
            collect_local_decl_bindings(value, out);
            for op in ops {
                collect_local_decl_bindings(&op.closure, out);
            }
        }
        IrExprKind::Handle { handler, body, .. } => {
            collect_local_decl_bindings(handler, out);
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
    arms: &[IrLoweredCaseArm],
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

fn collect_synthetic_expr_slice(
    exprs: &[IrExpr],
    out: SyntheticNameSetMut<'_>,
    collect: SyntheticCollector,
) {
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

fn collect_synthetic_seq_part_exprs(
    parts: &[IrSeqPart],
    out: SyntheticNameSetMut<'_>,
    collect: SyntheticCollector,
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

fn collect_synthetic_record_field_exprs(
    fields: &[IrRecordField],
    out: SyntheticNameSetMut<'_>,
    collect: SyntheticCollector,
) {
    for field in fields {
        collect(&field.expr, out);
    }
}

fn collect_used_synthetic_names_in_target(target: &IrAssignTarget, out: SyntheticNameSetMut<'_>) {
    match target {
        IrAssignTarget::Binding { .. } => {}
        IrAssignTarget::Index { base, indices } => {
            collect_used_synthetic_names(base, out);
            collect_synthetic_expr_slice(indices, out, collect_used_synthetic_names);
        }
        IrAssignTarget::RecordField { base, .. } => collect_used_synthetic_names(base, out),
    }
}

fn collect_call_arg_exprs(args: &[IrArg], out: BoundNameSetMut<'_>, collect: BindingCollector) {
    for arg in args {
        collect(&arg.expr, out);
    }
}

fn collect_used_in_case_arms(arms: &[IrLoweredCaseArm], out: BoundNameSetMut<'_>) {
    for arm in arms {
        if let Some(guard) = &arm.guard {
            collect_used_bindings(guard, out);
        }
        collect_used_bindings(&arm.expr, out);
    }
}
