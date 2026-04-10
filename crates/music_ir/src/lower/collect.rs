use super::*;

pub(super) fn collect_used_bindings(expr: &IrExpr, out: &mut HashSet<NameBindingId>) {
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
        IrExprKind::Let { value, .. } | IrExprKind::TempLet { value, .. } => {
            collect_used_bindings(value, out);
        }
        IrExprKind::Assign { target, value } => collect_used_in_assign_target(value, target, out),
        IrExprKind::Binary {
            left: base,
            right: index,
            ..
        } => {
            collect_used_bindings(base, out);
            collect_used_bindings(index, out);
        }
        IrExprKind::Index { base, indices } => {
            collect_used_bindings(base, out);
            collect_expr_slice(indices, out, collect_used_bindings);
        }
        IrExprKind::RecordGet { base, .. } => collect_used_bindings(base, out),
        IrExprKind::Sequence { exprs } => collect_expr_slice(exprs, out, collect_used_bindings),
        IrExprKind::Tuple { items, .. } | IrExprKind::Array { items, .. } => {
            collect_expr_slice(items, out, collect_used_bindings);
        }
        IrExprKind::ArrayCat { parts, .. } => {
            collect_seq_part_exprs(parts, out, collect_used_bindings);
        }
        IrExprKind::Record { fields, .. } => {
            collect_record_field_exprs(fields, out, collect_used_bindings);
        }
        IrExprKind::RecordUpdate { base, updates, .. } => {
            collect_used_bindings(base, out);
            collect_record_field_exprs(updates, out, collect_used_bindings);
        }
        IrExprKind::ClosureNew { captures, .. } => {
            collect_expr_slice(captures, out, collect_used_bindings);
        }
        IrExprKind::Not { expr } => collect_used_bindings(expr, out),
        IrExprKind::TyTest { base, .. } | IrExprKind::TyCast { base, .. } => {
            collect_used_bindings(base, out);
        }
        IrExprKind::Case { scrutinee, arms } => {
            collect_used_bindings(scrutinee, out);
            collect_used_in_case_arms(arms, out);
        }
        IrExprKind::Call { callee, args } => {
            collect_used_bindings(callee, out);
            collect_call_arg_exprs(args, out, collect_used_bindings);
        }
        IrExprKind::CallSeq { callee, args } => {
            collect_used_bindings(callee, out);
            collect_seq_part_exprs(args, out, collect_used_bindings);
        }
        IrExprKind::VariantNew { args, .. } | IrExprKind::Perform { args, .. } => {
            collect_expr_slice(args, out, collect_used_bindings);
        }
        IrExprKind::PerformSeq { args, .. } => {
            collect_seq_part_exprs(args, out, collect_used_bindings);
        }
        IrExprKind::Handle {
            value, ops, body, ..
        } => {
            collect_used_bindings(value, out);
            for op in ops {
                collect_used_bindings(&op.closure, out);
            }
            collect_used_bindings(body, out);
        }
        IrExprKind::Resume { expr } => {
            if let Some(expr) = expr {
                collect_used_bindings(expr, out);
            }
        }
        IrExprKind::DynamicImport { spec } => collect_used_bindings(spec, out),
    }
}

pub(super) fn collect_local_decl_bindings(expr: &IrExpr, out: &mut HashSet<NameBindingId>) {
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
        IrExprKind::Let { value, .. }
        | IrExprKind::TempLet { value, .. }
        | IrExprKind::Assign { value, .. } => {
            collect_local_decl_bindings(value, out);
        }
        IrExprKind::Binary {
            left: base,
            right: index,
            ..
        } => {
            collect_local_decl_bindings(base, out);
            collect_local_decl_bindings(index, out);
        }
        IrExprKind::Index { base, indices } => {
            collect_local_decl_bindings(base, out);
            collect_expr_slice(indices, out, collect_local_decl_bindings);
        }
        IrExprKind::RecordGet { base, .. } => collect_local_decl_bindings(base, out),
        IrExprKind::Sequence { exprs } => {
            collect_expr_slice(exprs, out, collect_local_decl_bindings);
        }
        IrExprKind::Tuple { items, .. } | IrExprKind::Array { items, .. } => {
            collect_expr_slice(items, out, collect_local_decl_bindings);
        }
        IrExprKind::ArrayCat { parts, .. } => {
            collect_seq_part_exprs(parts, out, collect_local_decl_bindings);
        }
        IrExprKind::Record { fields, .. } => {
            collect_record_field_exprs(fields, out, collect_local_decl_bindings);
        }
        IrExprKind::RecordUpdate { base, updates, .. } => {
            collect_local_decl_bindings(base, out);
            collect_record_field_exprs(updates, out, collect_local_decl_bindings);
        }
        IrExprKind::ClosureNew { captures, .. } => {
            collect_expr_slice(captures, out, collect_local_decl_bindings);
        }
        IrExprKind::Not { expr } => collect_local_decl_bindings(expr, out),
        IrExprKind::TyTest { base, .. } | IrExprKind::TyCast { base, .. } => {
            collect_local_decl_bindings(base, out);
        }
        IrExprKind::Case { scrutinee, arms } => {
            collect_local_decl_bindings(scrutinee, out);
            for arm in arms {
                collect_pattern_bindings(&arm.pattern, out);
                if let Some(guard) = &arm.guard {
                    collect_local_decl_bindings(guard, out);
                }
                collect_local_decl_bindings(&arm.expr, out);
            }
        }
        IrExprKind::Call { callee, args } => {
            collect_local_decl_bindings(callee, out);
            collect_call_arg_exprs(args, out, collect_local_decl_bindings);
        }
        IrExprKind::CallSeq { callee, args } => {
            collect_local_decl_bindings(callee, out);
            collect_seq_part_exprs(args, out, collect_local_decl_bindings);
        }
        IrExprKind::VariantNew { args, .. } | IrExprKind::Perform { args, .. } => {
            collect_expr_slice(args, out, collect_local_decl_bindings);
        }
        IrExprKind::PerformSeq { args, .. } => {
            collect_seq_part_exprs(args, out, collect_local_decl_bindings);
        }
        IrExprKind::Handle {
            value, ops, body, ..
        } => {
            collect_local_decl_bindings(value, out);
            for op in ops {
                collect_local_decl_bindings(&op.closure, out);
            }
            collect_local_decl_bindings(body, out);
        }
        IrExprKind::Resume { expr } => {
            if let Some(expr) = expr {
                collect_local_decl_bindings(expr, out);
            }
        }
        IrExprKind::DynamicImport { spec } => collect_local_decl_bindings(spec, out),
    }
}

fn collect_used_in_assign_target(
    value: &IrExpr,
    target: &IrAssignTarget,
    out: &mut HashSet<NameBindingId>,
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

fn collect_expr_slice(
    exprs: &[IrExpr],
    out: &mut HashSet<NameBindingId>,
    collect: fn(&IrExpr, &mut HashSet<NameBindingId>),
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
    out: &mut HashSet<NameBindingId>,
    collect: fn(&IrExpr, &mut HashSet<NameBindingId>),
) {
    for_each_seq_part_expr(parts, |expr| collect(expr, out));
}

fn collect_record_field_exprs(
    fields: &[IrRecordField],
    out: &mut HashSet<NameBindingId>,
    collect: fn(&IrExpr, &mut HashSet<NameBindingId>),
) {
    for field in fields {
        collect(&field.expr, out);
    }
}

fn collect_call_arg_exprs(
    args: &[IrArg],
    out: &mut HashSet<NameBindingId>,
    collect: fn(&IrExpr, &mut HashSet<NameBindingId>),
) {
    for arg in args {
        collect(&arg.expr, out);
    }
}

fn collect_used_in_case_arms(arms: &[IrLoweredCaseArm], out: &mut HashSet<NameBindingId>) {
    for arm in arms {
        if let Some(guard) = &arm.guard {
            collect_used_bindings(guard, out);
        }
        collect_used_bindings(&arm.expr, out);
    }
}
