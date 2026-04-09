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
        | IrExprKind::Unsupported { .. }
        | IrExprKind::Name { binding: None, .. } => {}
        IrExprKind::Let { value, .. } | IrExprKind::TempLet { value, .. } => {
            collect_used_bindings(value, out);
        }
        IrExprKind::Assign { target, value } => collect_used_in_assign_target(value, target, out),
        IrExprKind::Index { base, index } | IrExprKind::Binary { left: base, right: index, .. } => {
            collect_used_bindings(base, out);
            collect_used_bindings(index, out);
        }
        IrExprKind::RecordGet { base, .. } => collect_used_bindings(base, out),
        IrExprKind::Sequence { exprs } => collect_used_in_exprs(exprs, out),
        IrExprKind::Tuple { items, .. } | IrExprKind::Array { items, .. } => collect_used_in_exprs(items, out),
        IrExprKind::ArrayCat { parts, .. } => collect_used_in_seq_parts(parts, out),
        IrExprKind::Record { fields, .. } => collect_used_in_record_fields(fields, out),
        IrExprKind::RecordUpdate { base, updates, .. } => {
            collect_used_bindings(base, out);
            collect_used_in_record_fields(updates, out);
        }
        IrExprKind::ClosureNew { captures, .. } => collect_used_in_exprs(captures, out),
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
            collect_used_in_call_args(args, out);
        }
        IrExprKind::CallSeq { callee, args } => {
            collect_used_bindings(callee, out);
            collect_used_in_seq_parts(args, out);
        }
        IrExprKind::VariantNew { args, .. } | IrExprKind::Perform { args, .. } => collect_used_in_exprs(args, out),
        IrExprKind::PerformSeq { args, .. } => collect_used_in_seq_parts(args, out),
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
        | IrExprKind::Unsupported { .. } => {}
        IrExprKind::Let { value, .. } | IrExprKind::TempLet { value, .. } | IrExprKind::Assign { value, .. } => {
            collect_local_decl_bindings(value, out);
        }
        IrExprKind::Index { base, index } | IrExprKind::Binary { left: base, right: index, .. } => {
            collect_local_decl_bindings(base, out);
            collect_local_decl_bindings(index, out);
        }
        IrExprKind::RecordGet { base, .. } => collect_local_decl_bindings(base, out),
        IrExprKind::Sequence { exprs } => collect_local_in_exprs(exprs, out),
        IrExprKind::Tuple { items, .. } | IrExprKind::Array { items, .. } => collect_local_in_exprs(items, out),
        IrExprKind::ArrayCat { parts, .. } => collect_local_in_seq_parts(parts, out),
        IrExprKind::Record { fields, .. } => collect_local_in_record_fields(fields, out),
        IrExprKind::RecordUpdate { base, updates, .. } => {
            collect_local_decl_bindings(base, out);
            collect_local_in_record_fields(updates, out);
        }
        IrExprKind::ClosureNew { captures, .. } => collect_local_in_exprs(captures, out),
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
            collect_local_in_call_args(args, out);
        }
        IrExprKind::CallSeq { callee, args } => {
            collect_local_decl_bindings(callee, out);
            collect_local_in_seq_parts(args, out);
        }
        IrExprKind::VariantNew { args, .. } | IrExprKind::Perform { args, .. } => collect_local_in_exprs(args, out),
        IrExprKind::PerformSeq { args, .. } => collect_local_in_seq_parts(args, out),
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
    }
}

fn collect_used_in_assign_target(value: &IrExpr, target: &IrAssignTarget, out: &mut HashSet<NameBindingId>) {
    collect_used_bindings(value, out);
    match target {
        IrAssignTarget::Binding { .. } => {}
        IrAssignTarget::Index { base, index } => {
            collect_used_bindings(base, out);
            collect_used_bindings(index, out);
        }
        IrAssignTarget::RecordField { base, .. } => collect_used_bindings(base, out),
    }
}

fn collect_used_in_exprs(exprs: &[IrExpr], out: &mut HashSet<NameBindingId>) {
    for expr in exprs {
        collect_used_bindings(expr, out);
    }
}

fn for_each_seq_part_expr(parts: &[IrSeqPart], mut f: impl FnMut(&IrExpr)) {
    for part in parts {
        match part {
            IrSeqPart::Expr(expr) | IrSeqPart::Spread(expr) => f(expr),
        }
    }
}

fn collect_used_in_seq_parts(parts: &[IrSeqPart], out: &mut HashSet<NameBindingId>) {
    for_each_seq_part_expr(parts, |expr| collect_used_bindings(expr, out));
}

fn collect_used_in_record_fields(fields: &[IrRecordField], out: &mut HashSet<NameBindingId>) {
    for field in fields {
        collect_used_bindings(&field.expr, out);
    }
}

fn collect_used_in_call_args(args: &[IrArg], out: &mut HashSet<NameBindingId>) {
    for arg in args {
        collect_used_bindings(&arg.expr, out);
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

fn collect_local_in_exprs(exprs: &[IrExpr], out: &mut HashSet<NameBindingId>) {
    for expr in exprs {
        collect_local_decl_bindings(expr, out);
    }
}

fn collect_local_in_seq_parts(parts: &[IrSeqPart], out: &mut HashSet<NameBindingId>) {
    for_each_seq_part_expr(parts, |expr| collect_local_decl_bindings(expr, out));
}

fn collect_local_in_record_fields(fields: &[IrRecordField], out: &mut HashSet<NameBindingId>) {
    for field in fields {
        collect_local_decl_bindings(&field.expr, out);
    }
}

fn collect_local_in_call_args(args: &[IrArg], out: &mut HashSet<NameBindingId>) {
    for arg in args {
        collect_local_decl_bindings(&arg.expr, out);
    }
}
