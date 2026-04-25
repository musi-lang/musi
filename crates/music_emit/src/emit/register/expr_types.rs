use super::*;
use music_ir::IrModuleInitPart;

pub(super) fn register_expr_types(
    state: &mut ProgramState,
    module: &IrModule,
    layout: &mut ModuleLayout,
) {
    for callable in module.callables() {
        collect_expr_types(state, layout, &callable.body);
    }
    for global in module.globals() {
        collect_expr_types(state, layout, &global.body);
    }
    for init_part in module.init_parts() {
        if let IrModuleInitPart::Expr(expr) = init_part {
            collect_expr_types(state, layout, expr);
        }
    }
}

fn collect_expr_types(state: &mut ProgramState, layout: &mut ModuleLayout, expr: &IrExpr) {
    let handled = collect_expr_types_leaf(state, layout, expr)
        || collect_expr_types_aggregate(state, layout, expr)
        || collect_expr_types_binding_and_control(state, layout, expr)
        || collect_expr_types_call_and_effect(state, layout, expr);
    debug_assert!(handled, "unhandled expr type collection path");
}

fn collect_expr_types_leaf(
    state: &mut ProgramState,
    layout: &mut ModuleLayout,
    expr: &IrExpr,
) -> bool {
    match &expr.kind {
        IrExprKind::Unit
        | IrExprKind::Name { .. }
        | IrExprKind::Temp { .. }
        | IrExprKind::Lit(_)
        | IrExprKind::SyntaxValue { .. } => true,
        IrExprKind::TypeValue { ty_name } => {
            let _ = ensure_type(state, layout, ty_name);
            true
        }
        IrExprKind::TypeApply { callee, type_args } => {
            collect_expr_types(state, layout, callee);
            for ty in type_args {
                let _ = ensure_type(state, layout, ty);
            }
            true
        }
        _ => false,
    }
}

fn collect_expr_types_aggregate(
    state: &mut ProgramState,
    layout: &mut ModuleLayout,
    expr: &IrExpr,
) -> bool {
    match &expr.kind {
        IrExprKind::Sequence { exprs } => {
            collect_expr_types_iter(state, layout, exprs);
            true
        }
        IrExprKind::Tuple { ty_name, items } | IrExprKind::Array { ty_name, items } => {
            let _ = ensure_type(state, layout, ty_name);
            collect_expr_types_iter(state, layout, items);
            true
        }
        IrExprKind::Range {
            ty_name,
            lower: start,
            upper: end,
            ..
        } => {
            let _ = ensure_type(state, layout, ty_name);
            let _ = ensure_type(state, layout, "Any");
            collect_expr_types(state, layout, start);
            collect_expr_types(state, layout, end);
            true
        }
        IrExprKind::ArrayCat { ty_name, parts } => {
            let _ = ensure_type(state, layout, ty_name);
            collect_expr_types_seq_parts(state, layout, parts);
            true
        }
        IrExprKind::Record {
            ty_name, fields, ..
        } => {
            let _ = ensure_type(state, layout, ty_name);
            collect_expr_types_iter(state, layout, fields.iter().map(|field| &field.expr));
            true
        }
        IrExprKind::Index { base, indices } => {
            collect_expr_types(state, layout, base);
            collect_expr_types_iter(state, layout, indices.iter());
            true
        }
        IrExprKind::RangeContains {
            value,
            range,
            evidence,
        } => {
            let _ = ensure_type(state, layout, "Any");
            let _ = ensure_type(state, layout, "Bool");
            collect_expr_types(state, layout, value);
            collect_expr_types(state, layout, range);
            collect_expr_types(state, layout, evidence);
            true
        }
        IrExprKind::RangeMaterialize {
            range,
            evidence,
            result_ty_name,
        } => {
            let _ = ensure_type(state, layout, "Any");
            let _ = ensure_type(state, layout, result_ty_name);
            collect_expr_types(state, layout, range);
            collect_expr_types(state, layout, evidence);
            true
        }
        IrExprKind::ModuleLoad { spec } => {
            collect_expr_types(state, layout, spec);
            true
        }
        IrExprKind::ModuleGet { base, .. } | IrExprKind::RecordGet { base, .. } => {
            collect_expr_types(state, layout, base);
            true
        }
        IrExprKind::RecordUpdate {
            ty_name,
            base,
            updates,
            ..
        } => {
            let _ = ensure_type(state, layout, ty_name);
            collect_expr_types(state, layout, base);
            collect_expr_types_iter(state, layout, updates.iter().map(|update| &update.expr));
            true
        }
        IrExprKind::VariantNew { data_key, args, .. } => {
            let name = qualified_name(&data_key.module, &data_key.name);
            let _ = ensure_type(state, layout, name.as_ref());
            collect_expr_types_iter(state, layout, args);
            true
        }
        _ => false,
    }
}

fn collect_expr_types_binding_and_control(
    state: &mut ProgramState,
    layout: &mut ModuleLayout,
    expr: &IrExpr,
) -> bool {
    match &expr.kind {
        IrExprKind::Let { value, .. } | IrExprKind::TempLet { value, .. } => {
            collect_expr_types(state, layout, value);
            true
        }
        IrExprKind::Assign { target, value } => {
            collect_assign_target_types(state, layout, target);
            collect_expr_types(state, layout, value);
            true
        }
        IrExprKind::ClosureNew { captures, .. } => {
            collect_expr_types_iter(state, layout, captures);
            true
        }
        IrExprKind::Binary { left, right, .. }
        | IrExprKind::BoolAnd { left, right }
        | IrExprKind::BoolOr { left, right } => {
            collect_expr_types(state, layout, left);
            collect_expr_types(state, layout, right);
            true
        }
        IrExprKind::Not { expr } => {
            collect_expr_types(state, layout, expr);
            true
        }
        IrExprKind::TyTest { base, ty_name } | IrExprKind::TyCast { base, ty_name } => {
            let _ = ensure_type(state, layout, ty_name);
            collect_expr_types(state, layout, base);
            true
        }
        IrExprKind::Match { scrutinee, arms } => {
            collect_case_expr_types(state, layout, scrutinee, arms);
            true
        }
        _ => false,
    }
}

fn collect_expr_types_call_and_effect(
    state: &mut ProgramState,
    layout: &mut ModuleLayout,
    expr: &IrExpr,
) -> bool {
    match &expr.kind {
        IrExprKind::CallParts { callee, args } => {
            collect_call_parts_expr_types(state, layout, callee, args);
            true
        }
        IrExprKind::IntrinsicCall {
            param_tys,
            result_ty,
            args,
            ..
        } => {
            for ty in param_tys {
                let _ = ensure_type(state, layout, ty);
            }
            let _ = ensure_type(state, layout, result_ty);
            collect_expr_types_iter(state, layout, args.iter().map(|arg| &arg.expr));
            true
        }
        IrExprKind::Call { callee, args } => {
            collect_call_expr_types(state, layout, callee, args);
            true
        }
        IrExprKind::Request { args, .. } => {
            collect_expr_types_iter(state, layout, args);
            true
        }
        IrExprKind::RequestSeq { args, .. } => {
            let _ = ensure_type(state, layout, "[]Any");
            collect_expr_types_seq_parts(state, layout, args);
            true
        }
        IrExprKind::AnswerLit {
            effect_key,
            value,
            ops,
        } => {
            collect_answer_lit_expr_types(state, layout, effect_key, value, ops);
            true
        }
        IrExprKind::Handle {
            effect_key,
            answer,
            body,
            ..
        } => {
            collect_handle_expr_types(state, layout, effect_key, answer, body);
            true
        }
        IrExprKind::Resume { expr } => {
            if let Some(expr) = expr.as_deref() {
                collect_expr_types(state, layout, expr);
            }
            true
        }
        _ => false,
    }
}

fn collect_expr_types_seq_parts(
    state: &mut ProgramState,
    layout: &mut ModuleLayout,
    parts: &[IrSeqPart],
) {
    collect_expr_types_iter(
        state,
        layout,
        parts.iter().map(|part| match part {
            IrSeqPart::Expr(expr) | IrSeqPart::Spread(expr) => expr,
        }),
    );
}

fn collect_case_expr_types(
    state: &mut ProgramState,
    layout: &mut ModuleLayout,
    scrutinee: &IrExpr,
    arms: &[IrMatchArm],
) {
    collect_expr_types(state, layout, scrutinee);
    for arm in arms {
        if let Some(guard) = &arm.guard {
            collect_expr_types(state, layout, guard);
        }
        collect_expr_types(state, layout, &arm.expr);
    }
}

fn collect_call_expr_types(
    state: &mut ProgramState,
    layout: &mut ModuleLayout,
    callee: &IrExpr,
    args: &[IrArg],
) {
    collect_expr_types(state, layout, callee);
    collect_expr_types_iter(state, layout, args.iter().map(|arg| &arg.expr));
}

#[allow(dead_code)]
fn collect_call_parts_expr_types(
    state: &mut ProgramState,
    layout: &mut ModuleLayout,
    callee: &IrExpr,
    args: &[IrSeqPart],
) {
    let _ = ensure_type(state, layout, "[]Any");
    collect_expr_types(state, layout, callee);
    collect_expr_types_seq_parts(state, layout, args);
}

fn collect_answer_lit_expr_types(
    state: &mut ProgramState,
    layout: &mut ModuleLayout,
    effect_key: &DefinitionKey,
    value: &IrExpr,
    ops: &[IrHandleOp],
) {
    let answer_ty = answer_type_name(effect_key);
    let _ = ensure_type(state, layout, answer_ty.as_ref());
    collect_expr_types(state, layout, value);
    collect_expr_types_iter(state, layout, ops.iter().map(|op| &op.closure));
}

fn collect_handle_expr_types(
    state: &mut ProgramState,
    layout: &mut ModuleLayout,
    effect_key: &DefinitionKey,
    answer: &IrExpr,
    body: &IrExpr,
) {
    let answer_ty = answer_type_name(effect_key);
    let _ = ensure_type(state, layout, answer_ty.as_ref());
    collect_expr_types(state, layout, answer);
    collect_expr_types(state, layout, body);
}

fn collect_expr_types_iter<'a, I>(state: &mut ProgramState, layout: &mut ModuleLayout, exprs: I)
where
    I: IntoIterator<Item = &'a IrExpr>,
{
    for expr in exprs {
        collect_expr_types(state, layout, expr);
    }
}

fn collect_assign_target_types(
    state: &mut ProgramState,
    layout: &mut ModuleLayout,
    target: &IrAssignTarget,
) {
    match target {
        IrAssignTarget::Index { base, indices } => {
            collect_expr_types(state, layout, base);
            collect_expr_types_iter(state, layout, indices.iter());
        }
        IrAssignTarget::RecordField { base, .. } => collect_expr_types(state, layout, base),
        IrAssignTarget::Binding { .. } => {}
    }
}
