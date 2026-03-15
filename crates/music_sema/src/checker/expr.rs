//! Per-expression synthesis and checking.

use std::collections::{HashMap, HashSet};
use std::hash::BuildHasher;

use music_ast::expr::{
    Arg, ArrayElem, BinOp, Expr, FieldKey, HandlerOp, LetFields, MatchArm, Param, PwArm, PwGuard,
    RecDefField, RecField, TypeCheckKind, UnaryOp,
};
use music_ast::lit::{FStrPart, Lit};
use music_ast::pat::Pat;
use music_ast::ty::{Constraint, Rel, Ty, TyParam};
use music_ast::{ExprIdx, PatIdx, TyIdx};
use music_shared::{Span, Symbol};

use crate::checker::Checker;
use crate::checker::effects::check_effects_subset;
use crate::checker::pat::check_pat;
use crate::checker::stmt::check_stmt;
use crate::checker::ty::lower_ty;
use crate::def::{DefId, DefKind};
use crate::error::SemaError;
use crate::resolve;
use crate::scope::ScopeId;
use crate::types::{DictLookup, EffectRow, Obligation, RecordField, SumVariant, Type, TypeIdx, fmt_type};

/// Synthesises a type for `expr` (inference mode, direction ↑).
pub fn synth<S: BuildHasher>(ck: &mut Checker<'_, S>, expr_idx: ExprIdx) -> TypeIdx {
    let ty = synth_inner(ck, expr_idx);
    ck.record_type(expr_idx, ty);
    ty
}

/// Checks `expr` against `expected` (checking mode, direction ↓).
pub fn check<S: BuildHasher>(ck: &mut Checker<'_, S>, expr_idx: ExprIdx, expected: TypeIdx) {
    let found = synth_inner(ck, expr_idx);
    ck.record_type(expr_idx, expected);
    ck.unify_or_report(
        expected,
        found,
        resolve::expr_span(&ck.ctx.ast.exprs[expr_idx]),
    );
}

fn synth_inner<S: BuildHasher>(ck: &mut Checker<'_, S>, expr_idx: ExprIdx) -> TypeIdx {
    match &ck.ctx.ast.exprs[expr_idx] {
        Expr::Lit { lit, span } => {
            let (lit, span) = (lit.clone(), *span);
            synth_lit(ck, &lit, span)
        }
        Expr::Name { span, .. } => synth_name(ck, expr_idx, *span),
        Expr::Paren { inner, .. } | Expr::Annotated { inner, .. } => synth(ck, *inner),
        Expr::Tuple { elems, .. } => {
            let elems: Vec<ExprIdx> = elems.clone();
            let elem_tys: Vec<_> = elems.iter().map(|&e| synth(ck, e)).collect();
            ck.alloc_ty(Type::Tuple { elems: elem_tys })
        }
        Expr::Block { stmts, tail, .. } => {
            let (stmts, tail) = (stmts.clone(), *tail);
            synth_block(ck, &stmts, tail)
        }
        Expr::Let { fields, body, .. } => {
            let (fields, body) = (fields.clone(), *body);
            synth_let(ck, &fields, body)
        }
        Expr::Binding { fields, .. } => {
            let fields = fields.clone();
            synth_binding(ck, &fields)
        }
        Expr::Fn { params, ret_ty, body, span } => {
            let (params, ret_ty, body, span) = (params.clone(), *ret_ty, *body, *span);
            synth_fn(ck, &params, ret_ty, body, span)
        }
        Expr::Call { callee, args, span, .. } => {
            let (callee, args, span) = (*callee, args.clone(), *span);
            synth_call(ck, callee, &args, span)
        }
        Expr::BinOp { op, left, right, span } => synth_binop(ck, expr_idx, *op, *left, *right, *span),
        Expr::UnaryOp { op, operand, span, .. } => synth_unaryop(ck, *op, *operand, *span),
        Expr::Field { object, field, span, .. } => synth_field(ck, *object, *field, *span),
        Expr::Index { object, index, .. } => synth_index(ck, *object, *index),
        Expr::Record { fields, .. } => {
            let fields = fields.clone();
            synth_record(ck, &fields)
        }
        Expr::Array { elems, span } => {
            let (elems, span) = (elems.clone(), *span);
            synth_array(ck, &elems, span)
        }
        Expr::Piecewise { arms, span } => {
            let (arms, span) = (arms.clone(), *span);
            synth_piecewise(ck, &arms, span)
        }
        Expr::Match { scrutinee, arms, span } => {
            let (scrutinee, arms, span) = (*scrutinee, arms.clone(), *span);
            synth_match(ck, scrutinee, &arms, span)
        }
        Expr::Return { value, .. } => {
            if let Some(v) = *value { let _ty = synth(ck, v); }
            ck.named_ty(ck.ctx.well_known.never)
        }
        Expr::Variant { name, args, span, .. } => {
            let (name, args, span) = (*name, args.clone(), *span);
            for &a in &args { let _ty = synth(ck, a); }
            synth_variant(ck, name, span)
        }
        Expr::Update { base, fields, .. } => {
            let (base, fields) = (*base, fields.clone());
            synth_update(ck, base, &fields)
        }
        Expr::Choice { body, .. } => synth_choice(ck, *body),
        Expr::RecordDef { fields, .. } => {
            let fields = fields.clone();
            synth_record_def(ck, &fields)
        }
        Expr::Class { .. } | Expr::Instance { .. } | Expr::Effect { .. } | Expr::Foreign { .. } => {
            check_stmt(ck, expr_idx);
            ck.named_ty(ck.ctx.well_known.unit)
        }
        Expr::Import { path, .. } => synth_import(ck, *path),
        Expr::Export { items, span, .. } => {
            let (items, span) = (items.clone(), *span);
            for item in &items {
                if ck.scopes.lookup(ck.current_scope, item.name).is_none() {
                    let name_str = ck.ctx.interner.resolve(item.name);
                    let _d = ck.diags.report(
                        &SemaError::UndefinedName { name: Box::from(name_str) },
                        item.span,
                        ck.ctx.file_id,
                    );
                }
            }
            let _ = span;
            ck.named_ty(ck.ctx.well_known.unit)
        }
        Expr::Error { .. } => ck.error_ty(),
        Expr::TypeCheck { kind, operand, ty, binding, span } =>
            synth_type_check(ck, *kind, *operand, *ty, *binding, *span),
        Expr::Handle { effect_ty, ops, body, .. } => {
            let (effect_ty, ops, body) = (*effect_ty, ops.clone(), *body);
            synth_handle(ck, effect_ty, &ops, body)
        }
    }
}

fn synth_index<S: BuildHasher>(ck: &mut Checker<'_, S>, object: ExprIdx, index: ExprIdx) -> TypeIdx {
    let obj_ty = synth(ck, object);
    let _idx_ty = synth(ck, index);
    let obj_ty = ck.resolve_ty(obj_ty);
    match &ck.store.types[obj_ty] {
        Type::Array { elem, .. } => *elem,
        _ => ck.error_ty(),
    }
}

fn synth_update<S: BuildHasher>(ck: &mut Checker<'_, S>, base: ExprIdx, fields: &[RecField]) -> TypeIdx {
    let base_ty = synth(ck, base);
    for field in fields {
        match field {
            RecField::Named { value, .. } => {
                if let Some(v) = value {
                    let _ty = synth(ck, *v);
                }
            }
            RecField::Spread { expr, .. } => {
                let _ty = synth(ck, *expr);
            }
        }
    }
    base_ty
}

/// If `pat` is a function-like pattern (`Pat::Variant` with args), enters a
/// child scope and checks each arg pattern with a fresh type variable so the
/// param names are in scope when the value expression is checked.
/// Returns `Some(parent_scope)` if a scope was entered, `None` otherwise.
fn enter_fn_pat_scope<S: BuildHasher>(ck: &mut Checker<'_, S>, pat: PatIdx) -> Option<(ScopeId, Vec<TypeIdx>)> {
    if let Pat::Variant { args, .. } = &ck.ctx.ast.pats[pat] {
        let parent = ck.current_scope;
        if !args.is_empty() {
            ck.current_scope = ck.scopes.push_child(parent);
        }
        let mut param_tys = Vec::with_capacity(args.len());
        for &arg in args {
            let fresh = ck.fresh_var(Span::DUMMY);
            param_tys.push(fresh);
            check_pat(ck, arg, fresh);
        }
        Some((parent, param_tys))
    } else {
        None
    }
}

fn synth_block<S: BuildHasher>(ck: &mut Checker<'_, S>, stmts: &[ExprIdx], tail: Option<ExprIdx>) -> TypeIdx {
    for &stmt in stmts {
        let _ty = synth(ck, stmt);
    }
    if let Some(tail) = tail {
        synth(ck, tail)
    } else {
        ck.named_ty(ck.ctx.well_known.unit)
    }
}

fn synth_let<S: BuildHasher>(ck: &mut Checker<'_, S>, fields: &LetFields, body: Option<ExprIdx>) -> TypeIdx {
    let (parent_scope, ty_var_ids) = if fields.params.is_empty() {
        (None, vec![])
    } else {
        let (parent, ids) = ck.enter_ty_param_scope(&fields.params);
        (Some(parent), ids)
    };

    let prev_obligations = enter_constraint_scope(ck, &fields.constraints, &fields.params);

    let fn_pat_info = enter_fn_pat_scope(ck, fields.pat);

    let value_ty = match (fields.ty, fields.value) {
        (Some(ty_ann), Some(val)) => {
            let ann = lower_ty(ck, ty_ann);
            let prev_effects = ck.current_effects.clone();
            if let Type::Fn { ref effects, .. } = ck.store.types[ann]
                && !effects.is_pure()
            {
                ck.current_effects = effects.clone();
            }
            check(ck, val, ann);
            ck.current_effects = prev_effects;
            ann
        }
        (Some(ty_ann), None) => lower_ty(ck, ty_ann),
        (None, Some(val)) => synth(ck, val),
        (None, None) => ck.named_ty(ck.ctx.well_known.unit),
    };

    if let Some((p, _)) = &fn_pat_info {
        ck.current_scope = *p;
    }

    let pat_ty = wrap_fn_pat_ty(ck, value_ty, fn_pat_info.as_ref());
    store_pat_ty_info(ck, fields, &ty_var_ids);
    record_fn_constraints(ck, fields);
    check_pat(ck, fields.pat, pat_ty);

    let result = if let Some(body) = body {
        synth(ck, body)
    } else {
        ck.named_ty(ck.ctx.well_known.unit)
    };

    ck.store.active_obligations = prev_obligations;
    if let Some(p) = parent_scope {
        ck.current_scope = p;
    }
    result
}

fn synth_binding<S: BuildHasher>(ck: &mut Checker<'_, S>, fields: &LetFields) -> TypeIdx {
    let (parent_scope, ty_var_ids) = if fields.params.is_empty() {
        (None, vec![])
    } else {
        let (parent, ids) = ck.enter_ty_param_scope(&fields.params);
        (Some(parent), ids)
    };

    let prev_obligations = enter_constraint_scope(ck, &fields.constraints, &fields.params);

    let fn_pat_info = enter_fn_pat_scope(ck, fields.pat);

    let value_ty = match (fields.ty, fields.value) {
        (Some(ty_ann), Some(val)) => {
            let ann = lower_ty(ck, ty_ann);
            let prev_effects = ck.current_effects.clone();
            if let Type::Fn { ref effects, .. } = ck.store.types[ann]
                && !effects.is_pure()
            {
                ck.current_effects = effects.clone();
            }
            check(ck, val, ann);
            ck.current_effects = prev_effects;
            ann
        }
        (Some(ty_ann), None) => lower_ty(ck, ty_ann),
        (None, Some(val)) => synth(ck, val),
        (None, None) => ck.named_ty(ck.ctx.well_known.unit),
    };

    if let Some((p, _)) = &fn_pat_info {
        ck.current_scope = *p;
    }

    let pat_ty = wrap_fn_pat_ty(ck, value_ty, fn_pat_info.as_ref());
    store_pat_ty_info(ck, fields, &ty_var_ids);
    record_fn_constraints(ck, fields);
    check_pat(ck, fields.pat, pat_ty);

    ck.store.active_obligations = prev_obligations;
    if let Some(p) = parent_scope {
        ck.current_scope = p;
    }
    ck.named_ty(ck.ctx.well_known.unit)
}

/// If the binding has a function-like pattern (`go(acc, ys) := body`), wraps
/// the body's return type in a `Type::Fn` using the param types from
/// `enter_fn_pat_scope`. Otherwise returns `value_ty` unchanged.
fn wrap_fn_pat_ty<S: BuildHasher>(
    ck: &mut Checker<'_, S>,
    value_ty: TypeIdx,
    fn_pat_info: Option<&(ScopeId, Vec<TypeIdx>)>,
) -> TypeIdx {
    if let Some((_, param_tys)) = fn_pat_info {
        ck.alloc_ty(Type::Fn {
            params: param_tys.clone(),
            ret: value_ty,
            effects: EffectRow::PURE,
        })
    } else {
        value_ty
    }
}

/// Stores type info on the def for a let/binding pattern:
/// - For `Pat::Variant` (fn-like patterns), stores `ty_params` from bracket params.
/// - Type is stored later by `check_pat` on `Pat::Bind`.
fn store_pat_ty_info<S: BuildHasher>(ck: &mut Checker<'_, S>, fields: &LetFields, ty_param_defs: &[DefId]) {
    if !ty_param_defs.is_empty() {
        let pat = &ck.ctx.ast.pats[fields.pat];
        let pat_span = match pat {
            Pat::Variant { span, .. } | Pat::Bind { span, .. } => *span,
            _ => return,
        };
        if let Some(&def_id) = ck.ctx.pat_defs.get(&pat_span) {
            ck.defs.get_mut(def_id).ty_info.ty_params = ty_param_defs.to_vec();
        }
    }
}

fn synth_fn<S: BuildHasher>(
    ck: &mut Checker<'_, S>,
    params: &[Param],
    ret_ty: Option<TyIdx>,
    body: ExprIdx,
    span: Span,
) -> TypeIdx {
    let param_tys: Vec<TypeIdx> = params
        .iter()
        .map(|p| {
            if let Some(ty) = p.ty {
                lower_ty(ck, ty)
            } else {
                ck.fresh_var(p.span)
            }
        })
        .collect();

    for (p, &param_ty) in params.iter().zip(param_tys.iter()) {
        if let Some(&def_id) = ck.ctx.pat_defs.get(&p.span) {
            ck.defs.get_mut(def_id).ty_info.ty = Some(param_ty);
        }
    }

    let ret = if let Some(ret_ann) = ret_ty {
        let ann = lower_ty(ck, ret_ann);
        check(ck, body, ann);
        ann
    } else {
        synth(ck, body)
    };

    let effects = EffectRow {
        effects: vec![],
        row_var: Some(ck.store.unify.fresh_var_id(span)),
    };

    ck.alloc_ty(Type::Fn {
        params: param_tys,
        ret,
        effects,
    })
}

fn synth_field<S: BuildHasher>(ck: &mut Checker<'_, S>, object: ExprIdx, field: FieldKey, span: Span) -> TypeIdx {
    let obj_ty = synth(ck, object);
    let obj_ty = ck.resolve_ty(obj_ty);
    lookup_field(ck, obj_ty, field, span)
}

fn lookup_field<S: BuildHasher>(ck: &mut Checker<'_, S>, ty: TypeIdx, field: FieldKey, span: Span) -> TypeIdx {
    match &ck.store.types[ty] {
        Type::Record { fields, .. } => {
            if let FieldKey::Name { name, .. } = field {
                let fields = fields.clone();
                if let Some(f) = fields.iter().find(|f| f.name == name) {
                    f.ty
                } else {
                    report_no_such_field(ck, name, ty, span)
                }
            } else {
                ck.error_ty()
            }
        }
        Type::Tuple { elems } => {
            if let FieldKey::Pos { index, .. } = field {
                let elems = elems.clone();
                let idx = usize::try_from(index).expect("field index in range");
                if idx < elems.len() {
                    elems[idx]
                } else {
                    ck.error_ty()
                }
            } else {
                ck.error_ty()
            }
        }
        Type::Named { def, args } => {
            let (def, args) = (*def, args.clone());
            if let Some(underlying) = ck.defs.get(def).ty_info.ty {
                let ty_params = ck.defs.get(def).ty_info.ty_params.clone();
                let expanded = if !ty_params.is_empty() && ty_params.len() == args.len() {
                    let mut subst = HashMap::with_capacity(ty_params.len());
                    for (param_def, &arg_ty) in ty_params.iter().zip(&args) {
                        let _ = subst.insert(*param_def, arg_ty);
                    }
                    substitute_ty(ck, underlying, &subst)
                } else {
                    underlying
                };
                let expanded = ck.resolve_ty(expanded);
                lookup_field(ck, expanded, field, span)
            } else if let FieldKey::Name { name, .. } = field {
                report_no_such_field(ck, name, ty, span)
            } else {
                ck.error_ty()
            }
        }
        _ => ck.error_ty(),
    }
}

fn report_no_such_field<S: BuildHasher>(ck: &mut Checker<'_, S>, name: Symbol, ty: TypeIdx, span: Span) -> TypeIdx {
    let defs_vec: Vec<_> = ck.defs.iter().cloned().collect();
    let field_str = ck.ctx.interner.resolve(name);
    let ty_str = fmt_type(ty, &ck.store.types, &defs_vec, ck.ctx.interner, Some(&ck.store.unify));
    let _d = ck.diags.report(
        &SemaError::NoSuchField {
            field: Box::from(field_str),
            ty: ty_str,
        },
        span,
        ck.ctx.file_id,
    );
    ck.error_ty()
}

fn synth_record<S: BuildHasher>(ck: &mut Checker<'_, S>, fields: &[RecField]) -> TypeIdx {
    let rec_fields: Vec<_> = fields
        .iter()
        .filter_map(|f| match f {
            RecField::Named { name, value, .. } => {
                let ty = if let Some(v) = value {
                    synth(ck, *v)
                } else {
                    // Punning: `{ x }` means `{ x: x }`
                    ck.fresh_var(Span::DUMMY)
                };
                Some(RecordField { name: *name, ty })
            }
            RecField::Spread { .. } => None,
        })
        .collect();
    ck.alloc_ty(Type::Record {
        fields: rec_fields,
        open: false,
    })
}

fn synth_array<S: BuildHasher>(ck: &mut Checker<'_, S>, elems: &[ArrayElem], span: Span) -> TypeIdx {
    let elem_ty = ck.fresh_var(span);
    for elem in elems {
        match elem {
            ArrayElem::Elem { expr, .. } | ArrayElem::Spread { expr, .. } => {
                check(ck, *expr, elem_ty);
            }
        }
    }
    ck.alloc_ty(Type::Array {
        elem: elem_ty,
        len: None,
    })
}

fn synth_piecewise<S: BuildHasher>(ck: &mut Checker<'_, S>, arms: &[PwArm], span: Span) -> TypeIdx {
    let result_ty = ck.fresh_var(span);
    for arm in arms {
        if let PwGuard::When { expr, .. } = arm.guard {
            let guard_ty = synth(ck, expr);
            let bool_ty = ck.named_ty(ck.ctx.well_known.bool);
            ck.unify_or_report(bool_ty, guard_ty, span);
        }
        let arm_ty = synth(ck, arm.result);
        ck.unify_or_report(result_ty, arm_ty, span);
    }
    result_ty
}

fn synth_match<S: BuildHasher>(ck: &mut Checker<'_, S>, scrutinee: ExprIdx, arms: &[MatchArm], span: Span) -> TypeIdx {
    let scrut_ty = synth(ck, scrutinee);
    let result_ty = ck.fresh_var(span);
    for arm in arms {
        check_pat(ck, arm.pat, scrut_ty);
        if let Some(guard) = arm.guard {
            let bool_ty = ck.named_ty(ck.ctx.well_known.bool);
            check(ck, guard, bool_ty);
        }
        let arm_ty = synth(ck, arm.result);
        ck.unify_or_report(result_ty, arm_ty, span);
    }
    check_match_exhaustiveness(ck, scrut_ty, arms, span);
    result_ty
}

fn check_match_exhaustiveness<S: BuildHasher>(
    ck: &mut Checker<'_, S>,
    scrut_ty: TypeIdx,
    arms: &[MatchArm],
    span: Span,
) {
    use music_ast::pat::Pat;

    let has_wildcard = arms.iter().any(|arm| {
        matches!(
            &ck.ctx.ast.pats[arm.pat],
            Pat::Wild { .. } | Pat::Bind { inner: None, .. }
        )
    });
    if has_wildcard {
        return;
    }

    let resolved = ck.resolve_ty(scrut_ty);
    if matches!(ck.store.types[resolved], Type::Error | Type::Var(_)) {
        return;
    }
    match ck.store.types[resolved].clone() {
        Type::Sum { variants } => {
            let covered: HashSet<Symbol> = arms
                .iter()
                .filter_map(|arm| {
                    if let Pat::Variant { name, .. } = &ck.ctx.ast.pats[arm.pat] {
                        Some(*name)
                    } else {
                        None
                    }
                })
                .collect();
            for variant in &variants {
                if !covered.contains(&variant.name) {
                    let name_str = ck.ctx.interner.resolve(variant.name);
                    let _d = ck.diags.report(
                        &SemaError::NonExhaustiveMatch {
                            missing: Box::from(name_str),
                        },
                        span,
                        ck.ctx.file_id,
                    );
                }
            }
        }
        Type::Named { def, .. } => {
            let covered: HashSet<String> = arms
                .iter()
                .filter_map(|arm| {
                    if let Pat::Variant { name, .. } = &ck.ctx.ast.pats[arm.pat] {
                        Some(ck.ctx.interner.resolve(*name).to_owned())
                    } else {
                        None
                    }
                })
                .collect();

            // Well-known types with known variant sets.
            let well_known_variants: Option<&[&str]> =
                if def == ck.ctx.well_known.option {
                    Some(&["Some", "None"])
                } else if def == ck.ctx.well_known.bool {
                    Some(&["True", "False"])
                } else {
                    None
                };

            if let Some(expected) = well_known_variants {
                for case in expected {
                    if !covered.contains(*case) {
                        let _d = ck.diags.report(
                            &SemaError::NonExhaustiveMatch {
                                missing: Box::from(*case),
                            },
                            span,
                            ck.ctx.file_id,
                        );
                    }
                }
            } else if let Some(dt) = ck.defs.get(def).ty_info.ty {
                let resolved_def = ck.resolve_ty(dt);
                if let Type::Sum { variants } = ck.store.types[resolved_def].clone() {
                    for variant in &variants {
                        let name_str = ck.ctx.interner.resolve(variant.name);
                        if !covered.contains(name_str) {
                            let _d = ck.diags.report(
                                &SemaError::NonExhaustiveMatch {
                                    missing: Box::from(name_str),
                                },
                                span,
                                ck.ctx.file_id,
                            );
                        }
                    }
                } else {
                    report_missing_wildcard(ck, span);
                }
            } else {
                report_missing_wildcard(ck, span);
            }
        }
        _ => {
            report_missing_wildcard(ck, span);
        }
    }
}

fn report_missing_wildcard<S: BuildHasher>(ck: &mut Checker<'_, S>, span: Span) {
    let _d = ck.diags.report(
        &SemaError::NonExhaustiveMatch {
            missing: Box::from("_"),
        },
        span,
        ck.ctx.file_id,
    );
}

fn synth_lit<S: BuildHasher>(ck: &mut Checker<'_, S>, lit: &Lit, _span: Span) -> TypeIdx {
    match lit {
        Lit::Int { .. } => ck.named_ty(ck.ctx.well_known.ints.int),
        Lit::Float { .. } => ck.named_ty(ck.ctx.well_known.floats.float64),
        Lit::Str { .. } => ck.named_ty(ck.ctx.well_known.string),
        Lit::FStr { parts, .. } => {
            for part in parts {
                if let FStrPart::Interpolated { expr, .. } = part {
                    let _ty = synth(ck, *expr);
                }
            }
            ck.named_ty(ck.ctx.well_known.string)
        }
        Lit::Rune { .. } => ck.named_ty(ck.ctx.well_known.rune),
        Lit::Unit { .. } => ck.named_ty(ck.ctx.well_known.unit),
    }
}

fn synth_name<S: BuildHasher>(ck: &mut Checker<'_, S>, expr_idx: ExprIdx, span: Span) -> TypeIdx {
    if let Some(&def_id) = ck.ctx.expr_defs.get(&expr_idx) {
        let def = ck.defs.get(def_id);
        let ty_params = def.ty_info.ty_params.clone();
        let raw_ty = def.ty_info.ty.unwrap_or_else(|| ck.fresh_var(span));
        if ty_params.is_empty() {
            raw_ty
        } else {
            instantiate_ty_params(ck, raw_ty, &ty_params, span)
        }
    } else {
        ck.error_ty()
    }
}

/// Replaces each type-parameter `Type::Named` (identified by `DefId`) with a
/// fresh unification variable, returning a new type with substitutions applied.
fn instantiate_ty_params<S: BuildHasher>(
    ck: &mut Checker<'_, S>,
    ty: TypeIdx,
    ty_param_defs: &[DefId],
    span: Span,
) -> TypeIdx {
    let mut subst: HashMap<DefId, TypeIdx> = HashMap::with_capacity(ty_param_defs.len());
    for &def_id in ty_param_defs {
        let fresh = ck.fresh_var(span);
        let _prev = subst.insert(def_id, fresh);
    }
    substitute_ty(ck, ty, &subst)
}

fn substitute_ty<S: BuildHasher>(ck: &mut Checker<'_, S>, ty: TypeIdx, subst: &HashMap<DefId, TypeIdx>) -> TypeIdx {
    let ty = ck.resolve_ty(ty);
    match &ck.store.types[ty] {
        Type::Named { def, args } if args.is_empty() && subst.contains_key(def) => subst[def],
        Type::Named { def, args } => {
            let (def, args) = (*def, args.clone());
            let new_args = substitute_list(ck, &args, subst);
            ck.alloc_ty(Type::Named { def, args: new_args })
        }
        Type::Fn { params, ret, effects } => {
            let (params, ret, effects) = (params.clone(), *ret, effects.clone());
            let params = substitute_list(ck, &params, subst);
            let ret = substitute_ty(ck, ret, subst);
            ck.alloc_ty(Type::Fn { params, ret, effects })
        }
        Type::Tuple { elems } => {
            let elems = elems.clone();
            let elems = substitute_list(ck, &elems, subst);
            ck.alloc_ty(Type::Tuple { elems })
        }
        Type::AnonSum { variants } => {
            let variants = variants.clone();
            let variants = substitute_list(ck, &variants, subst);
            ck.alloc_ty(Type::AnonSum { variants })
        }
        Type::Record { fields, open } => {
            let (fields, open) = (fields.clone(), *open);
            let fields = substitute_record_fields(ck, &fields, subst);
            ck.alloc_ty(Type::Record { fields, open })
        }
        Type::Sum { variants } => {
            let variants = variants.clone();
            let variants = substitute_sum_variants(ck, &variants, subst);
            ck.alloc_ty(Type::Sum { variants })
        }
        Type::Array { elem, len } => {
            let (elem, len) = (*elem, *len);
            let elem = substitute_ty(ck, elem, subst);
            ck.alloc_ty(Type::Array { elem, len })
        }
        Type::Ref { inner } => {
            let inner = *inner;
            let inner = substitute_ty(ck, inner, subst);
            ck.alloc_ty(Type::Ref { inner })
        }
        Type::Quantified { kind, params, constraints, body } => {
            let (kind, params, constraints, body) = (*kind, params.clone(), constraints.clone(), *body);
            let body = substitute_ty(ck, body, subst);
            ck.alloc_ty(Type::Quantified { kind, params, constraints, body })
        }
        Type::Var(_) | Type::Rigid(_) | Type::Error => ty,
    }
}

fn substitute_list<S: BuildHasher>(
    ck: &mut Checker<'_, S>,
    tys: &[TypeIdx],
    subst: &HashMap<DefId, TypeIdx>,
) -> Vec<TypeIdx> {
    tys.iter().map(|&t| substitute_ty(ck, t, subst)).collect()
}

fn substitute_record_fields<S: BuildHasher>(
    ck: &mut Checker<'_, S>,
    fields: &[RecordField],
    subst: &HashMap<DefId, TypeIdx>,
) -> Vec<RecordField> {
    fields
        .iter()
        .map(|f| RecordField {
            name: f.name,
            ty: substitute_ty(ck, f.ty, subst),
        })
        .collect()
}

fn substitute_sum_variants<S: BuildHasher>(
    ck: &mut Checker<'_, S>,
    variants: &[SumVariant],
    subst: &HashMap<DefId, TypeIdx>,
) -> Vec<SumVariant> {
    variants
        .iter()
        .map(|v| SumVariant {
            name: v.name,
            fields: substitute_list(ck, &v.fields, subst),
        })
        .collect()
}

fn synth_call<S: BuildHasher>(ck: &mut Checker<'_, S>, callee: ExprIdx, args: &[Arg], span: Span) -> TypeIdx {
    let callee_ty = synth(ck, callee);
    let callee_ty = ck.resolve_ty(callee_ty);

    let (fn_params, fn_ret, fn_effects) = match &ck.store.types[callee_ty] {
        Type::Fn { params, ret, effects } => (Some((params.clone(), *ret, effects.clone())), None, false),
        Type::Error => (None, Some(false), false),
        Type::Var(_) => (None, None, true),
        _ => (None, Some(true), false),
    };

    if let Some((params, ret, effects)) = fn_params {
        {
            let arg_count = args.len();
            if arg_count != params.len() {
                let _d = ck.diags.report(
                    &SemaError::ArityMismatch {
                        expected: params.len(),
                        found: arg_count,
                    },
                    span,
                    ck.ctx.file_id,
                );
                return ck.error_ty();
            }

            for (arg, &param_ty) in args.iter().zip(params.iter()) {
                if let Arg::Pos { expr, .. } = arg {
                    check(ck, *expr, param_ty);
                }
            }

            let current = ck.current_effects.clone();
            check_effects_subset(ck, &effects, &current, span);

            return ret;
        }
    }

    match (fn_ret, fn_effects) {
        (Some(false), _) => ck.error_ty(),
        (_, true) => {
            // Unknown callee type — create fresh return and set up function constraint.
            let arg_tys: Vec<_> = args
                .iter()
                .map(|arg| match arg {
                    Arg::Pos { expr, .. } | Arg::Spread { expr, .. } => synth(ck, *expr),
                })
                .collect();
            let ret = ck.fresh_var(span);
            let fn_ty = ck.alloc_ty(Type::Fn {
                params: arg_tys,
                ret,
                effects: EffectRow::PURE,
            });
            ck.unify_or_report(callee_ty, fn_ty, span);
            ret
        }
        _ => {
            let defs_vec: Vec<_> = ck.defs.iter().cloned().collect();
            let ty_str = fmt_type(callee_ty, &ck.store.types, &defs_vec, ck.ctx.interner, Some(&ck.store.unify));
            let _d = ck
                .diags
                .report(&SemaError::NotCallable { ty: ty_str }, span, ck.ctx.file_id);
            ck.error_ty()
        }
    }
}

fn find_instance_method<S: BuildHasher>(ck: &Checker<'_, S>, target_ty: TypeIdx, op_name: &str) -> Option<DefId> {
    let op_sym = ck.ctx.interner.get(op_name)?;
    let resolved = ck.store.unify.resolve(target_ty, &ck.store.types);
    for inst in &ck.store.instances {
        let inst_target = ck.store.unify.resolve(inst.target, &ck.store.types);
        if inst_target == resolved
            && let Some(&def_id) = inst.members.iter().find(|(s, _)| *s == op_sym).map(|(_, id)| id)
        {
            return Some(def_id);
        }
    }
    None
}

fn synth_binop<S: BuildHasher>(
    ck: &mut Checker<'_, S>,
    expr_idx: ExprIdx,
    op: BinOp,
    left: ExprIdx,
    right: ExprIdx,
    span: Span,
) -> TypeIdx {
    let left_ty = synth(ck, left);
    let right_ty = synth(ck, right);

    // Try operator dispatch through typeclass instances before built-in rules.
    if let Some(op_name) = op.operator_name() {
        if let Some(method_def) = find_instance_method(ck, left_ty, op_name) {
            let _prev = ck.store.binop_dispatch.insert(expr_idx, method_def);
        } else if let Some(dict_lookup) = find_dict_method(ck, left_ty, op_name) {
            let _prev = ck.store.binop_dict_dispatch.insert(expr_idx, dict_lookup);
        }
    }

    match op {
        BinOp::Eq | BinOp::Ne | BinOp::Lt | BinOp::Le | BinOp::Gt | BinOp::Ge | BinOp::In => {
            ck.unify_or_report(left_ty, right_ty, span);
            ck.named_ty(ck.ctx.well_known.bool)
        }
        BinOp::And | BinOp::Or => {
            let bool_ty = ck.named_ty(ck.ctx.well_known.bool);
            ck.unify_or_report(bool_ty, left_ty, span);
            ck.unify_or_report(bool_ty, right_ty, span);
            bool_ty
        }
        BinOp::Add
        | BinOp::Sub
        | BinOp::Mul
        | BinOp::Div
        | BinOp::Rem
        | BinOp::Xor
        | BinOp::Shl
        | BinOp::Shr
        | BinOp::RangeInc
        | BinOp::RangeExc
        | BinOp::NilCoal => {
            ck.unify_or_report(left_ty, right_ty, span);
            left_ty
        }
        BinOp::ForceCoal => {
            let inner = unwrap_result_ty(ck, left_ty, span);
            ck.unify_or_report(inner, right_ty, span);
            inner
        }
        BinOp::Pipe => {
            let ret = ck.fresh_var(span);
            let fn_ty = ck.alloc_ty(Type::Fn {
                params: vec![left_ty],
                ret,
                effects: EffectRow::PURE,
            });
            ck.unify_or_report(fn_ty, right_ty, span);
            ret
        }
        BinOp::Assign => {
            ck.unify_or_report(left_ty, right_ty, span);
            ck.named_ty(ck.ctx.well_known.unit)
        }
        BinOp::Cons => {
            let arr_ty = ck.alloc_ty(Type::Array {
                elem: left_ty,
                len: None,
            });
            ck.unify_or_report(arr_ty, right_ty, span);
            right_ty
        }
    }
}

fn synth_unaryop<S: BuildHasher>(ck: &mut Checker<'_, S>, op: UnaryOp, operand: ExprIdx, span: Span) -> TypeIdx {
    let operand_ty = synth(ck, operand);
    match op {
        UnaryOp::Not => {
            let bool_ty = ck.named_ty(ck.ctx.well_known.bool);
            ck.unify_or_report(bool_ty, operand_ty, span);
            bool_ty
        }
        UnaryOp::Neg | UnaryOp::Defer | UnaryOp::Try | UnaryOp::Do => operand_ty,
        UnaryOp::ForceUnwrap | UnaryOp::Propagate => unwrap_option_ty(ck, operand_ty, span),
    }
}

fn synth_choice<S: BuildHasher>(ck: &mut Checker<'_, S>, body: TyIdx) -> TypeIdx {
    let parent = ck.current_scope;
    ck.current_scope = ck.scopes.push_child(parent);

    let sum_variant_indices = if let Ty::Sum { variants, .. } = &ck.ctx.ast.tys[body] {
        Some(variants.clone())
    } else {
        None
    };

    if let Some(variants_clone) = sum_variant_indices {
        for &variant_ty in &variants_clone {
            if let Ty::Named { name, .. } = &ck.ctx.ast.tys[variant_ty] {
                let id = ck.defs.alloc(*name, DefKind::Type, Span::DUMMY);
                let _prev = ck.scopes.define(ck.current_scope, *name, id);
            }
        }

        let mut sum_variants = Vec::with_capacity(variants_clone.len());
        for &variant_ty in &variants_clone {
            let (name, args) = match &ck.ctx.ast.tys[variant_ty] {
                Ty::Named { name, args, .. } => (Some(*name), Some(args.clone())),
                _ => (None, None),
            };
            if let (Some(name), Some(args)) = (name, args) {
                let fields: Vec<TypeIdx> = args.iter().map(|&a| lower_ty(ck, a)).collect();
                sum_variants.push(SumVariant { name, fields });
            } else {
                let ty = lower_ty(ck, variant_ty);
                sum_variants.push(SumVariant {
                    name: Symbol(0),
                    fields: vec![ty],
                });
            }
        }

        ck.current_scope = parent;
        return ck.alloc_ty(Type::Sum {
            variants: sum_variants,
        });
    }

    let ty = lower_ty(ck, body);
    ck.current_scope = parent;
    ty
}

fn synth_record_def<S: BuildHasher>(ck: &mut Checker<'_, S>, fields: &[RecDefField]) -> TypeIdx {
    let rec_fields: Vec<_> = fields
        .iter()
        .map(|f| RecordField {
            name: f.name,
            ty: lower_ty(ck, f.ty),
        })
        .collect();
    ck.alloc_ty(Type::Record {
        fields: rec_fields,
        open: false,
    })
}

fn synth_import<S: BuildHasher>(ck: &mut Checker<'_, S>, path: Symbol) -> TypeIdx {
    if let Some(&record_ty) = ck.ctx.import_types.get(&path) {
        record_ty
    } else {
        ck.named_ty(ck.ctx.well_known.unit)
    }
}

/// Expects `operand_ty` to be `Option<T>`, returning `T`.
/// Introduces a fresh `T`, unifies `operand_ty` with `Option<T>`, and returns `T`.
fn unwrap_option_ty<S: BuildHasher>(ck: &mut Checker<'_, S>, operand_ty: TypeIdx, span: Span) -> TypeIdx {
    let inner = ck.fresh_var(span);
    let option_ty = ck.alloc_ty(Type::Named {
        def: ck.ctx.well_known.option,
        args: vec![inner],
    });
    ck.unify_or_report(option_ty, operand_ty, span);
    inner
}

/// Expects `operand_ty` to be `Result<T, E>`, returning `T`.
/// Introduces fresh `T` and `E`, unifies `operand_ty` with `Result<T, E>`, and returns `T`.
fn unwrap_result_ty<S: BuildHasher>(ck: &mut Checker<'_, S>, operand_ty: TypeIdx, span: Span) -> TypeIdx {
    let ok_inner = ck.fresh_var(span);
    let err_inner = ck.fresh_var(span);
    let result_ty = ck.alloc_ty(Type::Named {
        def: ck.ctx.well_known.containers.result,
        args: vec![ok_inner, err_inner],
    });
    ck.unify_or_report(result_ty, operand_ty, span);
    ok_inner
}

fn synth_variant<S: BuildHasher>(ck: &mut Checker<'_, S>, name: Symbol, span: Span) -> TypeIdx {
    if let Some(def_id) = ck.scopes.lookup(ck.current_scope, name)
        && ck.defs.get(def_id).kind == DefKind::Variant
        && let Some(parent_id) = ck.defs.get(def_id).parent
    {
        let parent_ty = ck.defs.get(parent_id).ty_info.ty;
        if let Some(ty) = parent_ty {
            return ty;
        }
        return ck.named_ty(parent_id);
    }
    ck.fresh_var(span)
}

fn check_cast_safety<S: BuildHasher>(
    ck: &mut Checker<'_, S>,
    from_ty: TypeIdx,
    to_ty: TypeIdx,
    span: Span,
) {
    let from_resolved = ck.resolve_ty(from_ty);
    let to_resolved = ck.resolve_ty(to_ty);
    if let (Type::Named { def: from_def, .. }, Type::Named { def: to_def, .. }) =
        (&ck.store.types[from_resolved].clone(), &ck.store.types[to_resolved].clone())
        && from_def != to_def
    {
        let defs_vec: Vec<_> = ck.defs.iter().cloned().collect();
        let from_str = fmt_type(from_resolved, &ck.store.types, &defs_vec, ck.ctx.interner, Some(&ck.store.unify));
        let to_str = fmt_type(to_resolved, &ck.store.types, &defs_vec, ck.ctx.interner, Some(&ck.store.unify));
        let _d = ck.diags.report(
            &SemaError::UnsafeCast { from: from_str, to: to_str },
            span,
            ck.ctx.file_id,
        );
    }
}

fn synth_type_check<S: BuildHasher>(
    ck: &mut Checker<'_, S>,
    kind: TypeCheckKind,
    operand: ExprIdx,
    ty: TyIdx,
    binding: Option<Symbol>,
    span: Span,
) -> TypeIdx {
    match kind {
        TypeCheckKind::Test => synth_type_test(ck, operand, ty, binding, span),
        TypeCheckKind::Cast => {
            let operand_ty = synth(ck, operand);
            let target_ty = lower_ty(ck, ty);
            check_cast_safety(ck, operand_ty, target_ty, span);
            target_ty
        }
    }
}

fn synth_type_test<S: BuildHasher>(
    ck: &mut Checker<'_, S>,
    operand: ExprIdx,
    ty: TyIdx,
    binding: Option<Symbol>,
    span: Span,
) -> TypeIdx {
    let _operand_ty = synth(ck, operand);
    let test_ty = lower_ty(ck, ty);
    if let Some(name) = binding {
        let id = ck.defs.alloc(name, DefKind::Let, span);
        ck.defs.get_mut(id).ty_info.ty = Some(test_ty);
        let _prev = ck.scopes.define(ck.current_scope, name, id);
    }
    ck.named_ty(ck.ctx.well_known.bool)
}

fn synth_handle<S: BuildHasher>(
    ck: &mut Checker<'_, S>,
    effect_ty: TyIdx,
    ops: &[HandlerOp],
    body: ExprIdx,
) -> TypeIdx {
    let _eff_ty = lower_ty(ck, effect_ty);
    for op in ops {
        let parent = ck.current_scope;
        ck.current_scope = ck.scopes.push_child(parent);
        for param in &op.params {
            let param_ty = if let Some(ty) = param.ty {
                lower_ty(ck, ty)
            } else {
                ck.fresh_var(param.span)
            };
            let id = ck.defs.alloc(param.name, DefKind::Param, param.span);
            ck.defs.get_mut(id).ty_info.ty = Some(param_ty);
            let _prev = ck.scopes.define(ck.current_scope, param.name, id);
        }
        let _op_ty = synth(ck, op.body);
        ck.current_scope = parent;
    }
    check_handler_op_coverage(ck, effect_ty, ops);
    synth(ck, body)
}

fn check_handler_op_coverage<S: BuildHasher>(
    ck: &mut Checker<'_, S>,
    effect_ty_idx: TyIdx,
    ops: &[HandlerOp],
) {
    use music_ast::ty::Ty;

    let effect_name = match &ck.ctx.ast.tys[effect_ty_idx] {
        Ty::Named { name, .. } => *name,
        _ => return,
    };

    let required_ops = find_effect_required_ops(ck, effect_name);
    if required_ops.is_empty() {
        return;
    }

    let effect_name_str = ck.ctx.interner.resolve(effect_name).to_owned();
    let handled: HashSet<Symbol> =
        ops.iter().map(|op| op.name).collect();

    for (op_sym, op_span) in &required_ops {
        if !handled.contains(op_sym) {
            let op_name_str = ck.ctx.interner.resolve(*op_sym);
            let _d = ck.diags.report(
                &SemaError::MissingHandlerOp {
                    effect: Box::from(effect_name_str.as_str()),
                    op: Box::from(op_name_str),
                },
                *op_span,
                ck.ctx.file_id,
            );
        }
    }
}

/// Processes AST constraints into active obligations for the current scope.
/// Returns the previous obligations so they can be restored.
fn enter_constraint_scope<S: BuildHasher>(
    ck: &mut Checker<'_, S>,
    constraints: &[Constraint],
    params: &[TyParam],
) -> Vec<Obligation> {

    if constraints.is_empty() {
        return ck.store.active_obligations.clone();
    }

    let prev = ck.store.active_obligations.clone();
    let mut new_obligations = prev.clone();

    for constraint in constraints {
        if constraint.rel != Rel::Sub {
            continue;
        }
        let class_def = ck.scopes.lookup(ck.current_scope, constraint.bound.name);
        let Some(class_def) = class_def else { continue };

        // Find the type variable for constraint.param among the ty params
        let param_ty = params
            .iter()
            .find(|p| p.name == constraint.param)
            .and_then(|p| ck.ctx.pat_defs.get(&p.span).or_else(|| {
                // Type params are defined via enter_ty_param_scope, look up in current scope
                ck.scopes.lookup(ck.current_scope, p.name).as_ref().copied().map(|_| &p.name).and(None)
            }))
            .copied();

        // Fall back: look up the param name in current scope to get its DefId, then get its type
        let param_type_idx = if let Some(&def_id) = param_ty.as_ref() {
            ck.defs.get(def_id).ty_info.ty.unwrap_or_else(|| ck.named_ty(def_id))
        } else if let Some(def_id) = ck.scopes.lookup(ck.current_scope, constraint.param) {
            ck.named_ty(def_id)
        } else {
            continue;
        };

        let ob = Obligation {
            class: class_def,
            args: vec![param_type_idx],
            span: constraint.span,
        };
        new_obligations.push(ob.clone());
        ck.store.obligations.push(ob);
    }

    ck.store.active_obligations = new_obligations;
    prev
}

/// Records `fn_constraints` for a constrained function.
fn record_fn_constraints<S: BuildHasher>(ck: &mut Checker<'_, S>, fields: &LetFields) {
    if fields.constraints.is_empty() {
        return;
    }

    let pat = &ck.ctx.ast.pats[fields.pat];
    let pat_span = match pat {
        Pat::Variant { span, .. } | Pat::Bind { span, .. } => *span,
        _ => return,
    };

    let Some(&def_id) = ck.ctx.pat_defs.get(&pat_span) else { return };

    // Collect the obligations that were added for this function's constraints
    let fn_obs: Vec<Obligation> = ck.store.active_obligations.iter()
        .filter(|ob| {
            fields.constraints.iter().any(|c| {
                ck.scopes.lookup(ck.current_scope, c.bound.name) == Some(ob.class)
            })
        })
        .cloned()
        .collect();

    if !fn_obs.is_empty() {
        let _prev = ck.store.fn_constraints.insert(def_id, fn_obs);
    }
}

/// Searches active obligations for a method that matches the operator on a type variable.
fn find_dict_method<S: BuildHasher>(
    ck: &Checker<'_, S>,
    target_ty: TypeIdx,
    op_name: &str,
) -> Option<DictLookup> {
    let op_sym = ck.ctx.interner.get(op_name)?;
    let resolved = ck.store.unify.resolve(target_ty, &ck.store.types);
    let resolved_ty = &ck.store.types[resolved];

    if !matches!(resolved_ty, Type::Var(_) | Type::Rigid(_)) {
        return None;
    }

    for ob in &ck.store.active_obligations {
        let ob_ty = ck.store.unify.resolve(
            *ob.args.first()?,
            &ck.store.types,
        );
        if ob_ty != resolved {
            continue;
        }
        // Check if this class has a method matching op_sym
        if ck.ctx.class_op_members.contains_key(&(ob.class, op_sym)) {
            return Some(DictLookup {
                class: ob.class,
                method_sym: op_sym,
            });
        }
    }
    None
}

fn find_effect_required_ops<S: BuildHasher>(
    ck: &Checker<'_, S>,
    effect_name: Symbol,
) -> Vec<(Symbol, Span)> {
    let n = ck.ctx.ast.exprs.len();
    for i in 0..n {
        let idx = music_shared::Idx::from_raw(u32::try_from(i).expect("expr index in range"));
        if let Expr::Effect { name, ops, .. } = &ck.ctx.ast.exprs[idx]
            && *name == effect_name
        {
            return ops.iter().map(|op| (op.name, op.span)).collect();
        }
    }
    vec![]
}
