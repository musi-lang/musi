//! Per-expression synthesis and checking.

use std::collections::HashMap;

use music_ast::expr::{
    Arg, ArrayElem, BinOp, Expr, FieldKey, HandlerOp, LetFields, MatchArm, Param, PwArm, PwGuard,
    RecDefField, RecField, TypeCheckKind, UnaryOp,
};
use music_ast::lit::{FStrPart, Lit};
use music_ast::pat::Pat;
use music_ast::ty::Ty;
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
use crate::types::{EffectRow, RecordField, SumVariant, Type, TypeIdx, fmt_type};

/// Synthesises a type for `expr` (inference mode, direction ↑).
pub(crate) fn synth(ck: &mut Checker<'_>, expr_idx: ExprIdx) -> TypeIdx {
    let ty = synth_inner(ck, expr_idx);
    ck.record_type(expr_idx, ty);
    ty
}

/// Checks `expr` against `expected` (checking mode, direction ↓).
pub(crate) fn check(ck: &mut Checker<'_>, expr_idx: ExprIdx, expected: TypeIdx) {
    let found = synth_inner(ck, expr_idx);
    ck.record_type(expr_idx, expected);
    ck.unify_or_report(
        expected,
        found,
        resolve::expr_span(&ck.ctx.ast.exprs[expr_idx]),
    );
}

fn synth_inner(ck: &mut Checker<'_>, expr_idx: ExprIdx) -> TypeIdx {
    match ck.ctx.ast.exprs[expr_idx].clone() {
        Expr::Lit { lit, span } => synth_lit(ck, &lit, span),
        Expr::Name { span, .. } => synth_name(ck, expr_idx, span),
        Expr::Paren { inner, .. } | Expr::Annotated { inner, .. } => synth(ck, inner),
        Expr::Tuple { elems, .. } => {
            let elem_tys: Vec<_> = elems.iter().map(|&e| synth(ck, e)).collect();
            ck.alloc_ty(Type::Tuple { elems: elem_tys })
        }
        Expr::Block { stmts, tail, .. } => synth_block(ck, &stmts, tail),
        Expr::Let { fields, body, .. } => synth_let(ck, &fields, body),
        Expr::Binding { fields, .. } => synth_binding(ck, &fields),
        Expr::Fn {
            params,
            ret_ty,
            body,
            span,
        } => synth_fn(ck, &params, ret_ty, body, span),
        Expr::Call {
            callee, args, span, ..
        } => synth_call(ck, callee, &args, span),
        Expr::BinOp {
            op,
            left,
            right,
            span,
        } => synth_binop(ck, op, left, right, span),
        Expr::UnaryOp {
            op, operand, span, ..
        } => synth_unaryop(ck, op, operand, span),
        Expr::Field {
            object,
            field,
            span,
            ..
        } => synth_field(ck, object, field, span),
        Expr::Index { object, index, .. } => synth_index(ck, object, index),
        Expr::Record { fields, .. } => synth_record(ck, &fields),
        Expr::Array { elems, span } => synth_array(ck, &elems, span),
        Expr::Piecewise { arms, span } => synth_piecewise(ck, &arms, span),
        Expr::Match {
            scrutinee,
            arms,
            span,
        } => synth_match(ck, scrutinee, &arms, span),
        Expr::Return { value, .. } => {
            if let Some(v) = value {
                let _ty = synth(ck, v);
            }
            ck.named_ty(ck.ctx.well_known.never)
        }
        Expr::Variant { args, span, .. } => {
            for &a in &args {
                let _ty = synth(ck, a);
            }
            ck.fresh_var(span)
        }
        Expr::Update { base, fields, .. } => synth_update(ck, base, &fields),
        Expr::Choice { body, .. } => synth_choice(ck, body),
        Expr::RecordDef { fields, .. } => synth_record_def(ck, &fields),
        Expr::Class { .. } | Expr::Instance { .. } | Expr::Effect { .. } | Expr::Foreign { .. } => {
            check_stmt(ck, expr_idx);
            ck.named_ty(ck.ctx.well_known.unit)
        }
        Expr::Import { path, .. } => synth_import(ck, path),
        Expr::Export { .. } => ck.named_ty(ck.ctx.well_known.unit),
        Expr::Error { .. } => ck.error_ty(),
        Expr::TypeCheck {
            kind,
            operand,
            ty,
            binding,
            span,
        } => synth_type_check(ck, kind, operand, ty, binding, span),
        Expr::Handle {
            effect_ty,
            ops,
            body,
            ..
        } => synth_handle(ck, effect_ty, &ops, body),
    }
}

fn synth_index(ck: &mut Checker<'_>, object: ExprIdx, index: ExprIdx) -> TypeIdx {
    let obj_ty = synth(ck, object);
    let _idx_ty = synth(ck, index);
    let obj_ty = ck.resolve_ty(obj_ty);
    match &ck.store.types[obj_ty] {
        Type::Array { elem, .. } => *elem,
        _ => ck.error_ty(),
    }
}

fn synth_update(ck: &mut Checker<'_>, base: ExprIdx, fields: &[RecField]) -> TypeIdx {
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
fn enter_fn_pat_scope(ck: &mut Checker<'_>, pat: PatIdx) -> Option<(ScopeId, Vec<TypeIdx>)> {
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

fn synth_block(ck: &mut Checker<'_>, stmts: &[ExprIdx], tail: Option<ExprIdx>) -> TypeIdx {
    for &stmt in stmts {
        let _ty = synth(ck, stmt);
    }
    if let Some(tail) = tail {
        synth(ck, tail)
    } else {
        ck.named_ty(ck.ctx.well_known.unit)
    }
}

fn synth_let(ck: &mut Checker<'_>, fields: &LetFields, body: Option<ExprIdx>) -> TypeIdx {
    let (parent_scope, ty_var_ids) = if fields.params.is_empty() {
        (None, vec![])
    } else {
        let (parent, ids) = ck.enter_ty_param_scope(&fields.params);
        (Some(parent), ids)
    };

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
    check_pat(ck, fields.pat, pat_ty);

    let result = if let Some(body) = body {
        synth(ck, body)
    } else {
        ck.named_ty(ck.ctx.well_known.unit)
    };

    if let Some(p) = parent_scope {
        ck.current_scope = p;
    }
    result
}

fn synth_binding(ck: &mut Checker<'_>, fields: &LetFields) -> TypeIdx {
    let (parent_scope, ty_var_ids) = if fields.params.is_empty() {
        (None, vec![])
    } else {
        let (parent, ids) = ck.enter_ty_param_scope(&fields.params);
        (Some(parent), ids)
    };

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
    check_pat(ck, fields.pat, pat_ty);

    if let Some(p) = parent_scope {
        ck.current_scope = p;
    }
    ck.named_ty(ck.ctx.well_known.unit)
}

/// If the binding has a function-like pattern (`go(acc, ys) := body`), wraps
/// the body's return type in a `Type::Fn` using the param types from
/// `enter_fn_pat_scope`. Otherwise returns `value_ty` unchanged.
fn wrap_fn_pat_ty(
    ck: &mut Checker<'_>,
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
fn store_pat_ty_info(ck: &mut Checker<'_>, fields: &LetFields, ty_param_defs: &[DefId]) {
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

fn synth_fn(
    ck: &mut Checker<'_>,
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

fn synth_field(ck: &mut Checker<'_>, object: ExprIdx, field: FieldKey, span: Span) -> TypeIdx {
    let obj_ty = synth(ck, object);
    let obj_ty = ck.resolve_ty(obj_ty);
    match &ck.store.types[obj_ty] {
        Type::Record { fields, .. } => {
            if let FieldKey::Name { name, .. } = field {
                if let Some(f) = fields.iter().find(|f| f.name == name) {
                    f.ty
                } else {
                    let defs_vec: Vec<_> = ck.defs.iter().cloned().collect();
                    let field_str = ck.ctx.interner.resolve(name);
                    let ty_str = fmt_type(obj_ty, &ck.store.types, &defs_vec, ck.ctx.interner);
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
            } else {
                ck.error_ty()
            }
        }
        Type::Tuple { elems } => {
            if let FieldKey::Pos { index, .. } = field {
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
        _ => ck.error_ty(),
    }
}

fn synth_record(ck: &mut Checker<'_>, fields: &[RecField]) -> TypeIdx {
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

fn synth_array(ck: &mut Checker<'_>, elems: &[ArrayElem], span: Span) -> TypeIdx {
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

fn synth_piecewise(ck: &mut Checker<'_>, arms: &[PwArm], span: Span) -> TypeIdx {
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

fn synth_match(ck: &mut Checker<'_>, scrutinee: ExprIdx, arms: &[MatchArm], span: Span) -> TypeIdx {
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
    result_ty
}

fn synth_lit(ck: &mut Checker<'_>, lit: &Lit, _span: Span) -> TypeIdx {
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

fn synth_name(ck: &mut Checker<'_>, expr_idx: ExprIdx, span: Span) -> TypeIdx {
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
fn instantiate_ty_params(
    ck: &mut Checker<'_>,
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

fn substitute_ty(ck: &mut Checker<'_>, ty: TypeIdx, subst: &HashMap<DefId, TypeIdx>) -> TypeIdx {
    let ty = ck.resolve_ty(ty);
    match ck.store.types[ty].clone() {
        Type::Named { def, args } if args.is_empty() && subst.contains_key(&def) => subst[&def],
        Type::Named { def, args } => {
            let new_args = substitute_list(ck, &args, subst);
            ck.alloc_ty(Type::Named {
                def,
                args: new_args,
            })
        }
        Type::Fn {
            params,
            ret,
            effects,
        } => {
            let params = substitute_list(ck, &params, subst);
            let ret = substitute_ty(ck, ret, subst);
            ck.alloc_ty(Type::Fn {
                params,
                ret,
                effects,
            })
        }
        Type::Tuple { elems } => {
            let elems = substitute_list(ck, &elems, subst);
            ck.alloc_ty(Type::Tuple { elems })
        }
        Type::AnonSum { variants } => {
            let variants = substitute_list(ck, &variants, subst);
            ck.alloc_ty(Type::AnonSum { variants })
        }
        Type::Record { fields, open } => {
            let fields = substitute_record_fields(ck, &fields, subst);
            ck.alloc_ty(Type::Record { fields, open })
        }
        Type::Sum { variants } => {
            let variants = substitute_sum_variants(ck, &variants, subst);
            ck.alloc_ty(Type::Sum { variants })
        }
        Type::Array { elem, len } => {
            let elem = substitute_ty(ck, elem, subst);
            ck.alloc_ty(Type::Array { elem, len })
        }
        Type::Ref { inner } => {
            let inner = substitute_ty(ck, inner, subst);
            ck.alloc_ty(Type::Ref { inner })
        }
        Type::Quantified {
            kind,
            params,
            constraints,
            body,
        } => {
            let body = substitute_ty(ck, body, subst);
            ck.alloc_ty(Type::Quantified {
                kind,
                params,
                constraints,
                body,
            })
        }
        Type::Var(_) | Type::Rigid(_) | Type::Error => ty,
    }
}

fn substitute_list(
    ck: &mut Checker<'_>,
    tys: &[TypeIdx],
    subst: &HashMap<DefId, TypeIdx>,
) -> Vec<TypeIdx> {
    tys.iter().map(|&t| substitute_ty(ck, t, subst)).collect()
}

fn substitute_record_fields(
    ck: &mut Checker<'_>,
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

fn substitute_sum_variants(
    ck: &mut Checker<'_>,
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

fn synth_call(ck: &mut Checker<'_>, callee: ExprIdx, args: &[Arg], span: Span) -> TypeIdx {
    let callee_ty = synth(ck, callee);
    let callee_ty = ck.resolve_ty(callee_ty);

    match ck.store.types[callee_ty].clone() {
        Type::Fn {
            params,
            ret,
            effects,
        } => {
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

            ret
        }
        Type::Error => ck.error_ty(),
        Type::Var(_) => {
            // Unknown callee type — create fresh return and set up function constraint.
            let arg_tys: Vec<_> = args
                .iter()
                .map(|arg| match arg {
                    Arg::Pos { expr, .. } => synth(ck, *expr),
                    Arg::Spread { expr, .. } => synth(ck, *expr),
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
            let ty_str = fmt_type(callee_ty, &ck.store.types, &defs_vec, ck.ctx.interner);
            let _d = ck
                .diags
                .report(&SemaError::NotCallable { ty: ty_str }, span, ck.ctx.file_id);
            ck.error_ty()
        }
    }
}

fn synth_binop(
    ck: &mut Checker<'_>,
    op: BinOp,
    left: ExprIdx,
    right: ExprIdx,
    span: Span,
) -> TypeIdx {
    let left_ty = synth(ck, left);
    let right_ty = synth(ck, right);

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

fn synth_unaryop(ck: &mut Checker<'_>, op: UnaryOp, operand: ExprIdx, span: Span) -> TypeIdx {
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

fn synth_choice(ck: &mut Checker<'_>, body: TyIdx) -> TypeIdx {
    let parent = ck.current_scope;
    ck.current_scope = ck.scopes.push_child(parent);

    if let Ty::Sum { variants, .. } = &ck.ctx.ast.tys[body] {
        let variants_clone = variants.clone();

        for &variant_ty in &variants_clone {
            if let Ty::Named { name, .. } = &ck.ctx.ast.tys[variant_ty] {
                let id = ck.defs.alloc(*name, DefKind::Type, Span::DUMMY);
                let _prev = ck.scopes.define(ck.current_scope, *name, id);
            }
        }

        let mut sum_variants = Vec::with_capacity(variants_clone.len());
        for &variant_ty in &variants_clone {
            let ast_ty = ck.ctx.ast.tys[variant_ty].clone();
            if let Ty::Named { name, args, .. } = &ast_ty {
                let fields: Vec<TypeIdx> = args.iter().map(|&a| lower_ty(ck, a)).collect();
                sum_variants.push(SumVariant {
                    name: *name,
                    fields,
                });
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

fn synth_record_def(ck: &mut Checker<'_>, fields: &[RecDefField]) -> TypeIdx {
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

fn synth_import(ck: &mut Checker<'_>, path: Symbol) -> TypeIdx {
    if let Some(&record_ty) = ck.ctx.import_types.get(&path) {
        record_ty
    } else {
        ck.named_ty(ck.ctx.well_known.unit)
    }
}

/// Expects `operand_ty` to be `Option<T>`, returning `T`.
/// Introduces a fresh `T`, unifies `operand_ty` with `Option<T>`, and returns `T`.
fn unwrap_option_ty(ck: &mut Checker<'_>, operand_ty: TypeIdx, span: Span) -> TypeIdx {
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
fn unwrap_result_ty(ck: &mut Checker<'_>, operand_ty: TypeIdx, span: Span) -> TypeIdx {
    let ok_inner = ck.fresh_var(span);
    let err_inner = ck.fresh_var(span);
    let result_ty = ck.alloc_ty(Type::Named {
        def: ck.ctx.well_known.containers.result,
        args: vec![ok_inner, err_inner],
    });
    ck.unify_or_report(result_ty, operand_ty, span);
    ok_inner
}

fn synth_type_check(
    ck: &mut Checker<'_>,
    kind: TypeCheckKind,
    operand: ExprIdx,
    ty: TyIdx,
    binding: Option<Symbol>,
    span: Span,
) -> TypeIdx {
    match kind {
        TypeCheckKind::Test => synth_type_test(ck, operand, ty, binding, span),
        TypeCheckKind::Cast => {
            let _operand_ty = synth(ck, operand);
            lower_ty(ck, ty)
        }
    }
}

fn synth_type_test(
    ck: &mut Checker<'_>,
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

fn synth_handle(
    ck: &mut Checker<'_>,
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
    synth(ck, body)
}
