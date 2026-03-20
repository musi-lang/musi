//! Per-expression synthesis and checking.

use std::collections::{HashMap, HashSet};
use std::hash::BuildHasher;

use msc_ast::expr::Expr as AstExpr;
use msc_ast::expr::{
    Arg, ArrayElem, BinOp, Expr, FieldKey, HandlerOp, LetFields, MatchArm, Param, PwArm, PwGuard,
    RecDefField, RecField, TypeCheckKind, TypeForm, UnaryOp,
};
use msc_ast::lit::{FStrPart, Lit};
use msc_ast::pat::Pat;
use msc_ast::{ExprIdx, PatIdx};
use msc_shared::{Idx, Span, Symbol};

use crate::checker::Checker;
use crate::checker::decl::check_decl;
use crate::checker::dispatch::{
    check_handler_op_coverage, enter_constraint_scope, find_dict_method, find_instance_method,
    record_fn_constraints,
};
use crate::checker::effects::check_effects_subset;
use crate::checker::exhaustive::check_match_exhaustiveness;
use crate::checker::instantiate::{freshen_poly, substitute_ty};
use crate::checker::pat::check_pat;
use crate::checker::ty::lower_type_expr;
use msc_ast::expr::ParamMode;

use crate::def::{DefFlags, DefId, DefKind};
use crate::error::SemaError;
use crate::resolve;
use crate::scope::ScopeId;
use crate::subst::subst_type;
use crate::types::{
    EffectEntry, EffectRow, Polarity, RecordField, SumVariant, Type, TypeIdx, fmt_type,
};

/// Synthesises a type for `expr` (inference mode, direction ↑).
pub fn synth<S: BuildHasher>(ck: &mut Checker<'_, S>, expr_idx: ExprIdx) -> TypeIdx {
    let ty = synth_inner(ck, expr_idx);
    ck.record_type(expr_idx, ty);
    ty
}

/// Checks `expr` against `expected` (checking mode, direction ↓).
pub fn check<S: BuildHasher>(ck: &mut Checker<'_, S>, expr_idx: ExprIdx, expected: TypeIdx) {
    check_with_polarity(ck, expr_idx, expected, Polarity::Positive);
}

/// Checks `expr` against `expected` with an explicit polarity for blame tracking.
///
/// Use `Polarity::Negative` for contravariant positions such as function parameter types,
/// and `Polarity::Positive` for covariant positions such as return types and let bindings.
fn check_with_polarity<S: BuildHasher>(
    ck: &mut Checker<'_, S>,
    expr_idx: ExprIdx,
    expected: TypeIdx,
    polarity: Polarity,
) {
    let found = synth_inner(ck, expr_idx);
    ck.record_type(expr_idx, expected);
    ck.check_subtype_or_report_with_polarity(
        expected,
        found,
        resolve::expr_span(&ck.ctx.ast.exprs[expr_idx]),
        Some(expr_idx),
        polarity,
    );
}

#[expect(
    clippy::too_many_lines,
    reason = "exhaustive match dispatching to sub-functions"
)]
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
        Expr::Let { fields, .. } => {
            let fields = fields.clone();
            synth_let(ck, &fields)
        }
        Expr::Fn {
            params,
            ret_ty,
            body,
            span,
        } => {
            let (params, ret_ty, body, span) = (params.clone(), *ret_ty, *body, *span);
            synth_fn(ck, &params, ret_ty, body, span)
        }
        Expr::Call {
            callee, args, span, ..
        } => {
            let (callee, args, span) = (*callee, args.clone(), *span);
            synth_call(ck, callee, &args, span)
        }
        Expr::BinOp {
            op,
            left,
            right,
            span,
        } => synth_binop(ck, expr_idx, *op, *left, *right, *span),
        Expr::UnaryOp {
            op, operand, span, ..
        } => synth_unaryop(ck, *op, *operand, *span),
        Expr::Field {
            object,
            field,
            span,
            ..
        } => {
            if ck.ctx.expr_defs.contains_key(&expr_idx) {
                let _obj_ty = synth(ck, *object);
                return synth_name(ck, expr_idx, *span);
            }
            synth_field(ck, *object, *field, *span)
        }
        Expr::Index {
            object,
            index,
            span,
        } => synth_index(ck, *object, *index, *span),
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
        Expr::Match {
            scrutinee,
            arms,
            span,
        } => {
            let (scrutinee, arms, span) = (*scrutinee, arms.clone(), *span);
            synth_match(ck, scrutinee, &arms, span)
        }
        Expr::Return { value, .. } => {
            if let Some(v) = *value {
                let _ty = synth(ck, v);
            }
            ck.named_ty(ck.ctx.well_known.never)
        }
        Expr::Need { operand, span, .. } => synth_need(ck, *operand, *span),
        Expr::Resume { value, span, .. } => {
            let (value, span) = (*value, *span);
            synth_resume(ck, value, span)
        }
        Expr::Variant {
            name, args, span, ..
        } => {
            let (name, args, span) = (*name, args.clone(), *span);
            synth_variant_with_args(ck, name, &args, span)
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
            check_decl(ck, expr_idx);
            ck.named_ty(ck.ctx.well_known.unit)
        }
        Expr::Import { path, alias, .. } => synth_import(ck, *path, *alias),
        Expr::Export { items, span, .. } => {
            let (items, span) = (items.clone(), *span);
            for item in &items {
                if ck.scopes.lookup(ck.current_scope, item.name).is_none() {
                    let name_str = ck.ctx.interner.resolve(item.name);
                    let _d = ck.diags.report(
                        &SemaError::UndefinedName {
                            name: Box::from(name_str),
                        },
                        item.span,
                        ck.ctx.file_id,
                    );
                }
            }
            let _ = span;
            ck.named_ty(ck.ctx.well_known.unit)
        }
        Expr::Error { .. } => ck.error_ty(),
        Expr::TypeCheck {
            kind,
            operand,
            ty,
            binding,
            span,
        } => synth_type_check(ck, *kind, *operand, *ty, *binding, *span),
        Expr::Handle {
            effect_ty,
            ops,
            body,
            ..
        } => {
            let (effect_ty, ops, body) = (*effect_ty, ops.clone(), *body);
            synth_handle(ck, effect_ty, &ops, body)
        }
        // Type-expression variants: these appear in type annotations and are
        // lowered via lower_type_expr, not synthesised as value expressions.
        Expr::TypeApp { .. } | Expr::FnType { .. } | Expr::TypeExpr { .. } => {
            lower_type_expr(ck, expr_idx)
        }
    }
}

fn synth_index<S: BuildHasher>(
    ck: &mut Checker<'_, S>,
    object: ExprIdx,
    index: ExprIdx,
    span: Span,
) -> TypeIdx {
    let obj_ty = synth(ck, object);
    let idx_ty = synth(ck, index);
    // Index must be an Int.
    let int_ty = ck.named_ty(ck.ctx.well_known.ints.int);
    ck.unify_or_report(int_ty, idx_ty, span, None);
    let obj_ty = ck.resolve_ty(obj_ty);
    match &ck.store.types[obj_ty] {
        Type::Array { elem, .. } => *elem,
        Type::Error => ck.error_ty(),
        Type::Var(_) => {
            let elem = ck.fresh_var(span);
            let arr_ty = ck.alloc_ty(Type::Array { elem, len: None });
            ck.unify_or_report(obj_ty, arr_ty, span, None);
            elem
        }
        _ => {
            let defs_vec: Vec<_> = ck.defs.iter().cloned().collect();
            let ty_str = fmt_type(
                obj_ty,
                &ck.store.types,
                &defs_vec,
                ck.ctx.interner,
                Some(&ck.store.unify),
            );
            let _d = ck.diags.report(
                &SemaError::NotIndexable { ty: ty_str },
                span,
                ck.ctx.file_id,
            );
            ck.error_ty()
        }
    }
}

fn synth_update<S: BuildHasher>(
    ck: &mut Checker<'_, S>,
    base: ExprIdx,
    fields: &[RecField],
) -> TypeIdx {
    let base_ty = synth(ck, base);
    let resolved_base = ck.resolve_ty(base_ty);

    for field in fields {
        match field {
            RecField::Named { name, value, span } => {
                let (name, span) = (*name, *span);
                let value_ty = if let Some(v) = value {
                    synth(ck, *v)
                } else {
                    ck.fresh_var(span)
                };
                // Unify value type against the declared field type in the base record.
                if let Type::Record {
                    fields: rec_fields, ..
                } = ck.store.types[resolved_base].clone()
                {
                    if let Some(rec_field) = rec_fields.iter().find(|f| f.name == name) {
                        let expected_ty = rec_field.ty;
                        ck.unify_or_report(expected_ty, value_ty, span, None);
                    } else {
                        let defs_vec: Vec<_> = ck.defs.iter().cloned().collect();
                        let field_str = ck.ctx.interner.resolve(name);
                        let ty_str = fmt_type(
                            resolved_base,
                            &ck.store.types,
                            &defs_vec,
                            ck.ctx.interner,
                            Some(&ck.store.unify),
                        );
                        let _d = ck.diags.report(
                            &SemaError::NoSuchField {
                                field: Box::from(field_str),
                                ty: ty_str,
                            },
                            span,
                            ck.ctx.file_id,
                        );
                    }
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
fn enter_fn_pat_scope<S: BuildHasher>(
    ck: &mut Checker<'_, S>,
    pat: PatIdx,
) -> Option<(ScopeId, Vec<TypeIdx>)> {
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

fn synth_block<S: BuildHasher>(
    ck: &mut Checker<'_, S>,
    stmts: &[ExprIdx],
    tail: Option<ExprIdx>,
) -> TypeIdx {
    for (i, &stmt) in stmts.iter().enumerate() {
        let ty = synth(ck, stmt);
        if !ck.ctx.options.allow_unreachable_code {
            let resolved = ck.resolve_ty(ty);
            if let Type::Named { def, .. } = &ck.store.types[resolved] {
                if *def == ck.ctx.well_known.never && (i + 1 < stmts.len() || tail.is_some()) {
                    let next_span = if i + 1 < stmts.len() {
                        resolve::expr_span(&ck.ctx.ast.exprs[stmts[i + 1]])
                    } else {
                        resolve::expr_span(&ck.ctx.ast.exprs[tail.unwrap()])
                    };
                    let _d =
                        ck.diags
                            .report(&SemaError::UnreachableCode, next_span, ck.ctx.file_id);
                }
            }
        }
    }
    if let Some(tail) = tail {
        synth(ck, tail)
    } else {
        ck.named_ty(ck.ctx.well_known.unit)
    }
}

fn synth_let<S: BuildHasher>(ck: &mut Checker<'_, S>, fields: &LetFields) -> TypeIdx {
    let (parent_scope, ty_var_ids) = if fields.params.is_empty() {
        (None, vec![])
    } else {
        let (parent, ids) = ck.enter_ty_param_scope(&fields.params);
        (Some(parent), ids)
    };

    let prev_obligations = enter_constraint_scope(ck, &fields.constraints, &fields.params);

    let fn_pat_info = enter_fn_pat_scope(ck, fields.pat);

    let prev_repr_c = ck.current_repr_c;
    ck.current_repr_c = binding_has_repr_c(ck, fields.pat);

    let value_ty = match (fields.ty, fields.value) {
        (Some(ty_ann), Some(val)) => {
            let ann = lower_type_expr(ck, ty_ann);
            let val_span = resolve::expr_span(&ck.ctx.ast.exprs[val]);
            reject_abstract_record_construction(ck, ann, val, val_span);
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
        (Some(ty_ann), None) => lower_type_expr(ck, ty_ann),
        (None, Some(val)) => synth(ck, val),
        (None, None) => ck.named_ty(ck.ctx.well_known.unit),
    };

    ck.current_repr_c = prev_repr_c;

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

/// Returns true if the binding identified by `pat` has `DefFlags::REPR_C`.
fn binding_has_repr_c<S: BuildHasher>(ck: &Checker<'_, S>, pat: PatIdx) -> bool {
    let span = match &ck.ctx.ast.pats[pat] {
        msc_ast::Pat::Bind { span, .. } | msc_ast::Pat::Variant { span, .. } => *span,
        _ => return false,
    };
    ck.ctx
        .pat_defs
        .get(&span)
        .is_some_and(|&def_id| ck.defs.get(def_id).flags.has(DefFlags::REPR_C))
}

/// Emits an error when `val` is a direct record literal and `ann` resolves to
/// a named type that carries `DefFlags::ABSTRACT`.
fn reject_abstract_record_construction<S: BuildHasher>(
    ck: &mut Checker<'_, S>,
    ann: TypeIdx,
    val: ExprIdx,
    span: Span,
) {
    if !matches!(&ck.ctx.ast.exprs[val], Expr::Record { .. }) {
        return;
    }
    let resolved = ck.resolve_ty(ann);
    if let Type::Named { def, .. } = ck.store.types[resolved] {
        if ck.defs.get(def).flags.has(DefFlags::ABSTRACT) {
            let name_str = ck.ctx.interner.resolve(ck.defs.get(def).name);
            let _d = ck.diags.report(
                &SemaError::AbstractConstruct {
                    name: Box::from(name_str),
                },
                span,
                ck.ctx.file_id,
            );
        }
    }
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
fn store_pat_ty_info<S: BuildHasher>(
    ck: &mut Checker<'_, S>,
    fields: &LetFields,
    ty_param_defs: &[DefId],
) {
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
    ret_ty: Option<ExprIdx>,
    body: ExprIdx,
    span: Span,
) -> TypeIdx {
    let param_tys: Vec<TypeIdx> = params
        .iter()
        .map(|p| {
            if let Some(ty) = p.ty {
                lower_type_expr(ck, ty)
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
        let ann = lower_type_expr(ck, ret_ann);
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

fn synth_field<S: BuildHasher>(
    ck: &mut Checker<'_, S>,
    object: ExprIdx,
    field: FieldKey,
    span: Span,
) -> TypeIdx {
    let obj_ty = synth(ck, object);
    let obj_ty = ck.resolve_ty(obj_ty);
    lookup_field(ck, obj_ty, field, span)
}

fn lookup_field<S: BuildHasher>(
    ck: &mut Checker<'_, S>,
    ty: TypeIdx,
    field: FieldKey,
    span: Span,
) -> TypeIdx {
    match &ck.store.types[ty] {
        Type::Record { fields, rest } => {
            let (fields, rest) = (fields.clone(), *rest);
            if let FieldKey::Name { name, .. } = field {
                if let Some(f) = fields.iter().find(|f| f.name == name) {
                    if f.ty_params.is_empty() {
                        f.ty
                    } else {
                        freshen_poly(ck, f.ty, &f.ty_params.clone(), span)
                    }
                } else if let Some(rest_idx) = rest {
                    // Open record - unify the rest with a new single-field record
                    // so the row chain grows naturally through the binding.
                    let field_ty = ck.fresh_var(span);
                    let new_rest = ck.fresh_var(span);
                    let constraint = ck.alloc_ty(Type::Record {
                        fields: vec![RecordField {
                            name,
                            ty: field_ty,
                            ty_params: vec![],
                            binding: None,
                        }],
                        rest: Some(new_rest),
                    });
                    ck.unify_or_report(rest_idx, constraint, span, None);
                    field_ty
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
        Type::Var(_) => {
            // Unresolved type variable - create an open record constraint with
            // a row variable so that subsequent field accesses extend the chain.
            if let FieldKey::Name { name, .. } = field {
                let field_ty = ck.fresh_var(span);
                let row_var = ck.fresh_var(span);
                let open_rec = ck.alloc_ty(Type::Record {
                    fields: vec![RecordField {
                        name,
                        ty: field_ty,
                        ty_params: vec![],
                        binding: None,
                    }],
                    rest: Some(row_var),
                });
                ck.unify_or_report(ty, open_rec, span, None);
                field_ty
            } else {
                ck.error_ty()
            }
        }
        _ => ck.error_ty(),
    }
}

fn report_no_such_field<S: BuildHasher>(
    ck: &mut Checker<'_, S>,
    name: Symbol,
    ty: TypeIdx,
    span: Span,
) -> TypeIdx {
    let defs_vec: Vec<_> = ck.defs.iter().cloned().collect();
    let field_str = ck.ctx.interner.resolve(name);
    let ty_str = fmt_type(
        ty,
        &ck.store.types,
        &defs_vec,
        ck.ctx.interner,
        Some(&ck.store.unify),
    );
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
    let mut rec_fields: Vec<_> = fields
        .iter()
        .filter_map(|f| match f {
            RecField::Named { name, value, .. } => {
                let ty = if let Some(v) = value {
                    synth(ck, *v)
                } else {
                    // Punning: `{ x }` means `{ x: x }`
                    ck.fresh_var(Span::DUMMY)
                };
                Some(RecordField {
                    name: *name,
                    ty,
                    ty_params: vec![],
                    binding: None,
                })
            }
            RecField::Spread { .. } => None,
        })
        .collect();
    // Canonical field ordering: sort by name string so field indices are
    // consistent between construction and access across module boundaries.
    rec_fields.sort_by(|a, b| {
        match (
            ck.ctx.interner.try_resolve(a.name),
            ck.ctx.interner.try_resolve(b.name),
        ) {
            (Some(a_str), Some(b_str)) => a_str.cmp(b_str),
            _ => a.name.0.cmp(&b.name.0),
        }
    });
    ck.alloc_ty(Type::Record {
        fields: rec_fields,
        rest: None,
    })
}

fn synth_array<S: BuildHasher>(
    ck: &mut Checker<'_, S>,
    elems: &[ArrayElem],
    span: Span,
) -> TypeIdx {
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
            ck.unify_or_report(bool_ty, guard_ty, span, None);
        }
        let arm_ty = synth(ck, arm.result);
        ck.unify_or_report(result_ty, arm_ty, span, None);
    }
    result_ty
}

fn synth_match<S: BuildHasher>(
    ck: &mut Checker<'_, S>,
    scrutinee: ExprIdx,
    arms: &[MatchArm],
    span: Span,
) -> TypeIdx {
    let scrut_ty = synth(ck, scrutinee);
    let result_ty = ck.fresh_var(span);
    for arm in arms {
        check_pat(ck, arm.pat, scrut_ty);
        if let Some(guard) = arm.guard {
            let bool_ty = ck.named_ty(ck.ctx.well_known.bool);
            check(ck, guard, bool_ty);
        }
        let arm_ty = synth(ck, arm.result);
        ck.unify_or_report(result_ty, arm_ty, span, None);
    }
    check_match_exhaustiveness(ck, scrut_ty, arms, span);
    result_ty
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
        let result_ty = if ty_params.is_empty() {
            raw_ty
        } else {
            freshen_poly(ck, raw_ty, &ty_params, span)
        };
        // Auto-deref Ref for read access.
        let resolved = ck.resolve_ty(result_ty);
        match &ck.store.types[resolved] {
            Type::Ref { inner } => *inner,
            _ => result_ty,
        }
    } else {
        ck.error_ty()
    }
}

fn synth_call<S: BuildHasher>(
    ck: &mut Checker<'_, S>,
    callee: ExprIdx,
    args: &[Arg],
    span: Span,
) -> TypeIdx {
    let callee_ty = synth(ck, callee);
    let callee_ty = ck.resolve_ty(callee_ty);

    if let Type::Pi {
        param_def,
        param_ty,
        body,
        ..
    } = ck.store.types[callee_ty].clone()
    {
        if args.len() != 1 {
            let _d = ck.diags.report(
                &SemaError::ArityMismatch {
                    expected: 1,
                    found: args.len(),
                },
                span,
                ck.ctx.file_id,
            );
            return ck.error_ty();
        }
        if let Arg::Pos { expr, .. } = args[0] {
            // Pi-type parameter: contravariant position → Negative polarity for blame.
            use crate::types::Polarity;
            check_with_polarity(ck, expr, param_ty, Polarity::Negative);
        }
        let arg_ty = match args[0] {
            Arg::Pos { expr, .. } | Arg::Spread { expr, .. } => synth(ck, expr),
        };
        let substituted = subst_type(body, param_def, arg_ty, &mut ck.store.types);
        return substituted;
    }

    let (fn_params, fn_ret, fn_effects) = match &ck.store.types[callee_ty] {
        Type::Fn {
            params,
            ret,
            effects,
        } => (Some((params.clone(), *ret, effects.clone())), None, false),
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
                    // Function parameter: contravariant position → Negative polarity for blame.
                    use crate::types::Polarity;
                    check_with_polarity(ck, *expr, param_ty, Polarity::Negative);
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
            // Unknown callee type - create fresh return and set up function constraint.
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
            ck.unify_or_report(callee_ty, fn_ty, span, None);
            ret
        }
        _ => {
            let defs_vec: Vec<_> = ck.defs.iter().cloned().collect();
            let ty_str = fmt_type(
                callee_ty,
                &ck.store.types,
                &defs_vec,
                ck.ctx.interner,
                Some(&ck.store.unify),
            );
            let _d = ck
                .diags
                .report(&SemaError::NotCallable { ty: ty_str }, span, ck.ctx.file_id);
            ck.error_ty()
        }
    }
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
        BinOp::Eq | BinOp::Ne | BinOp::Lt | BinOp::Le | BinOp::Gt | BinOp::Ge => {
            ck.unify_or_report(left_ty, right_ty, span, Some(expr_idx));
            ck.named_ty(ck.ctx.well_known.bool)
        }
        BinOp::And | BinOp::Or => {
            ck.unify_or_report(left_ty, right_ty, span, Some(expr_idx));
            let resolved = ck.store.unify.resolve(left_ty, &ck.store.types);
            match &ck.store.types[resolved] {
                Type::Named { def, .. } => {
                    let d = *def;
                    let wk = &ck.ctx.well_known;
                    if d == wk.bool
                        || d == wk.ints.int
                        || d == wk.ints.int8
                        || d == wk.ints.int16
                        || d == wk.ints.int32
                        || d == wk.ints.int64
                        || d == wk.nats.nat
                        || d == wk.nats.nat8
                        || d == wk.nats.nat16
                        || d == wk.nats.nat32
                        || d == wk.nats.nat64
                    {
                        left_ty
                    } else {
                        let bool_ty = ck.named_ty(wk.bool);
                        ck.unify_or_report(bool_ty, left_ty, span, Some(expr_idx));
                        bool_ty
                    }
                }
                _ => left_ty,
            }
        }
        BinOp::Add
        | BinOp::Sub
        | BinOp::Mul
        | BinOp::Div
        | BinOp::Rem
        | BinOp::Xor
        | BinOp::RangeInc
        | BinOp::RangeExc
        | BinOp::NilCoal => {
            ck.unify_or_report(left_ty, right_ty, span, Some(expr_idx));
            left_ty
        }
        BinOp::Pipe => {
            let ret = ck.fresh_var(span);
            let fn_ty = ck.alloc_ty(Type::Fn {
                params: vec![left_ty],
                ret,
                effects: EffectRow::PURE,
            });
            ck.unify_or_report(fn_ty, right_ty, span, Some(expr_idx));
            ret
        }
        BinOp::Assign => {
            if !is_mutable_target(ck, left) {
                let _d = ck
                    .diags
                    .report(&SemaError::AssignToImmutable, span, ck.ctx.file_id);
            }
            ck.unify_or_report(left_ty, right_ty, span, Some(expr_idx));
            ck.named_ty(ck.ctx.well_known.unit)
        }
        BinOp::Cons => {
            let arr_ty = ck.alloc_ty(Type::Array {
                elem: left_ty,
                len: None,
            });
            ck.unify_or_report(arr_ty, right_ty, span, Some(expr_idx));
            right_ty
        }
    }
}

fn is_mutable_target<S: BuildHasher>(ck: &Checker<'_, S>, expr_idx: ExprIdx) -> bool {
    match &ck.ctx.ast.exprs[expr_idx] {
        Expr::Name { .. } => {
            if let Some(&def_id) = ck.ctx.expr_defs.get(&expr_idx) {
                let def = ck.defs.get(def_id);
                def.kind == DefKind::Var
                    || (def.kind == DefKind::Param && def.param_mode == Some(ParamMode::Mut))
            } else {
                false
            }
        }
        Expr::Index { object, .. } | Expr::Field { object, .. } => is_mutable_target(ck, *object),
        _ => true,
    }
}

fn synth_unaryop<S: BuildHasher>(
    ck: &mut Checker<'_, S>,
    op: UnaryOp,
    operand: ExprIdx,
    span: Span,
) -> TypeIdx {
    let operand_ty = synth(ck, operand);
    match op {
        UnaryOp::Not => {
            let bool_ty = ck.named_ty(ck.ctx.well_known.bool);
            ck.unify_or_report(bool_ty, operand_ty, span, None);
            bool_ty
        }
        UnaryOp::Neg => operand_ty,
        UnaryOp::ForceUnwrap | UnaryOp::Propagate => unwrap_option_ty(ck, operand_ty, span),
    }
}

fn synth_choice_sum<S: BuildHasher>(
    ck: &mut Checker<'_, S>,
    variants: &[ExprIdx],
    parent: ScopeId,
) -> TypeIdx {
    for &variant_expr in variants {
        let name_ref = match &ck.ctx.ast.exprs[variant_expr] {
            AstExpr::Name { name_ref, .. } => Some(*name_ref),
            AstExpr::TypeApp { callee, .. } => {
                if let AstExpr::Name { name_ref, .. } = &ck.ctx.ast.exprs[*callee] {
                    Some(*name_ref)
                } else {
                    None
                }
            }
            _ => None,
        };
        if let Some(nr_idx) = name_ref {
            let name = ck.ctx.ast.name_refs[nr_idx].name;
            let id = ck
                .defs
                .alloc(name, DefKind::Type, Span::DUMMY, ck.ctx.file_id);
            let _prev = ck.scopes.define(ck.current_scope, name, id);
        }
    }

    let mut sum_variants = Vec::with_capacity(variants.len());
    for &variant_expr in variants {
        let (name_opt, args_opt) = match &ck.ctx.ast.exprs[variant_expr] {
            AstExpr::Name { name_ref, .. } => {
                (Some(ck.ctx.ast.name_refs[*name_ref].name), Some(vec![]))
            }
            AstExpr::TypeApp { callee, args, .. } => {
                if let AstExpr::Name { name_ref, .. } = &ck.ctx.ast.exprs[*callee] {
                    (
                        Some(ck.ctx.ast.name_refs[*name_ref].name),
                        Some(args.clone()),
                    )
                } else {
                    (None, None)
                }
            }
            _ => (None, None),
        };
        if let (Some(name), Some(args)) = (name_opt, args_opt) {
            let fields: Vec<TypeIdx> = args.iter().map(|&a| lower_type_expr(ck, a)).collect();
            sum_variants.push(SumVariant { name, fields });
        } else {
            let ty = lower_type_expr(ck, variant_expr);
            sum_variants.push(SumVariant {
                name: Symbol(0),
                fields: vec![ty],
            });
        }
    }

    ck.current_scope = parent;
    ck.alloc_ty(Type::Sum {
        variants: sum_variants,
    })
}

fn synth_choice<S: BuildHasher>(ck: &mut Checker<'_, S>, body: ExprIdx) -> TypeIdx {
    let parent = ck.current_scope;
    ck.current_scope = ck.scopes.push_child(parent);

    match ck.ctx.ast.exprs[body].clone() {
        AstExpr::TypeExpr {
            kind: TypeForm::Sum { variants },
            ..
        } => synth_choice_sum(ck, &variants, parent),
        AstExpr::Name { name_ref, .. } => {
            let name = ck.ctx.ast.name_refs[name_ref].name;
            let id = ck
                .defs
                .alloc(name, DefKind::Type, Span::DUMMY, ck.ctx.file_id);
            let _prev = ck.scopes.define(ck.current_scope, name, id);
            ck.current_scope = parent;
            ck.alloc_ty(Type::Sum {
                variants: vec![SumVariant {
                    name,
                    fields: vec![],
                }],
            })
        }
        AstExpr::TypeApp { callee, args, .. } => {
            let name_opt = if let AstExpr::Name { name_ref, .. } = &ck.ctx.ast.exprs[callee] {
                Some(ck.ctx.ast.name_refs[*name_ref].name)
            } else {
                None
            };
            if let Some(name) = name_opt {
                let id = ck
                    .defs
                    .alloc(name, DefKind::Type, Span::DUMMY, ck.ctx.file_id);
                let _prev = ck.scopes.define(ck.current_scope, name, id);
                let fields: Vec<TypeIdx> = args.iter().map(|&a| lower_type_expr(ck, a)).collect();
                ck.current_scope = parent;
                ck.alloc_ty(Type::Sum {
                    variants: vec![SumVariant { name, fields }],
                })
            } else {
                let ty = lower_type_expr(ck, body);
                ck.current_scope = parent;
                ty
            }
        }
        _ => {
            let ty = lower_type_expr(ck, body);
            ck.current_scope = parent;
            ty
        }
    }
}

fn synth_record_def<S: BuildHasher>(ck: &mut Checker<'_, S>, fields: &[RecDefField]) -> TypeIdx {
    let mut rec_fields: Vec<_> = fields
        .iter()
        .map(|f| RecordField {
            name: f.name,
            ty: lower_type_expr(ck, f.ty),
            ty_params: vec![],
            binding: None,
        })
        .collect();
    // repr(C) records preserve declaration order; normal records sort
    // alphabetically for canonical field indices across modules.
    if !ck.current_repr_c {
        rec_fields.sort_by(|a, b| {
            ck.ctx
                .interner
                .resolve(a.name)
                .cmp(ck.ctx.interner.resolve(b.name))
        });
    }
    ck.alloc_ty(Type::Record {
        fields: rec_fields,
        rest: None,
    })
}

fn synth_import<S: BuildHasher>(
    ck: &mut Checker<'_, S>,
    path: Symbol,
    alias: Option<Symbol>,
) -> TypeIdx {
    let record_ty = if let Some(&ty) = ck.ctx.import_types.get(&path) {
        ty
    } else {
        ck.named_ty(ck.ctx.well_known.unit)
    };
    if let Some(alias_name) = alias
        && let Some(def_id) = ck.scopes.lookup(ck.current_scope, alias_name)
    {
        ck.defs.get_mut(def_id).ty_info.ty = Some(record_ty);
    }
    record_ty
}

/// Expects `operand_ty` to be `Option<T>`, returning `T`.
/// Introduces a fresh `T`, unifies `operand_ty` with `Option<T>`, and returns `T`.
fn unwrap_option_ty<S: BuildHasher>(
    ck: &mut Checker<'_, S>,
    operand_ty: TypeIdx,
    span: Span,
) -> TypeIdx {
    let inner = ck.fresh_var(span);
    if let Some(def) = ck
        .ctx
        .interner
        .get("Option")
        .and_then(|sym| ck.scopes.lookup(ck.current_scope, sym))
    {
        let option_ty = ck.alloc_ty(Type::Named {
            def,
            args: vec![inner],
        });
        ck.unify_or_report(option_ty, operand_ty, span, None);
    }
    inner
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

/// Synthesizes a variant expression, checking arg types against the variant's declared fields.
fn synth_variant_with_args<S: BuildHasher>(
    ck: &mut Checker<'_, S>,
    name: Symbol,
    args: &[ExprIdx],
    span: Span,
) -> TypeIdx {
    let arg_tys: Vec<TypeIdx> = args.iter().map(|&a| synth(ck, a)).collect();

    if let Some(def_id) = ck.scopes.lookup(ck.current_scope, name)
        && ck.defs.get(def_id).kind == DefKind::Variant
    {
        check_variant_args(ck, def_id, &arg_tys, span)
    } else {
        synth_variant(ck, name, span)
    }
}

fn check_variant_args<S: BuildHasher>(
    ck: &mut Checker<'_, S>,
    def_id: DefId,
    arg_tys: &[TypeIdx],
    span: Span,
) -> TypeIdx {
    if let Some(variant_ty_idx) = ck.defs.get(def_id).ty_info.ty {
        if let Some(ret) = check_variant_fn_args(ck, variant_ty_idx, arg_tys, span) {
            return ret;
        }
    }
    get_variant_parent_type(ck, def_id, span)
}

fn check_variant_fn_args<S: BuildHasher>(
    ck: &mut Checker<'_, S>,
    variant_ty_idx: TypeIdx,
    arg_tys: &[TypeIdx],
    span: Span,
) -> Option<TypeIdx> {
    let resolved = ck.resolve_ty(variant_ty_idx);
    if let Type::Fn {
        params: declared_params,
        ret,
        ..
    } = ck.store.types[resolved].clone()
    {
        if arg_tys.len() == declared_params.len() {
            for (&arg_ty, &param_ty) in arg_tys.iter().zip(declared_params.iter()) {
                ck.unify_or_report(param_ty, arg_ty, span, None);
            }
        } else {
            let _d = ck.diags.report(
                &SemaError::ArityMismatch {
                    expected: declared_params.len(),
                    found: arg_tys.len(),
                },
                span,
                ck.ctx.file_id,
            );
        }
        return Some(ret);
    }
    None
}

fn get_variant_parent_type<S: BuildHasher>(
    ck: &mut Checker<'_, S>,
    def_id: DefId,
    span: Span,
) -> TypeIdx {
    if let Some(parent_id) = ck.defs.get(def_id).parent {
        if let Some(ty) = ck.defs.get(parent_id).ty_info.ty {
            return ty;
        }
        return ck.named_ty(parent_id);
    }
    ck.fresh_var(span)
}

/// Checks a `need op(...)` effect operation call.
///
/// `need` performs an effect operation. After synthesizing the operand, we look
/// up the callee name in the AST to find the matching effect op declaration and
/// return its declared return type. If the effect op cannot be found, the
/// operand type is returned as a fallback (suppresses cascading errors).
///
/// Also propagates the effect to `current_effects` so that `check_effects_subset`
/// can catch uses in pure functions.
fn synth_need<S: BuildHasher>(ck: &mut Checker<'_, S>, operand: ExprIdx, _span: Span) -> TypeIdx {
    let operand_ty = synth(ck, operand);

    let Some(op_name) = extract_need_op_name(ck, operand) else {
        return operand_ty;
    };

    // Propagate the effect to the current function's effect row (2b).
    if let Some(effect_def) = find_effect_def_for_op(ck, op_name) {
        let already_present = ck
            .current_effects
            .effects
            .iter()
            .any(|e| e.def == effect_def);
        if !already_present {
            ck.current_effects.effects.push(EffectEntry {
                def: effect_def,
                args: vec![],
            });
        }
    }

    find_effect_op_return_type(ck, op_name).unwrap_or(operand_ty)
}

fn extract_need_op_name<S: BuildHasher>(ck: &Checker<'_, S>, operand: ExprIdx) -> Option<Symbol> {
    match &ck.ctx.ast.exprs[operand] {
        AstExpr::Call { callee, .. } => match &ck.ctx.ast.exprs[*callee] {
            AstExpr::Name { name_ref, .. } => Some(ck.ctx.ast.name_refs[*name_ref].name),
            _ => None,
        },
        AstExpr::Name { name_ref, .. } => Some(ck.ctx.ast.name_refs[*name_ref].name),
        _ => None,
    }
}

fn find_effect_op_return_type<S: BuildHasher>(
    ck: &mut Checker<'_, S>,
    op_name: Symbol,
) -> Option<TypeIdx> {
    let n = ck.ctx.ast.exprs.len();
    for i in 0..n {
        let idx = Idx::from_raw(u32::try_from(i).expect("expr index in range"));
        if let AstExpr::Effect { ops, .. } = &ck.ctx.ast.exprs[idx] {
            if let Some(op) = ops.iter().find(|op| op.name == op_name) {
                let declared_ty = lower_type_expr(ck, op.ty);
                let resolved = ck.resolve_ty(declared_ty);
                if let Type::Fn { ret, .. } = ck.store.types[resolved].clone() {
                    return Some(ret);
                }
                return Some(declared_ty);
            }
        }
    }
    None
}

/// Finds the `DefId` of the effect that declares `op_name`, by looking it up
/// in the current scope. Used by `synth_need` to propagate the effect to the
/// current function's effect row (sub-task 2b).
fn find_effect_def_for_op<S: BuildHasher>(ck: &Checker<'_, S>, op_name: Symbol) -> Option<DefId> {
    let n = ck.ctx.ast.exprs.len();
    for i in 0..n {
        let idx = Idx::from_raw(u32::try_from(i).expect("expr index in range"));
        if let AstExpr::Effect { name, ops, .. } = &ck.ctx.ast.exprs[idx] {
            if ops.iter().any(|op| op.name == op_name) {
                return ck.scopes.lookup(ck.current_scope, *name);
            }
        }
    }
    None
}

/// Checks a `resume` expression.
///
/// `resume` is only valid inside a handler op body. Outside a handler, it is an error.
fn synth_resume<S: BuildHasher>(
    ck: &mut Checker<'_, S>,
    value: Option<ExprIdx>,
    span: Span,
) -> TypeIdx {
    if !ck.in_handler {
        let _d = ck
            .diags
            .report(&SemaError::ResumeOutsideHandler, span, ck.ctx.file_id);
    }
    if let Some(v) = value {
        synth(ck, v)
    } else {
        ck.named_ty(ck.ctx.well_known.unit)
    }
}

fn check_cast_safety<S: BuildHasher>(
    ck: &mut Checker<'_, S>,
    from_ty: TypeIdx,
    to_ty: TypeIdx,
    span: Span,
) {
    let from_resolved = ck.resolve_ty(from_ty);
    let to_resolved = ck.resolve_ty(to_ty);
    if let (Type::Named { def: from_def, .. }, Type::Named { def: to_def, .. }) = (
        &ck.store.types[from_resolved].clone(),
        &ck.store.types[to_resolved].clone(),
    ) && from_def != to_def
    {
        let defs_vec: Vec<_> = ck.defs.iter().cloned().collect();
        let from_str = fmt_type(
            from_resolved,
            &ck.store.types,
            &defs_vec,
            ck.ctx.interner,
            Some(&ck.store.unify),
        );
        let to_str = fmt_type(
            to_resolved,
            &ck.store.types,
            &defs_vec,
            ck.ctx.interner,
            Some(&ck.store.unify),
        );
        let _d = ck.diags.report(
            &SemaError::UnsafeCast {
                from: from_str,
                to: to_str,
            },
            span,
            ck.ctx.file_id,
        );
    }
}

fn synth_type_check<S: BuildHasher>(
    ck: &mut Checker<'_, S>,
    kind: TypeCheckKind,
    operand: ExprIdx,
    ty: ExprIdx,
    binding: Option<Symbol>,
    span: Span,
) -> TypeIdx {
    match kind {
        TypeCheckKind::Test => synth_type_test(ck, operand, ty, binding, span),
        TypeCheckKind::Cast => {
            let operand_ty = synth(ck, operand);
            let target_ty = lower_type_expr(ck, ty);
            check_cast_safety(ck, operand_ty, target_ty, span);
            target_ty
        }
    }
}

fn synth_type_test<S: BuildHasher>(
    ck: &mut Checker<'_, S>,
    operand: ExprIdx,
    ty: ExprIdx,
    binding: Option<Symbol>,
    span: Span,
) -> TypeIdx {
    let _operand_ty = synth(ck, operand);
    let test_ty = lower_type_expr(ck, ty);
    if let Some(name) = binding {
        let id = ck.defs.alloc(name, DefKind::Let, span, ck.ctx.file_id);
        ck.defs.get_mut(id).ty_info.ty = Some(test_ty);
        let _prev = ck.scopes.define(ck.current_scope, name, id);
    }
    ck.named_ty(ck.ctx.well_known.bool)
}

fn synth_handle<S: BuildHasher>(
    ck: &mut Checker<'_, S>,
    effect_ty: ExprIdx,
    ops: &[HandlerOp],
    body: ExprIdx,
) -> TypeIdx {
    let _eff_ty = lower_type_expr(ck, effect_ty);
    let effect_op_types = collect_effect_op_types(ck, effect_ty);

    // Collect which op names are declared `fatal` in the effect definition (2a).
    let fatal_ops = collect_fatal_op_names(ck, effect_ty);

    let prev_in_handler = ck.in_handler;
    ck.in_handler = true;

    for op in ops {
        let is_fatal = fatal_ops.contains(&op.name);
        synth_handler_op(ck, op, &effect_op_types, is_fatal);
    }

    ck.in_handler = prev_in_handler;
    check_handler_op_coverage(ck, effect_ty, ops);

    // Temporarily make the handled effect available so that `check_effects_subset`
    // inside the body does not flag it as undeclared (2c). After synthesising the body
    // restore the previous row - the effect has been discharged by this handler.
    let prev_effects = ck.current_effects.clone();
    if let Some(effect_def) = resolve_effect_def(ck, effect_ty) {
        let already_present = ck
            .current_effects
            .effects
            .iter()
            .any(|e| e.def == effect_def);
        if !already_present {
            ck.current_effects.effects.push(EffectEntry {
                def: effect_def,
                args: vec![],
            });
        }
    }
    let body_ty = synth(ck, body);
    ck.current_effects = prev_effects;
    body_ty
}

fn synth_handler_op<S: BuildHasher>(
    ck: &mut Checker<'_, S>,
    op: &HandlerOp,
    effect_op_types: &HashMap<Symbol, TypeIdx>,
    is_fatal: bool,
) {
    let parent = ck.current_scope;
    ck.current_scope = ck.scopes.push_child(parent);

    let declared_params = get_handler_declared_params(ck, op.name, effect_op_types);
    synth_handler_params(ck, op, declared_params.as_ref());

    // 2a: `fatal` ops must not contain `resume`. Walk the body before synthesising
    // to catch all `resume` sites, including those inside nested expressions.
    if is_fatal {
        check_no_resume_in_fatal_op(ck, op.body);
    }

    let op_ty = synth(ck, op.body);
    if let Some(&decl_ty_idx) = effect_op_types.get(&op.name) {
        let resolved = ck.resolve_ty(decl_ty_idx);
        if let Type::Fn { ret, .. } = ck.store.types[resolved].clone() {
            ck.unify_or_report(ret, op_ty, op.span, None);
        }
    }
    ck.current_scope = parent;
}

fn synth_handler_params<S: BuildHasher>(
    ck: &mut Checker<'_, S>,
    op: &HandlerOp,
    declared_params: Option<&Vec<TypeIdx>>,
) {
    for (i, param) in op.params.iter().enumerate() {
        let param_ty = if let Some(ty) = param.ty {
            lower_type_expr(ck, ty)
        } else {
            ck.fresh_var(param.span)
        };
        if let Some(declared) = declared_params.as_ref().and_then(|v| v.get(i).copied()) {
            ck.unify_or_report(declared, param_ty, param.span, None);
        }
        let id = ck
            .defs
            .alloc(param.name, DefKind::Param, param.span, ck.ctx.file_id);
        ck.defs.get_mut(id).ty_info.ty = Some(param_ty);
        let _prev = ck.scopes.define(ck.current_scope, param.name, id);
    }
}

fn get_handler_declared_params<S: BuildHasher>(
    ck: &Checker<'_, S>,
    op_name: Symbol,
    effect_op_types: &HashMap<Symbol, TypeIdx>,
) -> Option<Vec<TypeIdx>> {
    effect_op_types.get(&op_name).and_then(|&decl_ty_idx| {
        let resolved = ck.resolve_ty(decl_ty_idx);
        if let Type::Fn { params, .. } = ck.store.types[resolved].clone() {
            Some(params)
        } else {
            None
        }
    })
}

/// Returns the set of op names that are declared `fatal` in the effect referenced by
/// `effect_ty_expr`. Used by `synth_handle` to enforce the no-resume-in-fatal rule (2a).
fn collect_fatal_op_names<S: BuildHasher>(
    ck: &Checker<'_, S>,
    effect_ty_expr: ExprIdx,
) -> HashSet<Symbol> {
    let effect_name = match &ck.ctx.ast.exprs[effect_ty_expr] {
        AstExpr::Name { name_ref, .. } => ck.ctx.ast.name_refs[*name_ref].name,
        AstExpr::TypeApp { callee, .. } => match &ck.ctx.ast.exprs[*callee] {
            AstExpr::Name { name_ref, .. } => ck.ctx.ast.name_refs[*name_ref].name,
            _ => return HashSet::new(),
        },
        _ => return HashSet::new(),
    };

    let n = ck.ctx.ast.exprs.len();
    for i in 0..n {
        let idx = Idx::from_raw(u32::try_from(i).expect("expr index in range"));
        if let AstExpr::Effect { name, ops, .. } = &ck.ctx.ast.exprs[idx]
            && *name == effect_name
        {
            return ops.iter().filter(|op| op.fatal).map(|op| op.name).collect();
        }
    }
    HashSet::new()
}

/// Resolves the `DefId` of the effect referred to by `effect_ty_expr` (the `with Eff` clause).
/// Used by `synth_handle` to discharge the effect from the body's required effects (2c).
fn resolve_effect_def<S: BuildHasher>(
    ck: &Checker<'_, S>,
    effect_ty_expr: ExprIdx,
) -> Option<DefId> {
    let effect_name = match &ck.ctx.ast.exprs[effect_ty_expr] {
        AstExpr::Name { name_ref, .. } => ck.ctx.ast.name_refs[*name_ref].name,
        AstExpr::TypeApp { callee, .. } => match &ck.ctx.ast.exprs[*callee] {
            AstExpr::Name { name_ref, .. } => ck.ctx.ast.name_refs[*name_ref].name,
            _ => return None,
        },
        _ => return None,
    };
    ck.scopes.lookup(ck.current_scope, effect_name)
}

/// Walks `body_expr` recursively and reports a `ResumeInFatalHandler` error for every
/// `Expr::Resume` found. Does not descend into nested `Handle` expressions (a `resume`
/// inside an inner handler is that handler's concern, not this one).
fn check_no_resume_in_fatal_op<S: BuildHasher>(ck: &mut Checker<'_, S>, body_expr: ExprIdx) {
    use msc_ast::visitor::{AstVisitor, walk_expr};
    use std::ops::ControlFlow;

    struct ResumeCollector {
        found: Vec<Span>,
    }

    impl AstVisitor for ResumeCollector {
        type Break = ();

        fn visit_expr(&mut self, idx: ExprIdx, ctx: &msc_ast::AstArenas) -> ControlFlow<()> {
            match &ctx.exprs[idx] {
                AstExpr::Resume { span, .. } => {
                    self.found.push(*span);
                    ControlFlow::Continue(())
                }
                // Don't descend into nested handlers - their resumes are their concern.
                AstExpr::Handle { .. } => ControlFlow::Continue(()),
                _ => walk_expr(self, idx, ctx),
            }
        }
    }

    let mut collector = ResumeCollector { found: vec![] };
    _ = collector.visit_expr(body_expr, ck.ctx.ast);
    for span in collector.found {
        let _d = ck
            .diags
            .report(&SemaError::ResumeInFatalHandler, span, ck.ctx.file_id);
    }
}

/// Returns a map from effect op name to the declared type index for each op in the effect
/// referenced by `effect_ty_expr`. Used by `synth_handle` to unify handler param types.
fn collect_effect_op_types<S: BuildHasher>(
    ck: &mut Checker<'_, S>,
    effect_ty_expr: ExprIdx,
) -> HashMap<Symbol, TypeIdx> {
    let effect_name = match &ck.ctx.ast.exprs[effect_ty_expr] {
        AstExpr::Name { name_ref, .. } => ck.ctx.ast.name_refs[*name_ref].name,
        AstExpr::TypeApp { callee, .. } => match &ck.ctx.ast.exprs[*callee] {
            AstExpr::Name { name_ref, .. } => ck.ctx.ast.name_refs[*name_ref].name,
            _ => return HashMap::new(),
        },
        _ => return HashMap::new(),
    };

    let n = ck.ctx.ast.exprs.len();
    for i in 0..n {
        let idx = Idx::from_raw(u32::try_from(i).expect("expr index in range"));
        if let AstExpr::Effect { name, ops, .. } = &ck.ctx.ast.exprs[idx]
            && *name == effect_name
        {
            let op_types: Vec<(Symbol, TypeIdx)> = ops
                .iter()
                .map(|op| {
                    let op_name = op.name;
                    let ty_idx = lower_type_expr(ck, op.ty);
                    (op_name, ty_idx)
                })
                .collect();
            return op_types.into_iter().collect();
        }
    }
    HashMap::new()
}
