//! Per-expression synthesis and checking.

use music_ast::expr::{
    Arg, ArrayElem, BinOp, Expr, FieldKey, LetFields, MatchArm, Param, RecField, UnaryOp,
};
use music_ast::lit::Lit;
use music_ast::pat::Pat;
use music_ast::ty::Ty;
use music_ast::{ExprIdx, PatIdx, TyIdx};
use music_shared::{Span, Symbol};

use crate::checker::Checker;
use crate::checker::effects::check_effects_subset;
use crate::checker::pat::check_pat;
use crate::checker::stmt::check_stmt;
use crate::checker::ty::lower_ty;
use crate::def::DefKind;
use crate::error::SemaError;
use crate::resolve;
use crate::scope::ScopeId;
use crate::types::{EffectRow, RecordField, Type, TypeIdx, fmt_type};

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
        Expr::Index { object, index, .. } => {
            let obj_ty = synth(ck, object);
            let _idx_ty = synth(ck, index);
            let obj_ty = ck.resolve_ty(obj_ty);
            match &ck.store.types[obj_ty] {
                Type::Array { elem, .. } => *elem,
                _ => ck.error_ty(),
            }
        }
        Expr::Record { fields, .. } => synth_record(ck, &fields),
        Expr::Array { elems, span } => synth_array(ck, &elems, span),
        Expr::Piecewise { arms, span } => {
            let result_ty = ck.fresh_var(span);
            for arm in &arms {
                let arm_ty = synth(ck, arm.result);
                ck.unify_or_report(result_ty, arm_ty, span);
            }
            result_ty
        }
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
        Expr::Update { base, fields, .. } => {
            let base_ty = synth(ck, base);
            for field in &fields {
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
        Expr::Choice { body, .. } => synth_choice(ck, body),
        Expr::Quantified { body, .. } => synth(ck, body),
        Expr::Class { .. } | Expr::Given { .. } | Expr::Effect { .. } | Expr::Foreign { .. } => {
            check_stmt(ck, expr_idx);
            ck.named_ty(ck.ctx.well_known.unit)
        }
        Expr::Import { path, .. } => synth_import(ck, path),
        Expr::Export { .. } => ck.named_ty(ck.ctx.well_known.unit),
        Expr::Error { .. } => ck.error_ty(),
    }
}

/// If `pat` is a function-like pattern (`Pat::Variant` with args), enters a
/// child scope and checks each arg pattern with a fresh type variable so the
/// param names are in scope when the value expression is checked.
/// Returns `Some(parent_scope)` if a scope was entered, `None` otherwise.
fn enter_fn_pat_scope(ck: &mut Checker<'_>, pat: PatIdx) -> Option<ScopeId> {
    if let Pat::Variant { args, .. } = &ck.ctx.ast.pats[pat] {
        if args.is_empty() {
            return None;
        }
        let parent = ck.current_scope;
        ck.current_scope = ck.scopes.push_child(parent);
        for &arg in args {
            let fresh = ck.fresh_var(Span::DUMMY);
            check_pat(ck, arg, fresh);
        }
        Some(parent)
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
    let parent_scope = if fields.params.is_empty() {
        None
    } else {
        Some(ck.enter_ty_param_scope(&fields.params))
    };

    let fn_pat_parent = enter_fn_pat_scope(ck, fields.pat);

    let value_ty = match (fields.ty, fields.value) {
        (Some(ty_ann), Some(val)) => {
            let ann = lower_ty(ck, ty_ann);
            check(ck, val, ann);
            ann
        }
        (Some(ty_ann), None) => lower_ty(ck, ty_ann),
        (None, Some(val)) => synth(ck, val),
        (None, None) => ck.named_ty(ck.ctx.well_known.unit),
    };

    if let Some(p) = fn_pat_parent {
        ck.current_scope = p;
    }

    check_pat(ck, fields.pat, value_ty);

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
    let parent_scope = if fields.params.is_empty() {
        None
    } else {
        Some(ck.enter_ty_param_scope(&fields.params))
    };

    let fn_pat_parent = enter_fn_pat_scope(ck, fields.pat);

    let value_ty = match (fields.ty, fields.value) {
        (Some(ty_ann), Some(val)) => {
            let ann = lower_ty(ck, ty_ann);
            check(ck, val, ann);
            ann
        }
        (Some(ty_ann), None) => lower_ty(ck, ty_ann),
        (None, Some(val)) => synth(ck, val),
        (None, None) => ck.named_ty(ck.ctx.well_known.unit),
    };

    if let Some(p) = fn_pat_parent {
        ck.current_scope = p;
    }

    check_pat(ck, fields.pat, value_ty);

    if let Some(p) = parent_scope {
        ck.current_scope = p;
    }
    ck.named_ty(ck.ctx.well_known.unit)
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
        Lit::Str { .. } | Lit::FStr { .. } => ck.named_ty(ck.ctx.well_known.string),
        Lit::Rune { .. } => ck.named_ty(ck.ctx.well_known.rune),
        Lit::Unit { .. } => ck.named_ty(ck.ctx.well_known.unit),
    }
}

fn synth_name(ck: &mut Checker<'_>, expr_idx: ExprIdx, span: Span) -> TypeIdx {
    if let Some(&def_id) = ck.ctx.expr_defs.get(&expr_idx) {
        ck.defs
            .get(def_id)
            .ty_info
            .ty
            .unwrap_or_else(|| ck.fresh_var(span))
    } else {
        ck.error_ty()
    }
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
                    Arg::Hole { span, .. } => ck.fresh_var(*span),
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
        | BinOp::ShrUn
        | BinOp::RangeInc
        | BinOp::RangeExc
        | BinOp::NilCoal => {
            ck.unify_or_report(left_ty, right_ty, span);
            left_ty
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
        UnaryOp::Neg | UnaryOp::Defer | UnaryOp::Spawn | UnaryOp::Await | UnaryOp::Try => {
            operand_ty
        }
    }
}

fn synth_choice(ck: &mut Checker<'_>, body: TyIdx) -> TypeIdx {
    let parent = ck.current_scope;
    ck.current_scope = ck.scopes.push_child(parent);

    if let Ty::Sum { variants, .. } = &ck.ctx.ast.tys[body] {
        for &variant_ty in variants {
            if let Ty::Named { name, .. } = &ck.ctx.ast.tys[variant_ty] {
                let id = ck.defs.alloc(*name, DefKind::Type, Span::DUMMY);
                let _prev = ck.scopes.define(ck.current_scope, *name, id);
            }
        }
    }

    let ty = lower_ty(ck, body);
    ck.current_scope = parent;
    ty
}

fn synth_import(ck: &mut Checker<'_>, path: Symbol) -> TypeIdx {
    if let Some(&record_ty) = ck.ctx.import_types.get(&path) {
        record_ty
    } else {
        ck.named_ty(ck.ctx.well_known.unit)
    }
}
