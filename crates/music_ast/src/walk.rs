use music_found::{Span, Spanned};

use crate::common::{FnDecl, LawDecl, MemberDecl, Param, RecordDefField, Signature, TyRef};
use crate::data::AstData;
use crate::expr::{
    CompClause, ExprKind, FStrPart, IndexKind, InstanceBody, InstanceDef, LetBinding, MatchArm,
    PiecewiseArm, PwGuard, QuoteKind, RecordField, SpliceKind,
};
use crate::{ExprId, ParamList, TyId};

/// Apply `f` to every direct `ExprId` child of the expression at `expr_id`.
///
/// Returns `expr_id` unchanged when no child was modified. Allocates a new
/// node only when at least one child differs.
pub fn map_expr_children(
    ast: &mut AstData,
    expr_id: ExprId,
    f: &mut impl FnMut(&mut AstData, ExprId) -> ExprId,
) -> ExprId {
    let spanned = ast.exprs.get(expr_id);
    let span = spanned.span;
    let kind = spanned.kind.clone();

    match kind {
        ExprKind::Lit(_)
        | ExprKind::Var(_)
        | ExprKind::ChoiceDef(_)
        | ExprKind::Import { .. }
        | ExprKind::ForeignImport(_)
        | ExprKind::Return(None)
        | ExprKind::Resume(None)
        | ExprKind::Splice(SpliceKind::Ident(_)) => expr_id,

        ExprKind::UnaryOp(..)
        | ExprKind::Need(_)
        | ExprKind::Return(Some(_))
        | ExprKind::Resume(Some(_))
        | ExprKind::Postfix { .. }
        | ExprKind::Access { .. }
        | ExprKind::TypeOp { .. }
        | ExprKind::Quote(_)
        | ExprKind::Splice(_)
        | ExprKind::BinOp(..)
        | ExprKind::Assign(..)
        | ExprKind::App(..)
        | ExprKind::TupleLit(_)
        | ExprKind::ArrayLit(_)
        | ExprKind::Seq(_)
        | ExprKind::VariantLit(..) => map_ops(ast, expr_id, &kind, span, f),

        _ => map_structure(ast, expr_id, &kind, span, f),
    }
}

fn map_ops(
    ast: &mut AstData,
    expr_id: ExprId,
    kind: &ExprKind,
    span: Span,
    f: &mut impl FnMut(&mut AstData, ExprId) -> ExprId,
) -> ExprId {
    match *kind {
        ExprKind::UnaryOp(op, e) => map1(ast, expr_id, e, |e| ExprKind::UnaryOp(op, e), span, f),
        ExprKind::Need(e) => map1(ast, expr_id, e, ExprKind::Need, span, f),
        ExprKind::Return(Some(e)) => map1(ast, expr_id, e, |e| ExprKind::Return(Some(e)), span, f),
        ExprKind::Resume(Some(e)) => map1(ast, expr_id, e, |e| ExprKind::Resume(Some(e)), span, f),
        ExprKind::Postfix { expr: e, op } => map1(
            ast,
            expr_id,
            e,
            |e| ExprKind::Postfix { expr: e, op },
            span,
            f,
        ),
        ExprKind::Access {
            expr: e,
            field,
            mode,
        } => map1(
            ast,
            expr_id,
            e,
            |e| ExprKind::Access {
                expr: e,
                field,
                mode,
            },
            span,
            f,
        ),
        ExprKind::TypeOp {
            expr: e,
            ty,
            kind: k,
        } => map1(
            ast,
            expr_id,
            e,
            |e| ExprKind::TypeOp {
                expr: e,
                ty,
                kind: k,
            },
            span,
            f,
        ),
        ExprKind::Quote(QuoteKind::Expr(e)) => map1(
            ast,
            expr_id,
            e,
            |e| ExprKind::Quote(QuoteKind::Expr(e)),
            span,
            f,
        ),
        ExprKind::Quote(QuoteKind::Block(ref v)) => map_list(
            ast,
            expr_id,
            v,
            |v| ExprKind::Quote(QuoteKind::Block(v)),
            span,
            f,
        ),
        ExprKind::Splice(SpliceKind::Expr(e)) => map1(
            ast,
            expr_id,
            e,
            |e| ExprKind::Splice(SpliceKind::Expr(e)),
            span,
            f,
        ),
        ExprKind::Splice(SpliceKind::Array(ref v)) => map_list(
            ast,
            expr_id,
            v,
            |v| ExprKind::Splice(SpliceKind::Array(v)),
            span,
            f,
        ),
        ExprKind::BinOp(op, l, r) => map2(
            ast,
            expr_id,
            l,
            r,
            |l, r| ExprKind::BinOp(op, l, r),
            span,
            f,
        ),
        ExprKind::Assign(l, r) => map2(ast, expr_id, l, r, ExprKind::Assign, span, f),
        ExprKind::App(callee, ref args) => map_app(ast, expr_id, callee, args, span, f),
        ExprKind::TupleLit(ref v) => map_list(ast, expr_id, v, ExprKind::TupleLit, span, f),
        ExprKind::ArrayLit(ref v) => map_list(ast, expr_id, v, ExprKind::ArrayLit, span, f),
        ExprKind::Seq(ref v) => map_list(ast, expr_id, v, ExprKind::Seq, span, f),
        ExprKind::VariantLit(id, ref v) => {
            map_list(ast, expr_id, v, |v| ExprKind::VariantLit(id, v), span, f)
        }
        _ => expr_id,
    }
}

fn map_structure(
    ast: &mut AstData,
    expr_id: ExprId,
    kind: &ExprKind,
    span: Span,
    f: &mut impl FnMut(&mut AstData, ExprId) -> ExprId,
) -> ExprId {
    match *kind {
        ExprKind::Branch {
            cond,
            then_br,
            else_br,
        } => map_branch(ast, expr_id, cond, then_br, else_br, span, f),
        ExprKind::Lambda {
            ref params,
            ret_ty,
            body,
        } => map_lambda(ast, expr_id, params, ret_ty, body, span, f),
        ExprKind::Let(ref binding) => map_let(ast, expr_id, binding, span, f),
        ExprKind::Index {
            expr,
            ref indices,
            kind: k,
        } => map_index(ast, expr_id, expr, indices, k, span, f),
        ExprKind::Match(s, ref arms) => map_match(ast, expr_id, s, arms, span, f),
        ExprKind::Comprehension { expr, ref clauses } => {
            map_comp(ast, expr_id, expr, clauses, span, f)
        }
        ExprKind::MatrixLit(ref rows) => map_matrix(ast, expr_id, rows, span, f),
        ExprKind::RecordLit(ref fields) => map_rec_lit(ast, expr_id, fields, span, f),
        ExprKind::RecordUpdate { base, ref fields } => {
            map_rec_update(ast, expr_id, base, fields, span, f)
        }
        ExprKind::FStrLit(ref parts) => map_fstr(ast, expr_id, parts, span, f),
        ExprKind::Piecewise(ref arms) => map_pw(ast, expr_id, arms, span, f),
        ExprKind::Handle {
            ref effect,
            ref handlers,
            body,
        } => map_handle(ast, expr_id, effect.clone(), handlers, body, span, f),
        ExprKind::RecordDef(ref fields) => map_rec_def(ast, expr_id, fields, span, f),
        ExprKind::EffectDef(ref m) => {
            map_member_node(ast, expr_id, m, ExprKind::EffectDef, span, f)
        }
        ExprKind::ClassDef {
            ref constraints,
            ref members,
        } => map_member_node(
            ast,
            expr_id,
            members,
            |m| ExprKind::ClassDef {
                constraints: constraints.clone(),
                members: m,
            },
            span,
            f,
        ),
        ExprKind::InstanceDef(ref inst) => map_inst(ast, expr_id, inst, span, f),
        _ => expr_id,
    }
}

// --- Primitives ---

fn map1(
    ast: &mut AstData,
    expr_id: ExprId,
    child: ExprId,
    wrap: impl FnOnce(ExprId) -> ExprKind,
    span: Span,
    f: &mut impl FnMut(&mut AstData, ExprId) -> ExprId,
) -> ExprId {
    let new_child = f(ast, child);
    if new_child == child {
        return expr_id;
    }
    ast.exprs.alloc(Spanned::new(wrap(new_child), span))
}

fn map2(
    ast: &mut AstData,
    expr_id: ExprId,
    lhs: ExprId,
    rhs: ExprId,
    wrap: impl FnOnce(ExprId, ExprId) -> ExprKind,
    span: Span,
    f: &mut impl FnMut(&mut AstData, ExprId) -> ExprId,
) -> ExprId {
    let new_lhs = f(ast, lhs);
    let new_rhs = f(ast, rhs);
    if new_lhs == lhs && new_rhs == rhs {
        return expr_id;
    }
    ast.exprs.alloc(Spanned::new(wrap(new_lhs, new_rhs), span))
}

fn map_ids(
    ast: &mut AstData,
    ids: &[ExprId],
    f: &mut impl FnMut(&mut AstData, ExprId) -> ExprId,
) -> (Vec<ExprId>, bool) {
    let mut changed = false;
    let new_ids: Vec<ExprId> = ids
        .iter()
        .map(|&id| {
            let new_id = f(ast, id);
            if new_id != id {
                changed = true;
            }
            new_id
        })
        .collect();
    (new_ids, changed)
}

fn map_params(
    ast: &mut AstData,
    params: &[Param],
    f: &mut impl FnMut(&mut AstData, ExprId) -> ExprId,
) -> (Vec<Param>, bool) {
    let mut changed = false;
    let new_params: Vec<Param> = params
        .iter()
        .map(|param| {
            let new_default = param.default.map(|d| {
                let new_d = f(ast, d);
                if new_d != d {
                    changed = true;
                }
                new_d
            });
            if new_default != param.default {
                changed = true;
            }
            Param {
                mutable: param.mutable,
                name: param.name,
                ty: param.ty,
                default: new_default,
            }
        })
        .collect();
    (new_params, changed)
}

fn map_list(
    ast: &mut AstData,
    expr_id: ExprId,
    items: &[ExprId],
    wrap: impl FnOnce(Vec<ExprId>) -> ExprKind,
    span: Span,
    f: &mut impl FnMut(&mut AstData, ExprId) -> ExprId,
) -> ExprId {
    let (new_items, changed) = map_ids(ast, items, f);
    if !changed {
        return expr_id;
    }
    ast.exprs.alloc(Spanned::new(wrap(new_items), span))
}

// --- Compound helpers ---

fn map_app(
    ast: &mut AstData,
    expr_id: ExprId,
    callee: ExprId,
    args: &[ExprId],
    span: Span,
    f: &mut impl FnMut(&mut AstData, ExprId) -> ExprId,
) -> ExprId {
    let new_callee = f(ast, callee);
    let (new_args, args_changed) = map_ids(ast, args, f);
    if new_callee == callee && !args_changed {
        return expr_id;
    }
    ast.exprs
        .alloc(Spanned::new(ExprKind::App(new_callee, new_args), span))
}

fn map_branch(
    ast: &mut AstData,
    expr_id: ExprId,
    cond: ExprId,
    then_br: ExprId,
    else_br: ExprId,
    span: Span,
    f: &mut impl FnMut(&mut AstData, ExprId) -> ExprId,
) -> ExprId {
    let new_cond = f(ast, cond);
    let new_then = f(ast, then_br);
    let new_else = f(ast, else_br);
    if new_cond == cond && new_then == then_br && new_else == else_br {
        return expr_id;
    }
    ast.exprs.alloc(Spanned::new(
        ExprKind::Branch {
            cond: new_cond,
            then_br: new_then,
            else_br: new_else,
        },
        span,
    ))
}

fn map_lambda(
    ast: &mut AstData,
    expr_id: ExprId,
    params: &ParamList,
    ret_ty: Option<TyId>,
    body: ExprId,
    span: Span,
    f: &mut impl FnMut(&mut AstData, ExprId) -> ExprId,
) -> ExprId {
    let new_body = f(ast, body);
    let (new_params, params_changed) = map_params(ast, params, f);
    if new_body == body && !params_changed {
        return expr_id;
    }
    ast.exprs.alloc(Spanned::new(
        ExprKind::Lambda {
            params: new_params,
            ret_ty,
            body: new_body,
        },
        span,
    ))
}

fn map_let(
    ast: &mut AstData,
    expr_id: ExprId,
    binding: &LetBinding,
    span: Span,
    f: &mut impl FnMut(&mut AstData, ExprId) -> ExprId,
) -> ExprId {
    let new_value = binding.value.map(|v| f(ast, v));
    let (new_sig, sig_changed) = binding.sig.as_ref().map_or((None, false), |sig| {
        let (new_params, params_changed) = map_params(ast, &sig.params, f);
        if params_changed {
            let new_sig = Signature {
                params: new_params,
                ..(**sig).clone()
            };
            (Some(Box::new(new_sig)), true)
        } else {
            (Some(sig.clone()), false)
        }
    });
    let value_changed = new_value != binding.value;
    if !value_changed && !sig_changed {
        return expr_id;
    }
    let new_binding = LetBinding {
        value: new_value,
        sig: new_sig,
        ..binding.clone()
    };
    ast.exprs
        .alloc(Spanned::new(ExprKind::Let(Box::new(new_binding)), span))
}

fn map_index(
    ast: &mut AstData,
    expr_id: ExprId,
    expr: ExprId,
    indices: &[ExprId],
    kind: IndexKind,
    span: Span,
    f: &mut impl FnMut(&mut AstData, ExprId) -> ExprId,
) -> ExprId {
    let new_expr = f(ast, expr);
    let (new_indices, indices_changed) = map_ids(ast, indices, f);
    if new_expr == expr && !indices_changed {
        return expr_id;
    }
    ast.exprs.alloc(Spanned::new(
        ExprKind::Index {
            expr: new_expr,
            indices: new_indices,
            kind,
        },
        span,
    ))
}

fn map_match(
    ast: &mut AstData,
    expr_id: ExprId,
    scrutinee: ExprId,
    arms: &[MatchArm],
    span: Span,
    f: &mut impl FnMut(&mut AstData, ExprId) -> ExprId,
) -> ExprId {
    let new_scrutinee = f(ast, scrutinee);
    let mut changed = new_scrutinee != scrutinee;
    let new_arms: Vec<MatchArm> = arms
        .iter()
        .map(|arm| {
            let new_body = f(ast, arm.body);
            let new_guard = arm.guard.map(|g| f(ast, g));
            if new_body != arm.body || new_guard != arm.guard {
                changed = true;
            }
            MatchArm {
                attrs: arm.attrs.clone(),
                pat: arm.pat,
                guard: new_guard,
                body: new_body,
            }
        })
        .collect();
    if !changed {
        return expr_id;
    }
    ast.exprs
        .alloc(Spanned::new(ExprKind::Match(new_scrutinee, new_arms), span))
}

fn map_comp(
    ast: &mut AstData,
    expr_id: ExprId,
    expr: ExprId,
    clauses: &[CompClause],
    span: Span,
    f: &mut impl FnMut(&mut AstData, ExprId) -> ExprId,
) -> ExprId {
    let new_expr = f(ast, expr);
    let mut changed = new_expr != expr;
    let new_clauses: Vec<CompClause> = clauses
        .iter()
        .map(|clause| match clause {
            CompClause::Generator { pat, iter } => {
                let new_iter = f(ast, *iter);
                if new_iter != *iter {
                    changed = true;
                }
                CompClause::Generator {
                    pat: *pat,
                    iter: new_iter,
                }
            }
            CompClause::Filter(e) => {
                let new_e = f(ast, *e);
                if new_e != *e {
                    changed = true;
                }
                CompClause::Filter(new_e)
            }
        })
        .collect();
    if !changed {
        return expr_id;
    }
    ast.exprs.alloc(Spanned::new(
        ExprKind::Comprehension {
            expr: new_expr,
            clauses: new_clauses,
        },
        span,
    ))
}

fn map_matrix(
    ast: &mut AstData,
    expr_id: ExprId,
    rows: &[Vec<ExprId>],
    span: Span,
    f: &mut impl FnMut(&mut AstData, ExprId) -> ExprId,
) -> ExprId {
    let mut changed = false;
    let new_rows: Vec<Vec<ExprId>> = rows
        .iter()
        .map(|row| {
            let (new_row, row_changed) = map_ids(ast, row, f);
            if row_changed {
                changed = true;
            }
            new_row
        })
        .collect();
    if !changed {
        return expr_id;
    }
    ast.exprs
        .alloc(Spanned::new(ExprKind::MatrixLit(new_rows), span))
}

fn map_rec_fields(
    ast: &mut AstData,
    fields: &[RecordField],
    f: &mut impl FnMut(&mut AstData, ExprId) -> ExprId,
) -> (Vec<RecordField>, bool) {
    let mut changed = false;
    let new_fields: Vec<RecordField> = fields
        .iter()
        .map(|field| match field {
            RecordField::Named { name, value } => {
                let new_value = value.map(|v| {
                    let new_v = f(ast, v);
                    if new_v != v {
                        changed = true;
                    }
                    new_v
                });
                RecordField::Named {
                    name: *name,
                    value: new_value,
                }
            }
            RecordField::Spread(e) => {
                let new_e = f(ast, *e);
                if new_e != *e {
                    changed = true;
                }
                RecordField::Spread(new_e)
            }
        })
        .collect();
    (new_fields, changed)
}

fn map_rec_lit(
    ast: &mut AstData,
    expr_id: ExprId,
    fields: &[RecordField],
    span: Span,
    f: &mut impl FnMut(&mut AstData, ExprId) -> ExprId,
) -> ExprId {
    let (new_fields, changed) = map_rec_fields(ast, fields, f);
    if !changed {
        return expr_id;
    }
    ast.exprs
        .alloc(Spanned::new(ExprKind::RecordLit(new_fields), span))
}

fn map_rec_update(
    ast: &mut AstData,
    expr_id: ExprId,
    base: ExprId,
    fields: &[RecordField],
    span: Span,
    f: &mut impl FnMut(&mut AstData, ExprId) -> ExprId,
) -> ExprId {
    let new_base = f(ast, base);
    let (new_fields, fields_changed) = map_rec_fields(ast, fields, f);
    if new_base == base && !fields_changed {
        return expr_id;
    }
    ast.exprs.alloc(Spanned::new(
        ExprKind::RecordUpdate {
            base: new_base,
            fields: new_fields,
        },
        span,
    ))
}

fn map_fstr(
    ast: &mut AstData,
    expr_id: ExprId,
    parts: &[FStrPart],
    span: Span,
    f: &mut impl FnMut(&mut AstData, ExprId) -> ExprId,
) -> ExprId {
    let mut changed = false;
    let new_parts: Vec<FStrPart> = parts
        .iter()
        .map(|part| match part {
            FStrPart::Lit(s) => FStrPart::Lit(s.clone()),
            FStrPart::Expr(e) => {
                let new_e = f(ast, *e);
                if new_e != *e {
                    changed = true;
                }
                FStrPart::Expr(new_e)
            }
        })
        .collect();
    if !changed {
        return expr_id;
    }
    ast.exprs
        .alloc(Spanned::new(ExprKind::FStrLit(new_parts), span))
}

fn map_pw(
    ast: &mut AstData,
    expr_id: ExprId,
    arms: &[PiecewiseArm],
    span: Span,
    f: &mut impl FnMut(&mut AstData, ExprId) -> ExprId,
) -> ExprId {
    let mut changed = false;
    let new_arms: Vec<PiecewiseArm> = arms
        .iter()
        .map(|arm| {
            let new_value = f(ast, arm.value);
            let new_guard = match arm.guard {
                PwGuard::Expr(e) => {
                    let new_e = f(ast, e);
                    if new_e != e {
                        changed = true;
                    }
                    PwGuard::Expr(new_e)
                }
                PwGuard::Wildcard => PwGuard::Wildcard,
            };
            if new_value != arm.value {
                changed = true;
            }
            PiecewiseArm {
                value: new_value,
                guard: new_guard,
            }
        })
        .collect();
    if !changed {
        return expr_id;
    }
    ast.exprs
        .alloc(Spanned::new(ExprKind::Piecewise(new_arms), span))
}

fn map_handle(
    ast: &mut AstData,
    expr_id: ExprId,
    effect: TyRef,
    handlers: &[FnDecl],
    body: ExprId,
    span: Span,
    f: &mut impl FnMut(&mut AstData, ExprId) -> ExprId,
) -> ExprId {
    let new_body = f(ast, body);
    let (new_handlers, handlers_changed) = map_fn_decls(ast, handlers, f);
    if new_body == body && !handlers_changed {
        return expr_id;
    }
    ast.exprs.alloc(Spanned::new(
        ExprKind::Handle {
            effect,
            handlers: new_handlers,
            body: new_body,
        },
        span,
    ))
}

fn map_rec_def(
    ast: &mut AstData,
    expr_id: ExprId,
    fields: &[RecordDefField],
    span: Span,
    f: &mut impl FnMut(&mut AstData, ExprId) -> ExprId,
) -> ExprId {
    let mut changed = false;
    let new_fields: Vec<RecordDefField> = fields
        .iter()
        .map(|field| {
            field.default.map_or_else(
                || field.clone(),
                |default| {
                    let new_default = f(ast, default);
                    if new_default != default {
                        changed = true;
                    }
                    RecordDefField {
                        name: field.name,
                        ty: field.ty,
                        default: Some(new_default),
                    }
                },
            )
        })
        .collect();
    if !changed {
        return expr_id;
    }
    ast.exprs
        .alloc(Spanned::new(ExprKind::RecordDef(new_fields), span))
}

fn map_fn_decls(
    ast: &mut AstData,
    decls: &[FnDecl],
    f: &mut impl FnMut(&mut AstData, ExprId) -> ExprId,
) -> (Vec<FnDecl>, bool) {
    let mut changed = false;
    let new_decls: Vec<FnDecl> = decls
        .iter()
        .map(|decl| {
            let new_body = decl.body.map(|b| {
                let new_b = f(ast, b);
                if new_b != b {
                    changed = true;
                }
                new_b
            });
            let new_params = decl.params.as_ref().map(|params| {
                let (new_p, p_changed) = map_params(ast, params, f);
                if p_changed {
                    changed = true;
                }
                new_p
            });
            FnDecl {
                attrs: decl.attrs.clone(),
                name: decl.name.clone(),
                params: new_params,
                ret_ty: decl.ret_ty,
                body: new_body,
            }
        })
        .collect();
    (new_decls, changed)
}

fn map_member_decls(
    ast: &mut AstData,
    members: &[MemberDecl],
    f: &mut impl FnMut(&mut AstData, ExprId) -> ExprId,
) -> (Vec<MemberDecl>, bool) {
    let mut changed = false;
    let new_members: Vec<MemberDecl> = members
        .iter()
        .map(|member| match member {
            MemberDecl::Fn(decl) => {
                let new_body = decl.body.map(|b| {
                    let new_b = f(ast, b);
                    if new_b != b {
                        changed = true;
                    }
                    new_b
                });
                let new_params = decl.params.as_ref().map(|params| {
                    let (new_p, p_changed) = map_params(ast, params, f);
                    if p_changed {
                        changed = true;
                    }
                    new_p
                });
                MemberDecl::Fn(FnDecl {
                    attrs: decl.attrs.clone(),
                    name: decl.name.clone(),
                    params: new_params,
                    ret_ty: decl.ret_ty,
                    body: new_body,
                })
            }
            MemberDecl::Law(law) => {
                let new_body = f(ast, law.body);
                if new_body != law.body {
                    changed = true;
                }
                let new_params = law.params.as_ref().map(|params| {
                    let (new_p, p_changed) = map_params(ast, params, f);
                    if p_changed {
                        changed = true;
                    }
                    new_p
                });
                MemberDecl::Law(LawDecl {
                    name: law.name,
                    params: new_params,
                    body: new_body,
                })
            }
        })
        .collect();
    (new_members, changed)
}

fn map_member_node(
    ast: &mut AstData,
    expr_id: ExprId,
    members: &[MemberDecl],
    wrap: impl FnOnce(Vec<MemberDecl>) -> ExprKind,
    span: Span,
    f: &mut impl FnMut(&mut AstData, ExprId) -> ExprId,
) -> ExprId {
    let (new_members, changed) = map_member_decls(ast, members, f);
    if !changed {
        return expr_id;
    }
    ast.exprs.alloc(Spanned::new(wrap(new_members), span))
}

fn map_inst(
    ast: &mut AstData,
    expr_id: ExprId,
    inst: &InstanceDef,
    span: Span,
    f: &mut impl FnMut(&mut AstData, ExprId) -> ExprId,
) -> ExprId {
    match inst.body {
        InstanceBody::Methods(ref members) => {
            let (new_members, changed) = map_member_decls(ast, members, f);
            if !changed {
                return expr_id;
            }
            let mut new_inst = inst.clone();
            new_inst.body = InstanceBody::Methods(new_members);
            ast.exprs.alloc(Spanned::new(
                ExprKind::InstanceDef(Box::new(new_inst)),
                span,
            ))
        }
        InstanceBody::Via(_) => expr_id,
    }
}

#[cfg(test)]
#[allow(clippy::unwrap_used, clippy::panic)]
mod tests;
