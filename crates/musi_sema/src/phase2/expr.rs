use musi_ast::{
    ChoiceCase, ChoiceCaseItem, CondId, CondKind, ExprId, ExprKind, Field, FnSig, Ident, LitKind,
    MatchCase, PatId, PatKind, StmtId, TemplatePart, TyExprId,
};
use musi_basic::span::Span;
use musi_lex::token::TokenKind;

use crate::error::SemaErrorKind;
use crate::phase1::{BindCtx, DeferredTask, define_named_ty, resolve_field_ty, resolve_ty_expr};
use crate::symbol::SymbolKind;
use crate::ty_repr::{FloatWidth, IntWidth, TyParamId, TyRepr, TyReprKind};

use super::ops::{ensure_bool, fresh_var_or, try_coerce_lit};
use super::pat::{bind_pat, bind_pat_with_kind};
use super::stmt::bind_stmt;

pub fn bind_expr(ctx: &mut BindCtx<'_>, expr_id: ExprId) -> TyRepr {
    let expr = ctx.arena.exprs.get(expr_id);
    let ty = bind_expr_inner(ctx, expr_id, &expr.kind, expr.span);
    ctx.model.set_expr_type(expr_id, ty.clone());
    ty
}

fn bind_expr_inner(ctx: &mut BindCtx<'_>, expr_id: ExprId, kind: &ExprKind, span: Span) -> TyRepr {
    match kind {
        ExprKind::Lit(lit) => bind_expr_lit(ctx, lit),
        ExprKind::Ident(ident) => bind_expr_ident(ctx, *ident, expr_id, span),
        ExprKind::Tuple(elems) => bind_expr_tuple(ctx, elems),
        ExprKind::Array(elems) => bind_expr_array(ctx, elems),
        ExprKind::Block { stmts, expr: tail } => bind_expr_block(ctx, stmts, *tail),
        ExprKind::Bind {
            pat,
            ty,
            init,
            mutable,
            ..
        } => bind_expr_bind(ctx, *pat, *ty, *init, *mutable),
        ExprKind::If {
            cond,
            then_br,
            else_br,
        } => bind_expr_if(ctx, *cond, *then_br, *else_br),
        ExprKind::While { cond, body } => bind_expr_while(ctx, *cond, *body),
        ExprKind::For { pat, iter, body } => bind_expr_for(ctx, *pat, *iter, *body),
        ExprKind::Return(opt) => bind_expr_return(ctx, *opt, span),
        ExprKind::Break(opt) => bind_expr_break(ctx, *opt, span),
        ExprKind::Cycle => bind_expr_cycle(ctx, span),
        ExprKind::Call { callee, args } => bind_expr_call(ctx, *callee, args),
        ExprKind::Binary { op, lhs, rhs } => bind_expr_binary(ctx, *op, *lhs, *rhs),
        ExprKind::Unary { op, operand } => bind_expr_unary(ctx, *op, *operand, span),
        ExprKind::Assign { target, value } => bind_expr_assign(ctx, *target, *value),
        ExprKind::Field { base, field } => bind_expr_field(ctx, *base, *field, span),
        ExprKind::Index { base, index } => bind_expr_index(ctx, *base, *index, span),
        ExprKind::Record { base, fields } => bind_expr_record(ctx, *base, fields),
        ExprKind::Fn { sig, body, .. } => bind_expr_fn(ctx, sig, *body),
        ExprKind::RecordDef {
            name,
            fields,
            ty_params,
            ..
        } => bind_expr_record_def(ctx, *name, fields, ty_params, span),
        ExprKind::Unsafe(block) => bind_expr(ctx, *block),
        ExprKind::Extern { .. } => TyRepr::unit(),
        ExprKind::ChoiceDef {
            name,
            cases,
            ty_params,
            ..
        } => bind_expr_choice_def(ctx, *name, cases, ty_params, span),
        ExprKind::Alias { name, ty, .. } => bind_expr_alias(ctx, *name, *ty),
        ExprKind::Defer(inner) => {
            let _ = bind_expr(ctx, *inner);
            TyRepr::unit()
        }
        ExprKind::Match { scrutinee, cases } => bind_expr_match(ctx, *scrutinee, cases),
        ExprKind::Range {
            start,
            end,
            inclusive,
        } => bind_expr_range(ctx, *start, *end, *inclusive),
        _ => TyRepr::any(),
    }
}

fn bind_expr_lit(ctx: &mut BindCtx<'_>, lit: &LitKind) -> TyRepr {
    if let LitKind::Template(parts) = lit {
        for part in parts {
            if let TemplatePart::Expr(id) = part {
                let _ = bind_expr(ctx, *id);
            }
        }
    }
    match lit {
        LitKind::Int(n) => {
            if *n < 0 {
                TyRepr::int(IntWidth::I32)
            } else {
                TyRepr::nat(IntWidth::I32)
            }
        }
        LitKind::Real(_) => TyRepr::float(FloatWidth::F64),
        LitKind::String(_) | LitKind::Template(_) => TyRepr::string(),
        LitKind::Rune(_) => TyRepr::rune(),
        LitKind::Bool(_) => TyRepr::bool(),
    }
}

fn bind_expr_ident(ctx: &mut BindCtx<'_>, ident: Ident, expr_id: ExprId, span: Span) -> TyRepr {
    if let Some(sym_id) = ctx.symbols.lookup(ident) {
        ctx.model.set_expr_symbol(expr_id, sym_id);
        ctx.model.set_ident_symbol(ident, sym_id);
        ctx.mark_used(sym_id);
        if let Some(sym) = ctx.symbols.get(sym_id) {
            return sym.ty.clone();
        }
    }
    let name = ctx.interner.resolve(ident.id);
    ctx.error(SemaErrorKind::UndefinedIdent(name.to_owned()), span);
    TyRepr::error()
}

fn bind_expr_tuple(ctx: &mut BindCtx<'_>, elems: &[ExprId]) -> TyRepr {
    let types: Vec<_> = elems.iter().map(|e| bind_expr(ctx, *e)).collect();
    TyRepr::tuple(types)
}

fn bind_expr_array(ctx: &mut BindCtx<'_>, elems: &[ExprId]) -> TyRepr {
    if elems.is_empty() {
        return TyRepr::array(ctx.unifier.fresh_var(), None);
    }
    let first_ty = bind_expr(ctx, elems[0]);
    for elem_id in &elems[1..] {
        let elem_ty = bind_expr(ctx, *elem_id);
        let span = ctx.arena.exprs.get(*elem_id).span;
        ctx.unify_or_err(&first_ty, &elem_ty, span);
    }
    TyRepr::array(first_ty, Some(elems.len()))
}

fn bind_expr_block(ctx: &mut BindCtx<'_>, stmts: &[StmtId], tail: Option<ExprId>) -> TyRepr {
    let _ = ctx.symbols.push_scope();
    for stmt_id in stmts {
        bind_stmt(ctx, *stmt_id);
    }
    let result_ty = tail.map_or(TyRepr::unit(), |e| bind_expr(ctx, e));
    ctx.symbols.pop_scope();
    result_ty
}

fn bind_expr_bind(
    ctx: &mut BindCtx<'_>,
    pat_id: PatId,
    ty_ann: Option<TyExprId>,
    init_id: ExprId,
    mutable: bool,
) -> TyRepr {
    let init_ty = bind_expr(ctx, init_id);
    let decl_ty = ty_ann.map(|id| resolve_ty_expr(ctx, id));

    let binding_ty = match decl_ty {
        Some(decl) => {
            let expr = ctx.arena.exprs.get(init_id);
            if let ExprKind::Lit(lit) = &expr.kind
                && let Some(coerced) = try_coerce_lit(lit, &decl)
            {
                ctx.model.set_expr_type(init_id, coerced);
            } else {
                let span = expr.span;
                ctx.unify_or_err(&decl, &init_ty, span);
            }
            decl
        }
        None => init_ty,
    };

    let init_expr = ctx.arena.exprs.get(init_id);
    let kind = infer_binding_kind(ctx, pat_id, &init_expr.kind, &binding_ty);

    if let Some(kind) = kind {
        bind_pat_with_kind(ctx, pat_id, &binding_ty, mutable, kind);
    }
    TyRepr::unit()
}

fn infer_binding_kind(
    ctx: &mut BindCtx<'_>,
    pat_id: PatId,
    init_kind: &ExprKind,
    binding_ty: &TyRepr,
) -> Option<SymbolKind> {
    match init_kind {
        ExprKind::RecordDef { name, .. } | ExprKind::ChoiceDef { name, .. } => {
            if let PatKind::Ident(pat_ident) = &ctx.arena.pats.get(pat_id).kind
                && let Some(name_ident) = name
                && pat_ident.id == name_ident.id
                && let Some(sym_id) = ctx.symbols.lookup(*pat_ident)
            {
                ctx.model.set_pat_symbol(pat_id, sym_id);
                return None;
            }

            if name.is_none()
                && let PatKind::Ident(pat_ident) = &ctx.arena.pats.get(pat_id).kind
                && let TyReprKind::Named(sym_id, _) = &binding_ty.kind
                && let Some(sym) = ctx.symbols.get_mut(*sym_id)
                && ctx.interner.resolve(sym.name.id) == "<anon>"
            {
                sym.name = *pat_ident;
                ctx.model.set_pat_symbol(pat_id, *sym_id);
                return None;
            }

            Some(SymbolKind::Type)
        }
        ExprKind::Alias { name, .. } => {
            if let PatKind::Ident(pat_ident) = &ctx.arena.pats.get(pat_id).kind
                && pat_ident.id == name.id
                && let Some(sym_id) = ctx.symbols.lookup(*pat_ident)
            {
                ctx.model.set_pat_symbol(pat_id, sym_id);
                return None;
            }
            Some(SymbolKind::Type)
        }
        ExprKind::Fn { .. } => Some(SymbolKind::Fn),
        _ => {
            if matches!(binding_ty.kind, TyReprKind::Fn(..)) {
                Some(SymbolKind::Fn)
            } else {
                Some(SymbolKind::Local)
            }
        }
    }
}

fn bind_expr_if(
    ctx: &mut BindCtx<'_>,
    cond_id: CondId,
    then_id: ExprId,
    else_id: Option<ExprId>,
) -> TyRepr {
    bind_cond(ctx, cond_id);
    let then_ty = bind_expr(ctx, then_id);
    match else_id {
        Some(else_id) => {
            let else_ty = bind_expr(ctx, else_id);
            let span = ctx.arena.exprs.get(else_id).span;
            ctx.unify_or_err(&then_ty, &else_ty, span);
            then_ty
        }
        None => TyRepr::unit(),
    }
}

fn bind_cond(ctx: &mut BindCtx<'_>, cond_id: CondId) {
    let cond = ctx.arena.conds.get(cond_id);
    match &cond.kind {
        CondKind::Expr(expr_id) => {
            let cond_ty = bind_expr(ctx, *expr_id);
            let span = ctx.arena.exprs.get(*expr_id).span;
            ensure_bool(ctx, &cond_ty, span);
        }
        CondKind::Case { pat, init, extra } => {
            let init_ty = bind_expr(ctx, *init);
            bind_pat(ctx, *pat, &init_ty, false);
            for extra_id in extra {
                let ty = bind_expr(ctx, *extra_id);
                let span = ctx.arena.exprs.get(*extra_id).span;
                ensure_bool(ctx, &ty, span);
            }
        }
    }
}

fn bind_expr_while(ctx: &mut BindCtx<'_>, cond_id: CondId, body_id: ExprId) -> TyRepr {
    let _ = ctx.symbols.push_scope();
    bind_cond(ctx, cond_id);
    bind_loop_body(ctx, body_id);
    ctx.symbols.pop_scope();
    TyRepr::unit()
}

fn bind_expr_for(ctx: &mut BindCtx<'_>, pat_id: PatId, iter_id: ExprId, body_id: ExprId) -> TyRepr {
    let iter_ty = bind_expr(ctx, iter_id);
    let elem_ty = match &iter_ty.kind {
        TyReprKind::Array(elem, ..) => (**elem).clone(),
        _ => ctx.unifier.fresh_var(),
    };

    let _ = ctx.symbols.push_scope();
    bind_pat(ctx, pat_id, &elem_ty, false);
    bind_loop_body(ctx, body_id);
    ctx.symbols.pop_scope();
    TyRepr::unit()
}

fn bind_loop_body(ctx: &mut BindCtx<'_>, body_id: ExprId) {
    let prev = ctx.in_loop;
    ctx.in_loop = true;
    let _ = bind_expr(ctx, body_id);
    ctx.in_loop = prev;
}

fn bind_expr_match(ctx: &mut BindCtx<'_>, scrutinee_id: ExprId, cases: &[MatchCase]) -> TyRepr {
    let scrutinee_ty = bind_expr(ctx, scrutinee_id);
    let mut result_ty = ctx.unifier.fresh_var();

    for case in cases {
        let _ = ctx.symbols.push_scope();
        bind_pat(ctx, case.pat, &scrutinee_ty, false);
        if let Some(guard_id) = case.guard {
            let _ = bind_expr(ctx, guard_id);
        }
        let body_ty = bind_expr(ctx, case.body);
        let span = ctx.arena.exprs.get(case.body).span;
        ctx.unify_or_err(&result_ty, &body_ty, span);
        result_ty = body_ty;
        ctx.symbols.pop_scope();
    }
    result_ty
}

fn bind_expr_return(ctx: &mut BindCtx<'_>, opt: Option<ExprId>, span: Span) -> TyRepr {
    if !ctx.in_fn {
        ctx.error(SemaErrorKind::ReturnOutsideFn, span);
    }
    if let Some(id) = opt {
        let _ = bind_expr(ctx, id);
    }
    TyRepr::never()
}

fn bind_expr_break(ctx: &mut BindCtx<'_>, opt: Option<ExprId>, span: Span) -> TyRepr {
    if !ctx.in_loop {
        ctx.error(SemaErrorKind::BreakOutsideLoop, span);
    }
    if let Some(id) = opt {
        let _ = bind_expr(ctx, id);
    }
    TyRepr::never()
}

fn bind_expr_cycle(ctx: &mut BindCtx<'_>, span: Span) -> TyRepr {
    if !ctx.in_loop {
        ctx.error(SemaErrorKind::CycleOutsideLoop, span);
    }
    TyRepr::never()
}

fn bind_expr_call(ctx: &mut BindCtx<'_>, callee_id: ExprId, args: &[ExprId]) -> TyRepr {
    let (callee_ty, instantiated_ty) = bind_callee(ctx, callee_id);

    let param_tys = match &instantiated_ty.kind {
        TyReprKind::Fn(params, _) => Some(params.as_slice()),
        _ => None,
    };

    let arg_tys = coerce_call_args(ctx, args, param_tys);

    match &instantiated_ty.kind {
        TyReprKind::Fn(params, ret) => {
            check_call_args(ctx, callee_id, params, &arg_tys);
            (**ret).clone()
        }
        TyReprKind::Var(_) => {
            let ret_ty = ctx.unifier.fresh_var();
            let fn_ty = TyRepr::func(arg_tys.clone(), ret_ty.clone());
            let span = ctx.arena.exprs.get(callee_id).span;
            ctx.unify_or_err(&instantiated_ty, &fn_ty, span);
            ret_ty
        }
        TyReprKind::Any | TyReprKind::Unknown => instantiated_ty,
        _ => not_callable(ctx, &callee_ty, ctx.arena.exprs.get(callee_id).span),
    }
}

fn bind_expr_pipe(ctx: &mut BindCtx<'_>, lhs_ty: &TyRepr, rhs_id: ExprId) -> TyRepr {
    let rhs_expr = ctx.arena.exprs.get(rhs_id);
    match &rhs_expr.kind {
        ExprKind::Call { callee, args } => {
            bind_pipe_call(ctx, *callee, args, lhs_ty, rhs_expr.span)
        }
        _ => bind_pipe(ctx, rhs_id, lhs_ty, rhs_expr.span),
    }
}

fn bind_pipe_call(
    ctx: &mut BindCtx<'_>,
    callee: ExprId,
    args: &[ExprId],
    lhs_ty: &TyRepr,
    span: Span,
) -> TyRepr {
    let (callee_ty, instantiated_ty) = bind_callee(ctx, callee);

    if let TyReprKind::Fn(params, ret) = &instantiated_ty.kind {
        if params.len() != args.len() + 1 {
            ctx.error(
                SemaErrorKind::ArityMismatch {
                    expected: params.len(),
                    got: args.len() + 1,
                },
                span,
            );
            return (**ret).clone();
        }

        ctx.unify_or_err(&params[0], lhs_ty, span);

        let arg_tys: Vec<_> = args.iter().map(|a| bind_expr(ctx, *a)).collect();

        for (param, arg) in params[1..].iter().zip(arg_tys.iter()) {
            ctx.unify_or_err(param, arg, span);
        }

        (**ret).clone()
    } else {
        not_callable(ctx, &callee_ty, span)
    }
}

fn bind_pipe(ctx: &mut BindCtx<'_>, rhs_id: ExprId, lhs_ty: &TyRepr, span: Span) -> TyRepr {
    let rhs_ty = bind_expr(ctx, rhs_id);

    if let TyReprKind::Fn(params, ret) = &rhs_ty.kind {
        if params.len() == 1 {
            ctx.unify_or_err(&params[0], lhs_ty, span);
        } else {
            ctx.error(
                SemaErrorKind::ArityMismatch {
                    expected: 1,
                    got: 1,
                },
                span,
            );
        }
        (**ret).clone()
    } else {
        not_callable(ctx, &rhs_ty, span)
    }
}

fn bind_callee(ctx: &mut BindCtx<'_>, callee_id: ExprId) -> (TyRepr, TyRepr) {
    let callee_ty = bind_expr(ctx, callee_id);
    let instantiated = instantiate_poly(ctx, &callee_ty);
    (callee_ty, instantiated)
}

fn instantiate_poly(ctx: &BindCtx<'_>, ty: &TyRepr) -> TyRepr {
    let TyReprKind::Poly { params, body } = &ty.kind else {
        return ty.clone();
    };

    let mut substituted = (**body).clone();
    for param in params {
        let fresh_var = ctx.unifier.fresh_var();
        substituted = substitute_ty_param(&substituted, *param, &fresh_var);
    }
    substituted
}

fn substitute_ty_param(ty: &TyRepr, param_id: TyParamId, replacement: &TyRepr) -> TyRepr {
    match &ty.kind {
        TyReprKind::TypeParam(id) if *id == param_id => replacement.clone(),
        TyReprKind::Optional(inner) => {
            TyRepr::optional(substitute_ty_param(inner, param_id, replacement))
        }
        TyReprKind::Ptr(inner) => TyRepr::ptr(substitute_ty_param(inner, param_id, replacement)),
        TyReprKind::Array(elem, size) => {
            TyRepr::array(substitute_ty_param(elem, param_id, replacement), *size)
        }
        TyReprKind::Tuple(elems) => TyRepr::tuple(
            elems
                .iter()
                .map(|e| substitute_ty_param(e, param_id, replacement))
                .collect(),
        ),
        TyReprKind::Fn(params, ret) => TyRepr::func(
            params
                .iter()
                .map(|p| substitute_ty_param(p, param_id, replacement))
                .collect(),
            substitute_ty_param(ret, param_id, replacement),
        ),
        _ => ty.clone(),
    }
}

fn coerce_call_args(
    ctx: &mut BindCtx<'_>,
    args: &[ExprId],
    param_tys: Option<&[TyRepr]>,
) -> Vec<TyRepr> {
    args.iter()
        .enumerate()
        .map(|(i, arg_id)| {
            let arg_ty = bind_expr(ctx, *arg_id);
            let Some(params) = param_tys else {
                return arg_ty;
            };
            let Some(expected) = params.get(i) else {
                return arg_ty;
            };
            let expr = ctx.arena.exprs.get(*arg_id);
            let ExprKind::Lit(lit) = &expr.kind else {
                return arg_ty;
            };
            let Some(coerced) = try_coerce_lit(lit, expected) else {
                return arg_ty;
            };
            ctx.model.set_expr_type(*arg_id, coerced.clone());
            coerced
        })
        .collect()
}

fn check_call_args(
    ctx: &mut BindCtx<'_>,
    callee_id: ExprId,
    params: &[TyRepr],
    arg_tys: &[TyRepr],
) {
    let span = ctx.arena.exprs.get(callee_id).span;
    if params.len() != arg_tys.len() {
        ctx.error(
            SemaErrorKind::ArityMismatch {
                expected: params.len(),
                got: arg_tys.len(),
            },
            span,
        );
        return;
    }
    for (param, arg) in params.iter().zip(arg_tys.iter()) {
        ctx.unify_or_err(param, arg, span);
    }
}

fn bind_expr_binary(
    ctx: &mut BindCtx<'_>,
    op: TokenKind,
    lhs_id: ExprId,
    rhs_id: ExprId,
) -> TyRepr {
    let lhs_ty = bind_expr(ctx, lhs_id);
    if op == TokenKind::BarGt {
        return bind_expr_pipe(ctx, &lhs_ty, rhs_id);
    }
    let rhs_ty = bind_expr(ctx, rhs_id);

    if lhs_ty.is_any() || rhs_ty.is_any() {
        return TyRepr::any();
    }

    let rhs_span = ctx.arena.exprs.get(rhs_id).span;

    match op {
        TokenKind::Eq
        | TokenKind::SlashEq
        | TokenKind::Lt
        | TokenKind::LtEq
        | TokenKind::Gt
        | TokenKind::GtEq
        | TokenKind::KwIs
        | TokenKind::KwIn => TyRepr::bool(),

        TokenKind::KwAnd | TokenKind::KwOr => {
            let lhs_span = ctx.arena.exprs.get(lhs_id).span;
            ensure_bool(ctx, &lhs_ty, lhs_span);
            ensure_bool(ctx, &rhs_ty, rhs_span);
            TyRepr::bool()
        }

        TokenKind::Plus => bind_op_plus(ctx, &lhs_ty, &rhs_ty, rhs_span),

        TokenKind::Minus | TokenKind::Star | TokenKind::Slash | TokenKind::Percent => {
            bind_op_arith(ctx, lhs_ty, &rhs_ty, rhs_span)
        }

        TokenKind::KwAs => rhs_ty,

        _ => TyRepr::any(),
    }
}

fn bind_expr_unary(ctx: &mut BindCtx<'_>, op: TokenKind, operand_id: ExprId, span: Span) -> TyRepr {
    let operand_ty = bind_expr(ctx, operand_id);
    if operand_ty.is_any() {
        return TyRepr::any();
    }
    match op {
        TokenKind::KwNot => {
            ensure_bool(ctx, &operand_ty, span);
            TyRepr::bool()
        }
        TokenKind::Minus => operand_ty,
        TokenKind::At => TyRepr::ptr(operand_ty),
        TokenKind::DotCaret => match &operand_ty.kind {
            TyReprKind::Ptr(inner) => (**inner).clone(),
            TyReprKind::Any | TyReprKind::Unknown => TyRepr::any(),
            _ => {
                let inner = ctx.unifier.fresh_var();
                let ptr_ty = TyRepr::ptr(inner.clone());
                ctx.unify_or_err(&operand_ty, &ptr_ty, span);
                inner
            }
        },
        _ => TyRepr::any(),
    }
}

fn bind_expr_assign(ctx: &mut BindCtx<'_>, target_id: ExprId, value_id: ExprId) -> TyRepr {
    let target_ty = bind_expr(ctx, target_id);
    check_mutability(ctx, target_id);
    let value_ty = bind_expr(ctx, value_id);
    let span = ctx.arena.exprs.get(value_id).span;
    ctx.unify_or_err(&target_ty, &value_ty, span);
    TyRepr::unit()
}

fn bind_op_plus(ctx: &mut BindCtx<'_>, lhs_ty: &TyRepr, rhs_ty: &TyRepr, span: Span) -> TyRepr {
    let lhs_resolved = ctx.unifier.apply(lhs_ty);
    let rhs_resolved = ctx.unifier.apply(rhs_ty);

    if let TyReprKind::Array(lhs_elem, _) = &lhs_resolved.kind
        && let TyReprKind::Array(rhs_elem, _) = &rhs_resolved.kind
    {
        ctx.unify_or_err(lhs_elem.as_ref(), rhs_elem.as_ref(), span);
        TyRepr::array(lhs_elem.as_ref().clone(), None)
    } else {
        ctx.unify_or_err(lhs_ty, rhs_ty, span);
        lhs_ty.clone()
    }
}

fn bind_op_arith(ctx: &mut BindCtx<'_>, lhs_ty: TyRepr, rhs_ty: &TyRepr, span: Span) -> TyRepr {
    ctx.unify_or_err(&lhs_ty, rhs_ty, span);
    lhs_ty
}

fn bind_expr_field(ctx: &mut BindCtx<'_>, base_id: ExprId, field: Ident, span: Span) -> TyRepr {
    let base_ty = bind_expr(ctx, base_id);
    match &base_ty.kind {
        TyReprKind::Named(sym_id, _) => {
            if let Some(member_id) = ctx.symbols.lookup_member(*sym_id, field) {
                ctx.model.set_ident_symbol(field, member_id);
                return ctx
                    .symbols
                    .get(member_id)
                    .map_or_else(TyRepr::error, |sym| sym.ty.clone());
            }
            no_such_field(ctx, &base_ty, field, span)
        }
        TyReprKind::Any | TyReprKind::Unknown => TyRepr::any(),
        _ => no_such_field(ctx, &base_ty, field, span),
    }
}

fn bind_expr_index(ctx: &mut BindCtx<'_>, base_id: ExprId, index_id: ExprId, span: Span) -> TyRepr {
    let base_ty = bind_expr(ctx, base_id);
    let _ = bind_expr(ctx, index_id);
    match &base_ty.kind {
        TyReprKind::Array(elem, ..) => (**elem).clone(),
        TyReprKind::Var(_) => {
            let elem_ty = ctx.unifier.fresh_var();
            let array_ty = TyRepr::array(elem_ty.clone(), None);
            ctx.unify_or_err(&base_ty, &array_ty, span);
            elem_ty
        }
        TyReprKind::Any | TyReprKind::Unknown => base_ty,
        _ => {
            ctx.error(SemaErrorKind::NotIndexable(format!("{base_ty}")), span);
            TyRepr::error()
        }
    }
}

fn check_mutability(ctx: &mut BindCtx<'_>, target_id: ExprId) {
    let expr = ctx.arena.exprs.get(target_id);
    if let ExprKind::Ident(ident) = expr.kind
        && let Some(sym_id) = ctx.symbols.lookup(ident)
        && let Some(sym) = ctx.symbols.get(sym_id)
        && !sym.mutable
    {
        let name = ctx.interner.resolve(ident.id);
        ctx.error(
            SemaErrorKind::AssignmentToImmutable(name.to_owned()),
            expr.span,
        );
    }
}

fn bind_expr_fn(ctx: &mut BindCtx<'_>, sig: &FnSig, body_id: ExprId) -> TyRepr {
    let type_params = register_ty_params(ctx, &sig.ty_params);
    let param_tys = collect_param_types(ctx, sig);
    let ret_ty = fresh_var_or(ctx, sig.ret);

    let scope_id = ctx.symbols.push_scope();
    register_params(ctx, sig, &param_tys);
    ctx.symbols.pop_scope();

    ctx.deferred.push(DeferredTask {
        body: body_id,
        scope: scope_id,
        expected_ret: ret_ty.clone(),
    });

    let fn_ty = TyRepr::func(param_tys, ret_ty);
    if type_params.is_empty() {
        fn_ty
    } else {
        TyRepr::poly(type_params, fn_ty)
    }
}

fn bind_expr_record_def(
    ctx: &mut BindCtx<'_>,
    name: Option<Ident>,
    fields: &Vec<Field>,
    _ty_params: &Vec<Ident>,
    span: Span,
) -> TyRepr {
    let ty = define_named_ty(ctx, name, span);
    for field in fields {
        let field_ty = resolve_field_ty(ctx, field.ty);
        _ = ctx.define_and_record(
            field.name,
            SymbolKind::Field,
            field_ty,
            field.name.span,
            field.mutable,
        );
    }
    ty
}

fn bind_expr_choice_def(
    ctx: &mut BindCtx<'_>,
    name: Option<Ident>,
    cases: &[ChoiceCase],
    ty_params: &[Ident],
    span: Span,
) -> TyRepr {
    let ty = define_named_ty(ctx, name, span);
    let _ = ctx.symbols.push_scope();
    let _ = register_ty_params(ctx, ty_params);

    for case in cases {
        let variant_ty = if case.fields.is_empty() {
            ty.clone()
        } else {
            let field_tys: Vec<_> = case
                .fields
                .iter()
                .map(|item| match item {
                    ChoiceCaseItem::Field(f) => resolve_field_ty(ctx, f.ty),
                    ChoiceCaseItem::Type(node_id) => resolve_ty_expr(ctx, *node_id),
                })
                .collect();
            TyRepr::func(field_tys, ty.clone())
        };

        _ = ctx.define_and_record(
            case.name,
            SymbolKind::Variant,
            variant_ty,
            case.name.span,
            false,
        );
        bind_choice_case_fields(ctx, &case.fields);
    }

    ctx.symbols.pop_scope();
    ty
}

fn bind_expr_alias(ctx: &mut BindCtx<'_>, name: Ident, ty_expr: TyExprId) -> TyRepr {
    let ty = resolve_ty_expr(ctx, ty_expr);
    _ = ctx.define_and_record(name, SymbolKind::Type, ty.clone(), name.span, false);
    ty
}

fn bind_expr_record(ctx: &mut BindCtx<'_>, base: Option<ExprId>, fields: &[Field]) -> TyRepr {
    if let Some(base_id) = base {
        let _ = bind_expr(ctx, base_id);
    }
    for field in fields {
        if let Some(init_id) = field.init {
            let _ = bind_expr(ctx, init_id);
        }
        if let Some(ty_id) = field.ty {
            let _ = resolve_ty_expr(ctx, ty_id);
        }
    }
    TyRepr::any()
}

fn bind_expr_range(
    ctx: &mut BindCtx<'_>,
    start: ExprId,
    end: Option<ExprId>,
    inclusive: bool,
) -> TyRepr {
    let start_ty = bind_expr(ctx, start);
    if let Some(end_id) = end {
        let end_ty = bind_expr(ctx, end_id);
        let span = ctx.arena.exprs.get(end_id).span;
        ctx.unify_or_err(&start_ty, &end_ty, span);
    }
    TyRepr::range(start_ty, inclusive)
}

fn register_ty_params(ctx: &mut BindCtx<'_>, ty_params: &[Ident]) -> Vec<TyParamId> {
    ty_params
        .iter()
        .enumerate()
        .map(|(i, ident)| {
            let param_id = TyParamId::new(u32::try_from(i).expect("too many type parameters"));
            let ty = TyRepr::type_param(param_id);
            _ = ctx.define_and_record(*ident, SymbolKind::Type, ty, ident.span, false);
            param_id
        })
        .collect()
}

fn collect_param_types(ctx: &mut BindCtx<'_>, sig: &FnSig) -> Vec<TyRepr> {
    sig.params
        .iter()
        .map(|param| fresh_var_or(ctx, param.ty))
        .collect()
}

fn register_params(ctx: &mut BindCtx<'_>, sig: &FnSig, param_tys: &[TyRepr]) {
    for (param, ty) in sig.params.iter().zip(param_tys.iter()) {
        if ctx
            .define_and_record(
                param.name,
                SymbolKind::Param,
                ty.clone(),
                param.name.span,
                param.mutable,
            )
            .is_err()
        {
            let name = ctx.interner.resolve(param.name.id);
            ctx.error(
                SemaErrorKind::DuplicateDef(name.to_owned()),
                param.name.span,
            );
        }
    }
}

fn bind_choice_case_fields(ctx: &mut BindCtx<'_>, fields: &[ChoiceCaseItem]) {
    for field_item in fields {
        if let ChoiceCaseItem::Field(field) = field_item {
            let field_ty = resolve_field_ty(ctx, field.ty);
            _ = ctx.define_and_record(
                field.name,
                SymbolKind::Field,
                field_ty,
                field.name.span,
                field.mutable,
            );
        }
    }
}

fn not_callable(ctx: &mut BindCtx<'_>, ty: &TyRepr, span: Span) -> TyRepr {
    ctx.error(
        SemaErrorKind::NotCallable {
            callee: format!("{ty}"),
            ty: format!("{ty}"),
        },
        span,
    );
    TyRepr::error()
}

fn no_such_field(ctx: &mut BindCtx<'_>, base_ty: &TyRepr, field: Ident, span: Span) -> TyRepr {
    let field_name = ctx.interner.resolve(field.id);
    ctx.error(
        SemaErrorKind::NoSuchField {
            ty: format!("{base_ty}"),
            field: field_name.to_owned(),
        },
        span,
    );
    TyRepr::error()
}
