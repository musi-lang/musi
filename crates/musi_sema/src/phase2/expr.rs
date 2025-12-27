use musi_ast::{
    ChoiceCase, ChoiceCaseItem, CondId, CondKind, ExprId, ExprKind, Field, Fields, FnSig, Ident,
    Idents, LitKind, MatchCase, PatId, PatKind, StmtId, StmtKind, TemplatePart, TyExpr, TyExprId,
    TyExprKind,
};
use musi_basic::span::Span;
use musi_lex::token::TokenKind;

use crate::error::SemaErrorKind;
use crate::symbol::SymbolKind;
use crate::ty_repr::{FloatWidth, IntWidth, TyRepr, TyReprKind};

use super::BindCtx;
use super::pat::{bind_pat, bind_pat_with_kind};

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
        } => bind_expr_record_def(ctx, *name, fields, ty_params),
        ExprKind::ChoiceDef {
            name,
            cases,
            ty_params,
            ..
        } => bind_expr_choice_def(ctx, *name, cases, ty_params),
        ExprKind::Alias { name, ty, .. } => bind_expr_alias(ctx, *name, *ty),
        ExprKind::Defer(inner) => {
            let _ = bind_expr(ctx, *inner);
            TyRepr::unit()
        }
        ExprKind::Match { scrutinee, cases } => bind_expr_match(ctx, *scrutinee, cases),
        ExprKind::Range { start, end, .. } => bind_expr_range(ctx, *start, *end),
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
        return TyRepr::array(ctx.unifier.fresh_var(), Some(0));
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

fn bind_stmt(ctx: &mut BindCtx<'_>, stmt_id: StmtId) {
    let stmt = ctx.arena.stmts.get(stmt_id);
    match &stmt.kind {
        StmtKind::Expr(expr_id) => {
            let _ = bind_expr(ctx, *expr_id);
        }
    }
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
            let span = ctx.arena.exprs.get(init_id).span;
            ctx.unify_or_err(&decl, &init_ty, span);
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
            ctx.unify_or_err(&cond_ty, &TyRepr::bool(), span);
        }
        CondKind::Case { pat, init, extra } => {
            let init_ty = bind_expr(ctx, *init);
            bind_pat(ctx, *pat, &init_ty, false);
            for extra_id in extra {
                let ty = bind_expr(ctx, *extra_id);
                let span = ctx.arena.exprs.get(*extra_id).span;
                ctx.unify_or_err(&ty, &TyRepr::bool(), span);
            }
        }
    }
}

fn bind_expr_while(ctx: &mut BindCtx<'_>, cond_id: CondId, body_id: ExprId) -> TyRepr {
    let _ = ctx.symbols.push_scope();
    bind_cond(ctx, cond_id);
    let prev = ctx.in_loop;
    ctx.in_loop = true;
    let _ = bind_expr(ctx, body_id);
    ctx.in_loop = prev;
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

    let prev = ctx.in_loop;
    ctx.in_loop = true;
    let _ = bind_expr(ctx, body_id);
    ctx.in_loop = prev;

    ctx.symbols.pop_scope();
    TyRepr::unit()
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
    let callee_ty = bind_expr(ctx, callee_id);
    let arg_tys: Vec<_> = args.iter().map(|a| bind_expr(ctx, *a)).collect();

    match &callee_ty.kind {
        TyReprKind::Fn(params, ret) => {
            validate_call_args(ctx, callee_id, params, &arg_tys);
            (**ret).clone()
        }
        TyReprKind::Any | TyReprKind::Unknown => callee_ty,
        _ => {
            let span = ctx.arena.exprs.get(callee_id).span;
            ctx.error(
                SemaErrorKind::NotCallable {
                    callee: format!("{callee_ty}"),
                    ty: format!("{callee_ty}"),
                },
                span,
            );
            TyRepr::error()
        }
    }
}

fn validate_call_args(
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
    let rhs_ty = bind_expr(ctx, rhs_id);

    match op {
        TokenKind::Eq
        | TokenKind::SlashEq
        | TokenKind::Lt
        | TokenKind::LtEq
        | TokenKind::Gt
        | TokenKind::GtEq => TyRepr::bool(),

        TokenKind::KwAnd | TokenKind::KwOr => {
            ctx.unify_or_err(&lhs_ty, &TyRepr::bool(), ctx.arena.exprs.get(lhs_id).span);
            ctx.unify_or_err(&rhs_ty, &TyRepr::bool(), ctx.arena.exprs.get(rhs_id).span);
            TyRepr::bool()
        }

        TokenKind::Plus
        | TokenKind::Minus
        | TokenKind::Star
        | TokenKind::Slash
        | TokenKind::Percent => {
            let span = ctx.arena.exprs.get(rhs_id).span;
            ctx.unify_or_err(&lhs_ty, &rhs_ty, span);
            lhs_ty
        }
        _ => TyRepr::any(),
    }
}

fn bind_expr_unary(ctx: &mut BindCtx<'_>, op: TokenKind, operand_id: ExprId, span: Span) -> TyRepr {
    let operand_ty = bind_expr(ctx, operand_id);
    match op {
        TokenKind::KwNot => {
            ctx.unify_or_err(&operand_ty, &TyRepr::bool(), span);
            TyRepr::bool()
        }
        TokenKind::Minus => operand_ty,
        TokenKind::At => TyRepr::ptr(operand_ty),
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

fn bind_expr_field(ctx: &mut BindCtx<'_>, base_id: ExprId, field: Ident, span: Span) -> TyRepr {
    let base_ty = bind_expr(ctx, base_id);
    if let Some(sym_id) = ctx.symbols.lookup(field) {
        ctx.model.set_ident_symbol(field, sym_id);
    }
    match &base_ty.kind {
        TyReprKind::Any | TyReprKind::Unknown => base_ty,
        _ => {
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
    }
}

fn bind_expr_index(ctx: &mut BindCtx<'_>, base_id: ExprId, index_id: ExprId, span: Span) -> TyRepr {
    let base_ty = bind_expr(ctx, base_id);
    let _ = bind_expr(ctx, index_id);
    match &base_ty.kind {
        TyReprKind::Array(elem, ..) => (**elem).clone(),
        TyReprKind::Any | TyReprKind::Unknown => base_ty,
        _ => {
            ctx.error(SemaErrorKind::NotIndexable(format!("{base_ty}")), span);
            TyRepr::error()
        }
    }
}

fn bind_expr_fn(ctx: &mut BindCtx<'_>, sig: &FnSig, body_id: ExprId) -> TyRepr {
    let param_tys = collect_param_types(ctx, sig);
    let _ = ctx.symbols.push_scope();
    register_params(ctx, sig, &param_tys);

    let prev = ctx.in_fn;
    ctx.in_fn = true;
    let body_ty = bind_expr(ctx, body_id);
    ctx.in_fn = prev;

    if let Some(ret_ty_expr) = sig.ret {
        let ret_ty = resolve_ty_expr(ctx, ret_ty_expr);
        ctx.unify_or_err(&ret_ty, &body_ty, ctx.arena.exprs.get(body_id).span);
    }

    ctx.symbols.pop_scope();
    TyRepr::func(param_tys, body_ty)
}

fn collect_param_types(ctx: &mut BindCtx<'_>, sig: &FnSig) -> Vec<TyRepr> {
    sig.params
        .iter()
        .map(|param| {
            if let Some(id) = param.ty {
                resolve_ty_expr(ctx, id)
            } else {
                ctx.unifier.fresh_var()
            }
        })
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

fn bind_expr_record_def(
    ctx: &mut BindCtx<'_>,
    name: Option<Ident>,
    fields: &Fields,
    _ty_params: &Idents,
) -> TyRepr {
    let ty = match name {
        Some(ident) => {
            let sym_id = ctx
                .define_and_record(ident, SymbolKind::Type, TyRepr::unit(), ident.span, false)
                .expect("duplicate type def");
            TyRepr::named(sym_id, vec![])
        }
        None => TyRepr::unit(),
    };
    for field in fields {
        let field_ty = match field.ty {
            Some(ty_id) => resolve_ty_expr(ctx, ty_id),
            None => TyRepr::any(),
        };
        let _ = ctx.define_and_record(
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
    _ty_params: &Idents,
) -> TyRepr {
    let ty = match name {
        Some(ident) => {
            let sym_id = ctx
                .define_and_record(ident, SymbolKind::Type, TyRepr::unit(), ident.span, false)
                .expect("duplicate type definition");
            TyRepr::named(sym_id, vec![])
        }
        None => TyRepr::unit(),
    };
    for case in cases {
        let _ = ctx.define_and_record(
            case.name,
            SymbolKind::Variant,
            TyRepr::unit(),
            case.name.span,
            false,
        );
        bind_choice_case_fields(ctx, &case.fields);
    }
    ty
}

fn bind_choice_case_fields(ctx: &mut BindCtx<'_>, fields: &[ChoiceCaseItem]) {
    for field_item in fields {
        if let ChoiceCaseItem::Field(field) = field_item {
            let field_ty = match field.ty {
                Some(ty_id) => resolve_ty_expr(ctx, ty_id),
                None => TyRepr::any(),
            };
            let _ = ctx.define_and_record(
                field.name,
                SymbolKind::Field,
                field_ty,
                field.name.span,
                field.mutable,
            );
        }
    }
}

fn bind_expr_alias(ctx: &mut BindCtx<'_>, name: Ident, ty_expr: TyExprId) -> TyRepr {
    let ty = resolve_ty_expr(ctx, ty_expr);
    let _ = ctx.define_and_record(name, SymbolKind::Type, ty.clone(), name.span, false);
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

fn bind_expr_range(ctx: &mut BindCtx<'_>, start: ExprId, end: Option<ExprId>) -> TyRepr {
    let _ = bind_expr(ctx, start);
    if let Some(end_id) = end {
        let _ = bind_expr(ctx, end_id);
    }
    TyRepr::any()
}

pub fn resolve_ty_expr(ctx: &mut BindCtx<'_>, ty_id: TyExprId) -> TyRepr {
    let ty_expr = ctx.arena.ty_exprs.get(ty_id);
    let resolved = resolve_ty_expr_inner(ctx, ty_expr);
    ctx.model.set_ty_expr_type(ty_id, resolved.clone());
    resolved
}

fn resolve_ty_expr_inner(ctx: &mut BindCtx<'_>, ty_expr: &TyExpr) -> TyRepr {
    match &ty_expr.kind {
        TyExprKind::Ident(ident) => resolve_ty_expr_ident(ctx, ty_expr, *ident),
        TyExprKind::Optional(inner) => TyRepr::optional(resolve_ty_expr(ctx, *inner)),
        TyExprKind::Ptr(inner) => TyRepr::ptr(resolve_ty_expr(ctx, *inner)),
        TyExprKind::Array { size, elem } => {
            let elem_ty = resolve_ty_expr(ctx, *elem);
            TyRepr::array(
                elem_ty,
                size.map(|s| usize::try_from(s).expect("size overflow")),
            )
        }
        TyExprKind::Tuple(elems) => {
            TyRepr::tuple(elems.iter().map(|e| resolve_ty_expr(ctx, *e)).collect())
        }
        TyExprKind::Fn { param, ret } => TyRepr::func(
            vec![resolve_ty_expr(ctx, *param)],
            resolve_ty_expr(ctx, *ret),
        ),
        TyExprKind::App { base, args } => resolve_ty_expr_app(ctx, *base, args),
    }
}

fn resolve_ty_expr_ident(ctx: &mut BindCtx<'_>, ty_expr: &TyExpr, ident: Ident) -> TyRepr {
    if let Some(sym_id) = ctx.symbols.lookup(ident)
        && let Some(sym) = ctx.symbols.get(sym_id)
        && matches!(sym.kind, SymbolKind::Builtin | SymbolKind::Type)
    {
        ctx.model.set_ty_expr_symbol(ty_expr.id, sym_id);
        ctx.model.set_ident_symbol(ident, sym_id);
        return sym.ty.clone();
    }
    let name = ctx.interner.resolve(ident.id);
    ctx.error(SemaErrorKind::UndefinedType(name.to_owned()), ident.span);
    TyRepr::error()
}

fn resolve_ty_expr_app(ctx: &mut BindCtx<'_>, base: Ident, args: &[TyExprId]) -> TyRepr {
    if let Some(sym_id) = ctx.symbols.lookup(base) {
        ctx.model.set_ident_symbol(base, sym_id);
        let arg_tys: Vec<_> = args.iter().map(|a| resolve_ty_expr(ctx, *a)).collect();
        return TyRepr::named(sym_id, arg_tys);
    }
    let name = ctx.interner.resolve(base.id);
    ctx.error(SemaErrorKind::UndefinedType(name.to_owned()), base.span);
    TyRepr::error()
}
