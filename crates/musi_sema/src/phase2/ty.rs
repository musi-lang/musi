use musi_ast::{Ident, TyExpr, TyExprId, TyExprKind};

use crate::error::SemaErrorKind;
use crate::symbol::SymbolKind;
use crate::ty_repr::TyRepr;

use super::BindCtx;

pub fn resolve_ty_expr(ctx: &mut BindCtx<'_>, ty_id: TyExprId) -> TyRepr {
    let ty_expr = ctx.arena.ty_exprs.get(ty_id);
    let resolved = resolve_ty_expr_kind(ctx, ty_expr);
    ctx.model.set_ty_expr_type(ty_id, resolved.clone());
    resolved
}

pub fn resolve_field_ty(ctx: &mut BindCtx<'_>, ty: Option<TyExprId>) -> TyRepr {
    ty.map_or_else(TyRepr::any, |ty_id| resolve_ty_expr(ctx, ty_id))
}

pub fn define_named_ty(ctx: &mut BindCtx<'_>, name: Option<Ident>) -> TyRepr {
    name.map_or_else(TyRepr::unit, |ident| {
        let sym_id = ctx
            .define_and_record(ident, SymbolKind::Type, TyRepr::unit(), ident.span, false)
            .expect("duplicate type definition");
        TyRepr::named(sym_id, vec![])
    })
}

fn resolve_ty_expr_kind(ctx: &mut BindCtx<'_>, ty_expr: &TyExpr) -> TyRepr {
    match &ty_expr.kind {
        TyExprKind::Ident(ident) => resolve_ty_ident(ctx, ty_expr, *ident),
        TyExprKind::Optional(inner) => TyRepr::optional(resolve_ty_expr(ctx, *inner)),
        TyExprKind::Ptr(inner) => TyRepr::ptr(resolve_ty_expr(ctx, *inner)),
        TyExprKind::Array { size, elem } => resolve_ty_array(ctx, *size, *elem),
        TyExprKind::Tuple(elems) => resolve_ty_tuple(ctx, elems),
        TyExprKind::Fn { param, ret } => resolve_ty_fn(ctx, *param, *ret),
        TyExprKind::App { base, args } => resolve_ty_app(ctx, *base, args),
    }
}

fn resolve_ty_ident(ctx: &mut BindCtx<'_>, ty_expr: &TyExpr, ident: Ident) -> TyRepr {
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

fn resolve_ty_array(ctx: &mut BindCtx<'_>, size: Option<i64>, elem: TyExprId) -> TyRepr {
    let elem_ty = resolve_ty_expr(ctx, elem);
    TyRepr::array(
        elem_ty,
        size.map(|s| usize::try_from(s).expect("size overflow")),
    )
}

fn resolve_ty_tuple(ctx: &mut BindCtx<'_>, elems: &[TyExprId]) -> TyRepr {
    TyRepr::tuple(elems.iter().map(|e| resolve_ty_expr(ctx, *e)).collect())
}

fn resolve_ty_fn(ctx: &mut BindCtx<'_>, param: TyExprId, ret: TyExprId) -> TyRepr {
    TyRepr::func(vec![resolve_ty_expr(ctx, param)], resolve_ty_expr(ctx, ret))
}

fn resolve_ty_app(ctx: &mut BindCtx<'_>, base: Ident, args: &[TyExprId]) -> TyRepr {
    if let Some(sym_id) = ctx.symbols.lookup(base) {
        ctx.model.set_ident_symbol(base, sym_id);
        let arg_tys: Vec<_> = args.iter().map(|a| resolve_ty_expr(ctx, *a)).collect();
        return TyRepr::named(sym_id, arg_tys);
    }
    let name = ctx.interner.resolve(base.id);
    ctx.error(SemaErrorKind::UndefinedType(name.to_owned()), base.span);
    TyRepr::error()
}
