use super::ctx::BindCtx;
use crate::{SemaErrorKind, TyParamId};

use crate::symbol::SymbolKind;
use crate::ty_repr::TyRepr;
use musi_ast::{
    ChoiceCaseItem, ExprId, ExprKind, FnSig, Ident, Prog, StmtKind, TyExpr, TyExprId, TyExprKind,
};
use musi_basic::span::Span;

pub fn resolve(ctx: &mut BindCtx<'_>, prog: &Prog) {
    for stmt_id in &prog.stmts {
        let stmt = ctx.arena.stmts.get(*stmt_id);
        let StmtKind::Expr(expr_id) = &stmt.kind;
        resolve_expr(ctx, *expr_id);
    }
}

pub fn resolve_ty_expr(ctx: &mut BindCtx<'_>, ty_id: TyExprId) -> TyRepr {
    let ty_expr = ctx.arena.ty_exprs.get(ty_id);
    let resolved = resolve_ty_expr_kind(ctx, ty_expr);
    ctx.model.set_ty_expr_type(ty_id, resolved.clone());
    resolved
}

pub fn resolve_field_ty(ctx: &mut BindCtx<'_>, ty: Option<TyExprId>) -> TyRepr {
    ty.map_or_else(TyRepr::any, |ty_id| resolve_ty_expr(ctx, ty_id))
}

pub fn define_named_ty(ctx: &mut BindCtx<'_>, name: Option<Ident>, span: Span) -> TyRepr {
    let (ident, is_anon) = if let Some(id) = name {
        (id, false)
    } else {
        let id = ctx.interner.intern("<anon>");
        (Ident { id, span }, true)
    };

    match ctx.define_and_record(ident, SymbolKind::Type, TyRepr::unit(), ident.span, false) {
        Ok(sym_id) | Err(sym_id) => {
            if !is_anon {
                ctx.model.set_ident_symbol(ident, sym_id);
            }
            let named_ty = TyRepr::named(sym_id, vec![]);
            if let Some(sym) = ctx.symbols.get_mut(sym_id) {
                sym.ty = named_ty.clone();
            }
            named_ty
        }
    }
}

fn resolve_expr(ctx: &mut BindCtx<'_>, expr_id: ExprId) {
    let expr = ctx.arena.exprs.get(expr_id);
    match &expr.kind {
        ExprKind::Bind { init, .. } => resolve_expr(ctx, *init),
        ExprKind::RecordDef { fields, .. } => {
            for field in fields {
                if let Some(ty_id) = field.ty {
                    resolve_field(ctx, field.name, ty_id, field.mutable);
                }
            }
        }
        ExprKind::ChoiceDef { cases, .. } => {
            for case in cases {
                for field_item in &case.fields {
                    if let ChoiceCaseItem::Field(field) = field_item
                        && let Some(ty_id) = field.ty
                    {
                        resolve_field(ctx, field.name, ty_id, field.mutable);
                    }
                }
            }
        }
        ExprKind::Alias { name, ty, .. } => {
            let resolved = resolve_ty_expr(ctx, *ty);
            update_symbol_ty(ctx, *name, resolved);
        }
        ExprKind::Fn { sig, .. } => {
            if let Some(name) = sig.name {
                let sig_ty = resolve_fn_sig(ctx, sig);
                update_symbol_ty(ctx, name, sig_ty);
            }
        }
        ExprKind::Extern { fns, .. } => {
            for sig in fns {
                if let Some(name) = sig.name {
                    let sig_ty = resolve_fn_sig(ctx, sig);
                    update_symbol_ty(ctx, name, sig_ty);
                }
            }
        }
        _ => {}
    }
}

fn resolve_fn_sig(ctx: &mut BindCtx<'_>, sig: &FnSig) -> TyRepr {
    let _ = ctx.symbols.push_scope();

    let mut param_ids = vec![];
    for (i, ident) in sig.ty_params.iter().enumerate() {
        let id = TyParamId::new(u32::try_from(i).expect("too many type parameters"));
        let ty = TyRepr::type_param(id);
        _ = ctx
            .symbols
            .define(*ident, SymbolKind::Type, ty, ident.span, false);
        param_ids.push(id);
    }

    let mut param_tys = vec![];
    for param in &sig.params {
        let param_ty = if let Some(ty_id) = param.ty {
            resolve_ty_expr(ctx, ty_id)
        } else {
            ctx.unifier.fresh_var()
        };
        param_tys.push(param_ty.clone());
        _ = ctx.define_and_record(
            param.name,
            SymbolKind::Param,
            param_ty,
            param.name.span,
            param.mutable,
        );
    }

    let ret_ty = sig
        .ret
        .map_or_else(TyRepr::unit, |ty_id| resolve_ty_expr(ctx, ty_id));

    ctx.symbols.pop_scope();

    let fn_ty = TyRepr::func(param_tys, ret_ty);
    if param_ids.is_empty() {
        fn_ty
    } else {
        TyRepr::poly(param_ids, fn_ty)
    }
}

fn resolve_field(ctx: &mut BindCtx<'_>, name: Ident, ty_id: TyExprId, mutable: bool) {
    let field_ty = resolve_ty_expr(ctx, ty_id);
    _ = ctx.define_and_record(name, SymbolKind::Field, field_ty, name.span, mutable);
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

fn update_symbol_ty(ctx: &mut BindCtx<'_>, name: Ident, ty: TyRepr) {
    if let Some(sym_id) = ctx.symbols.lookup(name)
        && let Some(sym) = ctx.symbols.get_mut(sym_id)
    {
        sym.ty = ty;
    }
}
