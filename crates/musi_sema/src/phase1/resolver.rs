use crate::TyParamId;
use crate::phase2::{BindCtx, resolve_ty_expr};
use crate::semantic::SemanticModel;
use crate::symbol::{SymbolKind, SymbolTable};
use crate::ty_repr::TyRepr;
use crate::unifier::Unifier;
use musi_ast::{
    AstArena, ChoiceCaseItem, ExprId, ExprKind, FnSig, Ident, Prog, StmtKind, TyExprId,
};
use musi_basic::diagnostic::DiagnosticBag;
use musi_basic::interner::Interner;

pub fn resolve(
    arena: &AstArena,
    interner: &mut Interner,
    prog: &Prog,
    symbols: &mut SymbolTable,
    model: &mut SemanticModel,
    unifier: &mut Unifier,
    diags: &mut DiagnosticBag,
) {
    let mut deferred = vec![];
    let mut ctx = BindCtx {
        arena,
        interner,
        model,
        symbols,
        unifier,
        diags,
        deferred: &mut deferred,
        in_loop: false,
        in_fn: false,
    };

    for stmt_id in &prog.stmts {
        let stmt = arena.stmts.get(*stmt_id);
        let StmtKind::Expr(expr_id) = &stmt.kind;
        resolve_expr(&mut ctx, *expr_id);
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
            if let Some(sym_id) = ctx.symbols.lookup(*name)
                && let Some(sym) = ctx.symbols.get_mut(sym_id)
            {
                sym.ty = resolved;
            }
        }
        ExprKind::Fn { sig, .. } => {
            if let Some(name) = sig.name {
                let sig_ty = resolve_fn_sig(ctx, sig);
                if let Some(sym_id) = ctx.symbols.lookup(name)
                    && let Some(sym) = ctx.symbols.get_mut(sym_id)
                {
                    sym.ty = sig_ty;
                }
            }
        }
        ExprKind::Extern { fns, .. } => {
            for sig in fns {
                if let Some(name) = sig.name {
                    let sig_ty = resolve_fn_sig(ctx, sig);
                    if let Some(sym_id) = ctx.symbols.lookup(name)
                        && let Some(sym) = ctx.symbols.get_mut(sym_id)
                    {
                        sym.ty = sig_ty;
                    }
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
        if let Some(ty_id) = param.ty {
            param_tys.push(resolve_ty_expr(ctx, ty_id));
        } else {
            param_tys.push(ctx.unifier.fresh_var());
        }
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
