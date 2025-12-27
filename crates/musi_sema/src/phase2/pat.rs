use musi_ast::{Ident, PatId, PatKind};

use crate::error::SemaErrorKind;
use crate::symbol::SymbolKind;
use crate::ty_repr::{TyRepr, TyReprKind};

use super::BindCtx;

pub fn bind_pat(ctx: &mut BindCtx<'_>, pat_id: PatId, expected: &TyRepr, mutable: bool) {
    bind_pat_with_kind(ctx, pat_id, expected, mutable, SymbolKind::Local);
}

pub fn bind_pat_with_kind(
    ctx: &mut BindCtx<'_>,
    pat_id: PatId,
    expected: &TyRepr,
    mutable: bool,
    kind: SymbolKind,
) {
    let pat = ctx.arena.pats.get(pat_id);
    ctx.model.set_pat_type(pat_id, expected.clone());

    match &pat.kind {
        PatKind::Ident(ident) => bind_pat_ident(ctx, pat_id, *ident, expected, mutable, kind),
        PatKind::Tuple(pats) => bind_pat_tuple(ctx, pats, expected, mutable, kind),
        PatKind::Choice { name, args, .. } => bind_pat_choice(ctx, pat_id, *name, args, mutable),
        _ => {}
    }
}

fn bind_pat_ident(
    ctx: &mut BindCtx<'_>,
    pat_id: PatId,
    ident: Ident,
    expected: &TyRepr,
    mutable: bool,
    kind: SymbolKind,
) {
    match ctx
        .symbols
        .define(ident, kind, expected.clone(), ident.span, mutable)
    {
        Ok(sym_id) => {
            ctx.model.set_pat_symbol(pat_id, sym_id);
            ctx.model.set_ident_symbol(ident, sym_id);
        }
        Err(_prev) => {
            let name = ctx.interner.resolve(ident.id);
            ctx.error(SemaErrorKind::DuplicateDef(name.to_owned()), ident.span);
        }
    }
}

fn bind_pat_tuple(
    ctx: &mut BindCtx<'_>,
    pats: &[PatId],
    expected: &TyRepr,
    mutable: bool,
    kind: SymbolKind,
) {
    if let TyReprKind::Tuple(elem_tys) = &expected.kind {
        for (sub_pat, elem_ty) in pats.iter().zip(elem_tys.iter()) {
            bind_pat_with_kind(ctx, *sub_pat, elem_ty, mutable, kind);
        }
    }
}

fn bind_pat_choice(
    ctx: &mut BindCtx<'_>,
    pat_id: PatId,
    name: Ident,
    args: &[PatId],
    mutable: bool,
) {
    if let Some(sym_id) = ctx.symbols.lookup(name) {
        ctx.model.set_pat_symbol(pat_id, sym_id);
        ctx.model.set_ident_symbol(name, sym_id);
    }
    for arg in args {
        bind_pat(ctx, *arg, &TyRepr::any(), mutable);
    }
}
