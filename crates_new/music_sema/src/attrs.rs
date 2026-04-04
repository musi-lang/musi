use std::collections::BTreeSet;

use music_arena::SliceRange;
use music_hir::{
    HirAttr, HirAttrArg, HirExprId, HirExprKind, HirForeignDecl, HirOrigin, HirTyId, HirTyKind,
};

use crate::context::CheckPass;
use crate::normalize::lower_type_expr;

pub fn validate_expr_attrs(
    ctx: &mut CheckPass<'_, '_>,
    origin: HirOrigin,
    attrs: SliceRange<HirAttr>,
    inner: HirExprId,
) {
    let inner_kind = ctx.expr(inner).kind;
    for attr in ctx.attrs(attrs) {
        let path = attr_path(ctx, &attr);
        match path.as_slice() {
            ["link" | "when"] => {
                if !matches!(inner_kind, HirExprKind::Foreign { .. }) {
                    ctx.diag(origin.span, "attr invalid target", "");
                }
            }
            ["repr" | "layout"] => {
                if !matches!(inner_kind, HirExprKind::Data { .. }) {
                    ctx.diag(origin.span, "attr invalid target", "");
                }
            }
            _ => {}
        }
    }
}

pub fn validate_foreign_decl(ctx: &mut CheckPass<'_, '_>, decl: &HirForeignDecl, abi: &str) {
    let _ = abi;
    for param in ctx.params(decl.params.clone()) {
        if let Some(expr) = param.ty {
            let origin = ctx.expr(expr).origin;
            let ty = lower_type_expr(ctx, expr, origin);
            validate_ffi_type(ctx, expr, ty);
        }
    }
    if let Some(sig) = decl.sig {
        let origin = ctx.expr(sig).origin;
        let ty = lower_type_expr(ctx, sig, origin);
        validate_ffi_type(ctx, sig, ty);
    } else {
        ctx.diag(decl.origin.span, "foreign signature required", "");
    }
    for attr in ctx.attrs(decl.attrs.clone()) {
        let path = attr_path(ctx, &attr);
        match path.as_slice() {
            ["link"] => validate_link_attr(ctx, &attr, decl.origin),
            ["when"] => validate_when_attr(ctx, &attr, decl.origin),
            _ => {}
        }
    }
}

fn validate_ffi_type(ctx: &mut CheckPass<'_, '_>, expr: HirExprId, ty: HirTyId) {
    match ctx.ty(ty).kind {
        HirTyKind::Int
        | HirTyKind::Float
        | HirTyKind::Bool
        | HirTyKind::Unit
        | HirTyKind::CString
        | HirTyKind::CPtr
        | HirTyKind::Unknown
        | HirTyKind::Error => {}
        _ => {
            let span = ctx.expr(expr).origin.span;
            ctx.diag(span, "invalid ffi type", "");
        }
    }
}

fn validate_link_attr(ctx: &mut CheckPass<'_, '_>, attr: &HirAttr, origin: HirOrigin) {
    let known = ctx.known();
    for arg in ctx.attr_args(attr.args.clone()) {
        if let Some(name) = arg.name.map(|ident| ident.name) {
            if name != known.name_key && name != ctx.intern("symbol") {
                ctx.diag(origin.span, "attr unknown arg", "");
            }
        }
        if !attr_value_is_string(ctx, &arg) {
            ctx.diag(origin.span, "attr invalid value", "");
        }
    }
}

fn validate_when_attr(ctx: &mut CheckPass<'_, '_>, attr: &HirAttr, origin: HirOrigin) {
    let allowed = ["os", "arch", "env", "abi", "vendor", "feature"]
        .into_iter()
        .map(|name| ctx.intern(name))
        .collect::<BTreeSet<_>>();
    for arg in ctx.attr_args(attr.args.clone()) {
        if let Some(name) = arg.name.map(|ident| ident.name) {
            if !allowed.contains(&name) {
                ctx.diag(origin.span, "attr unknown arg", "");
            }
        }
        if !attr_value_is_string(ctx, &arg) && !attr_value_is_string_array(ctx, &arg) {
            ctx.diag(origin.span, "attr invalid value", "");
        }
    }
    let _ = ctx.options().target.as_ref();
}

fn attr_value_is_string(ctx: &CheckPass<'_, '_>, arg: &HirAttrArg) -> bool {
    matches!(ctx.expr(arg.value).kind, HirExprKind::Lit { lit } if ctx.lit_is_string(lit))
}

fn attr_value_is_string_array(ctx: &CheckPass<'_, '_>, arg: &HirAttrArg) -> bool {
    let HirExprKind::Array { items } = ctx.expr(arg.value).kind else {
        return false;
    };
    ctx.array_items(items)
        .iter()
        .all(|item| matches!(ctx.expr(item.expr).kind, HirExprKind::Lit { lit } if ctx.lit_is_string(lit)))
}

fn attr_path<'a>(ctx: &'a CheckPass<'_, '_>, attr: &HirAttr) -> Vec<&'a str> {
    ctx.idents(attr.path)
        .into_iter()
        .map(|ident| ctx.resolve_symbol(ident.name))
        .collect()
}
