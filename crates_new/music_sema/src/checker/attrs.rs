use std::collections::BTreeSet;

use music_arena::SliceRange;
use music_hir::{
    HirAttr, HirAttrArg, HirExprId, HirExprKind, HirForeignDecl, HirLitKind, HirOrigin, HirPatKind,
    HirTyId, HirTyKind,
};

use super::{CheckPass, PassBase};
use super::normalize::lower_type_expr;

pub(super) fn extract_data_layout_hints(
    ctx: &mut PassBase<'_, '_, '_>,
    origin: HirOrigin,
    attr_ranges: &[SliceRange<HirAttr>],
) -> (Option<Box<str>>, Option<u32>, Option<u32>) {
    let mut repr_kind: Option<Box<str>> = None;
    let mut align: Option<u32> = None;
    let mut pack: Option<u32> = None;
    for range in attr_ranges {
        for attr in ctx.attrs(range.clone()) {
            let path = attr_path_base(ctx, &attr);
            match path.as_slice() {
                ["repr"] => {
                    if repr_kind.is_some() {
                        ctx.diag(origin.span, "duplicate @repr", "");
                        continue;
                    }
                    repr_kind = parse_named_string_arg(ctx, &attr, "kind");
                    if repr_kind.is_none() {
                        ctx.diag(origin.span, "attr invalid value", "");
                    }
                }
                ["layout"] => {
                    for arg in ctx.attr_args(attr.args.clone()) {
                        let Some(name) = arg.name else {
                            ctx.diag(origin.span, "attr invalid value", "");
                            continue;
                        };
                        let key = ctx.resolve_symbol(name.name);
                        match key {
                            "align" => {
                                if align.is_some() {
                                    ctx.diag(origin.span, "duplicate @layout align", "");
                                    continue;
                                }
                                align = parse_u32_value(ctx, arg.value);
                                if align.is_none() {
                                    ctx.diag(origin.span, "attr invalid value", "");
                                }
                            }
                            "pack" => {
                                if pack.is_some() {
                                    ctx.diag(origin.span, "duplicate @layout pack", "");
                                    continue;
                                }
                                pack = parse_u32_value(ctx, arg.value);
                                if pack.is_none() {
                                    ctx.diag(origin.span, "attr invalid value", "");
                                }
                            }
                            _ => ctx.diag(origin.span, "attr unknown arg", ""),
                        }
                    }
                }
                _ => {}
            }
        }
    }
    (repr_kind, align, pack)
}

fn validate_musi_lang_attr(
    ctx: &mut CheckPass<'_, '_, '_>,
    attr: &HirAttr,
    origin: HirOrigin,
    inner: HirExprId,
) {
    if !ctx.in_module_stmt() {
        ctx.diag(origin.span, "attr invalid target", "");
        return;
    }
    if let HirExprKind::Let {
        pat,
        has_param_clause,
        ..
    } = ctx.expr(inner).kind
    {
        if has_param_clause {
            ctx.diag(origin.span, "attr invalid target", "");
            return;
        }
        if !matches!(ctx.pat(pat).kind, HirPatKind::Bind { .. }) {
            ctx.diag(origin.span, "attr invalid target", "");
            return;
        }
    } else {
        ctx.diag(origin.span, "attr invalid target", "");
        return;
    }
    let name = parse_named_string_arg(ctx, attr, "name");
    if name.is_none() {
        ctx.diag(origin.span, "attr invalid value", "");
    }
}

fn validate_musi_intrinsic_attr(ctx: &mut CheckPass<'_, '_, '_>, attr: &HirAttr, origin: HirOrigin) {
    let opcode = parse_named_string_arg(ctx, attr, "opcode");
    if opcode.is_none() {
        ctx.diag(origin.span, "attr invalid value", "");
    }
}

pub fn validate_expr_attrs(
    ctx: &mut CheckPass<'_, '_, '_>,
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
                let ok = match inner_kind {
                    HirExprKind::Data { .. } => true,
                    HirExprKind::Let { value, .. } => {
                        matches!(ctx.expr(peel_value_wrappers(ctx, value)).kind, HirExprKind::Data { .. })
                    }
                    _ => false,
                };
                if !ok {
                    ctx.diag(origin.span, "attr invalid target", "");
                }
            }
            ["musi", "lang"] => validate_musi_lang_attr(ctx, &attr, origin, inner),
            _ => {}
        }
    }
}

pub fn validate_foreign_decl(ctx: &mut CheckPass<'_, '_, '_>, decl: &HirForeignDecl, abi: &str) {
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
            ["musi", "intrinsic"] => validate_musi_intrinsic_attr(ctx, &attr, decl.origin),
            _ => {}
        }
    }
}

fn validate_ffi_type(ctx: &mut CheckPass<'_, '_, '_>, expr: HirExprId, ty: HirTyId) {
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

pub(super) fn validate_link_attr(ctx: &mut CheckPass<'_, '_, '_>, attr: &HirAttr, origin: HirOrigin) {
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

pub(super) fn validate_when_attr(ctx: &mut CheckPass<'_, '_, '_>, attr: &HirAttr, origin: HirOrigin) {
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
    let _ = ctx.target();
}

fn attr_value_is_string(ctx: &CheckPass<'_, '_, '_>, arg: &HirAttrArg) -> bool {
    matches!(ctx.expr(arg.value).kind, HirExprKind::Lit { lit } if ctx.lit_is_string(lit))
}

fn attr_value_is_string_array(ctx: &CheckPass<'_, '_, '_>, arg: &HirAttrArg) -> bool {
    let HirExprKind::Array { items } = ctx.expr(arg.value).kind else {
        return false;
    };
    ctx.array_items(items)
        .iter()
        .all(|item| matches!(ctx.expr(item.expr).kind, HirExprKind::Lit { lit } if ctx.lit_is_string(lit)))
}

pub(super) fn attr_path<'a>(ctx: &'a CheckPass<'_, '_, '_>, attr: &HirAttr) -> Vec<&'a str> {
    ctx.idents(attr.path)
        .into_iter()
        .map(|ident| ctx.resolve_symbol(ident.name))
        .collect()
}

fn attr_path_base<'a>(ctx: &'a PassBase<'_, '_, '_>, attr: &HirAttr) -> Vec<&'a str> {
    ctx.idents(attr.path)
        .into_iter()
        .map(|ident| ctx.resolve_symbol(ident.name))
        .collect()
}

fn parse_named_string_arg(
    ctx: &PassBase<'_, '_, '_>,
    attr: &HirAttr,
    key: &str,
) -> Option<Box<str>> {
    for arg in ctx.attr_args(attr.args.clone()) {
        let Some(name) = arg.name else {
            continue;
        };
        if ctx.resolve_symbol(name.name) != key {
            continue;
        }
        if let HirExprKind::Lit { lit } = ctx.expr(arg.value).kind {
            if let Some(value) = ctx.lit_string_value(lit) {
                return Some(value.into_boxed_str());
            }
        }
    }
    None
}

fn parse_u32_value(ctx: &PassBase<'_, '_, '_>, expr: HirExprId) -> Option<u32> {
    let HirExprKind::Lit { lit } = ctx.expr(expr).kind else {
        return None;
    };
    match ctx.lit_kind(lit) {
        HirLitKind::Int { raw } => parse_int_lit(&raw),
        HirLitKind::Rune { value } => Some(value),
        _ => None,
    }
}

fn parse_int_lit(raw: &str) -> Option<u32> {
    let s: String = raw.chars().filter(|c| *c != '_').collect();
    let (radix, digits) = s
        .strip_prefix("0x")
        .map(|rest| (16, rest))
        .or_else(|| s.strip_prefix("0b").map(|rest| (2, rest)))
        .or_else(|| s.strip_prefix("0o").map(|rest| (8, rest)))
        .unwrap_or((10, s.as_str()));
    u32::from_str_radix(digits, radix).ok()
}

fn peel_value_wrappers(ctx: &CheckPass<'_, '_, '_>, mut expr: HirExprId) -> HirExprId {
    loop {
        match ctx.expr(expr).kind {
            HirExprKind::Attributed { expr: inner, .. } | HirExprKind::Export { expr: inner, .. } => {
                expr = inner;
            }
            _ => return expr,
        }
    }
}
