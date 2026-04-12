use std::collections::BTreeSet;

use music_arena::SliceRange;
use music_hir::{
    HirAttr, HirAttrArg, HirExprId, HirExprKind, HirLitKind, HirOrigin, HirPatKind, HirTyId,
    HirTyKind,
};

use super::normalize::lower_type_expr;
use super::{CheckPass, DiagKind, PassBase};

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
                        ctx.diag(origin.span, DiagKind::AttrDuplicateRepr, "");
                        continue;
                    }
                    repr_kind = parse_named_string_arg(ctx, &attr, "kind");
                    if repr_kind.is_none() {
                        ctx.diag(origin.span, DiagKind::AttrReprRequiresKindString, "");
                    }
                }
                ["layout"] => {
                    for arg in ctx.attr_args(attr.args.clone()) {
                        let Some(name) = arg.name else {
                            ctx.diag(origin.span, DiagKind::AttrLayoutArgRequiresName, "");
                            continue;
                        };
                        let key = ctx.resolve_symbol(name.name);
                        match key {
                            "align" => {
                                if align.is_some() {
                                    ctx.diag(origin.span, DiagKind::AttrDuplicateLayoutAlign, "");
                                    continue;
                                }
                                align = parse_u32_value(ctx, arg.value);
                                if align.is_none() {
                                    ctx.diag(origin.span, DiagKind::AttrLayoutAlignRequiresU32, "");
                                }
                            }
                            "pack" => {
                                if pack.is_some() {
                                    ctx.diag(origin.span, DiagKind::AttrDuplicateLayoutPack, "");
                                    continue;
                                }
                                pack = parse_u32_value(ctx, arg.value);
                                if pack.is_none() {
                                    ctx.diag(origin.span, DiagKind::AttrLayoutPackRequiresU32, "");
                                }
                            }
                            _ => ctx.diag(origin.span, DiagKind::AttrUnknownArg, ""),
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
        ctx.diag(origin.span, DiagKind::AttrMusiLangRequiresPlainBindLet, "");
        return;
    }
    if let HirExprKind::Let {
        pat,
        has_param_clause,
        ..
    } = ctx.expr(inner).kind
    {
        if has_param_clause {
            ctx.diag(origin.span, DiagKind::AttrMusiLangRequiresPlainBindLet, "");
            return;
        }
        if !matches!(ctx.pat(pat).kind, HirPatKind::Bind { .. }) {
            ctx.diag(origin.span, DiagKind::AttrMusiLangRequiresPlainBindLet, "");
            return;
        }
    } else {
        ctx.diag(origin.span, DiagKind::AttrMusiLangRequiresPlainBindLet, "");
        return;
    }
    let name = parse_named_string_arg(ctx, attr, "name");
    if name.is_none() {
        ctx.diag(origin.span, DiagKind::AttrMusiLangRequiresNameString, "");
    }
}

fn validate_musi_intrinsic_attr(
    ctx: &mut CheckPass<'_, '_, '_>,
    attr: &HirAttr,
    origin: HirOrigin,
) {
    let opcode = parse_named_string_arg(ctx, attr, "opcode");
    if opcode.is_none() {
        ctx.diag(
            origin.span,
            DiagKind::AttrMusiIntrinsicRequiresOpcodeString,
            "",
        );
    }
}

pub fn validate_expr_attrs(
    ctx: &mut CheckPass<'_, '_, '_>,
    origin: HirOrigin,
    attrs: SliceRange<HirAttr>,
    inner: HirExprId,
) {
    let inner_expr = ctx.expr(inner);
    let inner_kind = inner_expr.kind;
    let inner_is_foreign = inner_expr.mods.foreign.is_some();
    for attr in ctx.attrs(attrs) {
        let path = attr_path(ctx, &attr);
        match path.as_slice() {
            ["link" | "when"] => {
                if !inner_is_foreign {
                    ctx.diag(origin.span, DiagKind::AttrLinkRequiresForeignLet, "");
                }
            }
            ["repr" | "layout"] => {
                let ok = match &inner_kind {
                    HirExprKind::Data { .. } => true,
                    HirExprKind::Let { value, .. } => {
                        matches!(ctx.expr(*value).kind, HirExprKind::Data { .. })
                    }
                    _ => false,
                };
                if !ok {
                    ctx.diag(origin.span, DiagKind::AttrDataLayoutRequiresDataTarget, "");
                }
            }
            ["musi", "lang"] => validate_musi_lang_attr(ctx, &attr, origin, inner),
            _ => {}
        }
    }
}

pub fn validate_foreign_let(ctx: &mut CheckPass<'_, '_, '_>, expr: HirExprId, abi: &str) {
    let _ = abi;
    let origin = ctx.expr(expr).origin;
    let HirExprKind::Let { params, sig, .. } = ctx.expr(expr).kind else {
        ctx.diag(origin.span, DiagKind::AttrForeignRequiresForeignLet, "");
        return;
    };
    for param in ctx.params(params) {
        if let Some(expr) = param.ty {
            let origin = ctx.expr(expr).origin;
            let ty = lower_type_expr(ctx, expr, origin);
            validate_ffi_type(ctx, expr, ty);
        }
    }
    if let Some(sig) = sig {
        let origin = ctx.expr(sig).origin;
        let ty = lower_type_expr(ctx, sig, origin);
        validate_ffi_type(ctx, sig, ty);
    } else {
        let span = ctx.expr(expr).origin.span;
        ctx.diag(span, DiagKind::ForeignSignatureRequired, "");
    }
    for attr in ctx.attrs(ctx.expr(expr).mods.attrs) {
        let path = attr_path(ctx, &attr);
        match path.as_slice() {
            ["link"] => validate_link_attr(ctx, &attr, ctx.expr(expr).origin),
            ["when"] => validate_when_attr(ctx, &attr, ctx.expr(expr).origin),
            ["musi", "intrinsic"] => {
                validate_musi_intrinsic_attr(ctx, &attr, ctx.expr(expr).origin);
            }
            _ => {}
        }
    }
}

fn validate_ffi_type(ctx: &mut CheckPass<'_, '_, '_>, expr: HirExprId, ty: HirTyId) {
    let valid = match ctx.ty(ty).kind {
        HirTyKind::Int
        | HirTyKind::Float
        | HirTyKind::Bool
        | HirTyKind::Unit
        | HirTyKind::CString
        | HirTyKind::CPtr
        | HirTyKind::Unknown
        | HirTyKind::Error => true,
        HirTyKind::Named { name, .. } => ctx.data_def(ctx.resolve_symbol(name)).is_some(),
        _ => false,
    };
    if !valid {
        let span = ctx.expr(expr).origin.span;
        ctx.diag(span, DiagKind::InvalidFfiType, "");
    }
}

pub(super) fn validate_link_attr(
    ctx: &mut CheckPass<'_, '_, '_>,
    attr: &HirAttr,
    origin: HirOrigin,
) {
    let known = ctx.known();
    for arg in ctx.attr_args(attr.args.clone()) {
        if let Some(name) = arg.name.map(|ident| ident.name) {
            if name != known.name_key && name != ctx.intern("symbol") {
                ctx.diag(origin.span, DiagKind::AttrUnknownArg, "");
            }
        }
        if !attr_value_is_string(ctx, &arg) {
            ctx.diag(origin.span, DiagKind::AttrLinkRequiresStringValue, "");
        }
    }
}

pub(super) fn validate_when_attr(
    ctx: &mut CheckPass<'_, '_, '_>,
    attr: &HirAttr,
    origin: HirOrigin,
) {
    let allowed = ["os", "arch", "env", "abi", "vendor", "feature"]
        .into_iter()
        .map(|name| ctx.intern(name))
        .collect::<BTreeSet<_>>();
    for arg in ctx.attr_args(attr.args.clone()) {
        if let Some(name) = arg.name.map(|ident| ident.name) {
            if !allowed.contains(&name) {
                ctx.diag(origin.span, DiagKind::AttrUnknownArg, "");
            }
        }
        if !attr_value_is_string(ctx, &arg) && !attr_value_is_string_array(ctx, &arg) {
            let kind = if arg.name.map(|ident| ctx.resolve_symbol(ident.name)) == Some("feature") {
                DiagKind::AttrWhenRequiresStringList
            } else {
                DiagKind::AttrWhenRequiresStringValue
            };
            ctx.diag(origin.span, kind, "");
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

// Wrapper forms (`export`, attrs, `foreign`) live in `HirExpr.mods`, not `HirExprKind`.
