use std::collections::BTreeSet;

use music_arena::SliceRange;
use music_hir::{
    HirAttr, HirAttrArg, HirExprId, HirExprKind, HirLitKind, HirOrigin, HirPatKind, HirTyId,
    HirTyKind,
};

use super::{CheckPass, DiagKind, PassBase};

pub(super) fn extract_data_layout_hints(
    ctx: &mut PassBase<'_, '_, '_>,
    origin: HirOrigin,
    attr_ranges: &[SliceRange<HirAttr>],
) -> (Option<Box<str>>, Option<u32>, Option<u32>) {
    ctx.extract_data_layout_hints(origin, attr_ranges)
}

impl PassBase<'_, '_, '_> {
    fn extract_data_layout_hints(
        &mut self,
        origin: HirOrigin,
        attr_ranges: &[SliceRange<HirAttr>],
    ) -> (Option<Box<str>>, Option<u32>, Option<u32>) {
        let mut repr_kind: Option<Box<str>> = None;
        let mut align: Option<u32> = None;
        let mut pack: Option<u32> = None;
        for range in attr_ranges {
            for attr in self.attrs(range.clone()) {
                let path = self.attr_path_base(&attr);
                match path.as_slice() {
                    ["repr"] => {
                        if repr_kind.is_some() {
                            self.diag(origin.span, DiagKind::AttrDuplicateRepr, "");
                            continue;
                        }
                        repr_kind = self.parse_named_string_arg(&attr, "kind");
                        if repr_kind.is_none() {
                            self.diag(origin.span, DiagKind::AttrReprRequiresKindString, "");
                        }
                    }
                    ["layout"] => {
                        for arg in self.attr_args(attr.args.clone()) {
                            let Some(name) = arg.name else {
                                self.diag(origin.span, DiagKind::AttrLayoutArgRequiresName, "");
                                continue;
                            };
                            let key = self.resolve_symbol(name.name);
                            match key {
                                "align" => {
                                    if align.is_some() {
                                        self.diag(
                                            origin.span,
                                            DiagKind::AttrDuplicateLayoutAlign,
                                            "",
                                        );
                                        continue;
                                    }
                                    align = self.parse_u32_value(arg.value);
                                    if align.is_none() {
                                        self.diag(
                                            origin.span,
                                            DiagKind::AttrLayoutAlignRequiresU32,
                                            "",
                                        );
                                    }
                                }
                                "pack" => {
                                    if pack.is_some() {
                                        self.diag(
                                            origin.span,
                                            DiagKind::AttrDuplicateLayoutPack,
                                            "",
                                        );
                                        continue;
                                    }
                                    pack = self.parse_u32_value(arg.value);
                                    if pack.is_none() {
                                        self.diag(
                                            origin.span,
                                            DiagKind::AttrLayoutPackRequiresU32,
                                            "",
                                        );
                                    }
                                }
                                _ => self.diag(origin.span, DiagKind::AttrUnknownArg, ""),
                            }
                        }
                    }
                    _ => {}
                }
            }
        }
        (repr_kind, align, pack)
    }
}

impl CheckPass<'_, '_, '_> {
    fn in_foundation_module(&self) -> bool {
        self.module_key().as_str().starts_with("musi:")
    }

    fn is_known_lang_name(name: &str) -> bool {
        matches!(
            name,
            "Type"
                | "Array"
                | "Any"
                | "Unknown"
                | "Syntax"
                | "Empty"
                | "Unit"
                | "Bool"
                | "Nat"
                | "Int"
                | "Float"
                | "String"
                | "Range"
                | "ClosedRange"
                | "PartialRangeFrom"
                | "PartialRangeUpTo"
                | "PartialRangeThru"
                | "CString"
                | "CPtr"
        )
    }

    fn validate_musi_lang_attr(&mut self, attr: &HirAttr, origin: HirOrigin, inner: HirExprId) {
        if !self.in_foundation_module() {
            self.diag(
                origin.span,
                DiagKind::AttrMusiLangRequiresFoundationModule,
                "",
            );
        }
        if !self.in_module_stmt() {
            self.diag(origin.span, DiagKind::AttrMusiLangRequiresPlainBindLet, "");
            return;
        }
        if let HirExprKind::Let {
            pat,
            has_param_clause,
            ..
        } = self.expr(inner).kind
        {
            if has_param_clause {
                self.diag(origin.span, DiagKind::AttrMusiLangRequiresPlainBindLet, "");
                return;
            }
            if !matches!(self.pat(pat).kind, HirPatKind::Bind { .. }) {
                self.diag(origin.span, DiagKind::AttrMusiLangRequiresPlainBindLet, "");
                return;
            }
        } else {
            self.diag(origin.span, DiagKind::AttrMusiLangRequiresPlainBindLet, "");
            return;
        }
        if self.expr(inner).mods.export.is_none() {
            self.diag(origin.span, DiagKind::AttrMusiLangRequiresExport, "");
        }
        let name = self.parse_named_string_arg(attr, "name");
        match name.as_deref() {
            None => self.diag(origin.span, DiagKind::AttrMusiLangRequiresNameString, ""),
            Some(name) if !Self::is_known_lang_name(name) => {
                self.diag(origin.span, DiagKind::AttrMusiLangUnknownName, "");
            }
            Some(_) => {}
        }
    }

    fn validate_musi_intrinsic_attr(&mut self, attr: &HirAttr, origin: HirOrigin) {
        if !self.in_foundation_module() {
            self.diag(
                origin.span,
                DiagKind::AttrMusiIntrinsicRequiresFoundationModule,
                "",
            );
        }
        let opcode = self.parse_named_string_arg(attr, "opcode");
        if opcode.is_none() {
            self.diag(
                origin.span,
                DiagKind::AttrMusiIntrinsicRequiresOpcodeString,
                "",
            );
        }
    }

    pub fn validate_expr_attrs(
        &mut self,
        origin: HirOrigin,
        attrs: SliceRange<HirAttr>,
        inner: HirExprId,
    ) {
        let inner_expr = self.expr(inner);
        let inner_kind = inner_expr.kind;
        let inner_is_foreign = inner_expr.mods.foreign.is_some();
        for attr in self.attrs(attrs) {
            let path = self.attr_path(&attr);
            match path.as_slice() {
                ["link" | "when"] => {
                    if !inner_is_foreign {
                        self.diag(origin.span, DiagKind::AttrLinkRequiresForeignLet, "");
                    }
                }
                ["repr" | "layout"] => {
                    let ok = match &inner_kind {
                        HirExprKind::Data { .. } => true,
                        HirExprKind::Let { value, .. } => {
                            matches!(self.expr(*value).kind, HirExprKind::Data { .. })
                        }
                        _ => false,
                    };
                    if !ok {
                        self.diag(origin.span, DiagKind::AttrDataLayoutRequiresDataTarget, "");
                    }
                }
                ["musi", "lang"] => self.validate_musi_lang_attr(&attr, origin, inner),
                _ => {}
            }
        }
    }

    pub fn validate_foreign_let(&mut self, expr: HirExprId, abi: &str) {
        let _ = abi;
        let origin = self.expr(expr).origin;
        let HirExprKind::Let { params, sig, .. } = self.expr(expr).kind else {
            self.diag(origin.span, DiagKind::AttrForeignRequiresForeignLet, "");
            return;
        };
        for param in self.params(params) {
            if let Some(expr) = param.ty {
                let origin = self.expr(expr).origin;
                let ty = self.lower_type_expr(expr, origin);
                self.validate_ffi_type(expr, ty);
            }
        }
        if let Some(sig) = sig {
            let origin = self.expr(sig).origin;
            let ty = self.lower_type_expr(sig, origin);
            self.validate_ffi_type(sig, ty);
        } else {
            let span = self.expr(expr).origin.span;
            self.diag(span, DiagKind::ForeignSignatureRequired, "");
        }
        for attr in self.attrs(self.expr(expr).mods.attrs) {
            let path = self.attr_path(&attr);
            match path.as_slice() {
                ["link"] => self.validate_link_attr(&attr, self.expr(expr).origin),
                ["when"] => self.validate_when_attr(&attr, self.expr(expr).origin),
                ["musi", "intrinsic"] => {
                    self.validate_musi_intrinsic_attr(&attr, self.expr(expr).origin);
                }
                _ => {}
            }
        }
    }

    fn validate_ffi_type(&mut self, expr: HirExprId, ty: HirTyId) {
        let valid = match self.ty(ty).kind {
            HirTyKind::Int
            | HirTyKind::Float
            | HirTyKind::Bool
            | HirTyKind::Unit
            | HirTyKind::CString
            | HirTyKind::CPtr
            | HirTyKind::Unknown
            | HirTyKind::Error => true,
            HirTyKind::Named { name, .. } => self.data_def(self.resolve_symbol(name)).is_some(),
            _ => false,
        };
        if !valid {
            let span = self.expr(expr).origin.span;
            self.diag(span, DiagKind::InvalidFfiType, "");
        }
    }

    pub(super) fn validate_link_attr(&mut self, attr: &HirAttr, origin: HirOrigin) {
        let known = self.known();
        for arg in self.attr_args(attr.args.clone()) {
            if let Some(name) = arg.name.map(|ident| ident.name) {
                if name != known.name_key && name != self.intern("symbol") {
                    self.diag(origin.span, DiagKind::AttrUnknownArg, "");
                }
            }
            if !self.attr_value_is_string(&arg) {
                self.diag(origin.span, DiagKind::AttrLinkRequiresStringValue, "");
            }
        }
    }

    pub(super) fn validate_when_attr(&mut self, attr: &HirAttr, origin: HirOrigin) {
        let allowed = ["os", "arch", "env", "abi", "vendor", "feature"]
            .into_iter()
            .map(|name| self.intern(name))
            .collect::<BTreeSet<_>>();
        for arg in self.attr_args(attr.args.clone()) {
            if let Some(name) = arg.name.map(|ident| ident.name) {
                if !allowed.contains(&name) {
                    self.diag(origin.span, DiagKind::AttrUnknownArg, "");
                }
            }
            if !self.attr_value_is_string(&arg) && !self.attr_value_is_string_array(&arg) {
                let kind =
                    if arg.name.map(|ident| self.resolve_symbol(ident.name)) == Some("feature") {
                        DiagKind::AttrWhenRequiresStringList
                    } else {
                        DiagKind::AttrWhenRequiresStringValue
                    };
                self.diag(origin.span, kind, "");
            }
        }
        let _ = self.target();
    }
    fn attr_value_is_string(&self, arg: &HirAttrArg) -> bool {
        matches!(self.expr(arg.value).kind, HirExprKind::Lit { lit } if self.lit_is_string(lit))
    }

    fn attr_value_is_string_array(&self, arg: &HirAttrArg) -> bool {
        let HirExprKind::Array { items } = self.expr(arg.value).kind else {
            return false;
        };
        self.array_items(items)
            .iter()
            .all(|item| matches!(self.expr(item.expr).kind, HirExprKind::Lit { lit } if self.lit_is_string(lit)))
    }

    pub(super) fn attr_path<'a>(&'a self, attr: &HirAttr) -> Vec<&'a str> {
        self.idents(attr.path)
            .into_iter()
            .map(|ident| self.resolve_symbol(ident.name))
            .collect()
    }
}

pub(super) fn attr_path<'a>(ctx: &'a CheckPass<'_, '_, '_>, attr: &HirAttr) -> Vec<&'a str> {
    ctx.attr_path(attr)
}

impl PassBase<'_, '_, '_> {
    fn attr_path_base<'a>(&'a self, attr: &HirAttr) -> Vec<&'a str> {
        self.idents(attr.path)
            .into_iter()
            .map(|ident| self.resolve_symbol(ident.name))
            .collect()
    }

    fn parse_named_string_arg(&self, attr: &HirAttr, key: &str) -> Option<Box<str>> {
        for arg in self.attr_args(attr.args.clone()) {
            let Some(name) = arg.name else {
                continue;
            };
            if self.resolve_symbol(name.name) != key {
                continue;
            }
            if let HirExprKind::Lit { lit } = self.expr(arg.value).kind
                && let Some(value) = self.lit_string_value(lit)
            {
                return Some(value.into_boxed_str());
            }
        }
        None
    }

    fn parse_u32_value(&self, expr: HirExprId) -> Option<u32> {
        let HirExprKind::Lit { lit } = self.expr(expr).kind else {
            return None;
        };
        match self.lit_kind(lit) {
            HirLitKind::Int { raw } => parse_int_lit(&raw),
            HirLitKind::Rune { value } => Some(value),
            _ => None,
        }
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
