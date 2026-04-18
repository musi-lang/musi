use std::collections::BTreeSet;

use music_arena::SliceRange;
use music_builtin::{is_builtin_intrinsic_name, is_builtin_intrinsic_symbol, is_builtin_type_name};
use music_hir::{
    HirAttr, HirAttrArg, HirExprId, HirExprKind, HirOrigin, HirPatKind, HirTyId, HirTyKind,
};
use music_names::{Ident, Symbol};

use super::decls::expr_has_structural_target;
use super::{CheckPass, DiagKind, PassBase};

pub(super) fn extract_data_layout_hints(
    ctx: &mut PassBase<'_, '_, '_>,
    origin: HirOrigin,
    attr_ranges: &[SliceRange<HirAttr>],
) -> (Option<Box<str>>, Option<u32>, Option<u32>, bool) {
    ctx.extract_data_layout_hints(origin, attr_ranges)
}

impl PassBase<'_, '_, '_> {
    fn extract_data_layout_hints(
        &mut self,
        origin: HirOrigin,
        attr_ranges: &[SliceRange<HirAttr>],
    ) -> (Option<Box<str>>, Option<u32>, Option<u32>, bool) {
        let mut repr_kind: Option<Box<str>> = None;
        let mut align: Option<u32> = None;
        let mut pack: Option<u32> = None;
        let mut frozen = false;
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
                                _ => self.diag_named(
                                    name.span,
                                    DiagKind::AttrUnknownArg,
                                    format!("unknown attribute argument `{key}`"),
                                ),
                            }
                        }
                    }
                    ["frozen"] => frozen = true,
                    _ => {}
                }
            }
        }
        (repr_kind, align, pack, frozen)
    }
}

impl CheckPass<'_, '_, '_> {
    fn in_foundation_module(&self) -> bool {
        self.module_key().as_str().starts_with("musi:")
    }

    fn in_intrinsics_module(&self) -> bool {
        self.module_key().as_str() == "musi:intrinsics"
    }

    fn is_known_name(name: &str) -> bool {
        is_builtin_type_name(name)
    }

    fn validate_known_attr(&mut self, attr: &HirAttr, origin: HirOrigin, inner: HirExprId) {
        if !self.in_foundation_module() {
            self.diag(origin.span, DiagKind::AttrKnownRequiresFoundationModule, "");
        }
        if !self.in_module_stmt() {
            self.diag(origin.span, DiagKind::AttrKnownRequiresPlainBindLet, "");
            return;
        }
        let HirExprKind::Let {
            pat,
            has_param_clause,
            ..
        } = self.expr(inner).kind
        else {
            self.diag(origin.span, DiagKind::AttrKnownRequiresPlainBindLet, "");
            return;
        };
        if has_param_clause || !matches!(self.pat(pat).kind, HirPatKind::Bind { .. }) {
            self.diag(origin.span, DiagKind::AttrKnownRequiresPlainBindLet, "");
            return;
        }
        if self.expr(inner).mods.export.is_none() {
            self.diag(origin.span, DiagKind::AttrKnownRequiresExport, "");
        }
        let name = self.parse_named_string_arg(attr, "name");
        match name.as_deref() {
            None => self.diag(origin.span, DiagKind::AttrKnownRequiresNameString, ""),
            Some(name) if !Self::is_known_name(name) => {
                self.diag(origin.span, DiagKind::AttrKnownUnknownName, "");
            }
            Some(_) => {}
        }
    }

    fn validate_intrinsic_attr(&mut self, attr: &HirAttr, origin: HirOrigin, is_foreign: bool) {
        if !self.in_intrinsics_module() {
            self.diag(
                origin.span,
                DiagKind::AttrIntrinsicRequiresIntrinsicsModule,
                "",
            );
        }
        if !is_foreign {
            self.diag(origin.span, DiagKind::AttrIntrinsicRequiresForeignLet, "");
        }
        match self.parse_named_string_arg(attr, "name").as_deref() {
            None => self.diag(origin.span, DiagKind::AttrIntrinsicRequiresNameString, ""),
            Some(name) if !is_builtin_intrinsic_name(name) => {
                self.diag(origin.span, DiagKind::AttrKnownUnknownName, "");
            }
            Some(_) => {}
        }
    }

    fn is_data_target(&self, inner: HirExprId) -> bool {
        match self.expr(inner).kind {
            HirExprKind::Data { .. } => true,
            HirExprKind::Let { value, .. } => {
                matches!(self.expr(value).kind, HirExprKind::Data { .. })
            }
            _ => false,
        }
    }

    fn is_callable_target(&self, inner: HirExprId) -> bool {
        let expr = self.expr(inner);
        if expr.mods.foreign.is_some() {
            return true;
        }
        match expr.kind {
            HirExprKind::Let {
                has_param_clause,
                ref params,
                ..
            } => has_param_clause || !self.params(params.clone()).is_empty(),
            _ => false,
        }
    }

    fn is_structural_target(&self, inner: HirExprId) -> bool {
        match self.expr(inner).kind {
            HirExprKind::Data { .. } | HirExprKind::Effect { .. } | HirExprKind::Class { .. } => {
                true
            }
            HirExprKind::Let { value, .. } => expr_has_structural_target(self, value),
            _ => false,
        }
    }

    pub fn validate_export_mods(&mut self, origin: HirOrigin, inner: HirExprId) {
        if self
            .expr(inner)
            .mods
            .export
            .as_ref()
            .is_some_and(|export| export.opaque)
            && !self.is_structural_target(inner)
        {
            self.diag(
                origin.span,
                DiagKind::AttrOpaqueRequiresStructuralExport,
                "",
            );
        }
    }

    fn validate_hot_cold_attrs(&mut self, origin: HirOrigin, attrs: &[HirAttr], inner: HirExprId) {
        let mut hot = false;
        let mut cold = false;
        for attr in attrs {
            let path = self.attr_path(attr);
            if path.as_slice() == ["hot"] {
                hot = true;
            } else if path.as_slice() == ["cold"] {
                cold = true;
            }
        }
        if !(hot || cold) {
            return;
        }
        if !self.is_callable_target(inner) {
            self.diag(origin.span, DiagKind::AttrHotColdRequiresCallable, "");
        }
        if hot && cold {
            self.diag(origin.span, DiagKind::AttrHotColdConflict, "");
        }
    }

    fn validate_deprecated_attr(&mut self, attr: &HirAttr, origin: HirOrigin) {
        let known = self.known();
        self.validate_string_attr_args(
            attr,
            origin,
            &[known.message_key, known.replace_key, known.version_key],
            DiagKind::AttrDeprecatedRequiresStringValue,
        );
    }

    fn validate_string_attr_args(
        &mut self,
        attr: &HirAttr,
        origin: HirOrigin,
        allowed_names: &[Symbol],
        value_diag: DiagKind,
    ) {
        for arg in self.attr_args(attr.args.clone()) {
            if let Some(name) = arg.name
                && !allowed_names.contains(&name.name)
            {
                self.diag_unknown_attr_argument(name);
            }
            if !self.attr_value_is_string(&arg) {
                self.diag(origin.span, value_diag, "");
            }
        }
    }

    fn diag_unknown_attr_argument(&mut self, name: Ident) {
        let argument_name = self.resolve_symbol(name.name).to_owned();
        self.diag_named(
            name.span,
            DiagKind::AttrUnknownArg,
            format!("unknown attribute argument `{argument_name}`"),
        );
    }

    fn validate_since_attr(&mut self, attr: &HirAttr, origin: HirOrigin) {
        let known = self.known();
        let mut found = false;
        for arg in self.attr_args(attr.args.clone()) {
            if let Some(name) = arg.name
                && name.name != known.version_key
            {
                self.diag_unknown_attr_argument(name);
            }
            if arg.name.map(|ident| ident.name) == Some(known.version_key)
                && self.attr_value_is_string(&arg)
            {
                found = true;
            } else {
                self.diag(origin.span, DiagKind::AttrSinceRequiresVersionString, "");
            }
        }
        if !found {
            self.diag(origin.span, DiagKind::AttrSinceRequiresVersionString, "");
        }
    }

    pub fn validate_expr_attrs(
        &mut self,
        origin: HirOrigin,
        attrs: SliceRange<HirAttr>,
        inner: HirExprId,
    ) {
        let attrs = self.attrs(attrs);
        let inner_expr = self.expr(inner);
        let inner_is_foreign = inner_expr.mods.foreign.is_some();

        self.validate_hot_cold_attrs(origin, &attrs, inner);

        for attr in attrs {
            let path = self.attr_path(&attr);
            match path.as_slice() {
                ["link" | "when"] if !inner_is_foreign => {
                    self.diag(origin.span, DiagKind::AttrLinkRequiresForeignLet, "");
                }
                ["repr" | "layout"] if !self.is_data_target(inner) => {
                    self.diag(origin.span, DiagKind::AttrDataLayoutRequiresDataTarget, "");
                }
                ["frozen"] => {
                    let export = inner_expr.mods.export.as_ref();
                    let valid = self.is_data_target(inner)
                        && export.is_some()
                        && export.is_some_and(|mods| !mods.opaque);
                    if !valid {
                        self.diag(
                            origin.span,
                            DiagKind::AttrFrozenRequiresExportedNonOpaqueData,
                            "",
                        );
                    }
                }
                ["known"] => self.validate_known_attr(&attr, origin, inner),
                ["intrinsic"] => self.validate_intrinsic_attr(&attr, origin, inner_is_foreign),
                ["deprecated"] => self.validate_deprecated_attr(&attr, origin),
                ["since"] => self.validate_since_attr(&attr, origin),
                _ => {}
            }
        }
    }

    pub fn validate_foreign_let(&mut self, expr: HirExprId, abi: &str) {
        let origin = self.expr(expr).origin;
        let HirExprKind::Let { params, sig, .. } = self.expr(expr).kind else {
            self.diag(origin.span, DiagKind::AttrForeignRequiresForeignLet, "");
            return;
        };
        for param in self.params(params) {
            if let Some(expr) = param.ty {
                let origin = self.expr(expr).origin;
                let ty = self.lower_type_expr(expr, origin);
                self.validate_ffi_type(expr, ty, abi);
            }
        }
        if let Some(sig) = sig {
            let origin = self.expr(sig).origin;
            let ty = self.lower_type_expr(sig, origin);
            self.validate_ffi_type(sig, ty, abi);
        } else {
            let span = self.expr(expr).origin.span;
            self.diag(span, DiagKind::ForeignSignatureRequired, "");
        }
        for attr in self.attrs(self.expr(expr).mods.attrs) {
            let path = self.attr_path(&attr);
            match path.as_slice() {
                ["link"] => {
                    self.validate_link_attr(&attr, self.expr(expr).origin);
                    if abi == "musi" {
                        self.validate_musi_link_attr(&attr, self.expr(expr).origin);
                    }
                }
                ["when"] => self.validate_when_attr(&attr, self.expr(expr).origin),
                ["intrinsic"] => {
                    self.validate_intrinsic_attr(&attr, self.expr(expr).origin, true);
                }
                _ => {}
            }
        }
    }

    fn validate_ffi_type(&mut self, expr: HirExprId, ty: HirTyId, abi: &str) {
        if abi == "musi" {
            return;
        }
        let valid = match self.ty(ty).kind {
            HirTyKind::Int
            | HirTyKind::Int8
            | HirTyKind::Int16
            | HirTyKind::Int32
            | HirTyKind::Int64
            | HirTyKind::Nat
            | HirTyKind::Nat8
            | HirTyKind::Nat16
            | HirTyKind::Nat32
            | HirTyKind::Nat64
            | HirTyKind::Float
            | HirTyKind::Float32
            | HirTyKind::Float64
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
        self.validate_string_attr_args(
            attr,
            origin,
            &[known.name_key, known.symbol_key],
            DiagKind::AttrLinkRequiresStringValue,
        );
    }

    fn validate_musi_link_attr(&mut self, attr: &HirAttr, origin: HirOrigin) {
        if let Some(symbol) = self.parse_named_string_arg(attr, "symbol")
            && !is_builtin_intrinsic_symbol(&symbol)
        {
            self.diag(origin.span, DiagKind::AttrKnownUnknownName, "");
        }
    }

    pub(super) fn validate_when_attr(&mut self, attr: &HirAttr, origin: HirOrigin) {
        let allowed = [
            "os",
            "arch",
            "archFamily",
            "env",
            "abi",
            "vendor",
            "family",
            "feature",
            "pointerWidth",
            "endian",
            "jit",
            "jitIsa",
            "jitCallConv",
            "jitFeature",
        ]
        .into_iter()
        .map(|name| self.intern(name))
        .collect::<BTreeSet<_>>();
        for arg in self.attr_args(attr.args.clone()) {
            if let Some(name) = arg.name
                && !allowed.contains(&name.name)
            {
                self.diag_unknown_attr_argument(name);
            }
            let arg_name = arg.name.map(|ident| self.resolve_symbol(ident.name));
            let valid = if arg_name == Some("pointerWidth") {
                self.attr_value_is_string(&arg)
                    || self.attr_value_is_string_array(&arg)
                    || self.attr_value_is_int(&arg)
                    || self.attr_value_is_int_array(&arg)
            } else {
                self.attr_value_is_string(&arg) || self.attr_value_is_string_array(&arg)
            };
            if !valid {
                let kind = if matches!(arg_name, Some("feature" | "family" | "jitFeature")) {
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
        self.array_items(items).iter().all(
            |item| matches!(self.expr(item.expr).kind, HirExprKind::Lit { lit } if self.lit_is_string(lit)),
        )
    }

    fn attr_value_is_int(&self, arg: &HirAttrArg) -> bool {
        matches!(
            self.expr(arg.value).kind,
            HirExprKind::Lit { lit }
                if matches!(self.lit_kind(lit), music_hir::HirLitKind::Int { .. })
        )
    }

    fn attr_value_is_int_array(&self, arg: &HirAttrArg) -> bool {
        let HirExprKind::Array { items } = self.expr(arg.value).kind else {
            return false;
        };
        self.array_items(items).iter().all(|item| {
            matches!(
                self.expr(item.expr).kind,
                HirExprKind::Lit { lit }
                    if matches!(self.lit_kind(lit), music_hir::HirLitKind::Int { .. })
            )
        })
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
        self.attr_args(attr.args.clone()).iter().find_map(|arg| {
            let name = arg.name?;
            if self.resolve_symbol(name.name) != key {
                return None;
            }
            match self.expr(arg.value).kind {
                HirExprKind::Lit { lit } => self.lit_string_value(lit).map(Into::into),
                _ => None,
            }
        })
    }

    fn parse_u32_value(&self, expr_id: HirExprId) -> Option<u32> {
        match self.expr(expr_id).kind {
            HirExprKind::Lit { lit } => match self.lit_kind(lit) {
                music_hir::HirLitKind::Int { raw } => raw.parse().ok(),
                _ => None,
            },
            _ => None,
        }
    }
}
