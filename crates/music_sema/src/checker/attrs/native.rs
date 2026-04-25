use std::collections::BTreeSet;

use music_base::diag::DiagContext;
use music_hir::{HirAttr, HirExprId, HirExprKind, HirOrigin, HirTyId, HirTyKind};
use music_names::Symbol;

use crate::checker::{CheckPass, DiagKind};

impl CheckPass<'_, '_, '_> {
    pub fn validate_native_let(&mut self, expr: HirExprId, abi: &str) {
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
                ["target"] => self.validate_when_attr(&attr, self.expr(expr).origin),
                ["musi", "intrinsic"] => {
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
            let ty = self.render_ty(ty);
            self.diag_with(
                span,
                DiagKind::InvalidFfiType,
                DiagContext::new().with("type", ty),
            );
        }
    }

    pub(in crate::checker) fn validate_link_attr(&mut self, attr: &HirAttr, origin: HirOrigin) {
        let known = self.known();
        self.validate_string_attr_args(
            attr,
            origin,
            &[known.name_key, known.symbol_key],
            DiagKind::AttrLinkRequiresStringValue,
        );
    }

    pub(in crate::checker) fn validate_when_attr(&mut self, attr: &HirAttr, origin: HirOrigin) {
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
}
