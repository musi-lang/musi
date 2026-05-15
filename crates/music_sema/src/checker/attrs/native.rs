use std::collections::BTreeSet;

use music_arena::SliceRange;
use music_base::diag::DiagContext;
use music_hir::{
    HirAttr, HirExprId, HirExprKind, HirOrigin, HirParam, HirPatId, HirPatKind, HirTyId, HirTyKind,
};
use music_names::{Ident, NameBindingId, Symbol};

use crate::checker::decls::import_record_export_for_expr;
use crate::checker::exprs::check_expr;
use crate::checker::surface::import_surface_ty;
use crate::checker::{CheckPass, DiagKind};

type SeenNativeTypeBindings = BTreeSet<NameBindingId>;

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
                let ty = self.lower_native_type_expr(expr, origin);
                self.validate_ffi_type(expr, ty, abi);
            }
        }
        if let Some(sig) = sig {
            let origin = self.expr(sig).origin;
            let ty = self.lower_native_type_expr(sig, origin);
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

    pub(in crate::checker) fn lower_native_params(
        &mut self,
        range: SliceRange<HirParam>,
    ) -> Box<[HirTyId]> {
        let builtins = self.builtins();
        self.params(range)
            .into_iter()
            .map(|param| {
                let ty = param.ty.map_or(builtins.unknown, |expr| {
                    let origin = self.expr(expr).origin;
                    self.lower_native_type_expr(expr, origin)
                });
                if let Some(binding) = self.binding_id_for_decl(param.name) {
                    self.insert_binding_type(binding, ty);
                }
                if let Some(default) = param.default {
                    let facts = check_expr(self, default);
                    let origin = self.expr(default).origin;
                    self.type_mismatch(origin, ty, facts.ty);
                }
                ty
            })
            .collect::<Vec<_>>()
            .into_boxed_slice()
    }

    pub(in crate::checker) fn lower_native_type_expr(
        &mut self,
        expr: HirExprId,
        origin: HirOrigin,
    ) -> HirTyId {
        self.lower_native_type_expr_seen(expr, origin, &mut BTreeSet::new())
    }

    fn lower_native_type_expr_seen(
        &mut self,
        expr: HirExprId,
        origin: HirOrigin,
        seen: &mut SeenNativeTypeBindings,
    ) -> HirTyId {
        match self.expr(expr).kind {
            HirExprKind::Name { name } => self.lower_native_name_type_expr(name, seen),
            HirExprKind::Field { base, name, .. } => self
                .lower_native_import_field_type_expr(base, name)
                .unwrap_or_else(|| self.lower_type_expr(expr, origin)),
            _ => self.lower_type_expr(expr, origin),
        }
    }

    fn lower_native_name_type_expr(
        &mut self,
        name: Ident,
        seen: &mut SeenNativeTypeBindings,
    ) -> HirTyId {
        let ty = self.named_type_for_symbol(name.name);
        let HirTyKind::Named {
            name: ty_name,
            args,
        } = self.ty(ty).kind
        else {
            return ty;
        };
        if ty_name != name.name || !self.ty_ids(args).is_empty() {
            return ty;
        }
        let Some(binding) = self.binding_id_for_use(name) else {
            return ty;
        };
        if !seen.insert(binding) {
            return self.builtins().error;
        }
        let lowered = self
            .binding_value_expr(self.root_expr_id(), binding)
            .and_then(|value| self.lower_native_alias_value_type_expr(value, seen))
            .unwrap_or(ty);
        let _ = seen.remove(&binding);
        lowered
    }

    fn lower_native_alias_value_type_expr(
        &mut self,
        expr: HirExprId,
        seen: &mut BTreeSet<NameBindingId>,
    ) -> Option<HirTyId> {
        let origin = self.expr(expr).origin;
        match self.expr(expr).kind {
            HirExprKind::Name { name } => Some(self.lower_native_name_type_expr(name, seen)),
            HirExprKind::Field { base, name, .. } => {
                self.lower_native_import_field_type_expr(base, name)
            }
            HirExprKind::Tuple { .. }
            | HirExprKind::ArrayTy { .. }
            | HirExprKind::Record { .. }
            | HirExprKind::Pi { .. }
            | HirExprKind::Apply { .. }
            | HirExprKind::Index { .. }
            | HirExprKind::Binary { .. }
            | HirExprKind::Prefix { .. } => {
                Some(self.lower_native_type_expr_seen(expr, origin, seen))
            }
            _ => None,
        }
    }

    fn lower_native_import_field_type_expr(
        &mut self,
        base: HirExprId,
        name: Ident,
    ) -> Option<HirTyId> {
        let (surface, export) = import_record_export_for_expr(self, base, name)?;
        let exported_ty = import_surface_ty(self, &surface, export.ty);
        if self.ty(exported_ty).kind == HirTyKind::Type {
            if let Some(ty) = self.builtin_type_alias_for_name(export.name.as_ref()) {
                return Some(ty);
            }
            return Some(self.named_type_for_symbol(name.name));
        }
        Some(exported_ty)
    }

    fn binding_value_expr(&self, expr_id: HirExprId, binding: NameBindingId) -> Option<HirExprId> {
        match self.expr(expr_id).kind {
            HirExprKind::Sequence { exprs } => self
                .expr_ids(exprs)
                .into_iter()
                .find_map(|expr| self.binding_value_expr(expr, binding)),
            HirExprKind::Let { pat, value, .. } => self
                .pat_binds(pat, binding)
                .then_some(value)
                .or_else(|| self.binding_value_expr(value, binding)),
            _ => None,
        }
    }

    fn pat_binds(&self, pat_id: HirPatId, binding: NameBindingId) -> bool {
        match self.pat(pat_id).kind {
            HirPatKind::Bind { name } => self.binding_id_for_decl(name) == Some(binding),
            _ => false,
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
            &[known.name_key],
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
                let kind = if matches!(arg_name, Some("feature" | "family")) {
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
