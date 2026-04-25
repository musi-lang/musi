use music_arena::SliceRange;
use music_hir::{HirAttr, HirExprId, HirExprKind, HirOrigin};

use super::decls::expr_has_structural_target;
use super::{CheckPass, DiagKind, PassBase};

mod compiler_owned;
mod layout;
mod lifecycle;
mod native;
mod profile;
mod value;

pub(super) fn extract_data_layout_hints(
    ctx: &mut PassBase<'_, '_, '_>,
    origin: HirOrigin,
    attr_ranges: &[SliceRange<HirAttr>],
) -> (Option<Box<str>>, Option<u32>, Option<u32>, bool) {
    ctx.extract_data_layout_hints(origin, attr_ranges)
}

impl CheckPass<'_, '_, '_> {
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
        if expr.mods.native.is_some() {
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
            HirExprKind::Data { .. } | HirExprKind::Effect { .. } | HirExprKind::Shape { .. } => {
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

    pub fn validate_expr_attrs(
        &mut self,
        origin: HirOrigin,
        attrs: SliceRange<HirAttr>,
        inner: HirExprId,
    ) {
        let attrs = self.attrs(attrs);
        let inner_expr = self.expr(inner);
        let inner_is_native = inner_expr.mods.native.is_some();

        self.validate_profile_attrs(origin, &attrs, inner);

        for attr in attrs {
            let path = self.attr_path(&attr);
            match path.as_slice() {
                ["link" | "target"] if !inner_is_native => {
                    self.diag(origin.span, DiagKind::AttrLinkRequiresForeignLet, "");
                }
                ["repr" | "layout"] if !self.is_data_target(inner) => {
                    self.diag(origin.span, DiagKind::AttrDataLayoutRequiresDataTarget, "");
                }
                ["frozen"] => self.validate_frozen_attr(origin, inner),
                ["musi", "builtin"] => self.validate_builtin_attr(&attr, origin, inner),
                ["musi", "intrinsic"] => {
                    self.validate_intrinsic_attr(&attr, origin, inner_is_native);
                }
                ["lifecycle"] => self.validate_lifecycle_attr(&attr, origin),
                _ => {}
            }
        }
    }
}
