use music_builtin::{is_builtin_intrinsic_name, is_builtin_intrinsic_symbol, is_builtin_type_name};
use music_hir::{HirAttr, HirExprId, HirExprKind, HirOrigin, HirPatKind};

use crate::checker::{CheckPass, DiagKind};

impl CheckPass<'_, '_, '_> {
    fn in_foundation_module(&self) -> bool {
        self.module_key().as_str().starts_with("musi:")
    }

    fn in_intrinsics_module(&self) -> bool {
        self.module_key().as_str() == "musi:intrinsics"
    }

    pub(in crate::checker::attrs) fn validate_builtin_attr(
        &mut self,
        attr: &HirAttr,
        origin: HirOrigin,
        inner: HirExprId,
    ) {
        if !self.in_foundation_module() {
            self.diag(
                origin.span,
                DiagKind::AttrBuiltinRequiresFoundationModule,
                "",
            );
        }
        if !self.in_module_stmt() {
            self.diag(origin.span, DiagKind::AttrBuiltinRequiresPlainBindLet, "");
            return;
        }
        let HirExprKind::Let {
            pat,
            has_param_clause,
            ..
        } = self.expr(inner).kind
        else {
            self.diag(origin.span, DiagKind::AttrBuiltinRequiresPlainBindLet, "");
            return;
        };
        if has_param_clause || !matches!(self.pat(pat).kind, HirPatKind::Bind { .. }) {
            self.diag(origin.span, DiagKind::AttrBuiltinRequiresPlainBindLet, "");
            return;
        }
        if self.expr(inner).mods.export.is_none() {
            self.diag(origin.span, DiagKind::AttrBuiltinRequiresExport, "");
        }
        match self.parse_named_string_arg(attr, "name").as_deref() {
            None => self.diag(origin.span, DiagKind::AttrBuiltinRequiresNameString, ""),
            Some(name) if !is_builtin_type_name(name) => {
                self.diag(origin.span, DiagKind::AttrBuiltinUnknownName, "");
            }
            Some(_) => {}
        }
    }

    pub(in crate::checker) fn validate_intrinsic_attr(
        &mut self,
        attr: &HirAttr,
        origin: HirOrigin,
        is_native: bool,
    ) {
        if !self.in_intrinsics_module() {
            self.diag(
                origin.span,
                DiagKind::AttrIntrinsicRequiresIntrinsicsModule,
                "",
            );
        }
        if !is_native {
            self.diag(origin.span, DiagKind::AttrIntrinsicRequiresForeignLet, "");
        }
        match self.parse_named_string_arg(attr, "name").as_deref() {
            None => self.diag(origin.span, DiagKind::AttrIntrinsicRequiresNameString, ""),
            Some(name) if !is_builtin_intrinsic_name(name) => {
                self.diag(origin.span, DiagKind::AttrIntrinsicUnknownName, "");
            }
            Some(_) => {}
        }
    }

    pub(in crate::checker::attrs) fn validate_musi_link_attr(
        &mut self,
        attr: &HirAttr,
        origin: HirOrigin,
    ) {
        if let Some(symbol) = self.parse_named_string_arg(attr, "symbol")
            && !is_builtin_intrinsic_symbol(&symbol)
        {
            self.diag(origin.span, DiagKind::AttrIntrinsicUnknownName, "");
        }
    }
}
