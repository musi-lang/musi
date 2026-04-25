use music_arena::SliceRange;
use music_base::diag::DiagContext;
use music_hir::{HirAttr, HirExprId, HirOrigin};

use crate::checker::{CheckPass, DiagKind, PassBase};

#[derive(Default)]
struct DataLayoutHints {
    repr_kind: Option<Box<str>>,
    align: Option<u32>,
    pack: Option<u32>,
    frozen: bool,
}

impl PassBase<'_, '_, '_> {
    pub(in crate::checker) fn extract_data_layout_hints(
        &mut self,
        origin: HirOrigin,
        attr_ranges: &[SliceRange<HirAttr>],
    ) -> (Option<Box<str>>, Option<u32>, Option<u32>, bool) {
        let mut hints = DataLayoutHints::default();
        for range in attr_ranges {
            for attr in self.attrs(range.clone()) {
                self.extract_data_layout_attr(origin, &attr, &mut hints);
            }
        }
        (hints.repr_kind, hints.align, hints.pack, hints.frozen)
    }

    fn extract_data_layout_attr(
        &mut self,
        origin: HirOrigin,
        attr: &HirAttr,
        hints: &mut DataLayoutHints,
    ) {
        let path = self.attr_path_base(attr);
        match path.as_slice() {
            ["repr"] => self.extract_repr_hint(origin, attr, hints),
            ["layout"] => self.extract_layout_hints(origin, attr, hints),
            ["frozen"] => hints.frozen = true,
            _ => {}
        }
    }

    fn extract_repr_hint(
        &mut self,
        origin: HirOrigin,
        attr: &HirAttr,
        hints: &mut DataLayoutHints,
    ) {
        if hints.repr_kind.is_some() {
            self.diag(origin.span, DiagKind::AttrDuplicateRepr, "");
            return;
        }
        hints.repr_kind = self.parse_named_string_arg(attr, "kind");
        if hints.repr_kind.is_none() {
            self.diag(origin.span, DiagKind::AttrReprRequiresKindString, "");
        }
    }

    fn extract_layout_hints(
        &mut self,
        origin: HirOrigin,
        attr: &HirAttr,
        hints: &mut DataLayoutHints,
    ) {
        for arg in self.attr_args(attr.args.clone()) {
            let Some(name) = arg.name else {
                self.diag(origin.span, DiagKind::AttrLayoutArgRequiresName, "");
                continue;
            };
            match self.resolve_symbol(name.name) {
                "align" => self.extract_layout_align_hint(origin, arg.value, hints),
                "pack" => self.extract_layout_pack_hint(origin, arg.value, hints),
                key => self.diag_with(
                    name.span,
                    DiagKind::AttrUnknownArg,
                    DiagContext::new().with("argument", key),
                ),
            }
        }
    }

    fn extract_layout_align_hint(
        &mut self,
        origin: HirOrigin,
        expr: HirExprId,
        hints: &mut DataLayoutHints,
    ) {
        if hints.align.is_some() {
            self.diag(origin.span, DiagKind::AttrDuplicateLayoutAlign, "");
            return;
        }
        hints.align = self.parse_u32_value(expr);
        if hints.align.is_none() {
            self.diag(origin.span, DiagKind::AttrLayoutAlignRequiresU32, "");
        }
    }

    fn extract_layout_pack_hint(
        &mut self,
        origin: HirOrigin,
        expr: HirExprId,
        hints: &mut DataLayoutHints,
    ) {
        if hints.pack.is_some() {
            self.diag(origin.span, DiagKind::AttrDuplicateLayoutPack, "");
            return;
        }
        hints.pack = self.parse_u32_value(expr);
        if hints.pack.is_none() {
            self.diag(origin.span, DiagKind::AttrLayoutPackRequiresU32, "");
        }
    }
}

impl CheckPass<'_, '_, '_> {
    pub(in crate::checker::attrs) fn validate_frozen_attr(
        &mut self,
        origin: HirOrigin,
        inner: HirExprId,
    ) {
        let inner_expr = self.expr(inner);
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
}
