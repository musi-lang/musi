use music_arena::SliceRange;
use music_base::diag::DiagContext;
use music_hir::{HirAttr, HirExprId, HirExprKind, HirOrigin};

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
            ["layout"] => self.extract_layout_hints(origin, attr, hints),
            ["frozen"] => hints.frozen = true,
            _ => {}
        }
    }

    fn extract_layout_hints(
        &mut self,
        origin: HirOrigin,
        attr: &HirAttr,
        hints: &mut DataLayoutHints,
    ) {
        let mut positional_index: usize = 0;
        for arg in self.attr_args(attr.args.clone()) {
            if let Some(name) = arg.name {
                match self.resolve_symbol(name.name) {
                    "form" => self.extract_layout_form_hint(origin, arg.value, hints),
                    "align" => self.extract_layout_align_hint(origin, arg.value, hints),
                    key => self.diag_with(
                        name.span,
                        DiagKind::AttrUnknownArg,
                        DiagContext::new().with("argument", key),
                    ),
                }
            } else {
                match positional_index {
                    0 => self.extract_layout_form_hint(origin, arg.value, hints),
                    1 => self.extract_layout_align_hint(origin, arg.value, hints),
                    _ => self.diag(origin.span, DiagKind::AttrLayoutArgRequiresName, ""),
                }
                positional_index += 1;
            }
        }
    }

    fn extract_layout_form_hint(
        &mut self,
        origin: HirOrigin,
        expr: HirExprId,
        hints: &mut DataLayoutHints,
    ) {
        let form = match self.expr(expr).kind {
            HirExprKind::Variant { tag, .. } | HirExprKind::Name { name: tag } => {
                Some(self.resolve_symbol(tag.name).to_owned())
            }
            _ => None,
        };
        let Some(form) = form else {
            self.diag(origin.span, DiagKind::AttrReprRequiresKindString, "");
            return;
        };
        match form.as_str() {
            "packed" => {
                if hints.pack.is_some() {
                    self.diag(origin.span, DiagKind::AttrDuplicateLayoutPack, "");
                } else {
                    hints.pack = Some(1);
                }
            }
            "c" | "transparent" => {
                if hints.repr_kind.is_some() {
                    self.diag(origin.span, DiagKind::AttrDuplicateRepr, "");
                } else {
                    hints.repr_kind = Some(form.into_boxed_str());
                }
            }
            _ => {
                self.diag(origin.span, DiagKind::AttrReprRequiresKindString, "");
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

    // Packing is derived from `form := .packed` (pack=1) today.
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
