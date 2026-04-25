use music_hir::{HirAttr, HirExprId, HirExprKind, HirOrigin};

use crate::checker::{CheckPass, DiagKind};

impl CheckPass<'_, '_, '_> {
    pub(in crate::checker::attrs) fn validate_profile_attrs(
        &mut self,
        origin: HirOrigin,
        attrs: &[HirAttr],
        inner: HirExprId,
    ) {
        let mut hot = false;
        let mut cold = false;
        for attr in attrs {
            let path = self.attr_path(attr);
            if path.as_slice() != ["profile"] {
                continue;
            }
            for arg in self.attr_args(attr.args.clone()) {
                let Some(name) = arg.name else {
                    self.diag(origin.span, DiagKind::AttrHotColdConflict, "");
                    continue;
                };
                if self.resolve_symbol(name.name) != "level" {
                    self.diag_unknown_attr_argument(name);
                    continue;
                }
                match self.expr(arg.value).kind {
                    HirExprKind::Variant { tag, args } => {
                        if !self.args(args).is_empty() {
                            self.diag(origin.span, DiagKind::AttrHotColdConflict, "");
                            continue;
                        }
                        match self.resolve_symbol(tag.name) {
                            "hot" => hot = true,
                            "cold" => cold = true,
                            _ => self.diag(origin.span, DiagKind::AttrHotColdConflict, ""),
                        }
                    }
                    _ => self.diag(origin.span, DiagKind::AttrHotColdConflict, ""),
                }
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
}
