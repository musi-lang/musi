use music_base::diag::DiagContext;
use music_hir::{HirAttr, HirAttrArg, HirExprId, HirExprKind, HirOrigin};

use crate::checker::{CheckPass, DiagKind};

impl CheckPass<'_, '_, '_> {
    pub(in crate::checker::attrs) fn validate_lifecycle_attr(
        &mut self,
        attr: &HirAttr,
        origin: HirOrigin,
    ) {
        for arg in self.attr_args(attr.args.clone()) {
            let Some(name) = arg.name else {
                let argument = self.expr_subject(arg.value);
                self.diag_with(
                    origin.span,
                    DiagKind::AttrUnknownArg,
                    DiagContext::new().with("argument", argument),
                );
                continue;
            };
            match self.resolve_symbol(name.name) {
                "since" => {
                    if !self.attr_value_is_string(&arg) {
                        self.diag(origin.span, DiagKind::AttrSinceRequiresVersionString, "");
                    }
                }
                "deprecated" => self.validate_deprecated_payload(arg.value, origin),
                _ => self.diag_unknown_attr_argument(name),
            }
        }
    }

    fn validate_deprecated_payload(&mut self, value: HirExprId, origin: HirOrigin) {
        let HirExprKind::Record { items } = self.expr(value).kind else {
            self.diag(origin.span, DiagKind::AttrDeprecatedRequiresStringValue, "");
            return;
        };
        let known = self.known();
        for field in self.record_items(items) {
            if field.spread {
                self.diag(origin.span, DiagKind::AttrDeprecatedRequiresStringValue, "");
                continue;
            }
            let Some(field_name_ident) = field.name else {
                self.diag(origin.span, DiagKind::AttrDeprecatedRequiresStringValue, "");
                continue;
            };
            let field_name = field_name_ident.name;
            if ![known.message_key, known.replace_key, known.version_key].contains(&field_name) {
                self.diag_unknown_attr_argument(field_name_ident);
                continue;
            }
            let arg = HirAttrArg {
                name: Some(field_name_ident),
                value: field.value,
            };
            if !self.attr_value_is_string(&arg) {
                self.diag(origin.span, DiagKind::AttrDeprecatedRequiresStringValue, "");
            }
        }
    }
}
