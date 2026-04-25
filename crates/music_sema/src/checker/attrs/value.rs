use music_base::diag::DiagContext;
use music_hir::{HirAttr, HirAttrArg, HirExprId, HirExprKind, HirLitKind};
use music_names::Ident;

use crate::checker::{CheckPass, DiagKind, PassBase};

impl CheckPass<'_, '_, '_> {
    pub(in crate::checker) fn diag_unknown_attr_argument(&mut self, name: Ident) {
        let argument_name = self.resolve_symbol(name.name).to_owned();
        self.diag_with(
            name.span,
            DiagKind::AttrUnknownArg,
            DiagContext::new().with("argument", argument_name),
        );
    }

    pub(in crate::checker::attrs) fn attr_value_is_string(&self, arg: &HirAttrArg) -> bool {
        matches!(self.expr(arg.value).kind, HirExprKind::Lit { lit } if self.lit_is_string(lit))
    }

    pub(in crate::checker::attrs) fn attr_value_is_string_array(&self, arg: &HirAttrArg) -> bool {
        let HirExprKind::Array { items } = self.expr(arg.value).kind else {
            return false;
        };
        self.array_items(items).iter().all(
            |item| matches!(self.expr(item.expr).kind, HirExprKind::Lit { lit } if self.lit_is_string(lit)),
        )
    }

    pub(in crate::checker::attrs) fn attr_value_is_int(&self, arg: &HirAttrArg) -> bool {
        matches!(
            self.expr(arg.value).kind,
            HirExprKind::Lit { lit }
                if matches!(self.lit_kind(lit), HirLitKind::Int { .. })
        )
    }

    pub(in crate::checker::attrs) fn attr_value_is_int_array(&self, arg: &HirAttrArg) -> bool {
        let HirExprKind::Array { items } = self.expr(arg.value).kind else {
            return false;
        };
        self.array_items(items).iter().all(|item| {
            matches!(
                self.expr(item.expr).kind,
                HirExprKind::Lit { lit }
                    if matches!(self.lit_kind(lit), HirLitKind::Int { .. })
            )
        })
    }

    pub(in crate::checker) fn attr_path<'a>(&'a self, attr: &HirAttr) -> Vec<&'a str> {
        self.idents(attr.path)
            .into_iter()
            .map(|ident| self.resolve_symbol(ident.name))
            .collect()
    }
}

impl PassBase<'_, '_, '_> {
    pub(in crate::checker::attrs) fn attr_path_base<'a>(&'a self, attr: &HirAttr) -> Vec<&'a str> {
        self.idents(attr.path)
            .into_iter()
            .map(|ident| self.resolve_symbol(ident.name))
            .collect()
    }

    pub(in crate::checker::attrs) fn parse_named_string_arg(
        &self,
        attr: &HirAttr,
        key: &str,
    ) -> Option<Box<str>> {
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

    pub(in crate::checker::attrs) fn parse_u32_value(&self, expr_id: HirExprId) -> Option<u32> {
        match self.expr(expr_id).kind {
            HirExprKind::Lit { lit } => match self.lit_kind(lit) {
                HirLitKind::Int { raw } => raw.parse().ok(),
                _ => None,
            },
            _ => None,
        }
    }
}
