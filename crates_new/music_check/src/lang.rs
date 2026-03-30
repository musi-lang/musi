use music_basic::Span;
use music_hir::{
    HirAttrArgKind, HirAttrId, HirDeclMods, HirExprId, HirExprKind, HirLitKind, HirPatId,
    HirPatKind,
};
use music_names::Symbol;

use crate::SemaErrorKind;

use super::checker::Checker;

#[derive(Debug, Default, Clone)]
pub struct LangItems {
    pub option_ty: Option<Symbol>,
    pub option_span: Option<Span>,
}

impl Checker<'_> {
    pub(super) fn register_lang_items_on_let(
        &mut self,
        mods: &HirDeclMods,
        pat: HirPatId,
        value: Option<HirExprId>,
    ) {
        let Some((attr_span, item)) = self.musi_lang_item_from_attrs(&mods.attrs) else {
            return;
        };

        if item != self.state.known.lang_option {
            self.error(
                attr_span,
                SemaErrorKind::UnknownLangItem {
                    name: self.ctx.interner.resolve(item).to_owned(),
                },
            );
            return;
        }

        let Some(value) = value else {
            self.error(attr_span, SemaErrorKind::LangItemMissingValue);
            return;
        };

        if !matches!(
            self.ctx.store.exprs.get(value).kind,
            HirExprKind::Data { .. }
        ) {
            self.error(attr_span, SemaErrorKind::LangItemRequiresData);
            return;
        }

        let Some(ty_sym) = self.primary_pat_symbol(pat) else {
            self.error(attr_span, SemaErrorKind::LangItemRequiresName);
            return;
        };

        match self.state.lang.option_ty {
            None => {
                self.state.lang.option_ty = Some(ty_sym);
                self.state.lang.option_span = Some(attr_span);
            }
            Some(prev) if prev == ty_sym => {}
            Some(_) => {
                self.error(
                    attr_span,
                    SemaErrorKind::DuplicateLangItem {
                        name: self.ctx.interner.resolve(item).to_owned(),
                    },
                );
            }
        }
    }

    fn musi_lang_item_from_attrs(&mut self, attrs: &[HirAttrId]) -> Option<(Span, Symbol)> {
        for id in attrs.iter().copied() {
            let attr = self.ctx.store.attrs.get(id).clone();
            let [first, second] = attr.path.segments.as_ref() else {
                continue;
            };
            if first.name != self.state.known.musi || second.name != self.state.known.lang {
                continue;
            }

            let mut found = None::<(Span, HirExprId)>;
            for arg in &attr.args {
                match arg.kind {
                    HirAttrArgKind::Positional { value } => {
                        if found.is_some() {
                            self.error(arg.origin.span, SemaErrorKind::LangItemNameDuplicate);
                            continue;
                        }
                        found = Some((arg.origin.span, value));
                    }
                    HirAttrArgKind::Named { name, value } => {
                        if name.name != self.state.known.name_key {
                            self.error(
                                arg.origin.span,
                                SemaErrorKind::AttrUnknownArg {
                                    attr: String::from("musi.lang"),
                                    name: self.ctx.interner.resolve(name.name).to_owned(),
                                },
                            );
                            continue;
                        }
                        if found.is_some() {
                            self.error(arg.origin.span, SemaErrorKind::LangItemNameDuplicate);
                            continue;
                        }
                        found = Some((arg.origin.span, value));
                    }
                }
            }

            let Some((arg_span, value_expr)) = found else {
                self.error(attr.origin.span, SemaErrorKind::LangItemNameRequired);
                return None;
            };

            let Some(item_sym) = self.decode_lang_item_value(value_expr) else {
                self.error(arg_span, SemaErrorKind::LangItemNameRequiresString);
                return None;
            };

            return Some((attr.origin.span, item_sym));
        }
        None
    }

    fn decode_lang_item_value(&mut self, value: HirExprId) -> Option<Symbol> {
        let HirExprKind::Lit { lit } = self.ctx.store.exprs.get(value).kind.clone() else {
            return None;
        };
        let HirLitKind::String(s) = lit.kind else {
            return None;
        };
        let decoded = self.decode_string_span(s.span);
        Some(self.ctx.interner.intern(&decoded))
    }

    fn primary_pat_symbol(&self, pat: HirPatId) -> Option<Symbol> {
        let pat = self.ctx.store.pats.get(pat);
        match &pat.kind {
            HirPatKind::Bind { name, .. } => Some(name.name),
            _ => None,
        }
    }
}
