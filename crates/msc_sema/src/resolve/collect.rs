//! Pass 1: top-level definition collection.

use msc_ast::attr::{Attr, AttrValue};
use msc_ast::decl::ForeignDecl;
use msc_ast::expr::{Expr, LetFields};
use msc_ast::lit::Lit;
use msc_ast::{ExprIdx, Pat, PatIdx};
use msc_shared::Symbol;

use crate::def::DefKind;

use super::{Resolver, binding_def_kind};

impl Resolver<'_> {
    pub(super) fn collect_top_level(&mut self, expr_idx: ExprIdx) {
        self.collect_top_level_with_attrs(expr_idx, &[]);
    }

    fn collect_top_level_with_attrs(&mut self, expr_idx: ExprIdx, attrs: &[Attr]) {
        match &self.ast.exprs[expr_idx] {
            Expr::Binding {
                exported, fields, ..
            } => {
                self.define_fn_name(fields.pat, binding_def_kind(fields.kind));
                self.define_pat(fields.pat, binding_def_kind(fields.kind));
                if *exported {
                    self.mark_pat_exported(fields.pat);
                }
                self.maybe_mark_entrypoint(fields.pat, attrs);
                self.maybe_mark_lang_item_pat(fields.pat, attrs);
            }
            Expr::Let {
                fields, body: None, ..
            } => {
                self.collect_top_level_let(fields, attrs);
            }
            Expr::Class { name, exported, .. } => {
                self.collect_named_def(*name, DefKind::Class, *exported, expr_idx, attrs);
            }
            Expr::Effect { name, exported, .. } => {
                self.collect_named_def(*name, DefKind::Effect, *exported, expr_idx, attrs);
            }
            Expr::Foreign {
                exported, decls, ..
            } => {
                self.collect_foreign_decls(*exported, decls, attrs);
            }
            Expr::Instance {
                target, exported, ..
            } => {
                let target_name_ref = match &self.ast.exprs[*target] {
                    Expr::Name { name_ref, .. } => *name_ref,
                    Expr::TypeApp { callee, .. } => match &self.ast.exprs[*callee] {
                        Expr::Name { name_ref, .. } => *name_ref,
                        _ => return,
                    },
                    _ => return,
                };
                let target_name = self.ast.name_refs[target_name_ref].name;
                let id = self.defs.alloc(
                    target_name,
                    DefKind::Instance,
                    self.span_of_expr(expr_idx),
                    self.file_id,
                );
                if *exported {
                    self.defs.get_mut(id).exported = true;
                }
            }
            Expr::Import { path, alias, .. } => {
                if let Some(alias_name) = alias {
                    let id = self.defs.alloc(
                        *alias_name,
                        DefKind::Import,
                        self.span_of_expr(expr_idx),
                        self.file_id,
                    );
                    self.define_in_scope(*alias_name, id, self.span_of_expr(expr_idx));
                    let _prev = self.import_alias_defs.insert(id, *path);
                } else if let Some(names) = self.import_names.get(path) {
                    for &(name, def_id) in names {
                        let _prev = self.scopes.define(self.current_scope, name, def_id);
                        self.defs.get_mut(def_id).use_count += 1;
                    }
                }
            }
            Expr::Annotated { inner, attrs, .. } => {
                self.collect_top_level_with_attrs(*inner, attrs);
            }
            _ => {}
        }
    }

    fn collect_top_level_let(&mut self, fields: &LetFields, attrs: &[Attr]) {
        let kind = if fields.value.is_some_and(|v| {
            matches!(
                self.ast.exprs[v],
                Expr::Choice { .. } | Expr::RecordDef { .. }
            )
        }) {
            DefKind::Type
        } else {
            binding_def_kind(fields.kind)
        };
        self.define_fn_name(fields.pat, kind);
        self.define_pat(fields.pat, kind);
        self.maybe_mark_entrypoint(fields.pat, attrs);
        self.maybe_mark_lang_item_pat(fields.pat, attrs);
    }

    fn collect_named_def(
        &mut self,
        name: Symbol,
        kind: DefKind,
        exported: bool,
        expr_idx: ExprIdx,
        attrs: &[Attr],
    ) {
        let span = self.span_of_expr(expr_idx);
        let id = self.defs.alloc(name, kind, span, self.file_id);
        if exported {
            self.defs.get_mut(id).exported = true;
        }
        self.define_in_scope(name, id, span);
        let _inserted = self.output.pat_defs.insert(span, id);
        if let Some(lang_sym) = self.lang_item_from_attrs(attrs) {
            self.defs.get_mut(id).lang_item = Some(lang_sym);
        }
    }

    fn collect_foreign_decls(&mut self, exported: bool, decls: &[ForeignDecl], attrs: &[Attr]) {
        // When there is exactly one decl, the outer attrs (including #[lang := "..."]) apply to it.
        let single_decl_attrs = if decls.len() == 1 { attrs } else { &[] };
        for decl in decls {
            match decl {
                ForeignDecl::Fn { name, span, .. } => {
                    let id = self
                        .defs
                        .alloc(*name, DefKind::ForeignFn, *span, self.file_id);
                    if exported {
                        self.defs.get_mut(id).exported = true;
                    }
                    self.define_in_scope(*name, id, *span);
                    let _inserted = self.output.pat_defs.insert(*span, id);
                    if let Some(lang_sym) = self.lang_item_from_attrs(single_decl_attrs) {
                        self.defs.get_mut(id).lang_item = Some(lang_sym);
                    }
                }
                ForeignDecl::OpaqueType { name, span } => {
                    let id = self
                        .defs
                        .alloc(*name, DefKind::OpaqueType, *span, self.file_id);
                    if exported {
                        self.defs.get_mut(id).exported = true;
                    }
                    self.define_in_scope(*name, id, *span);
                    let _inserted = self.output.pat_defs.insert(*span, id);
                }
            }
        }
    }

    fn maybe_mark_entrypoint(&mut self, pat: PatIdx, attrs: &[Attr]) {
        let has_ep = attrs
            .iter()
            .any(|a| self.interner.resolve(a.name) == "entrypoint");
        if !has_ep {
            return;
        }
        let span = match &self.ast.pats[pat] {
            Pat::Variant { span, .. } | Pat::Bind { span, .. } => *span,
            _ => return,
        };
        if let Some(&def_id) = self.output.pat_defs.get(&span) {
            self.defs.get_mut(def_id).is_entry_point = true;
        }
    }

    fn lang_item_from_attrs(&self, attrs: &[Attr]) -> Option<Symbol> {
        for attr in attrs {
            if self.interner.resolve(attr.name) != "lang" {
                continue;
            }
            if let Some(AttrValue::Lit {
                lit: Lit::Str { value, .. },
                ..
            }) = &attr.value
            {
                return Some(*value);
            }
        }
        None
    }

    fn maybe_mark_lang_item_pat(&mut self, pat: PatIdx, attrs: &[Attr]) {
        let Some(sym) = self.lang_item_from_attrs(attrs) else {
            return;
        };
        let span = match &self.ast.pats[pat] {
            Pat::Variant { span, .. } | Pat::Bind { span, .. } => *span,
            _ => return,
        };
        if let Some(&def_id) = self.output.pat_defs.get(&span) {
            self.defs.get_mut(def_id).lang_item = Some(sym);
        }
    }
}
