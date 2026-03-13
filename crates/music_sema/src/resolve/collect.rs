//! Pass 1: top-level definition collection.

use music_ast::ExprIdx;
use music_ast::attr::Attr;
use music_ast::decl::ForeignDecl;
use music_ast::expr::{Expr, LetFields};

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
            }
            Expr::Let {
                fields, body: None, ..
            } => {
                self.collect_top_level_let(fields, attrs);
            }
            Expr::Class { name, exported, .. } => {
                self.collect_named_def(*name, DefKind::Class, *exported, expr_idx);
            }
            Expr::Effect { name, exported, .. } => {
                self.collect_named_def(*name, DefKind::Effect, *exported, expr_idx);
            }
            Expr::Foreign {
                exported, decls, ..
            } => {
                self.collect_foreign_decls(*exported, decls);
            }
            Expr::Given {
                target, exported, ..
            } => {
                let id = self
                    .defs
                    .alloc(target.name, DefKind::Given, self.span_of_expr(expr_idx));
                if *exported {
                    self.defs.get_mut(id).exported = true;
                }
            }
            Expr::Import { path, .. } => {
                if let Some(names) = self.import_names.get(path) {
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
    }

    fn collect_named_def(
        &mut self,
        name: music_shared::Symbol,
        kind: DefKind,
        exported: bool,
        expr_idx: ExprIdx,
    ) {
        let id = self.defs.alloc(name, kind, self.span_of_expr(expr_idx));
        if exported {
            self.defs.get_mut(id).exported = true;
        }
        self.define_in_scope(name, id, self.span_of_expr(expr_idx));
    }

    fn collect_foreign_decls(&mut self, exported: bool, decls: &[ForeignDecl]) {
        for decl in decls {
            match decl {
                ForeignDecl::Fn { name, span, .. } => {
                    let id = self.defs.alloc(*name, DefKind::ForeignFn, *span);
                    if exported {
                        self.defs.get_mut(id).exported = true;
                    }
                    self.define_in_scope(*name, id, *span);
                }
                ForeignDecl::OpaqueType { name, span } => {
                    let id = self.defs.alloc(*name, DefKind::OpaqueType, *span);
                    if exported {
                        self.defs.get_mut(id).exported = true;
                    }
                    self.define_in_scope(*name, id, *span);
                }
            }
        }
    }

    fn maybe_mark_entrypoint(&mut self, pat: music_ast::PatIdx, attrs: &[Attr]) {
        use music_ast::pat::Pat;
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
}
