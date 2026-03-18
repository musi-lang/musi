//! Pass 1: top-level definition collection.

use msc_ast::attr::{Attr, AttrValue};
use msc_ast::decl::ForeignDecl;
use msc_ast::expr::{Expr, LetFields};
use msc_ast::lit::Lit;
use msc_ast::{ExprIdx, Pat, PatIdx};
use msc_shared::Symbol;

use crate::attr_util::validate_attrs;
use crate::def::{DefFlags, DefId, DefKind};

use super::{Resolver, binding_def_kind};

impl Resolver<'_> {
    pub(super) fn collect_top_level(&mut self, expr_idx: ExprIdx) {
        self.collect_top_level_with_attrs(expr_idx, &[]);
    }

    fn collect_top_level_with_attrs(&mut self, expr_idx: ExprIdx, attrs: &[Attr]) {
        for warning in validate_attrs(attrs, self.interner) {
            let _d = self
                .diags
                .report(&warning.message, warning.span, self.file_id);
        }
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
                self.apply_flags_to_pat(fields.pat, attrs);
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
            // Top-level expressions that are not declarations (e.g. bare calls for side effects)
            // are intentionally skipped here. They are evaluated for their effects in pass 2
            // when the resolver walks the full module body.
            _ => {}
        }
    }

    fn collect_top_level_let(&mut self, fields: &LetFields, attrs: &[Attr]) {
        let is_type_def = fields.value.is_some_and(|v| {
            matches!(
                self.ast.exprs[v],
                Expr::Choice { .. } | Expr::RecordDef { .. }
            )
        });
        let kind = if is_type_def {
            DefKind::Type
        } else {
            binding_def_kind(fields.kind)
        };
        self.define_fn_name(fields.pat, kind);
        self.define_pat(fields.pat, kind);
        self.maybe_mark_entrypoint(fields.pat, attrs);
        self.maybe_mark_lang_item_pat(fields.pat, attrs);
        self.apply_flags_to_pat(fields.pat, attrs);
        if is_type_def {
            self.maybe_mark_abstract_pat(fields.pat, attrs);
        }
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
        self.apply_def_flags(id, attrs);
        if kind == DefKind::Type {
            self.maybe_mark_abstract(id, attrs);
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

    /// Applies `#[inline]`, `#[deprecated]` flags from `attrs` to the def
    /// identified by `pat`'s span.
    fn apply_flags_to_pat(&mut self, pat: PatIdx, attrs: &[Attr]) {
        let span = match &self.ast.pats[pat] {
            Pat::Variant { span, .. } | Pat::Bind { span, .. } => *span,
            _ => return,
        };
        if let Some(&def_id) = self.output.pat_defs.get(&span) {
            self.apply_def_flags(def_id, attrs);
        }
    }

    /// Applies `#[abstract]` to the def identified by `pat`'s span.
    fn maybe_mark_abstract_pat(&mut self, pat: PatIdx, attrs: &[Attr]) {
        let span = match &self.ast.pats[pat] {
            Pat::Variant { span, .. } | Pat::Bind { span, .. } => *span,
            _ => return,
        };
        if let Some(&def_id) = self.output.pat_defs.get(&span) {
            self.maybe_mark_abstract(def_id, attrs);
        }
    }

    /// Reads `#[inline]` and `#[deprecated]` from `attrs` and records them on `id`.
    ///
    // Note: INLINE/INLINE_NEVER flags are recorded but no inlining pass exists yet.
    fn apply_def_flags(&mut self, id: DefId, attrs: &[Attr]) {
        for attr in attrs {
            let name = self.interner.resolve(attr.name);
            match name {
                "inline" => {
                    let flag = match &attr.value {
                        Some(AttrValue::Lit {
                            lit: Lit::Str { value, .. },
                            ..
                        }) if self.interner.resolve(*value).trim_matches('"') == "never" => {
                            DefFlags::INLINE_NEVER
                        }
                        _ => DefFlags::INLINE,
                    };
                    let current = self.defs.get(id).flags;
                    self.defs.get_mut(id).flags = current.with(flag);
                }
                "deprecated" => {
                    let msg_sym = match &attr.value {
                        Some(AttrValue::Lit {
                            lit: Lit::Str { value, .. },
                            ..
                        }) => *value,
                        Some(AttrValue::Tuple { lits, .. }) => lits
                            .first()
                            .and_then(|l| match l {
                                Lit::Str { value, .. } => Some(*value),
                                _ => None,
                            })
                            .unwrap_or_else(|| self.interner.intern("this item is deprecated")),
                        _ => self.interner.intern("this item is deprecated"),
                    };
                    self.defs.get_mut(id).deprecated = Some(msg_sym);
                }
                "repr" => {
                    let flag = self.extract_repr_flag(attr);
                    if flag != 0 {
                        let current = self.defs.get(id).flags;
                        self.defs.get_mut(id).flags = current.with(flag);
                    }
                }
                _ => {}
            }
        }
    }

    /// Extracts the repr flag from a `#[repr(...)]` attribute.
    ///
    /// Supported forms:
    /// - `#[repr("C")]` or `#[repr("packed")]` (positional)
    /// - `#[repr(kind := "C")]` or `#[repr(kind := "packed")]` (named)
    /// - `#[repr := "C"]` (literal shorthand)
    fn extract_repr_flag(&self, attr: &Attr) -> u8 {
        match &attr.value {
            Some(AttrValue::Lit {
                lit: Lit::Str { value, .. },
                ..
            }) => self.repr_str_to_flag(*value),
            Some(AttrValue::Tuple { lits, .. }) => {
                let mut flag = 0u8;
                for lit in lits {
                    if let Lit::Str { value, .. } = lit {
                        flag |= self.repr_str_to_flag(*value);
                    }
                }
                flag
            }
            Some(AttrValue::Named { fields, .. }) => {
                let mut flag = 0u8;
                for field in fields {
                    if self.interner.resolve(field.name) == "kind" {
                        if let Lit::Str { value, .. } = &field.value {
                            flag |= self.repr_str_to_flag(*value);
                        }
                    }
                }
                flag
            }
            _ => 0,
        }
    }

    fn repr_str_to_flag(&self, sym: Symbol) -> u8 {
        match self.interner.resolve(sym).trim_matches('"') {
            "C" => DefFlags::REPR_C,
            "packed" => DefFlags::REPR_PACKED,
            _ => 0,
        }
    }

    /// Sets `DefFlags::ABSTRACT` on `id` when `attrs` contains `#[abstract]`.
    fn maybe_mark_abstract(&mut self, id: DefId, attrs: &[Attr]) {
        let has_abstract = attrs
            .iter()
            .any(|a| self.interner.resolve(a.name) == "abstract");
        if has_abstract {
            let current = self.defs.get(id).flags;
            self.defs.get_mut(id).flags = current.with(DefFlags::ABSTRACT);
        }
    }
}
