//! Pattern resolution and definition.

use msc_ast::PatIdx;
use msc_ast::pat::Pat;

use crate::def::DefKind;

use super::Resolver;

impl Resolver<'_> {
    /// Defines the function name from a function-like pattern (`Pat::Variant`),
    /// without defining the argument patterns. Used to make the name visible
    /// before its body is resolved, enabling recursion and module-level visibility.
    pub(super) fn define_fn_name(&mut self, pat_idx: PatIdx, kind: DefKind) {
        if let Pat::Variant {
            name, span, args, ..
        } = &self.ast.pats[pat_idx]
        {
            let effective_kind = if args.is_empty() { kind } else { DefKind::Fn };
            let id = self.defs.alloc(*name, effective_kind, *span, self.file_id);
            self.define_in_scope(*name, id, *span);
            let _inserted = self.output.pat_defs.insert(*span, id);
        }
    }

    /// Resolve pattern bindings, creating defs for each bound name.
    pub(super) fn resolve_pat(&mut self, pat_idx: PatIdx) {
        match self.ast.pats[pat_idx].clone() {
            Pat::Bind {
                name, span, inner, ..
            } => {
                let id = self.defs.alloc(name, DefKind::Let, span, self.file_id);
                self.define_in_scope(name, id, span);
                let _inserted = self.output.pat_defs.insert(span, id);
                if let Some(inner) = inner {
                    self.resolve_pat(inner);
                }
            }
            Pat::Variant { args, .. } => {
                for &arg in &args {
                    self.resolve_pat(arg);
                }
            }
            Pat::Record { fields, .. } => {
                for field in &fields {
                    if let Some(pat) = field.pat {
                        self.resolve_pat(pat);
                    }
                }
            }
            Pat::Tuple { elems, .. } | Pat::Array { elems, .. } => {
                for &elem in &elems {
                    self.resolve_pat(elem);
                }
            }
            Pat::Or { left, right, .. } => {
                self.resolve_pat(left);
                self.resolve_pat(right);
            }
            Pat::Wild { .. } | Pat::Lit { .. } | Pat::Error { .. } => {}
        }
    }

    pub(super) fn define_pat(&mut self, pat_idx: PatIdx, kind: DefKind) {
        match self.ast.pats[pat_idx].clone() {
            Pat::Bind {
                name, span, inner, ..
            } => {
                let id = self.defs.alloc(name, kind, span, self.file_id);
                self.define_in_scope(name, id, span);
                let _inserted = self.output.pat_defs.insert(span, id);
                if let Some(inner) = inner {
                    self.define_pat(inner, kind);
                }
            }
            Pat::Variant { args, .. } => {
                for &arg in &args {
                    self.define_pat(arg, kind);
                }
            }
            Pat::Record { fields, .. } => {
                for field in &fields {
                    if let Some(pat) = field.pat {
                        self.define_pat(pat, kind);
                    }
                }
            }
            Pat::Tuple { elems, .. } | Pat::Array { elems, .. } => {
                for &elem in &elems {
                    self.define_pat(elem, kind);
                }
            }
            Pat::Or { left, right, .. } => {
                self.define_pat(left, kind);
                self.define_pat(right, kind);
            }
            Pat::Wild { .. } | Pat::Lit { .. } | Pat::Error { .. } => {}
        }
    }

    pub(super) fn mark_pat_exported(&mut self, pat_idx: PatIdx) {
        match self.ast.pats[pat_idx].clone() {
            Pat::Bind { span, inner, .. } => {
                if let Some(def_id) = self.output.pat_defs.get(&span).copied() {
                    self.defs.get_mut(def_id).exported = true;
                }
                if let Some(inner) = inner {
                    self.mark_pat_exported(inner);
                }
            }
            Pat::Tuple { elems, .. } | Pat::Array { elems, .. } => {
                for &elem in &elems {
                    self.mark_pat_exported(elem);
                }
            }
            Pat::Record { fields, .. } => {
                for field in &fields {
                    if let Some(pat) = field.pat {
                        self.mark_pat_exported(pat);
                    }
                }
            }
            Pat::Or { left, right, .. } => {
                self.mark_pat_exported(left);
                self.mark_pat_exported(right);
            }
            Pat::Variant { args, .. } => {
                for &arg in &args {
                    self.mark_pat_exported(arg);
                }
            }
            Pat::Wild { .. } | Pat::Lit { .. } | Pat::Error { .. } => {}
        }
    }
}
