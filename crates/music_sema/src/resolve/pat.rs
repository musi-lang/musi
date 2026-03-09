//! Pattern resolution and definition.

use music_ast::pat::Pat;
use music_shared::Idx;

use crate::def::DefKind;

use super::Resolver;

impl Resolver<'_> {
    /// Resolve pattern bindings, creating defs for each bound name.
    pub(super) fn resolve_pat(&mut self, pat_idx: Idx<Pat>) {
        match self.ast.pats[pat_idx].clone() {
            Pat::Bind {
                name, span, inner, ..
            } => {
                let id = self.defs.alloc(name, DefKind::Let, span);
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

    pub(super) fn define_pat(&mut self, pat_idx: Idx<Pat>, kind: DefKind) {
        match self.ast.pats[pat_idx].clone() {
            Pat::Bind {
                name, span, inner, ..
            } => {
                let id = self.defs.alloc(name, kind, span);
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
}
