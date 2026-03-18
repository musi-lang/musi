//! Type resolution helpers.

use music_ast::ty::{Ty, TyNamedRef};
use music_ast::TyIdx;

use super::Resolver;

impl Resolver<'_> {
    pub(super) fn resolve_ty(&mut self, ty_idx: TyIdx) {
        match self.ast.tys[ty_idx].clone() {
            Ty::Named {
                name_ref,
                args,
                span,
            } => {
                let nr = self.ast.name_refs[name_ref];
                if let Some(def_id) = self.scopes.lookup(self.current_scope, nr.name) {
                    self.output.name_ref_defs[usize::try_from(name_ref.raw()).unwrap()] =
                        Some(def_id);
                    self.defs.get_mut(def_id).use_count += 1;
                } else {
                    self.report_undefined(nr.name, span);
                }
                for &arg in &args {
                    self.resolve_ty(arg);
                }
            }
            Ty::Qualified {
                module_ref,
                args,
                span,
                ..
            } => {
                let nr = self.ast.name_refs[module_ref];
                if let Some(def_id) = self.scopes.lookup(self.current_scope, nr.name) {
                    self.output.name_ref_defs[usize::try_from(module_ref.raw()).unwrap()] =
                        Some(def_id);
                    self.defs.get_mut(def_id).use_count += 1;
                } else {
                    self.report_undefined(nr.name, span);
                }
                for &arg in &args {
                    self.resolve_ty(arg);
                }
            }
            Ty::Option { inner, .. } => {
                self.resolve_ty(inner);
            }
            Ty::Fn { params, ret, .. } => {
                for &p in &params {
                    self.resolve_ty(p);
                }
                self.resolve_ty(ret);
            }
            Ty::Product { fields, .. } => {
                for &f in &fields {
                    self.resolve_ty(f);
                }
            }
            Ty::Sum { variants, .. } => {
                for &v in &variants {
                    self.resolve_ty(v);
                }
            }
            Ty::Array { elem, .. } => {
                self.resolve_ty(elem);
            }
            Ty::Var { name_ref } => {
                let nr = self.ast.name_refs[name_ref];
                if let Some(def_id) = self.scopes.lookup(self.current_scope, nr.name) {
                    self.output.name_ref_defs[usize::try_from(name_ref.raw()).unwrap()] =
                        Some(def_id);
                    self.defs.get_mut(def_id).use_count += 1;
                } else {
                    self.report_undefined(nr.name, nr.span);
                }
            }
            Ty::Error { .. } => {}
        }
    }

    pub(super) fn resolve_ty_named_ref(&mut self, named: &TyNamedRef) {
        let nr = self.ast.name_refs[named.name_ref];
        if let Some(def_id) = self.scopes.lookup(self.current_scope, nr.name) {
            self.output.name_ref_defs[usize::try_from(named.name_ref.raw()).unwrap()] =
                Some(def_id);
            self.defs.get_mut(def_id).use_count += 1;
        } else {
            self.report_undefined(nr.name, named.span);
        }
        for &arg in &named.args {
            self.resolve_ty(arg);
        }
    }
}
