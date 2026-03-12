//! Type resolution helpers.

use music_ast::TyIdx;
use music_ast::ty::{Ty, TyNamedRef};

use super::Resolver;

impl Resolver<'_> {
    pub(super) fn resolve_ty(&mut self, ty_idx: TyIdx) {
        match self.ast.tys[ty_idx].clone() {
            Ty::Named { name, args, span } => {
                if self.scopes.lookup(self.current_scope, name).is_none() {
                    self.report_undefined(name, span);
                }
                for &arg in &args {
                    self.resolve_ty(arg);
                }
            }
            Ty::Option { inner, .. } | Ty::Ref { inner, .. } => {
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
            Ty::Quantified {
                params,
                constraints,
                body,
                ..
            } => {
                let parent = self.enter_ty_param_scope(&params, &constraints);
                self.resolve_ty(body);
                self.current_scope = parent;
            }
            Ty::Var { name, span } => {
                if self.scopes.lookup(self.current_scope, name).is_none() {
                    self.report_undefined(name, span);
                }
            }
            Ty::Error { .. } => {}
        }
    }

    pub(super) fn resolve_ty_named_ref(&mut self, named: &TyNamedRef) {
        if self.scopes.lookup(self.current_scope, named.name).is_none() {
            self.report_undefined(named.name, named.span);
        }
        for &arg in &named.args {
            self.resolve_ty(arg);
        }
    }
}
