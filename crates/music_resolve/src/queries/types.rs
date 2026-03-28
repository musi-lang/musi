use crate::errors::ResolveErrorKind;

use super::*;

impl ResolveDb {
    pub(super) fn resolve_ty(&mut self, ty_id: TyId, scope: ScopeId) {
        let kind = self.db.ast.types.get(ty_id).kind.clone();
        match kind {
            TyKind::Named { name, args } => {
                match self.resolution.scopes.resolve(scope, name.name) {
                    Some(def_id) => {
                        let _ = self.resolution.ty_res.insert(ty_id, def_id);
                    }
                    None => {
                        self.errors.push(ResolveError {
                            kind: ResolveErrorKind::UndefinedType(name.name),
                            span: name.span,
                        });
                    }
                }
                for arg in args {
                    self.resolve_ty(arg, scope);
                }
            }
            TyKind::Arrow { from, to } | TyKind::EffectArrow { from, to } => {
                self.resolve_ty(from, scope);
                self.resolve_ty(to, scope);
            }
            TyKind::Sum(tys) | TyKind::Product(tys) | TyKind::Tuple(tys) => {
                for t in tys {
                    self.resolve_ty(t, scope);
                }
            }
            TyKind::Mut(inner) | TyKind::Option(inner) => {
                self.resolve_ty(inner, scope);
            }
            TyKind::Array { elem, .. } => {
                self.resolve_ty(elem, scope);
            }
            TyKind::Pi {
                name,
                param_ty,
                ret_ty,
            } => {
                self.resolve_ty(param_ty, scope);
                let pi_scope = self
                    .resolution
                    .scopes
                    .push(ScopeKind::TypeParams, Some(scope));
                let _ = self.define_and_bind(
                    name.name,
                    name.span,
                    DefKind::TypeParam,
                    Visibility::Private,
                    pi_scope,
                );
                self.resolve_ty(ret_ty, pi_scope);
            }
        }
    }
}
