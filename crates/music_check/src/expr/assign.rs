use music_hir::{HirChainKind, HirExprId, HirExprKind, HirMemberKey, HirOrigin};

use crate::{SemaErrorKind, env};

use crate::checker::Checker;
use crate::unify;
use crate::{EffectRow, SemTy, SemTyId};

impl Checker<'_> {
    pub(super) fn synth_assign(
        &mut self,
        origin: HirOrigin,
        left: HirExprId,
        right: HirExprId,
    ) -> (SemTyId, EffectRow) {
        let (expected, mut effs) = self.assign_target(origin, left);
        let (_rhs, rhs_effs) = self.check_expr(right, expected);
        effs.union_with(&rhs_effs);
        (self.state.builtins.unit, effs)
    }

    fn assign_target(&mut self, origin: HirOrigin, expr: HirExprId) -> (SemTyId, EffectRow) {
        let kind = self.ctx.store.exprs.get(expr).kind.clone();
        match kind {
            HirExprKind::Named { ident } => {
                let Some(binding) = self.binding_for_use(ident.span) else {
                    self.error(origin.span, SemaErrorKind::AssignTargetInvalid);
                    return (self.state.builtins.unknown, EffectRow::empty());
                };

                if !self.binding_is_mut(binding) {
                    self.error(
                        ident.span,
                        SemaErrorKind::AssignTargetRequiresMutableBinding {
                            name: self.ctx.interner.resolve(ident.name).to_owned(),
                        },
                    );
                }

                let scheme = self.state.env.get_value(binding).cloned();
                let ty = match scheme.as_ref() {
                    Some(scheme) => self.instantiate_scheme(ident.span, scheme).ty,
                    None => self.state.builtins.unknown,
                };
                (ty, EffectRow::empty())
            }
            HirExprKind::Member { base, chain, key } => {
                if chain != HirChainKind::Normal {
                    let (_t, effs) = self.synth_expr(base);
                    self.error(origin.span, SemaErrorKind::AssignTargetInvalid);
                    return (self.state.builtins.unknown, effs);
                }

                let (base_ty, effs) = self.synth_expr(base);
                let base_ty = unify::resolve(&self.state.semtys, base_ty);
                let base_sem = self.state.semtys.get(base_ty).clone();

                match base_sem {
                    SemTy::Mut { base } => {
                        let mut inner = unify::resolve(&self.state.semtys, base);
                        loop {
                            let SemTy::Mut { base } = self.state.semtys.get(inner).clone() else {
                                break;
                            };
                            inner = unify::resolve(&self.state.semtys, base);
                        }

                        let slot = self.project_member_slot(origin, inner, &key);
                        (slot, effs)
                    }
                    SemTy::Unknown | SemTy::Any | SemTy::Error => {
                        (self.state.builtins.unknown, effs)
                    }
                    _ => {
                        self.error(origin.span, SemaErrorKind::AssignTargetRequiresWritableBase);
                        (self.state.builtins.unknown, effs)
                    }
                }
            }
            HirExprKind::Index { base, indices } => {
                let (base_ty, mut effs) = self.synth_expr(base);
                let base_ty = unify::resolve(&self.state.semtys, base_ty);
                let base_sem = self.state.semtys.get(base_ty).clone();

                match base_sem {
                    SemTy::Mut { base } => {
                        let slot = self.project_indices(origin, base, &indices, &mut effs);
                        (slot, effs)
                    }
                    SemTy::Unknown | SemTy::Any | SemTy::Error => {
                        (self.state.builtins.unknown, effs)
                    }
                    _ => {
                        self.error(origin.span, SemaErrorKind::AssignTargetRequiresWritableBase);
                        (self.state.builtins.unknown, effs)
                    }
                }
            }
            _ => {
                let (_t, effs) = self.synth_expr(expr);
                self.error(origin.span, SemaErrorKind::AssignTargetInvalid);
                (self.state.builtins.unknown, effs)
            }
        }
    }

    fn project_member_slot(
        &mut self,
        origin: HirOrigin,
        base_ty: SemTyId,
        key: &HirMemberKey,
    ) -> SemTyId {
        let base_ty = unify::resolve(&self.state.semtys, base_ty);
        match (self.state.semtys.get(base_ty).clone(), key) {
            (SemTy::Tuple { items }, HirMemberKey::IntLit { span, .. }) => {
                let idx = super::parse_int_lit_u32(self.slice(*span));
                let len = u32::try_from(items.len()).unwrap_or(0);
                let ty = idx
                    .and_then(|i| usize::try_from(i).ok())
                    .and_then(|i| items.get(i).copied());
                if let Some(ty) = ty {
                    ty
                } else {
                    self.error(
                        *span,
                        SemaErrorKind::TupleIndexOutOfRange {
                            index: idx.unwrap_or(0),
                            len,
                        },
                    );
                    self.state.builtins.error
                }
            }
            (SemTy::Record { fields }, HirMemberKey::Name(field)) => {
                fields.get(&field.name).copied().unwrap_or_else(|| {
                    self.error(
                        origin.span,
                        SemaErrorKind::FieldNotFound {
                            name: self.ctx.interner.resolve(field.name).to_owned(),
                        },
                    );
                    self.state.builtins.error
                })
            }
            (SemTy::Named { name, args }, HirMemberKey::Name(field)) => {
                if self.error_if_opaque_repr_access(origin.span, name) {
                    return self.state.builtins.unknown;
                }

                let Some(def) = self.state.env.get_data_def(name).cloned() else {
                    return self.state.builtins.unknown;
                };
                let Some(fields) = def.fields.as_ref() else {
                    return self.state.builtins.unknown;
                };
                let Some(field_def) = fields.get(&field.name) else {
                    self.error(
                        origin.span,
                        SemaErrorKind::FieldNotFound {
                            name: self.ctx.interner.resolve(field.name).to_owned(),
                        },
                    );
                    return self.state.builtins.error;
                };

                let cap = usize::try_from(def.generic_count).unwrap_or(0);
                let mut subst = Vec::with_capacity(cap);
                for i in 0..def.generic_count {
                    subst.push(
                        args.get(usize::try_from(i).unwrap_or(usize::MAX))
                            .copied()
                            .unwrap_or(self.state.builtins.unknown),
                    );
                }
                env::substitute_generics(&mut self.state.semtys, field_def.ty, &subst)
            }
            (SemTy::Unknown | SemTy::Any | SemTy::Error, _) => self.state.builtins.unknown,
            _ => {
                self.error(origin.span, SemaErrorKind::AssignTargetInvalid);
                self.state.builtins.unknown
            }
        }
    }
}
