use music_ast::TyId;
use music_ast::ty::TyKind;
use music_resolve::def::DefKind;

use super::SemaDb;
use crate::types::{NominalKey, SemaTypeId, SemaTypeList, Ty};

impl SemaDb {
    /// Converts an AST `TyId` to a semantic `SemaTypeId`.
    pub(super) fn lower_ty(&mut self, ty_id: TyId) -> SemaTypeId {
        let ty_kind = self.db.ast.types.get(ty_id).kind.clone();
        match ty_kind {
            TyKind::Named { name, args } => {
                if let Some(&def_id) = self.resolution.ty_res.get(&ty_id) {
                    let def = self.resolution.defs.get(def_id);
                    match def.kind {
                        DefKind::Builtin(bt) => self.env.builtin(bt),
                        DefKind::TypeParam => self.env.intern(Ty::Param(name.name)),
                        DefKind::Effect => self.env.intern(Ty::Effect(name.name)),
                        DefKind::Type | DefKind::Import | DefKind::Value | DefKind::Function => {
                            let base = self.env.intern(Ty::Named(NominalKey {
                                module_name: def.module_name.clone(),
                                name: name.name,
                            }));
                            if args.is_empty() {
                                base
                            } else {
                                let lowered_args: SemaTypeList =
                                    args.iter().map(|&arg| self.lower_ty(arg)).collect();
                                self.env.intern(Ty::App(base, lowered_args))
                            }
                        }
                        _ => self.env.intern(Ty::Named(NominalKey {
                            module_name: def.module_name.clone(),
                            name: name.name,
                        })),
                    }
                } else {
                    self.env.intern(Ty::Any)
                }
            }
            TyKind::Arrow { from, to } => {
                let from_ty = self.lower_ty(from);
                let to_ty = self.lower_ty(to);
                self.env.intern(Ty::Arrow {
                    param: from_ty,
                    ret: to_ty,
                })
            }
            TyKind::EffectArrow { from, to } => {
                let from_ty = self.lower_ty(from);
                let to_ty = self.lower_ty(to);
                self.env.intern(Ty::EffectArrow {
                    param: from_ty,
                    ret: to_ty,
                    effects: Vec::new(),
                })
            }
            TyKind::Tuple(elems) => {
                let elem_tys: Vec<_> = elems.iter().map(|&t| self.lower_ty(t)).collect();
                self.env.intern(Ty::Tuple(elem_tys))
            }
            TyKind::Sum(members) => {
                let member_tys: Vec<_> = members.iter().map(|&t| self.lower_ty(t)).collect();
                self.env.intern(Ty::Union(member_tys))
            }
            TyKind::Product(members) => {
                let member_tys: Vec<_> = members.iter().map(|&t| self.lower_ty(t)).collect();
                self.env.intern(Ty::Tuple(member_tys))
            }
            TyKind::Mut(inner) => {
                let inner_ty = self.lower_ty(inner);
                self.env.intern(Ty::Mut(inner_ty))
            }
            TyKind::Option(inner) => {
                let inner_ty = self.lower_ty(inner);
                let unit_ty = self.env.intern(Ty::Unit);
                self.env.intern(Ty::Union(vec![inner_ty, unit_ty]))
            }
            TyKind::Array { elem, .. } => {
                let elem_ty = self.lower_ty(elem);
                self.env.intern(Ty::Array(elem_ty))
            }
            TyKind::Pi { ret_ty, .. } => self.lower_ty(ret_ty),
        }
    }
}
