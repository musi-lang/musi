use std::collections::HashMap;

use music_basic::Span;
use music_hir::{HirEffectItem, HirEffectSet, HirTy, HirTyId, HirTyKind};
use music_names::Symbol;

use crate::checker::Checker;

use super::*;

impl<'a> Checker<'a> {
    pub(super) fn lower_effect_set(
        &mut self,
        set: &HirEffectSet,
        ty_params: &HashMap<Symbol, u32>,
    ) -> EffectRow {
        let mut out = EffectRow::empty();
        out.is_open = set.rest.is_some();
        for HirEffectItem { name, arg, .. } in set.items.iter() {
            let arg = arg.map(|t| self.lower_hir_ty(t, ty_params));
            out.add(EffectKey {
                name: name.name,
                arg,
            });
        }
        out
    }

    pub(super) fn lower_hir_ty(
        &mut self,
        ty: HirTyId,
        ty_params: &HashMap<Symbol, u32>,
    ) -> SemTyId {
        let ty = self.ctx.store.tys.get(ty).clone();
        match ty.kind {
            HirTyKind::Error => self.state.builtins.error,
            HirTyKind::Named { name, args } => {
                if args.is_empty() {
                    match name.name {
                        s if s == self.state.known.any => return self.state.builtins.any,
                        s if s == self.state.known.unknown => return self.state.builtins.unknown,
                        s if s == self.state.known.type_ => return self.state.builtins.type_,
                        s if s == self.state.known.syntax => return self.state.builtins.syntax,
                        s if s == self.state.known.empty => return self.state.builtins.empty,
                        s if s == self.state.known.unit => return self.state.builtins.unit,
                        s if s == self.state.known.bool_ => return self.state.builtins.bool_,
                        s if s == self.state.known.int_ => return self.state.builtins.int_,
                        s if s == self.state.known.float_ => return self.state.builtins.float_,
                        s if s == self.state.known.string_ => return self.state.builtins.string_,
                        s if s == self.state.known.cstring => return self.state.builtins.cstring,
                        s if s == self.state.known.cptr => return self.state.builtins.cptr,
                        _ => {}
                    }
                }
                if let Some(&idx) = ty_params.get(&name.name) {
                    return self.state.semtys.alloc(SemTy::Generic(idx));
                }
                let new_args: Vec<_> = args
                    .iter()
                    .copied()
                    .map(|a| self.lower_hir_ty(a, ty_params))
                    .collect();
                self.state.semtys.alloc(SemTy::Named {
                    name: name.name,
                    args: new_args.into_boxed_slice(),
                })
            }
            HirTyKind::Tuple { items } => {
                let new_items: Vec<_> = items
                    .iter()
                    .copied()
                    .map(|a| self.lower_hir_ty(a, ty_params))
                    .collect();
                self.state.semtys.alloc(SemTy::Tuple {
                    items: new_items.into_boxed_slice(),
                })
            }
            HirTyKind::Array { dims, elem } => {
                let elem = self.lower_hir_ty(elem, ty_params);
                self.state.semtys.alloc(SemTy::Array { dims, elem })
            }
            HirTyKind::Arrow {
                flavor,
                input,
                output,
            } => {
                let input = self.lower_hir_ty(input, ty_params);
                let output = self.lower_hir_ty(output, ty_params);
                self.state.semtys.alloc(SemTy::Arrow {
                    flavor,
                    input,
                    output,
                })
            }
            HirTyKind::Binary { op, left, right } => {
                let left = self.lower_hir_ty(left, ty_params);
                let right = self.lower_hir_ty(right, ty_params);
                self.state.semtys.alloc(SemTy::Binary { op, left, right })
            }
            HirTyKind::Mut { base } => {
                let base = self.lower_hir_ty(base, ty_params);
                self.state.semtys.alloc(SemTy::Mut { base })
            }
            HirTyKind::Pi { .. } => self.state.builtins.unknown,
        }
    }

    pub(super) fn lower_sem_ty_to_hir(&mut self, ty: SemTyId) -> HirTyId {
        let ty = unify::resolve(&self.state.semtys, ty);
        let kind = match self.state.semtys.get(ty).clone() {
            SemTy::Error => HirTyKind::Error,
            SemTy::Unknown => HirTyKind::Named {
                name: music_names::Ident::dummy(self.state.known.unknown),
                args: Box::new([]),
            },
            SemTy::Any => HirTyKind::Named {
                name: music_names::Ident::dummy(self.state.known.any),
                args: Box::new([]),
            },
            SemTy::InferVar(var) => {
                if let Some(bound) = self.state.semtys.infer_binding(var) {
                    return self.lower_sem_ty_to_hir(bound);
                }
                HirTyKind::Named {
                    name: music_names::Ident::dummy(self.state.known.unknown),
                    args: Box::new([]),
                }
            }
            SemTy::Generic(_) => HirTyKind::Named {
                name: music_names::Ident::dummy(self.state.known.unknown),
                args: Box::new([]),
            },
            SemTy::Named { name, args } => {
                let hir_args: Vec<_> = args
                    .iter()
                    .copied()
                    .map(|a| self.lower_sem_ty_to_hir(a))
                    .collect();
                HirTyKind::Named {
                    name: music_names::Ident::dummy(name),
                    args: hir_args.into_boxed_slice(),
                }
            }
            SemTy::Tuple { items } => {
                let hir_items: Vec<_> = items
                    .iter()
                    .copied()
                    .map(|a| self.lower_sem_ty_to_hir(a))
                    .collect();
                HirTyKind::Tuple {
                    items: hir_items.into_boxed_slice(),
                }
            }
            SemTy::Array { dims, elem } => HirTyKind::Array {
                dims,
                elem: self.lower_sem_ty_to_hir(elem),
            },
            SemTy::Arrow {
                flavor,
                input,
                output,
            } => HirTyKind::Arrow {
                flavor,
                input: self.lower_sem_ty_to_hir(input),
                output: self.lower_sem_ty_to_hir(output),
            },
            SemTy::Binary { op, left, right } => HirTyKind::Binary {
                op,
                left: self.lower_sem_ty_to_hir(left),
                right: self.lower_sem_ty_to_hir(right),
            },
            SemTy::Mut { base } => HirTyKind::Mut {
                base: self.lower_sem_ty_to_hir(base),
            },
            SemTy::Record { .. } => HirTyKind::Named {
                name: music_names::Ident::dummy(self.state.known.unknown),
                args: Box::new([]),
            },
        };

        self.ctx.store.tys.alloc(HirTy {
            origin: dummy_origin(Span::DUMMY),
            kind,
        })
    }
}
