use std::collections::BTreeSet;

use music_arena::SliceRange;
use music_hir::{HirDim, HirTyField, HirTyId, HirTyKind};
use music_names::Symbol;

use crate::checker::PassBase;
use crate::effects::{EffectKey, EffectRow};

use super::TypeSubstMap;

impl PassBase<'_, '_, '_> {
    pub fn substitute_ty(&mut self, ty: HirTyId, subst: &TypeSubstMap) -> HirTyId {
        self.substitute_ty_kind(ty, self.ty(ty).kind, subst)
    }

    fn substitute_ty_kind(
        &mut self,
        ty: HirTyId,
        kind: HirTyKind,
        subst: &TypeSubstMap,
    ) -> HirTyId {
        match kind {
            HirTyKind::Named { name, args } => self.substitute_named_ty(name, args, subst),
            HirTyKind::Pi {
                binder,
                binder_ty,
                body,
                is_effectful,
            } => self.substitute_pi_ty(binder, binder_ty, body, is_effectful, subst),
            HirTyKind::Arrow {
                params,
                ret,
                is_effectful,
            } => self.substitute_arrow_ty(params, ret, is_effectful, subst),
            HirTyKind::Sum { left, right } => self.substitute_sum_ty(left, right, subst),
            HirTyKind::Tuple { items } => self.substitute_tuple_ty(items, subst),
            HirTyKind::Seq { item } => {
                self.substitute_item_ty(item, subst, |item| HirTyKind::Seq { item })
            }
            HirTyKind::Array { dims, item } => self.substitute_array_ty(dims, item, subst),
            HirTyKind::Range { bound } => {
                self.substitute_item_ty(bound, subst, |bound| HirTyKind::Range { bound })
            }
            HirTyKind::Handler {
                effect,
                input,
                output,
            } => self.substitute_handler_ty(effect, input, output, subst),
            HirTyKind::Mut { inner } => self.substitute_mut_ty(inner, subst),
            HirTyKind::AnyShape { capability } => self.substitute_shape_ty(capability, subst, true),
            HirTyKind::SomeShape { capability } => {
                self.substitute_shape_ty(capability, subst, false)
            }
            HirTyKind::Record { fields } => self.substitute_record_ty(fields, subst),
            _ => ty,
        }
    }

    fn substitute_pi_ty(
        &mut self,
        binder: Symbol,
        binder_ty: HirTyId,
        body: HirTyId,
        is_effectful: bool,
        subst: &TypeSubstMap,
    ) -> HirTyId {
        let binder_ty = self.substitute_ty(binder_ty, subst);
        let mut next = subst.clone();
        let _ = next.remove(&binder);
        let body = self.substitute_ty(body, &next);
        self.alloc_ty(HirTyKind::Pi {
            binder,
            binder_ty,
            body,
            is_effectful,
        })
    }

    fn substitute_arrow_ty(
        &mut self,
        params: SliceRange<HirTyId>,
        ret: HirTyId,
        is_effectful: bool,
        subst: &TypeSubstMap,
    ) -> HirTyId {
        let params = self.substitute_ty_list(params, subst);
        let ret = self.substitute_ty(ret, subst);
        self.alloc_ty(HirTyKind::Arrow {
            params,
            ret,
            is_effectful,
        })
    }

    fn substitute_sum_ty(
        &mut self,
        left: HirTyId,
        right: HirTyId,
        subst: &TypeSubstMap,
    ) -> HirTyId {
        let left = self.substitute_ty(left, subst);
        let right = self.substitute_ty(right, subst);
        self.alloc_ty(HirTyKind::Sum { left, right })
    }

    fn substitute_named_ty(
        &mut self,
        name: Symbol,
        args: SliceRange<HirTyId>,
        subst: &TypeSubstMap,
    ) -> HirTyId {
        if let Some(found) = subst.get(&name).copied() {
            if self.ty_ids(args).is_empty() {
                return found;
            }
            let args = self.substitute_ty_list(args, subst);
            return self.apply_substituted_type_constructor(found, args);
        }
        let args = self.substitute_ty_list(args, subst);
        if name == self.known().bits {
            let arg_ids = self.ty_ids(args);
            if let [width_ty] = arg_ids.as_slice() {
                if let HirTyKind::NatLit(width) = self.ty(*width_ty).kind
                    && width > 0
                    && let Ok(width) = u32::try_from(width)
                {
                    return self.alloc_ty(HirTyKind::Bits { width });
                }
            }
        }
        self.alloc_ty(HirTyKind::Named { name, args })
    }

    fn substitute_tuple_ty(&mut self, items: SliceRange<HirTyId>, subst: &TypeSubstMap) -> HirTyId {
        let items = self.substitute_ty_list(items, subst);
        self.alloc_ty(HirTyKind::Tuple { items })
    }

    fn substitute_item_ty(
        &mut self,
        item: HirTyId,
        subst: &TypeSubstMap,
        ctor: impl FnOnce(HirTyId) -> HirTyKind,
    ) -> HirTyId {
        let item_ty = self.substitute_ty(item, subst);
        self.alloc_ty(ctor(item_ty))
    }

    fn substitute_array_ty(
        &mut self,
        dims: SliceRange<HirDim>,
        item: HirTyId,
        subst: &TypeSubstMap,
    ) -> HirTyId {
        let item_ty = self.substitute_ty(item, subst);
        self.alloc_ty(HirTyKind::Array {
            dims,
            item: item_ty,
        })
    }

    fn substitute_handler_ty(
        &mut self,
        effect: HirTyId,
        input: HirTyId,
        output: HirTyId,
        subst: &TypeSubstMap,
    ) -> HirTyId {
        let effect = self.substitute_ty(effect, subst);
        let input = self.substitute_ty(input, subst);
        let output = self.substitute_ty(output, subst);
        self.alloc_ty(HirTyKind::Handler {
            effect,
            input,
            output,
        })
    }

    fn substitute_mut_ty(&mut self, inner: HirTyId, subst: &TypeSubstMap) -> HirTyId {
        let inner = self.substitute_ty(inner, subst);
        self.alloc_ty(HirTyKind::Mut { inner })
    }

    fn substitute_shape_ty(
        &mut self,
        shape: HirTyId,
        subst: &TypeSubstMap,
        is_any: bool,
    ) -> HirTyId {
        let shape = self.substitute_ty(shape, subst);
        if is_any {
            self.alloc_ty(HirTyKind::AnyShape { capability: shape })
        } else {
            self.alloc_ty(HirTyKind::SomeShape { capability: shape })
        }
    }

    fn substitute_record_ty(
        &mut self,
        fields: SliceRange<HirTyField>,
        subst: &TypeSubstMap,
    ) -> HirTyId {
        let fields = self
            .ty_fields(fields)
            .into_iter()
            .map(|field| HirTyField::new(field.name, self.substitute_ty(field.ty, subst)))
            .collect::<Vec<_>>();
        let fields = self.alloc_ty_fields(fields);
        self.alloc_ty(HirTyKind::Record { fields })
    }

    fn substitute_ty_list(
        &mut self,
        tys: SliceRange<HirTyId>,
        subst: &TypeSubstMap,
    ) -> SliceRange<HirTyId> {
        let ctx = self;
        let tys = ctx
            .ty_ids(tys)
            .into_iter()
            .map(|ty| ctx.substitute_ty(ty, subst))
            .collect::<Vec<_>>();
        ctx.alloc_ty_list(tys)
    }

    fn apply_substituted_type_constructor(
        &mut self,
        constructor: HirTyId,
        args: SliceRange<HirTyId>,
    ) -> HirTyId {
        match self.ty(constructor).kind {
            HirTyKind::Named {
                name,
                args: existing_args,
            } => {
                let mut combined = self.ty_ids(existing_args);
                combined.extend(self.ty_ids(args));
                let args = self.alloc_ty_list(combined);
                self.alloc_ty(HirTyKind::Named { name, args })
            }
            _ if self.ty_ids(args).is_empty() => constructor,
            _ => constructor,
        }
    }

    pub fn substitute_effect_row(&mut self, row: &EffectRow, subst: &TypeSubstMap) -> EffectRow {
        let ctx = self;
        let mut items = BTreeSet::new();
        for item in &row.items {
            let _ = items.insert(EffectKey {
                name: item.name.clone(),
                arg: item.arg.map(|arg| ctx.substitute_ty(arg, subst)),
            });
        }
        EffectRow {
            items,
            open: row
                .open
                .as_deref()
                .map(|name| ctx.fresh_open_row_name(name)),
        }
    }
}
