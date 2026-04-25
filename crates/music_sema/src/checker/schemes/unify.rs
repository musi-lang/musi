use std::collections::HashMap;

use music_arena::SliceRange;
use music_hir::{HirDim, HirOrigin, HirTyField, HirTyId, HirTyKind};
use music_module::ModuleKey;
use music_names::Symbol;

use crate::api::{ConstraintAnswer, ConstraintKind, GivenFacts};
use crate::checker::CheckPass;

use super::TypeSubstMap;

impl CheckPass<'_, '_, '_> {
    pub(super) fn given_matches(
        &mut self,
        origin: HirOrigin,
        given: &GivenFacts,
        shape_args: &[HirTyId],
        stack: &mut Vec<String>,
    ) -> bool {
        let ctx = self;
        let Some(subst) = ctx.unify_given_args(&given.type_params, &given.shape_args, shape_args)
        else {
            return false;
        };
        let obligations = given
            .constraints
            .iter()
            .map(|constraint| ctx.instantiate_obligation(constraint, &subst))
            .collect::<Vec<_>>();
        obligations
            .iter()
            .all(|obligation| ctx.solve_obligation(origin, obligation, stack))
    }

    pub(super) fn given_provider_answer(
        &mut self,
        origin: HirOrigin,
        given: &GivenFacts,
        module: ModuleKey,
        shape_args: &[HirTyId],
        stack: &mut Vec<String>,
    ) -> Option<ConstraintAnswer> {
        let subst = self.unify_given_args(&given.type_params, &given.shape_args, shape_args)?;
        let obligations = given
            .constraints
            .iter()
            .map(|constraint| self.instantiate_obligation(constraint, &subst))
            .collect::<Vec<_>>();
        let args = obligations
            .iter()
            .filter(|obligation| matches!(obligation.kind, ConstraintKind::Implements))
            .map(|obligation| self.resolve_obligation_answer(origin, obligation, stack))
            .collect::<Option<Vec<_>>>()?
            .into_boxed_slice();
        Some(ConstraintAnswer::Provider {
            module,
            name: self.given_provider_name(given),
            args,
        })
    }

    pub(super) fn unify_given_args(
        &mut self,
        type_params: &[Symbol],
        pattern_args: &[HirTyId],
        actual_args: &[HirTyId],
    ) -> Option<TypeSubstMap> {
        let ctx = self;
        if pattern_args.len() != actual_args.len() {
            return None;
        }
        let mut subst = HashMap::new();
        for (pattern, actual) in pattern_args
            .iter()
            .copied()
            .zip(actual_args.iter().copied())
        {
            if !ctx.unify_ty(type_params, pattern, actual, &mut subst) {
                return None;
            }
        }
        Some(subst)
    }
}

impl CheckPass<'_, '_, '_> {
    pub(super) fn unify_ty(
        &mut self,
        type_params: &[Symbol],
        pattern: HirTyId,
        actual: HirTyId,
        subst: &mut TypeSubstMap,
    ) -> bool {
        match self.ty(pattern).kind {
            HirTyKind::Named { name, args } if type_params.contains(&name) => {
                self.unify_type_constructor_param(type_params, name, args, actual, subst)
            }
            HirTyKind::Named {
                name: left_name,
                args: left_args,
            } => self.unify_named_ty(type_params, actual, subst, left_name, left_args),
            HirTyKind::Arrow {
                params: left_params,
                ret: left_ret,
                is_effectful: left_effectful,
            } => self.unify_arrow_ty(
                type_params,
                actual,
                subst,
                left_params,
                left_ret,
                left_effectful,
            ),
            HirTyKind::Sum { left, right } => {
                self.unify_sum_ty(type_params, left, right, actual, subst)
            }
            HirTyKind::Tuple { items } => self.unify_tuple_ty(type_params, items, actual, subst),
            HirTyKind::Array { dims, item } => {
                self.unify_array_ty(type_params, dims, item, actual, subst)
            }
            HirTyKind::Mut { inner } => self.unify_mut_ty(type_params, inner, actual, subst),
            HirTyKind::AnyShape { capability } => {
                self.unify_any_shape_ty(type_params, capability, actual, subst)
            }
            HirTyKind::SomeShape { capability } => {
                self.unify_some_shape_ty(type_params, capability, actual, subst)
            }
            HirTyKind::Record { fields } => {
                self.unify_record_ty(type_params, actual, subst, fields)
            }
            _ => self.ty_matches(pattern, actual),
        }
    }

    fn unify_sum_ty(
        &mut self,
        type_params: &[Symbol],
        left: HirTyId,
        right: HirTyId,
        actual: HirTyId,
        subst: &mut TypeSubstMap,
    ) -> bool {
        let HirTyKind::Sum {
            left: actual_left,
            right: actual_right,
        } = self.ty(actual).kind
        else {
            return false;
        };
        self.unify_ty(type_params, left, actual_left, subst)
            && self.unify_ty(type_params, right, actual_right, subst)
    }

    fn unify_tuple_ty(
        &mut self,
        type_params: &[Symbol],
        items: SliceRange<HirTyId>,
        actual: HirTyId,
        subst: &mut TypeSubstMap,
    ) -> bool {
        let HirTyKind::Tuple {
            items: actual_items,
        } = self.ty(actual).kind
        else {
            return false;
        };
        self.unify_ty_lists(type_params, items, actual_items, subst)
    }

    fn unify_array_ty(
        &mut self,
        type_params: &[Symbol],
        dims: SliceRange<HirDim>,
        item: HirTyId,
        actual: HirTyId,
        subst: &mut TypeSubstMap,
    ) -> bool {
        let HirTyKind::Array {
            dims: actual_dims,
            item: actual_item,
        } = self.ty(actual).kind
        else {
            return false;
        };
        self.dims(dims) == self.dims(actual_dims)
            && self.unify_ty(type_params, item, actual_item, subst)
    }

    fn unify_mut_ty(
        &mut self,
        type_params: &[Symbol],
        inner: HirTyId,
        actual: HirTyId,
        subst: &mut TypeSubstMap,
    ) -> bool {
        let HirTyKind::Mut {
            inner: actual_inner,
        } = self.ty(actual).kind
        else {
            return false;
        };
        self.unify_ty(type_params, inner, actual_inner, subst)
    }

    fn unify_any_shape_ty(
        &mut self,
        type_params: &[Symbol],
        shape: HirTyId,
        actual: HirTyId,
        subst: &mut TypeSubstMap,
    ) -> bool {
        let HirTyKind::AnyShape {
            capability: actual_capability,
        } = self.ty(actual).kind
        else {
            return false;
        };
        self.unify_ty(type_params, shape, actual_capability, subst)
    }

    fn unify_some_shape_ty(
        &mut self,
        type_params: &[Symbol],
        shape: HirTyId,
        actual: HirTyId,
        subst: &mut TypeSubstMap,
    ) -> bool {
        let HirTyKind::SomeShape {
            capability: actual_capability,
        } = self.ty(actual).kind
        else {
            return false;
        };
        self.unify_ty(type_params, shape, actual_capability, subst)
    }

    pub(super) fn unify_type_param(
        &self,
        name: Symbol,
        actual: HirTyId,
        subst: &mut TypeSubstMap,
    ) -> bool {
        let ctx = self;
        if let Some(bound) = subst.get(&name).copied() {
            return ctx.ty_matches(bound, actual);
        }
        let _prev = subst.insert(name, actual);
        true
    }

    pub(super) fn unify_type_constructor_param(
        &mut self,
        type_params: &[Symbol],
        name: Symbol,
        args: SliceRange<HirTyId>,
        actual: HirTyId,
        subst: &mut TypeSubstMap,
    ) -> bool {
        let pattern_args = self.ty_ids(args);
        if pattern_args.is_empty() {
            return self.unify_type_param(name, actual, subst);
        }
        let HirTyKind::Named {
            name: actual_name,
            args: actual_args,
        } = self.ty(actual).kind
        else {
            return false;
        };
        let actual_args = self.ty_ids(actual_args);
        if actual_args.len() < pattern_args.len() {
            return false;
        }
        let prefix_len = actual_args.len() - pattern_args.len();
        let constructor_args = self.alloc_ty_list(actual_args[..prefix_len].iter().copied());
        let constructor = self.alloc_ty(HirTyKind::Named {
            name: actual_name,
            args: constructor_args,
        });
        if !self.unify_type_param(name, constructor, subst) {
            return false;
        }
        pattern_args
            .into_iter()
            .zip(actual_args[prefix_len..].iter().copied())
            .all(|(pattern, actual)| self.unify_ty(type_params, pattern, actual, subst))
    }

    fn unify_named_ty(
        &mut self,
        type_params: &[Symbol],
        actual: HirTyId,
        subst: &mut TypeSubstMap,
        left_name: Symbol,
        left_args: SliceRange<HirTyId>,
    ) -> bool {
        let ctx = self;
        let HirTyKind::Named {
            name: right_name,
            args: right_args,
        } = ctx.ty(actual).kind
        else {
            return false;
        };
        left_name == right_name && ctx.unify_ty_lists(type_params, left_args, right_args, subst)
    }

    fn unify_arrow_ty(
        &mut self,
        type_params: &[Symbol],
        actual: HirTyId,
        subst: &mut TypeSubstMap,
        left_params: SliceRange<HirTyId>,
        left_ret: HirTyId,
        left_effectful: bool,
    ) -> bool {
        let ctx = self;
        let HirTyKind::Arrow {
            params: right_params,
            ret: right_ret,
            is_effectful: right_effectful,
        } = ctx.ty(actual).kind
        else {
            return false;
        };
        left_effectful == right_effectful
            && ctx.unify_ty_lists(type_params, left_params, right_params, subst)
            && ctx.unify_ty(type_params, left_ret, right_ret, subst)
    }

    fn unify_record_ty(
        &mut self,
        type_params: &[Symbol],
        actual: HirTyId,
        subst: &mut TypeSubstMap,
        fields: SliceRange<HirTyField>,
    ) -> bool {
        let ctx = self;
        let HirTyKind::Record {
            fields: actual_fields,
        } = ctx.ty(actual).kind
        else {
            return false;
        };
        let pattern_fields = ctx.ty_fields(fields);
        let actual_fields = ctx.ty_fields(actual_fields);
        if pattern_fields.len() != actual_fields.len() {
            return false;
        }
        let actual_fields = actual_fields
            .into_iter()
            .map(|field| (field.name, field.ty))
            .collect::<HashMap<_, _>>();
        pattern_fields.into_iter().all(|field| {
            actual_fields
                .get(&field.name)
                .is_some_and(|ty| ctx.unify_ty(type_params, field.ty, *ty, subst))
        })
    }

    pub(super) fn unify_ty_lists(
        &mut self,
        type_params: &[Symbol],
        pattern: SliceRange<HirTyId>,
        actual: SliceRange<HirTyId>,
        subst: &mut TypeSubstMap,
    ) -> bool {
        let ctx = self;
        let pattern = ctx.ty_ids(pattern);
        let actual = ctx.ty_ids(actual);
        pattern.len() == actual.len()
            && pattern
                .into_iter()
                .zip(actual)
                .all(|(pattern, actual)| ctx.unify_ty(type_params, pattern, actual, subst))
    }
}
