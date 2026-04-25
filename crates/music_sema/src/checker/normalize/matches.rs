use std::collections::BTreeMap;

use music_arena::SliceRange;
use music_hir::{HirTyId, HirTyKind};
use music_names::Symbol;

use crate::checker::PassBase;

use super::HirTyFieldRange;

impl PassBase<'_, '_, '_> {
    pub fn ty_matches(&self, expected: HirTyId, found: HirTyId) -> bool {
        if expected == found {
            return true;
        }
        let left = self.ty(expected).kind;
        let right = self.ty(found).kind;
        if matches!(left, HirTyKind::Any) {
            return true;
        }
        if let HirTyKind::Mut { inner } = right {
            if self.ty_matches(expected, inner) {
                return true;
            }
        }
        if matches!(left, HirTyKind::Error | HirTyKind::Unknown)
            || matches!(right, HirTyKind::Error | HirTyKind::Unknown)
        {
            return true;
        }
        self.ty_matches_kinds(&left, &right)
    }

    pub(super) fn ty_matches_kinds(&self, left: &HirTyKind, right: &HirTyKind) -> bool {
        Self::ty_matches_primitives(left, right)
            || self.ty_matches_named_or_arrow(left, right)
            || self.ty_matches_sums_and_collections(left, right)
            || self.ty_matches_ranges_and_mut(left, right)
            || self.ty_matches_handler_or_record(left, right)
    }

    pub(super) fn ty_matches_primitives(left: &HirTyKind, right: &HirTyKind) -> bool {
        matches!(
            (left, right),
            (HirTyKind::Type, HirTyKind::Type)
                | (HirTyKind::Syntax, HirTyKind::Syntax)
                | (HirTyKind::Any, HirTyKind::Any)
                | (HirTyKind::Empty, HirTyKind::Empty)
                | (HirTyKind::Unit, HirTyKind::Unit)
                | (HirTyKind::Bool, HirTyKind::Bool)
                | (HirTyKind::Nat, HirTyKind::Nat)
                | (HirTyKind::Int, HirTyKind::Int)
                | (HirTyKind::Int8, HirTyKind::Int8)
                | (HirTyKind::Int16, HirTyKind::Int16)
                | (HirTyKind::Int32, HirTyKind::Int32)
                | (HirTyKind::Int64, HirTyKind::Int64)
                | (HirTyKind::Nat8, HirTyKind::Nat8)
                | (HirTyKind::Nat16, HirTyKind::Nat16)
                | (HirTyKind::Nat32, HirTyKind::Nat32)
                | (HirTyKind::Nat64, HirTyKind::Nat64)
                | (HirTyKind::Float, HirTyKind::Float)
                | (HirTyKind::Float32, HirTyKind::Float32)
                | (HirTyKind::Float64, HirTyKind::Float64)
                | (HirTyKind::String, HirTyKind::String)
                | (HirTyKind::CString, HirTyKind::CString)
                | (HirTyKind::CPtr, HirTyKind::CPtr)
        ) || matches!((left, right), (HirTyKind::Bits { width: left }, HirTyKind::Bits { width: right }) if left == right)
    }

    pub(super) fn ty_matches_named_or_arrow(&self, left: &HirTyKind, right: &HirTyKind) -> bool {
        match (left, right) {
            (
                HirTyKind::Named {
                    name: left_name,
                    args: left_args,
                },
                HirTyKind::Named {
                    name: right_name,
                    args: right_args,
                },
            ) => self.named_tys_match(*left_name, *left_args, *right_name, *right_args),
            (
                HirTyKind::Arrow {
                    params: left_params,
                    ret: left_ret,
                    is_effectful: left_effectful,
                },
                HirTyKind::Arrow {
                    params: right_params,
                    ret: right_ret,
                    is_effectful: right_effectful,
                },
            ) => self.arrow_tys_match(
                *left_params,
                *left_ret,
                *left_effectful,
                *right_params,
                *right_ret,
                *right_effectful,
            ),
            _ => false,
        }
    }

    pub(super) fn ty_matches_sums_and_collections(
        &self,
        left: &HirTyKind,
        right: &HirTyKind,
    ) -> bool {
        match (left, right) {
            (
                HirTyKind::Sum { left, right },
                HirTyKind::Sum {
                    left: other_left,
                    right: other_right,
                },
            ) => self.ty_matches(*left, *other_left) && self.ty_matches(*right, *other_right),
            (HirTyKind::Tuple { items: left }, HirTyKind::Tuple { items: right }) => {
                self.list_tys_match(*left, *right)
            }
            (
                HirTyKind::Array {
                    dims: left_dims,
                    item: left_item,
                },
                HirTyKind::Array {
                    dims: right_dims,
                    item: right_item,
                },
            ) => {
                self.dims(left_dims.clone()) == self.dims(right_dims.clone())
                    && self.ty_matches(*left_item, *right_item)
            }
            (
                HirTyKind::Seq { item: left_item },
                HirTyKind::Array {
                    item: right_item, ..
                },
            ) => self.ty_matches(*left_item, *right_item),
            _ => false,
        }
    }

    pub(super) fn ty_matches_ranges_and_mut(&self, left: &HirTyKind, right: &HirTyKind) -> bool {
        match (left, right) {
            (HirTyKind::Seq { item: left }, HirTyKind::Seq { item: right })
            | (HirTyKind::Range { bound: left }, HirTyKind::Range { bound: right })
            | (HirTyKind::Mut { inner: left }, HirTyKind::Mut { inner: right })
            | (
                HirTyKind::AnyShape { capability: left },
                HirTyKind::AnyShape { capability: right },
            )
            | (
                HirTyKind::SomeShape { capability: left },
                HirTyKind::SomeShape { capability: right },
            ) => self.ty_matches(*left, *right),
            _ => false,
        }
    }

    pub(super) fn ty_matches_handler_or_record(&self, left: &HirTyKind, right: &HirTyKind) -> bool {
        match (left, right) {
            (
                HirTyKind::Handler {
                    effect: left_effect,
                    input: left_input,
                    output: left_output,
                },
                HirTyKind::Handler {
                    effect: right_effect,
                    input: right_input,
                    output: right_output,
                },
            ) => {
                self.ty_matches(*left_effect, *right_effect)
                    && self.ty_matches(*left_input, *right_input)
                    && self.ty_matches(*left_output, *right_output)
            }
            (HirTyKind::Record { fields: left }, HirTyKind::Record { fields: right }) => {
                self.record_tys_match(left.clone(), right.clone())
            }
            _ => false,
        }
    }

    pub(super) fn list_tys_match(
        &self,
        left: SliceRange<HirTyId>,
        right: SliceRange<HirTyId>,
    ) -> bool {
        let left = self.ty_ids(left);
        let right = self.ty_ids(right);
        left.len() == right.len()
            && left
                .into_iter()
                .zip(right)
                .all(|(left, right)| self.ty_matches(left, right))
    }

    pub(super) fn named_tys_match(
        &self,
        left_name: Symbol,
        left_args: SliceRange<HirTyId>,
        right_name: Symbol,
        right_args: SliceRange<HirTyId>,
    ) -> bool {
        left_name == right_name && self.list_tys_match(left_args, right_args)
    }

    pub(super) fn arrow_tys_match(
        &self,
        left_params: SliceRange<HirTyId>,
        left_ret: HirTyId,
        left_effectful: bool,
        right_params: SliceRange<HirTyId>,
        right_ret: HirTyId,
        right_effectful: bool,
    ) -> bool {
        left_effectful == right_effectful
            && self.list_tys_match(left_params, right_params)
            && self.ty_matches(left_ret, right_ret)
    }

    pub(super) fn record_tys_match(&self, left: HirTyFieldRange, right: HirTyFieldRange) -> bool {
        let left_fields = self.ty_fields(left);
        let right_fields = self.ty_fields(right);
        let right_map = right_fields
            .into_iter()
            .map(|field| (field.name, field.ty))
            .collect::<BTreeMap<_, _>>();
        left_fields.len() == right_map.len()
            && left_fields.into_iter().all(|field| {
                right_map
                    .get(&field.name)
                    .is_some_and(|right_ty| self.ty_matches(field.ty, *right_ty))
            })
    }
}
