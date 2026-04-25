use music_arena::{Idx, SliceRange};
use music_hir::{
    HirArg, HirArrayItem, HirAttr, HirAttrArg, HirBinder, HirConstraint, HirDim, HirEffectSet,
    HirExpr, HirExprId, HirFieldDef, HirHandleClause, HirLit, HirLitId, HirMatchArm, HirMemberDef,
    HirOrigin, HirParam, HirPat, HirPatId, HirRecordItem, HirRecordPatField, HirTy, HirTyField,
    HirTyId, HirTyKind, HirVariantDef, HirVariantFieldDef, HirVariantPatArg,
};
use music_names::Ident;

use crate::checker::state::aliases::{
    ArgList, ArrayItemList, AttrArgList, AttrList, BinderList, ConstraintList, DimList,
    EffectItemList, ExprIdList, FieldDefList, HandleClauseList, IdentList, MatchArmList,
    MemberDefList, ParamList, PatIdList, RecordItemList, RecordPatFieldList, TyFieldList, TyIdList,
    VariantDefList, VariantFieldDefList, VariantPatArgList,
};

use crate::checker::state::PassBase;

impl PassBase<'_, '_, '_> {
    pub fn expr(&self, id: HirExprId) -> HirExpr {
        self.module.resolved.module.store.exprs.get(id).clone()
    }

    pub fn pat(&self, id: HirPatId) -> HirPat {
        self.module.resolved.module.store.pats.get(id).clone()
    }

    pub fn ty(&self, id: HirTyId) -> HirTy {
        self.module.resolved.module.store.tys.get(id).clone()
    }

    pub fn lit(&self, id: HirLitId) -> HirLit {
        self.module.resolved.module.store.lits.get(id).clone()
    }
}

impl PassBase<'_, '_, '_> {
    pub fn expr_ids(&self, range: SliceRange<HirExprId>) -> ExprIdList {
        self.module
            .resolved
            .module
            .store
            .expr_ids
            .get(range)
            .to_vec()
    }

    pub fn args(&self, range: SliceRange<HirArg>) -> ArgList {
        self.module.resolved.module.store.args.get(range).to_vec()
    }

    pub fn dims(&self, range: SliceRange<HirDim>) -> DimList {
        self.module.resolved.module.store.dims.get(range).to_vec()
    }

    pub fn ty_ids(&self, range: SliceRange<HirTyId>) -> TyIdList {
        self.module.resolved.module.store.ty_ids.get(range).to_vec()
    }

    pub fn ty_fields(&self, range: SliceRange<HirTyField>) -> TyFieldList {
        self.module
            .resolved
            .module
            .store
            .ty_fields
            .get(range)
            .to_vec()
    }

    pub fn array_items(&self, range: SliceRange<HirArrayItem>) -> ArrayItemList {
        self.module
            .resolved
            .module
            .store
            .array_items
            .get(range)
            .to_vec()
    }

    pub fn record_items(&self, range: SliceRange<HirRecordItem>) -> RecordItemList {
        self.module
            .resolved
            .module
            .store
            .record_items
            .get(range)
            .to_vec()
    }
}

impl PassBase<'_, '_, '_> {
    pub fn params(&self, range: SliceRange<HirParam>) -> ParamList {
        self.module.resolved.module.store.params.get(range).to_vec()
    }

    pub fn attrs(&self, range: SliceRange<HirAttr>) -> AttrList {
        self.module.resolved.module.store.attrs.get(range).to_vec()
    }

    pub fn attr_args(&self, range: SliceRange<HirAttrArg>) -> AttrArgList {
        self.module
            .resolved
            .module
            .store
            .attr_args
            .get(range)
            .to_vec()
    }

    pub fn members(&self, range: SliceRange<HirMemberDef>) -> MemberDefList {
        self.module
            .resolved
            .module
            .store
            .members
            .get(range)
            .to_vec()
    }

    pub fn handle_clauses(&self, range: SliceRange<HirHandleClause>) -> HandleClauseList {
        self.module
            .resolved
            .module
            .store
            .handle_clauses
            .get(range)
            .to_vec()
    }

    pub fn match_arms(&self, range: SliceRange<HirMatchArm>) -> MatchArmList {
        self.module
            .resolved
            .module
            .store
            .match_arms
            .get(range)
            .to_vec()
    }

    pub fn constraints(&self, range: SliceRange<HirConstraint>) -> ConstraintList {
        self.module
            .resolved
            .module
            .store
            .constraints
            .get(range)
            .to_vec()
    }

    pub fn variants(&self, range: SliceRange<HirVariantDef>) -> VariantDefList {
        self.module
            .resolved
            .module
            .store
            .variants
            .get(range)
            .to_vec()
    }

    pub fn variant_fields(&self, range: SliceRange<HirVariantFieldDef>) -> VariantFieldDefList {
        self.module
            .resolved
            .module
            .store
            .variant_fields
            .get(range)
            .to_vec()
    }

    pub fn fields(&self, range: SliceRange<HirFieldDef>) -> FieldDefList {
        self.module.resolved.module.store.fields.get(range).to_vec()
    }

    pub fn effect_items(&self, set: &HirEffectSet) -> EffectItemList {
        self.module
            .resolved
            .module
            .store
            .effect_items
            .get(set.items.clone())
            .to_vec()
    }
}

impl PassBase<'_, '_, '_> {
    pub fn pat_ids(&self, range: SliceRange<HirPatId>) -> PatIdList {
        self.module
            .resolved
            .module
            .store
            .pat_ids
            .get(range)
            .to_vec()
    }

    pub fn record_pat_fields(&self, range: SliceRange<HirRecordPatField>) -> RecordPatFieldList {
        self.module
            .resolved
            .module
            .store
            .record_pat_fields
            .get(range)
            .to_vec()
    }

    pub fn variant_pat_args(&self, range: SliceRange<HirVariantPatArg>) -> VariantPatArgList {
        self.module
            .resolved
            .module
            .store
            .variant_pat_args
            .get(range)
            .to_vec()
    }

    pub fn idents(&self, range: SliceRange<Ident>) -> IdentList {
        self.module.resolved.module.store.idents.get(range).to_vec()
    }

    pub fn binders(&self, range: SliceRange<HirBinder>) -> BinderList {
        self.module
            .resolved
            .module
            .store
            .binders
            .get(range)
            .to_vec()
    }
}

impl PassBase<'_, '_, '_> {
    pub fn alloc_dims<I>(&mut self, dims: I) -> SliceRange<HirDim>
    where
        I: IntoIterator<Item = HirDim>,
    {
        self.module.resolved.module.store.dims.alloc_from_iter(dims)
    }

    pub fn alloc_ty(&mut self, kind: HirTyKind) -> HirTyId {
        self.module
            .resolved
            .module
            .store
            .alloc_ty(HirTy::new(HirOrigin::dummy(), kind))
    }

    pub fn alloc_ty_list<I>(&mut self, tys: I) -> SliceRange<HirTyId>
    where
        I: IntoIterator<Item = HirTyId>,
    {
        self.module.resolved.module.store.alloc_ty_list(tys)
    }

    pub fn alloc_ty_fields<I>(&mut self, fields: I) -> SliceRange<HirTyField>
    where
        I: IntoIterator<Item = HirTyField>,
    {
        self.module
            .resolved
            .module
            .store
            .alloc_ty_field_list(fields)
    }
}

pub(super) fn idx_to_usize<T>(idx: Idx<T>) -> usize {
    usize::try_from(idx.raw()).unwrap_or(usize::MAX)
}
