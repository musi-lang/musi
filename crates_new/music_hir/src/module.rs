use music_arena::{Arena, Idx, SliceArena, SliceRange};
use music_base::SourceId;

use crate::expr::{
    HirArg, HirArrayItem, HirCaseArm, HirExpr, HirLit, HirRecordItem, HirTemplatePart,
};
use crate::pat::{HirPat, HirRecordPatField};
use crate::ty::{HirDim, HirTy};

pub type HirExprId = Idx<HirExpr>;
pub type HirPatId = Idx<HirPat>;
pub type HirTyId = Idx<HirTy>;
pub type HirLitId = Idx<HirLit>;

#[derive(Debug)]
pub struct HirStore {
    pub exprs: Arena<HirExpr>,
    pub pats: Arena<HirPat>,
    pub tys: Arena<HirTy>,
    pub lits: Arena<HirLit>,

    pub expr_ids: SliceArena<HirExprId>,
    pub pat_ids: SliceArena<HirPatId>,
    pub ty_ids: SliceArena<HirTyId>,
    pub args: SliceArena<HirArg>,
    pub array_items: SliceArena<HirArrayItem>,
    pub record_items: SliceArena<HirRecordItem>,
    pub record_pat_fields: SliceArena<HirRecordPatField>,
    pub template_parts: SliceArena<HirTemplatePart>,
    pub case_arms: SliceArena<HirCaseArm>,
    pub dims: SliceArena<HirDim>,
}

impl HirStore {
    #[must_use]
    pub const fn new() -> Self {
        Self {
            exprs: Arena::new(),
            pats: Arena::new(),
            tys: Arena::new(),
            lits: Arena::new(),
            expr_ids: SliceArena::new(),
            pat_ids: SliceArena::new(),
            ty_ids: SliceArena::new(),
            args: SliceArena::new(),
            array_items: SliceArena::new(),
            record_items: SliceArena::new(),
            record_pat_fields: SliceArena::new(),
            template_parts: SliceArena::new(),
            case_arms: SliceArena::new(),
            dims: SliceArena::new(),
        }
    }

    #[must_use]
    pub fn alloc_expr(&mut self, expr: HirExpr) -> HirExprId {
        self.exprs.alloc(expr)
    }

    #[must_use]
    pub fn alloc_pat(&mut self, pat: HirPat) -> HirPatId {
        self.pats.alloc(pat)
    }

    #[must_use]
    pub fn alloc_ty(&mut self, ty: HirTy) -> HirTyId {
        self.tys.alloc(ty)
    }

    #[must_use]
    pub fn alloc_lit(&mut self, lit: HirLit) -> HirLitId {
        self.lits.alloc(lit)
    }

    #[must_use]
    pub fn alloc_expr_list<I>(&mut self, exprs: I) -> SliceRange<HirExprId>
    where
        I: IntoIterator<Item = HirExprId>,
    {
        self.expr_ids.alloc_from_iter(exprs)
    }

    #[must_use]
    pub fn alloc_pat_list<I>(&mut self, pats: I) -> SliceRange<HirPatId>
    where
        I: IntoIterator<Item = HirPatId>,
    {
        self.pat_ids.alloc_from_iter(pats)
    }
}

impl Default for HirStore {
    fn default() -> Self {
        Self::new()
    }
}

#[derive(Debug)]
pub struct HirModule {
    pub source_id: SourceId,
    pub store: HirStore,
    pub root: HirExprId,
}

impl HirModule {
    #[must_use]
    pub const fn new(source_id: SourceId, store: HirStore, root: HirExprId) -> Self {
        Self {
            source_id,
            store,
            root,
        }
    }
}

#[cfg(test)]
#[allow(clippy::unwrap_used, clippy::panic)]
mod tests;
