use musi_ast::{ExprId, PatId, TyExprId};

use crate::symbol::SymbolId;
use crate::ty_repr::TyRepr;
use crate::types::{OptSymbolId, OptSymbolIds, OptTyReprs};

#[derive(Debug)]
pub struct SemanticModel {
    expr_types: OptTyReprs,
    expr_symbols: OptSymbolIds,
    pat_types: OptTyReprs,
    pat_symbols: OptSymbolIds,
    ty_expr_types: OptTyReprs,
}

impl SemanticModel {
    #[must_use]
    pub fn new(expr_count: usize, pat_count: usize, ty_expr_count: usize) -> Self {
        Self {
            expr_types: vec![None; expr_count],
            expr_symbols: vec![None; expr_count],
            pat_types: vec![None; pat_count],
            pat_symbols: vec![None; pat_count],
            ty_expr_types: vec![None; ty_expr_count],
        }
    }

    #[must_use]
    pub fn type_of_expr(&self, id: ExprId) -> Option<&TyRepr> {
        self.expr_types.get(id.as_usize()).and_then(|t| t.as_ref())
    }

    #[must_use]
    pub fn symbol_of_expr(&self, id: ExprId) -> OptSymbolId {
        self.expr_symbols.get(id.as_usize()).copied().flatten()
    }

    pub fn set_expr_type(&mut self, id: ExprId, ty: TyRepr) {
        let idx = id.as_usize();
        if idx < self.expr_types.len() {
            self.expr_types[idx] = Some(ty);
        }
    }

    pub fn set_expr_symbol(&mut self, id: ExprId, symbol: SymbolId) {
        let idx = id.as_usize();
        if idx < self.expr_symbols.len() {
            self.expr_symbols[idx] = Some(symbol);
        }
    }

    #[must_use]
    pub fn type_of_pat(&self, id: PatId) -> Option<&TyRepr> {
        self.pat_types.get(id.as_usize()).and_then(|t| t.as_ref())
    }

    #[must_use]
    pub fn symbol_of_pat(&self, id: PatId) -> OptSymbolId {
        self.pat_symbols.get(id.as_usize()).copied().flatten()
    }

    pub fn set_pat_type(&mut self, id: PatId, ty: TyRepr) {
        let idx = id.as_usize();
        if idx < self.pat_types.len() {
            self.pat_types[idx] = Some(ty);
        }
    }

    pub fn set_pat_symbol(&mut self, id: PatId, symbol: SymbolId) {
        let idx = id.as_usize();
        if idx < self.pat_symbols.len() {
            self.pat_symbols[idx] = Some(symbol);
        }
    }

    #[must_use]
    pub fn type_of_ty_expr(&self, id: TyExprId) -> Option<&TyRepr> {
        self.ty_expr_types
            .get(id.as_usize())
            .and_then(|t| t.as_ref())
    }

    pub fn set_ty_expr_type(&mut self, id: TyExprId, ty: TyRepr) {
        let idx = id.as_usize();
        if idx < self.ty_expr_types.len() {
            self.ty_expr_types[idx] = Some(ty);
        }
    }
}
