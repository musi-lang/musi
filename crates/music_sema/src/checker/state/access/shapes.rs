use std::collections::HashMap;

use music_hir::HirExprId;
use music_names::Symbol;

use crate::api::{GivenFacts, ShapeFacts};

use crate::checker::state::PassBase;

impl PassBase<'_, '_, '_> {
    pub fn shape_id(&self, symbol: Symbol) -> Option<HirExprId> {
        self.decls.shape_index.get(&symbol).copied()
    }

    pub fn insert_shape_id(&mut self, symbol: Symbol, id: HirExprId) {
        let _prev = self.decls.shape_index.insert(symbol, id);
    }

    pub fn insert_shape_facts(&mut self, id: HirExprId, facts: ShapeFacts) {
        let _prev = self.decls.shape_facts.insert(id, facts);
    }

    pub fn insert_shape_facts_by_name(&mut self, name: Symbol, facts: ShapeFacts) {
        let _prev = self.decls.shape_facts_by_name.insert(name, facts);
    }

    pub fn shape_facts(&self, id: HirExprId) -> Option<&ShapeFacts> {
        self.decls.shape_facts.get(&id)
    }

    pub fn shape_facts_by_name(&self, name: Symbol) -> Option<&ShapeFacts> {
        self.decls.shape_facts_by_name.get(&name)
    }

    pub fn insert_given_facts(&mut self, id: HirExprId, facts: GivenFacts) {
        let _prev = self.decls.given_facts.insert(id, facts);
    }

    pub const fn given_facts(&self) -> &HashMap<HirExprId, GivenFacts> {
        &self.decls.given_facts
    }
}
