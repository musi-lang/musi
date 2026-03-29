use music_storage::Arena;

use super::*;

/// Arena storage for all HIR nodes in a module.
#[derive(Debug, Default)]
pub struct HirStore {
    pub exprs: Arena<HirExpr>,
    pub pats: Arena<HirPat>,
    pub tys: Arena<HirTy>,
    pub attrs: Arena<HirAttr>,
    pub splices: Arena<HirSplice>,
}

impl HirStore {
    #[must_use]
    pub const fn new() -> Self {
        Self {
            exprs: Arena::new(),
            pats: Arena::new(),
            tys: Arena::new(),
            attrs: Arena::new(),
            splices: Arena::new(),
        }
    }
}

/// Typed representation of a single module after semantic analysis.
#[derive(Debug)]
pub struct HirModule {
    pub store: HirStore,
    pub root: HirExprId,
}

impl HirModule {
    #[must_use]
    pub const fn new(store: HirStore, root: HirExprId) -> Self {
        Self { store, root }
    }
}

#[cfg(test)]
#[allow(clippy::unwrap_used, clippy::panic)]
mod tests;
