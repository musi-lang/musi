use music_ast::ExprId;
use music_db::Db;
use music_resolve::def::{DefId, DefInfo};
use music_resolve::queries::ResolutionMap;
use music_sema::env::{DispatchInfo, TypeEnv};
use music_sema::types::SemaTypeId;

/// Bundles the complete typed IR for a module: the compiler database,
/// name resolution results, and type-checking environment.
///
/// Consumed by `music_emit` to lower to SEAM bytecode.
pub struct Thir {
    pub db: Db,
    pub resolution: ResolutionMap,
    pub type_env: TypeEnv,
}

impl Thir {
    #[must_use]
    pub const fn new(db: Db, resolution: ResolutionMap, type_env: TypeEnv) -> Self {
        Self {
            db,
            resolution,
            type_env,
        }
    }

    /// Look up the inferred type of an expression node.
    #[must_use]
    pub fn expr_type(&self, expr_id: ExprId) -> Option<SemaTypeId> {
        self.type_env.type_map.get(&expr_id).copied()
    }

    /// Look up the dispatch decision for a call-site expression.
    #[must_use]
    pub fn dispatch(&self, expr_id: ExprId) -> Option<&DispatchInfo> {
        self.type_env.dispatch.get(&expr_id)
    }

    /// Retrieve the definition metadata for a resolved `DefId`.
    ///
    /// # Panics
    ///
    /// Panics if `def_id` is not present in the resolution arena.
    #[must_use]
    pub fn def_info(&self, def_id: DefId) -> &DefInfo {
        self.resolution.defs.get(def_id)
    }
}
