use music_ast::ExprId;
use music_db::Db;
use music_shared::Symbol;
use music_resolve::def::{DefId, DefInfo};
use music_resolve::queries::ResolutionMap;
use music_sema::env::{DispatchInfo, TypeEnv, VariantInfo};
use music_sema::types::SemaTypeId;

/// Bundles the complete typed IR for a module: the compiler database,
/// name resolution results, and type-checking environment.
///
/// Consumed by `music_emit` to lower to SEAM bytecode.
pub struct HirBundle {
    pub db: Db,
    pub resolution: ResolutionMap,
    pub type_env: TypeEnv,
}

impl HirBundle {
    #[must_use]
    pub const fn new(db: Db, resolution: ResolutionMap, type_env: TypeEnv) -> Self {
        Self {
            db,
            resolution,
            type_env,
        }
    }

    #[must_use]
    pub fn expr_type(&self, expr_id: ExprId) -> Option<SemaTypeId> {
        self.type_env.type_map.get(&expr_id).copied()
    }

    #[must_use]
    pub fn dispatch(&self, expr_id: ExprId) -> Option<&DispatchInfo> {
        self.type_env.dispatch.get(&expr_id)
    }

    #[must_use]
    pub fn variant_info(&self, expr_id: ExprId) -> Option<&VariantInfo> {
        self.type_env.variant_info.get(&expr_id)
    }

    /// Returns the stable class ID for a type class by name, if assigned.
    #[must_use]
    pub fn class_id(&self, name: Symbol) -> Option<u16> {
        self.type_env.class_ids.get(&name).copied()
    }

    /// # Panics
    ///
    /// Panics if `def_id` is not present in the resolution arena.
    #[must_use]
    pub fn def_info(&self, def_id: DefId) -> &DefInfo {
        self.resolution.defs.get(def_id)
    }
}
