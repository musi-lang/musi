use std::collections::HashMap;
use std::path::PathBuf;

use music_db::Db;
use music_resolve::def::{DefId, DefInfo};
use music_resolve::{ModuleGraph, ModuleId, ModuleLoader, ResolutionMap};
use music_sema::{DispatchInfo, SemaTypeId, TypeEnv, VariantInfo};
use music_shared::Symbol;
use music_shared::diag::Diag;

use music_ast::ExprId;

/// The complete typed state for one module.
///
/// This is the canonical handoff from resolution/sema into later compiler
/// stages. It owns the module database, resolution results, and type facts.
pub struct TypedModule {
    pub db: Db,
    pub resolution: ResolutionMap,
    pub type_env: TypeEnv,
    pub diagnostics: Vec<Diag>,
    pub has_errors: bool,
    pub module_id: Option<ModuleId>,
    pub path: PathBuf,
}

/// The complete typed state for a resolved project.
pub struct TypedProject {
    pub loader: ModuleLoader,
    pub graph: ModuleGraph,
    pub modules: HashMap<ModuleId, TypedModule>,
    pub order: Vec<ModuleId>,
}

impl TypedModule {
    #[must_use]
    pub fn new(db: Db, resolution: ResolutionMap, type_env: TypeEnv) -> Self {
        Self {
            db,
            resolution,
            type_env,
            diagnostics: Vec::new(),
            has_errors: false,
            module_id: None,
            path: PathBuf::new(),
        }
    }

    #[must_use]
    pub fn with_status(
        db: Db,
        resolution: ResolutionMap,
        type_env: TypeEnv,
        diagnostics: Vec<Diag>,
        has_errors: bool,
        module_id: Option<ModuleId>,
        path: PathBuf,
    ) -> Self {
        let mut module = Self::new(db, resolution, type_env);
        module.diagnostics = diagnostics;
        module.has_errors = has_errors;
        module.module_id = module_id;
        module.path = path;
        module
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
