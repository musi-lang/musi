mod expr;
mod items;
mod patterns;
mod seed;
mod types;

use std::collections::HashMap;
use std::path::PathBuf;

use music_arena::Arena;
use music_ast::common::{Constraint, MemberDecl, ModifierSet, Param};
use music_ast::expr::{
    CaseArm, CompClause, DataBody, ExprKind, FStrPart, HandlerClause, ImportKind, InstanceBody,
    LetBinding, PwGuard, QuoteKind, RecordField, SpliceKind,
};
use music_ast::pat::PatKind;
use music_ast::ty::TyKind;
use music_ast::{ExprId, ExprList, ParamList, PatId, TyId};
use music_db::Db;
use music_owned::prelude::{PRELUDE_CLASSES, PRELUDE_MODULE_NAME};
use music_owned::types::BuiltinType;
use music_shared::{Ident, Span, Symbol, SymbolList};

use crate::def::{DefId, DefInfo, DefKind, Visibility};
use crate::errors::ResolveError;
use crate::graph::ModuleGraph;
use crate::loader::ModuleLoader;
use crate::scope::{ScopeArena, ScopeId, ScopeKind};

/// Stores all resolution results: definitions, scope chains, and
/// per-node resolution mappings.
pub struct ResolutionMap {
    pub defs: Arena<DefInfo>,
    pub expr_res: HashMap<ExprId, DefId>,
    pub ty_res: HashMap<TyId, DefId>,
    pub pat_variant_res: HashMap<PatId, DefId>,
    pub imported_effect_modules: HashMap<Symbol, String>,
    pub scopes: ScopeArena,
    pub captures: HashMap<ExprId, SymbolList>,
}

impl ResolutionMap {
    #[must_use]
    pub fn new() -> Self {
        Self::with_capacity(0)
    }

    #[must_use]
    pub fn with_capacity(hint: usize) -> Self {
        Self {
            defs: Arena::with_capacity(hint),
            expr_res: HashMap::with_capacity(hint),
            ty_res: HashMap::with_capacity(hint / 2),
            pat_variant_res: HashMap::new(),
            imported_effect_modules: HashMap::new(),
            scopes: ScopeArena::new(),
            captures: HashMap::new(),
        }
    }
}

impl Default for ResolutionMap {
    fn default() -> Self {
        Self::new()
    }
}

/// Wraps `Db` with resolution state. Owns the `Db` during resolution
/// and returns it via `finish()`.
pub struct ResolveDb {
    pub db: Db,
    pub resolution: ResolutionMap,
    pub errors: Vec<ResolveError>,
    module_scope: ScopeId,
    loader: ModuleLoader,
    graph: ModuleGraph,
    current_file: PathBuf,
    current_lambda: Option<ExprId>,
}

impl ResolveDb {
    #[must_use]
    pub fn new(db: Db, root: PathBuf) -> Self {
        let hint = db.ast.root.len();
        let mut resolution = ResolutionMap::with_capacity(hint);
        let module_scope = resolution.scopes.push(ScopeKind::Module, None);
        let loader = ModuleLoader::new(root);
        Self {
            db,
            resolution,
            errors: Vec::new(),
            module_scope,
            loader,
            graph: ModuleGraph::new(),
            current_file: PathBuf::new(),
            current_lambda: None,
        }
    }

    #[must_use]
    pub fn with_graph(
        db: Db,
        loader: ModuleLoader,
        graph: ModuleGraph,
        current_file: PathBuf,
    ) -> Self {
        let hint = db.ast.root.len();
        let mut resolution = ResolutionMap::with_capacity(hint);
        let module_scope = resolution.scopes.push(ScopeKind::Module, None);
        Self {
            db,
            resolution,
            errors: Vec::new(),
            module_scope,
            loader,
            graph,
            current_file,
            current_lambda: None,
        }
    }

    pub fn set_current_file(&mut self, path: PathBuf) {
        self.current_file = path;
    }

    #[must_use]
    pub const fn loader_mut(&mut self) -> &mut ModuleLoader {
        &mut self.loader
    }

    #[must_use]
    pub fn take_graph(self) -> ModuleGraph {
        self.graph
    }

    pub fn resolve_module(&mut self) {
        let root = self.db.ast.root.clone();
        for &expr_id in &root {
            self.resolve_top_level(expr_id);
        }
    }

    #[must_use]
    pub fn finish(self) -> (Db, ResolutionMap, Vec<ResolveError>) {
        (self.db, self.resolution, self.errors)
    }

    #[must_use]
    pub fn finish_with_graph(self) -> (Db, ResolutionMap, Vec<ResolveError>, ModuleGraph) {
        (self.db, self.resolution, self.errors, self.graph)
    }
}
