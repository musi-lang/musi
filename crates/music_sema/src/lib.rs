//! Semantic analysis for the Musi compiler.
//!
//! This crate performs two passes over a parsed module:
//!
//! 1. **Name resolution** — binds every identifier to a [`DefId`], reports
//!    undefined names and duplicate definitions.
//!
//! 2. **Type checking** — bidirectional inference with unification variables.
//!    Synthesises types bottom-up and checks them top-down; emits diagnostics
//!    for type mismatches, arity errors, and effect violations.
//!
//! # Entry point
//!
//! ```ignore
//! let result = music_sema::analyze(&module, &mut interner, file_id, &mut diags);
//! ```

pub mod checker;
pub mod def;
pub mod error;
pub mod exports;
pub mod resolve;
pub mod scope;
pub mod types;
pub mod unify;
pub mod well_known;

pub use checker::{CheckContext, Checker, CheckerResult};
pub use def::{DefId, DefInfo, DefKind, DefTable, DefTyInfo};
pub use error::SemaError;
pub use resolve::ResolveOutput;
pub use scope::{ScopeId, ScopeTree};
pub use types::{EffectRow, InstanceInfo, Obligation, TyVarId, Type, TypeIdx};
pub use unify::UnifyTable;
pub use well_known::WellKnown;

pub use exports::{ExportBinding, ModuleExports, collect_exports, exports_to_record_type};
pub use resolve::ImportNames;

use std::collections::HashMap;
use std::mem;

use music_ast::{ExprIdx, ParsedModule};
use music_shared::{Arena, DiagnosticBag, FileId, Interner, Span, Symbol};

/// Maps from AST nodes to their resolved definitions.
pub struct ResolutionMap {
    /// Maps each identifier expression to the definition it refers to.
    pub expr_defs: HashMap<ExprIdx, DefId>,
    /// Maps each binding-site span to its [`DefId`].
    pub pat_defs: HashMap<Span, DefId>,
    /// Maps law span → inferred (implicit) law variables, for LSP inlay hints.
    pub law_inferred_vars: HashMap<Span, Vec<(Symbol, DefId)>>,
}

/// The complete result of semantic analysis of a single module.
pub struct SemaResult {
    /// All definitions encountered (index = `DefId.0`).
    pub defs: Vec<DefInfo>,
    /// Name resolution maps.
    pub resolution: ResolutionMap,
    /// The inferred type of each expression node.
    pub expr_types: HashMap<ExprIdx, TypeIdx>,
    /// The type arena.
    pub types: Arena<Type>,
    /// The unification table (retained so callers can freeze types).
    pub unify: UnifyTable,
    /// Typeclass instances discovered during analysis.
    pub instances: Vec<InstanceInfo>,
    /// Well-known prelude type definitions (needed by IR lowering).
    pub well_known: WellKnown,
}

/// Prelude data passed into analysis.
pub struct Prelude {
    pub well_known: WellKnown,
    pub defs: Vec<DefInfo>,
    pub instances: Vec<InstanceInfo>,
}

/// Shared mutable state for multi-module analysis.
///
/// Owns the type arena, unification table, definition table, scope tree,
/// and well-known prelude — all shared across modules.
pub struct SharedAnalysisState {
    pub types: Arena<Type>,
    pub unify: UnifyTable,
    pub defs: DefTable,
    pub scopes: ScopeTree,
    pub well_known: WellKnown,
    pub prelude_scope: ScopeId,
}

/// Per-module analysis output (does not own shared state).
pub struct ModuleSemaOutput {
    pub module_scope: ScopeId,
    pub resolution: ResolutionMap,
    pub expr_types: HashMap<ExprIdx, TypeIdx>,
    pub instances: Vec<InstanceInfo>,
}

impl SharedAnalysisState {
    /// Creates a new shared state with prelude scope and well-known definitions.
    #[must_use]
    pub fn new(interner: &mut Interner) -> Self {
        let mut defs = DefTable::new();
        let mut scopes = ScopeTree::new();
        let prelude_scope = scopes.push_root();
        let well_known =
            well_known::init_well_known(interner, &mut defs, prelude_scope, &mut scopes);
        let mut types = Arena::new();
        well_known::assign_well_known_types(&mut defs, &well_known, &mut types);
        Self {
            types,
            unify: UnifyTable::new(),
            defs,
            scopes,
            well_known,
            prelude_scope,
        }
    }

    /// Converts the shared state and a per-module output into a [`SemaResult`].
    #[must_use]
    pub fn into_sema_result(self, output: ModuleSemaOutput) -> SemaResult {
        SemaResult {
            defs: self.defs.into_vec(),
            resolution: output.resolution,
            expr_types: output.expr_types,
            types: self.types,
            unify: self.unify,
            instances: output.instances,
            well_known: self.well_known,
        }
    }
}

/// Runs analysis for a single module using shared cross-module state.
#[allow(clippy::implicit_hasher)]
pub fn analyze_shared(
    module: &ParsedModule,
    state: &mut SharedAnalysisState,
    interner: &mut Interner,
    file_id: FileId,
    diags: &mut DiagnosticBag,
    import_names: &ImportNames,
) -> ModuleSemaOutput {
    let module_scope = state.scopes.push_child(state.prelude_scope);

    let resolved = resolve::resolve_with_imports(
        module,
        interner,
        file_id,
        diags,
        &mut state.defs,
        &mut state.scopes,
        module_scope,
        import_names,
    );

    let ctx = CheckContext {
        ast: &module.arenas,
        interner,
        file_id,
        well_known: &state.well_known,
        expr_defs: &resolved.expr_defs,
        pat_defs: &resolved.pat_defs,
        import_types: &HashMap::new(),
        law_inferred_vars: &resolved.law_inferred_vars,
    };

    let mut checker = Checker::new_with_state(
        ctx,
        diags,
        &mut state.defs,
        &mut state.scopes,
        module_scope,
        mem::take(&mut state.types),
        mem::take(&mut state.unify),
    );

    for stmt in &module.stmts {
        let _ty = checker.synth(stmt.expr);
    }

    checker.resolve_obligations();
    let result = checker.finish();

    state.types = result.types;
    state.unify = result.unify;

    analyze_emit_unused_warnings(&state.defs, interner, file_id, diags);

    ModuleSemaOutput {
        module_scope,
        resolution: ResolutionMap {
            expr_defs: resolved.expr_defs,
            pat_defs: resolved.pat_defs,
            law_inferred_vars: resolved.law_inferred_vars,
        },
        expr_types: result.expr_types,
        instances: result.instances,
    }
}

/// Runs name resolution and type checking on `module`.
///
/// Diagnostics (errors and warnings) are pushed into `diags`. The caller
/// should check `diags.has_errors()` after this call.
pub fn analyze(
    module: &ParsedModule,
    interner: &mut Interner,
    file_id: FileId,
    diags: &mut DiagnosticBag,
) -> SemaResult {
    let empty_imports = HashMap::new();
    analyze_with_imports(module, interner, file_id, diags, &empty_imports)
}

/// Like [`analyze`], but with pre-computed import types for cross-module resolution.
#[allow(clippy::implicit_hasher)]
pub fn analyze_with_imports(
    module: &ParsedModule,
    interner: &mut Interner,
    file_id: FileId,
    diags: &mut DiagnosticBag,
    import_types: &HashMap<Symbol, TypeIdx>,
) -> SemaResult {
    let (mut defs, well_known, mut scopes, module_scope, resolved, types) =
        analyze_setup(module, interner, file_id, diags);

    let ctx = CheckContext {
        ast: &module.arenas,
        interner,
        file_id,
        well_known: &well_known,
        expr_defs: &resolved.expr_defs,
        pat_defs: &resolved.pat_defs,
        import_types,
        law_inferred_vars: &resolved.law_inferred_vars,
    };
    let mut checker = Checker::new_with_state(
        ctx,
        diags,
        &mut defs,
        &mut scopes,
        module_scope,
        types,
        UnifyTable::new(),
    );

    for stmt in &module.stmts {
        let _ty = checker.synth(stmt.expr);
    }

    checker.resolve_obligations();
    let result = checker.finish();

    analyze_emit_unused_warnings(&defs, interner, file_id, diags);

    SemaResult {
        defs: defs.into_vec(),
        resolution: ResolutionMap {
            expr_defs: resolved.expr_defs,
            pat_defs: resolved.pat_defs,
            law_inferred_vars: resolved.law_inferred_vars,
        },
        expr_types: result.expr_types,
        types: result.types,
        unify: result.unify,
        instances: result.instances,
        well_known,
    }
}

fn analyze_setup<'a>(
    module: &'a ParsedModule,
    interner: &'a mut Interner,
    file_id: FileId,
    diags: &'a mut DiagnosticBag,
) -> (
    DefTable,
    WellKnown,
    ScopeTree,
    ScopeId,
    ResolveOutput,
    Arena<Type>,
) {
    let mut defs = DefTable::new();
    let mut scopes = ScopeTree::new();
    let module_scope = scopes.push_root();

    let well_known = well_known::init_well_known(interner, &mut defs, module_scope, &mut scopes);
    let mut types = Arena::new();
    well_known::assign_well_known_types(&mut defs, &well_known, &mut types);

    let resolved = resolve::resolve(
        module,
        interner,
        file_id,
        diags,
        &mut defs,
        &mut scopes,
        module_scope,
    );

    (defs, well_known, scopes, module_scope, resolved, types)
}

fn analyze_emit_unused_warnings(
    defs: &DefTable,
    interner: &Interner,
    file_id: FileId,
    diags: &mut DiagnosticBag,
) {
    for def in defs.iter() {
        if def.use_count == 0
            && def.span != Span::DUMMY
            && !def.exported
            && !def.is_entry_point
            && def.name != Symbol(u32::MAX)
            && !matches!(
                def.kind,
                DefKind::Given | DefKind::Variant | DefKind::Type | DefKind::Law
            )
        {
            let name_str = interner.resolve(def.name);
            if name_str == "_" || name_str.starts_with('_') {
                continue;
            }
            let name = Box::from(name_str);
            let err = match def.kind {
                DefKind::Param | DefKind::LawVar => SemaError::UnusedParameter { name },
                DefKind::OpaqueType => SemaError::UnusedType { name },
                DefKind::Class => SemaError::UnusedClass { name },
                DefKind::Effect | DefKind::EffectOp => SemaError::UnusedEffect { name },
                DefKind::Import => SemaError::UnusedImport { name },
                _ => SemaError::UnusedVariable { name },
            };
            let _d = diags.report(&err, def.span, file_id);
        }
    }
}
