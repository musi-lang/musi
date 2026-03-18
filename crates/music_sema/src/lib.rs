//! Semantic analysis for the Musi compiler.
//!
//! This crate performs two passes over a parsed module:
//!
//! 1. **Name resolution** - binds every identifier to a [`DefId`], reports
//!    undefined names and duplicate definitions.
//!
//! 2. **Type checking** - bidirectional inference with unification variables.
//!    Synthesises types bottom-up and checks them top-down; emits diagnostics
//!    for type mismatches, arity errors, and effect violations.
//!
//! # Entry point
//!
//! ```ignore
//! let result = music_sema::analyze(&module, &mut interner, file_id, &mut diags);
//! ```

pub(crate) mod checker;
pub mod consistency;
pub mod def;
pub mod error;
pub mod exports;
pub mod lang_items;
pub mod resolve;
pub mod scope;
pub mod subst;
pub mod subtype;
pub mod types;
pub mod unify;
pub mod well_known;

pub use def::{DefId, DefInfo, DefKind, DefTable};
pub use error::SemaError;
pub use lang_items::LangItemRegistry;
pub use resolve::ResolveOutput;
pub use scope::ScopeTree;
pub use types::{
    CastInfo, DictLookup, EffectRow, InstanceInfo, Obligation, TyVarId, Type, TypeIdx,
};
pub use unify::{UnifyTable, types_match};
pub use well_known::WellKnown;

pub use exports::{ExportBinding, ModuleExports, collect_exports};
pub use resolve::{ImportNames, SubModuleExports};

use std::collections::{HashMap, HashSet};
use std::hash::BuildHasher;
use std::mem;

use music_ast::{ExprIdx, ParsedModule};
use music_shared::{Arena, DiagnosticBag, FileId, Interner, Span, Symbol};

use crate::checker::{CheckContext, Checker};
use crate::scope::ScopeId;

/// Maps from AST nodes to their resolved definitions.
pub struct ResolutionMap {
    /// Maps each identifier expression to the definition it refers to.
    pub expr_defs: HashMap<ExprIdx, DefId>,
    /// Maps each binding-site span to its [`DefId`].
    pub pat_defs: HashMap<Span, DefId>,
    /// Parallel to the `name_refs` arena: `DefId` for each resolved `NameRef`.
    pub name_ref_defs: Vec<Option<DefId>>,
    /// Maps law span -> inferred (implicit) law variables, for LSP inlay hints.
    pub law_inferred_vars: HashMap<Span, Vec<(Symbol, DefId)>>,
    /// Maps (class `DefId`, operator `Symbol`) -> member `DefId` for operator dispatch.
    pub class_op_members: HashMap<(DefId, Symbol), DefId>,
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
    /// Maps `BinOp` expression index -> instance method `DefId` for operator dispatch.
    pub binop_dispatch: HashMap<ExprIdx, DefId>,
    /// Maps `BinOp` expression -> dictionary lookup for polymorphic dispatch.
    pub binop_dict_dispatch: HashMap<ExprIdx, DictLookup>,
    /// Maps function `DefId` -> ordered class constraints (for dictionary passing).
    pub fn_constraints: HashMap<DefId, Vec<Obligation>>,
    /// Runtime casts inserted at `Any` boundaries, keyed by expression.
    pub casts: HashMap<ExprIdx, CastInfo>,
    /// Well-known prelude type definitions (needed by bytecode emission).
    pub well_known: WellKnown,
    /// Registry of definitions carrying `#[lang := "..."]` annotations.
    pub lang_items: LangItemRegistry,
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
/// and well-known prelude - all shared across modules.
pub struct SharedAnalysisState {
    pub types: Arena<Type>,
    pub unify: UnifyTable,
    pub defs: DefTable,
    pub scopes: ScopeTree,
    pub well_known: WellKnown,
    pub prelude_scope: ScopeId,
    pub lang_items: LangItemRegistry,
    injected_lang_items: HashSet<DefId>,
    pub instances: Vec<InstanceInfo>,
}

/// Per-module analysis output (does not own shared state).
pub struct ModuleSemaOutput {
    pub module_scope: ScopeId,
    pub resolution: ResolutionMap,
    pub expr_types: HashMap<ExprIdx, TypeIdx>,
    pub instances: Vec<InstanceInfo>,
    /// Maps `BinOp` expression index -> instance method `DefId` for operator dispatch.
    pub binop_dispatch: HashMap<ExprIdx, DefId>,
    /// Maps `BinOp` expression -> dictionary lookup for polymorphic dispatch.
    pub binop_dict_dispatch: HashMap<ExprIdx, DictLookup>,
    /// Maps function `DefId` -> ordered class constraints (for dictionary passing).
    pub fn_constraints: HashMap<DefId, Vec<Obligation>>,
    /// Runtime casts inserted at `Any` boundaries, keyed by expression.
    pub casts: HashMap<ExprIdx, CastInfo>,
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
            lang_items: LangItemRegistry::new(),
            injected_lang_items: HashSet::new(),
            instances: vec![],
        }
    }

    pub fn collect_lang_items(&mut self, interner: &Interner) {
        for def in self.defs.iter() {
            if let Some(sym) = def.lang_item {
                let raw = interner.resolve(sym);
                let name = raw.strip_prefix('"').unwrap_or(raw);
                let name = name.strip_suffix('"').unwrap_or(name);
                let _prev = self.lang_items.register(name, def.id);
            }
        }
    }

    /// Injects lang-item definitions into the prelude scope so all modules can
    /// reference them without explicit imports.
    ///
    /// For lang-item classes, also injects their child methods (Haskell-style),
    /// skipping names already present in prelude (e.g. runtime foreign fns).
    pub fn inject_lang_items_into_prelude(&mut self) {
        let lang_class_ids = self.collect_lang_class_ids();
        self.inject_lang_item_defs();
        self.inject_lang_class_methods(&lang_class_ids);
        self.mark_injected_lang_items();
    }

    /// Returns `DefIds` of lang-item classes not yet injected.
    fn collect_lang_class_ids(&self) -> Vec<DefId> {
        self.defs
            .iter()
            .filter(|d| {
                d.lang_item.is_some()
                    && d.kind == DefKind::Class
                    && !self.injected_lang_items.contains(&d.id)
            })
            .map(|d| d.id)
            .collect()
    }

    /// Injects lang-item definitions into the prelude scope if not already injected.
    fn inject_lang_item_defs(&mut self) {
        for def in self.defs.iter() {
            if def.lang_item.is_some() && !self.injected_lang_items.contains(&def.id) {
                let _ = self.scopes.define(self.prelude_scope, def.name, def.id);
            }
        }
    }

    /// Injects child methods of lang-item classes into the prelude, except names already defined.
    fn inject_lang_class_methods(&mut self, lang_class_ids: &[DefId]) {
        for def in self.defs.iter() {
            if def.parent.is_some()
                && lang_class_ids.contains(&def.parent.unwrap())
                && def.kind == DefKind::Fn
                && def.name != Symbol(u32::MAX)
                && self
                    .scopes
                    .lookup_local(self.prelude_scope, def.name)
                    .is_none()
            {
                let _ = self.scopes.define(self.prelude_scope, def.name, def.id);
            }
        }
    }

    /// Marks all lang-item definitions as injected.
    fn mark_injected_lang_items(&mut self) {
        for def in self.defs.iter() {
            if def.lang_item.is_some() {
                let _ = self.injected_lang_items.insert(def.id);
            }
        }
    }

    /// Converts the shared state and a per-module output into a [`SemaResult`].
    #[must_use]
    pub fn into_sema_result(mut self, output: ModuleSemaOutput) -> SemaResult {
        self.instances.extend(output.instances);
        SemaResult {
            defs: self.defs.into_vec(),
            resolution: output.resolution,
            expr_types: output.expr_types,
            types: self.types,
            unify: self.unify,
            instances: self.instances,
            binop_dispatch: output.binop_dispatch,
            binop_dict_dispatch: output.binop_dict_dispatch,
            fn_constraints: output.fn_constraints,
            casts: output.casts,
            well_known: self.well_known,
            lang_items: self.lang_items,
        }
    }
}

/// Runs analysis for a single module using shared cross-module state.
#[allow(clippy::too_many_arguments)]
pub fn analyze_shared<S: BuildHasher>(
    module: &ParsedModule,
    state: &mut SharedAnalysisState,
    interner: &mut Interner,
    file_id: FileId,
    diags: &mut DiagnosticBag,
    import_names: &ImportNames,
    import_types: &HashMap<Symbol, TypeIdx, S>,
    sub_module_exports: &SubModuleExports,
) -> ModuleSemaOutput {
    let module_scope = state.scopes.push_child(state.prelude_scope);

    let mut resolve_state = resolve::ResolveState {
        defs: &mut state.defs,
        scopes: &mut state.scopes,
        module_scope,
    };
    let resolved = resolve::resolve_with_imports(
        module,
        interner,
        file_id,
        diags,
        &mut resolve_state,
        import_names,
        sub_module_exports,
    );

    let ctx = CheckContext {
        ast: &module.arenas,
        interner,
        file_id,
        well_known: &state.well_known,
        expr_defs: &resolved.expr_defs,
        pat_defs: &resolved.pat_defs,
        import_types,
        law_inferred_vars: &resolved.law_inferred_vars,
        class_op_members: &resolved.class_op_members,
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

    let inherited_count = state.instances.len();
    checker
        .store
        .instances
        .extend(state.instances.iter().cloned());

    for stmt in &module.stmts {
        let _ty = checker.synth(stmt.expr);
    }

    checker.resolve_obligations();
    let result = checker.finish();

    state.types = result.types;
    state.unify = result.unify;

    // Accumulate only instances declared in THIS module (skip inherited).
    state
        .instances
        .extend(result.instances[inherited_count..].iter().cloned());

    analyze_emit_unused_warnings(&state.defs, interner, file_id, diags);

    ModuleSemaOutput {
        module_scope,
        resolution: ResolutionMap {
            expr_defs: resolved.expr_defs,
            pat_defs: resolved.pat_defs,
            name_ref_defs: resolved.name_ref_defs,
            law_inferred_vars: resolved.law_inferred_vars,
            class_op_members: resolved.class_op_members,
        },
        expr_types: result.expr_types,
        instances: result.instances[inherited_count..].to_vec(),
        binop_dispatch: result.binop_dispatch,
        binop_dict_dispatch: result.binop_dict_dispatch,
        fn_constraints: result.fn_constraints,
        casts: result.casts,
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
pub fn analyze_with_imports<S: BuildHasher>(
    module: &ParsedModule,
    interner: &mut Interner,
    file_id: FileId,
    diags: &mut DiagnosticBag,
    import_types: &HashMap<Symbol, TypeIdx, S>,
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
        class_op_members: &resolved.class_op_members,
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

    let mut lang_items = LangItemRegistry::new();
    for def in defs.iter() {
        if let Some(sym) = def.lang_item {
            let raw = interner.resolve(sym);
            let name = raw.strip_prefix('"').unwrap_or(raw);
            let name = name.strip_suffix('"').unwrap_or(name);
            let _prev = lang_items.register(name, def.id);
        }
    }

    SemaResult {
        defs: defs.into_vec(),
        resolution: ResolutionMap {
            expr_defs: resolved.expr_defs,
            pat_defs: resolved.pat_defs,
            name_ref_defs: resolved.name_ref_defs,
            law_inferred_vars: resolved.law_inferred_vars,
            class_op_members: resolved.class_op_members,
        },
        expr_types: result.expr_types,
        types: result.types,
        unify: result.unify,
        instances: result.instances,
        binop_dispatch: result.binop_dispatch,
        binop_dict_dispatch: result.binop_dict_dispatch,
        fn_constraints: result.fn_constraints,
        casts: result.casts,
        well_known,
        lang_items,
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
                DefKind::Instance
                    | DefKind::Variant
                    | DefKind::Type
                    | DefKind::Law
                    | DefKind::Class
                    | DefKind::ForeignFn
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
                _ => SemaError::UnusedBinding { name },
            };
            let _d = diags.report(&err, def.span, file_id);
        }
    }
}
