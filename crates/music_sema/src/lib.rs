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
pub use types::{EffectRow, InstanceInfo, Obligation, TyVarId, Type};
pub use unify::UnifyTable;
pub use well_known::WellKnown;

use std::collections::HashMap;

use music_ast::ParsedModule;
use music_shared::{Arena, DiagnosticBag, FileId, Idx, Interner, Span};

/// Maps from AST nodes to their resolved definitions.
pub struct ResolutionMap {
    /// Maps each identifier expression to the definition it refers to.
    pub expr_defs: HashMap<Idx<music_ast::Expr>, DefId>,
    /// Maps each binding-site span to its [`DefId`].
    pub pat_defs: HashMap<Span, DefId>,
}

/// The complete result of semantic analysis of a single module.
pub struct SemaResult {
    /// All definitions encountered (index = `DefId.0`).
    pub defs: Vec<DefInfo>,
    /// Name resolution maps.
    pub resolution: ResolutionMap,
    /// The inferred type of each expression node.
    pub expr_types: HashMap<Idx<music_ast::Expr>, Idx<Type>>,
    /// The type arena.
    pub types: Arena<Type>,
    /// The unification table (retained so callers can freeze types).
    pub unify: UnifyTable,
    /// Typeclass instances discovered during analysis.
    pub instances: Vec<InstanceInfo>,
}

/// Prelude data passed into analysis.
pub struct Prelude {
    pub well_known: WellKnown,
    pub defs: Vec<DefInfo>,
    pub instances: Vec<InstanceInfo>,
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
    let (mut defs, well_known, mut scopes, module_scope, resolved) =
        analyze_setup(module, interner, file_id, diags);

    let ctx = CheckContext {
        ast: &module.arenas,
        interner,
        file_id,
        well_known: &well_known,
        expr_defs: &resolved.expr_defs,
    };
    let mut checker = Checker::new(ctx, diags, &mut defs, &mut scopes, module_scope);

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
        },
        expr_types: result.expr_types,
        types: result.types,
        unify: result.unify,
        instances: result.instances,
    }
}

fn analyze_setup<'a>(
    module: &'a ParsedModule,
    interner: &'a mut Interner,
    file_id: FileId,
    diags: &'a mut DiagnosticBag,
) -> (DefTable, WellKnown, ScopeTree, ScopeId, ResolveOutput) {
    let mut defs = DefTable::new();
    let mut scopes = ScopeTree::new();
    let module_scope = scopes.push_root();

    let well_known = well_known::init_well_known(interner, &mut defs, module_scope, &mut scopes);

    let resolved = resolve::resolve(
        module,
        interner,
        file_id,
        diags,
        &mut defs,
        &mut scopes,
        module_scope,
    );

    (defs, well_known, scopes, module_scope, resolved)
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
            && matches!(def.kind, DefKind::Let | DefKind::Var | DefKind::Param)
        {
            let name_str = interner.resolve(def.name);
            if name_str == "_" || name_str.starts_with('_') {
                continue;
            }
            let err = if def.kind == DefKind::Param {
                SemaError::UnusedParameter {
                    name: Box::from(name_str),
                }
            } else {
                SemaError::UnusedVariable {
                    name: Box::from(name_str),
                }
            };
            let _d = diags.report(&err, def.span, file_id);
        }
    }
}
