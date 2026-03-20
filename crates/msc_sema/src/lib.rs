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
//! let result = msc_sema::analyze(&module, &mut interner, file_id, &mut diags);
//! ```

pub mod attr_util;
pub(crate) mod checker;
pub mod consistency;
pub mod def;
pub mod error;
pub mod exports;
pub mod lang_items;
pub mod options;
pub mod pipeline;
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
pub use options::SemaOptions;
pub use pipeline::{
    ModuleAnalysisCtx, ModuleSemaOutput, SharedAnalysisState, analyze, analyze_shared,
    analyze_with_imports,
};
pub use resolve::{ImportNames, SubModuleExports};

use std::collections::HashMap;

use msc_ast::ExprIdx;
use msc_shared::{Arena, Span, Symbol};

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
