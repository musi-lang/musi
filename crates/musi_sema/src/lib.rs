//! Semantic analysis for the Musi compiler.
//!
//! This crate performs two passes over a parsed module:
//!
//! 1. **Name resolution** -- binds every identifier to a [`DefId`], reports
//!    undefined names (with edit-distance suggestions) and duplicate definitions.
//!
//! 2. **Type checking** -- bidirectional, HM-style inference with unification
//!    variables.  Infers types bottom-up and checks them top-down; emits
//!    diagnostics for type mismatches.
//!
//! # Entry point
//!
//! ```ignore
//! let result = musi_sema::analyze(&module, &interner, file_id, &mut diags);
//! ```

#![allow(clippy::module_name_repetitions)]
#![allow(clippy::exhaustive_structs)]
#![allow(clippy::exhaustive_enums)]

pub mod check;
pub mod def;
pub mod resolve;
pub mod scope;
pub mod types;

pub use check::{TypeChecker, UnifyTable};
pub use def::{DefId, DefInfo, DefKind};
pub use resolve::{ResolveResult, resolve};
pub use scope::{ScopeId, ScopeTree};
pub use types::{PrimTy, Type, TypeVarId};

use std::collections::HashMap;

use musi_parse::ParsedModule;
use musi_parse::ast::Expr;
use musi_shared::{DiagnosticBag, FileId, Idx, Interner, Span};

/// The complete result of semantic analysis of a single module.
pub struct SemaResult {
    /// All definitions encountered (index = [`DefId`].0).
    pub defs: Vec<DefInfo>,
    /// Maps each `Ident` expression to the definition it refers to.
    pub expr_defs: HashMap<Idx<Expr>, DefId>,
    /// Maps each binding-site span to its [`DefId`].
    pub pat_defs: HashMap<Span, DefId>,
    /// The inferred type of each expression node.
    pub expr_types: HashMap<Idx<Expr>, Type>,
}

/// Runs name resolution and type checking on `module`.
///
/// Diagnostics (errors and warnings) are pushed into `diags`.  The caller
/// should check `diags.has_errors()` after this call.
pub fn analyze(
    module: &ParsedModule,
    interner: &Interner,
    file_id: FileId,
    diags: &mut DiagnosticBag,
) -> SemaResult {
    // Pass 1 & 2: name resolution.
    let resolved = resolve(module, interner, file_id, diags);

    // Pass 3: type checking.
    // The checker borrows expr_defs and pat_defs; we scope it so those borrows
    // are released before we move the fields into SemaResult.
    let (defs, expr_types) = {
        let mut checker = TypeChecker::new(
            interner,
            file_id,
            diags,
            &resolved.expr_defs,
            &resolved.pat_defs,
            resolved.defs,
        );
        checker.check_module(module);
        (checker.defs, checker.expr_types)
    };

    SemaResult {
        defs,
        expr_defs: resolved.expr_defs,
        pat_defs: resolved.pat_defs,
        expr_types,
    }
}

#[cfg(test)]
mod tests;
