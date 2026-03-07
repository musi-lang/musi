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
use std::hash::BuildHasher;

use musi_ast::{Expr, Modifier, ParsedModule};
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

/// Externally-visible types exported by a module (name → inferred [`Type`]).
pub struct ModuleExports {
    pub names: HashMap<String, Type>,
}

/// Extracts the exported name-to-type map from a completed analysis.
#[must_use]
pub fn exports_of(
    result: &SemaResult,
    module: &ParsedModule,
    interner: &Interner,
) -> ModuleExports {
    let mut names: HashMap<String, Type> = HashMap::new();
    for &item_idx in module.ctx.expr_lists.get_slice(module.items) {
        match module.ctx.exprs.get(item_idx) {
            Expr::FnDef {
                name, modifiers, ..
            } => {
                if modifiers.iter().any(|m| matches!(m, Modifier::Export)) {
                    let name_str = interner.resolve(*name).to_owned();
                    // Find the DefId for this function in the resolver results
                    if let Some(ty) = result
                        .defs
                        .iter()
                        .find(|d| interner.resolve(d.name) == name_str)
                        .and_then(|d| d.ty.clone())
                    {
                        let _prev = names.insert(name_str, ty);
                    }
                }
            }
            Expr::Export { items, .. } => {
                // `export { name1, name2 } from "path"` — re-exports
                for item in items {
                    let name_str = interner.resolve(item.name).to_owned();
                    if let Some(ty) = result
                        .defs
                        .iter()
                        .find(|d| interner.resolve(d.name) == name_str)
                        .and_then(|d| d.ty.clone())
                    {
                        let _prev = names.insert(name_str, ty);
                    }
                }
            }
            _ => {}
        }
    }
    ModuleExports { names }
}

/// Runs name resolution and type checking on `module`.
///
/// Diagnostics (errors and warnings) are pushed into `diags`.  The caller
/// should check `diags.has_errors()` after this call.
///
/// `imports` maps each imported module path to its exported types, enabling
/// cross-module type resolution.
pub fn analyze<S: BuildHasher>(
    module: &ParsedModule,
    interner: &Interner,
    file_id: FileId,
    diags: &mut DiagnosticBag,
    imports: &HashMap<String, ModuleExports, S>,
) -> SemaResult {
    // Pass 1 & 2: name resolution.
    let resolved = resolve(module, interner, file_id, diags, imports);

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

    // Emit warnings for unused definitions.
    for def in &defs {
        if def.use_count == 0
            && def.span != Span::DUMMY
            && matches!(def.kind, DefKind::Const | DefKind::Var | DefKind::Param)
        {
            let name_str = interner.resolve(def.name);
            if name_str == "_" || name_str.starts_with('_') {
                continue;
            }
            let msg = if def.kind == DefKind::Param {
                format!("unused parameter `{name_str}`")
            } else {
                format!("unused variable `{name_str}`")
            };
            let _d = diags.warning(msg, def.span, file_id);
        }
    }

    SemaResult {
        defs,
        expr_defs: resolved.expr_defs,
        pat_defs: resolved.pat_defs,
        expr_types,
    }
}

#[cfg(test)]
mod tests;
