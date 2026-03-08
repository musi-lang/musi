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
pub use def::{DefId, DefInfo, DefKind, TypeFlavor};
pub use resolve::{ResolveResult, resolve};
pub use scope::{ScopeId, ScopeTree};
pub use types::{PrimTy, Type, TypeVarId};

use std::collections::HashMap;
use std::hash::BuildHasher;

use musi_ast::{Expr, Modifier, ParsedModule};
use musi_shared::{DiagnosticBag, FileId, Idx, Interner, Span, Symbol};

/// The complete result of semantic analysis of a single module.
pub struct SemaResult {
    /// All definitions encountered (index = [`DefId`].0).
    pub defs: Vec<DefInfo>,
    /// Maps each `Ident` expression to the definition it refers to.
    pub expr_defs: HashMap<Idx<Expr>, DefId>,
    /// Maps each binding-site span to its [`DefId`].
    pub pat_defs: HashMap<Span, DefId>,
    /// Maps each type-annotation span to the `DefId` of the referenced type.
    pub ty_refs: HashMap<Span, DefId>,
    /// The inferred type of each expression node.
    pub expr_types: HashMap<Idx<Expr>, Type>,
    /// The unification table used during type checking.
    /// Retained so callers can call [`UnifyTable::freeze_type`] before exporting types.
    pub unify_table: UnifyTable,
    /// Maps each named function's `DefId` to its ordered parameter names.
    /// Used to generate call-site parameter-name inlay hints.
    pub fn_params: HashMap<DefId, Vec<Symbol>>,
    /// Call sites: `(fn_def_id, arg_expr_indices)` pairs for parameter-name hints.
    pub call_sites: Vec<(DefId, Vec<Idx<Expr>>)>,
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
                    let Some(name_str) = interner.try_resolve(*name).map(str::to_owned) else {
                        continue;
                    };
                    // Find the DefId for this function in the resolver results
                    if let Some(ty) = result
                        .defs
                        .iter()
                        .filter_map(|d| {
                            interner.try_resolve(d.name).and_then(|n| {
                                if n == name_str { d.ty.clone() } else { None }
                            })
                        })
                        .next()
                    {
                        // Freeze: resolve bound vars and erase free ones so that
                        // TypeVarIds do not escape into a foreign UnifyTable.
                        let _prev = names.insert(name_str, result.unify_table.freeze_type(ty));
                    }
                }
            }
            Expr::Export { items, .. } => {
                // `export { name1, name2 } from "path"` — re-exports
                for item in items {
                    let Some(name_str) = interner.try_resolve(item.name).map(str::to_owned) else {
                        continue;
                    };
                    if let Some(ty) = result
                        .defs
                        .iter()
                        .filter_map(|d| {
                            interner.try_resolve(d.name).and_then(|n| {
                                if n == name_str { d.ty.clone() } else { None }
                            })
                        })
                        .next()
                    {
                        let _prev = names.insert(name_str, result.unify_table.freeze_type(ty));
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
    let (defs, expr_types, unify_table) = {
        let mut checker = TypeChecker::new(
            interner,
            file_id,
            diags,
            &resolved.expr_defs,
            &resolved.pat_defs,
            resolved.defs,
        );
        checker.check_module(module);
        (checker.defs, checker.expr_types, checker.unify_table)
    };

    // Emit warnings for unused definitions.
    for def in &defs {
        if def.use_count == 0
            && def.span != Span::DUMMY
            && matches!(def.kind, DefKind::Const | DefKind::Var | DefKind::Param)
            && !def.is_extrin_param
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
        ty_refs: resolved.ty_refs,
        expr_types,
        unify_table,
        fn_params: resolved.fn_params,
        call_sites: resolved.call_sites,
    }
}

#[cfg(test)]
mod tests;
