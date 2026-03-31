//! Semantic analysis.
//!
//! Current scope:
//! - AST to HIR lowering
//! - first-pass lexical name resolution
//! - basic type/effect checking over resolved HIR (gradual types via `Any`/`Unknown`)

mod attrs;
mod bind;
mod callable;
mod checker;
mod effects;
mod env;
mod errors;
mod export_build;
mod expr;
pub mod iface;
mod import_build;
mod lang;
mod lower;
mod ty;
mod unify;

use std::collections::HashMap;

use music_ast::SyntaxTree;
use music_basic::{SourceId, SourceMap, Span};
use music_hir::{HirModule, HirOrigin};
use music_names::{Interner, NameBindingId, NameResolution, NameSite, SymbolSlice};
use music_resolve::{ResolveError, ResolveOptions, ResolvedModule};

pub(crate) use effects::{EffectKey, EffectRow};
pub use errors::{SemaError, SemaErrorKind, SemaErrorKinds, SemaErrors};
pub(crate) use ty::SemTyDisplay;
pub(crate) use ty::{SemTy, SemTyId, SemTys};

use crate::checker::Checker;

/// Output of `analyze_module`: resolution + type/effect analysis.
#[derive(Debug)]
pub struct AnalyzedModule {
    pub module: HirModule,
    pub exports: SymbolSlice,
    pub names: NameResolution,
    pub resolve_errors: Vec<ResolveError>,
    pub check_errors: Vec<SemaError>,
    pub interface: iface::ModuleExportSummary,
}

#[must_use]
pub fn analyze_module(
    tree: &SyntaxTree,
    sources: &SourceMap,
    interner: &mut Interner,
    options: ResolveOptions<'_>,
    sema_import_env: Option<&dyn iface::SemaImportEnv>,
) -> AnalyzedModule {
    let ResolvedModule {
        mut module,
        exports,
        names,
        errors: resolve_errors,
    } = music_resolve::resolve_module(tree, sources, interner, options);

    let mut check_errors = Vec::new();
    let mut checker = Checker::new(
        tree.source_id(),
        sources,
        interner,
        &names,
        &mut module.store,
        &mut check_errors,
        sema_import_env,
    );
    let interface = checker.check_module(module.root);

    AnalyzedModule {
        module,
        exports,
        names,
        resolve_errors,
        check_errors,
        interface,
    }
}

const fn dummy_origin(span: Span) -> HirOrigin {
    HirOrigin::new(span, None)
}

const fn site(source_id: SourceId, span: Span) -> NameSite {
    NameSite::new(source_id, span)
}

fn binding_by_site(names: &NameResolution) -> HashMap<NameSite, NameBindingId> {
    let mut out = HashMap::with_capacity(names.bindings.len());
    for (id, binding) in &names.bindings {
        let _prev = out.insert(binding.site, id);
    }
    out
}
