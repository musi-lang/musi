mod bind;
mod callable;
mod check;
mod effects;
mod env;
mod expr;
mod lang;
mod lower;
mod ty;
mod unify;

use music_ast::SyntaxTree;
use music_basic::{SourceMap, Span};
use music_hir::{HirModule, HirOrigin};
use music_names::{Interner, NameBindingId, NameResolution, NameSite, Symbol};
use music_resolve::{ResolveOptions, ResolvedModule};

use crate::SemaError;

pub use effects::{EffectKey, EffectRow};
pub use ty::{SemTy, SemTyId, SemTys};

pub(super) use ty::SemTyDisplay;

/// Output of `analyze_module`: resolution + type/effect analysis.
#[derive(Debug)]
pub struct AnalyzedModule {
    pub module: HirModule,
    pub exports: Box<[Symbol]>,
    pub names: NameResolution,
    pub resolve_errors: Vec<music_resolve::ResolveError>,
    pub check_errors: Vec<SemaError>,
}

#[must_use]
pub fn analyze_module(
    tree: &SyntaxTree,
    sources: &SourceMap,
    interner: &mut Interner,
    options: ResolveOptions<'_>,
) -> AnalyzedModule {
    let ResolvedModule {
        mut module,
        exports,
        names,
        errors: resolve_errors,
    } = music_resolve::resolve_module(tree, sources, interner, options);

    let mut check_errors = Vec::new();
    let mut checker = check::Checker::new(
        tree.source_id(),
        sources,
        interner,
        &names,
        &mut module.store,
        &mut check_errors,
    );
    checker.check_module(module.root);

    AnalyzedModule {
        module,
        exports,
        names,
        resolve_errors,
        check_errors,
    }
}

fn dummy_origin(span: Span) -> HirOrigin {
    HirOrigin::new(span, None)
}

fn site(source_id: music_basic::SourceId, span: Span) -> NameSite {
    NameSite::new(source_id, span)
}

fn binding_by_site(names: &NameResolution) -> std::collections::HashMap<NameSite, NameBindingId> {
    let mut out = std::collections::HashMap::with_capacity(names.bindings.len());
    for (id, binding) in &names.bindings {
        let _prev = out.insert(binding.site, id);
    }
    out
}
