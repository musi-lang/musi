use music_names::Interner;
use music_resolve::ResolvedModule;

use crate::api::{SemaModule, SemaOptions};

mod attrs;
mod collect;
mod const_eval;
mod decls;
mod diag_subject;
mod exprs;
mod normalize;
mod pats;
pub mod schemes;
mod state;
mod surface;
mod variant_payload;

use state::{
    CheckPass, CollectPass, DataDef, DataVariantDef, DeclState, FactState, ModuleState, PassBase,
    PassParts, RuntimeEnv, TypingState, finish_module, prepare_module,
};

use crate::api::ModuleSurface;
use crate::diag::SemaDiagKind as DiagKind;

struct Checker<'interner, 'env> {
    module: ModuleState,
    runtime: RuntimeEnv<'interner, 'env>,
    typing: TypingState,
    decls: DeclState,
    facts: FactState,
}

#[must_use]
pub fn check_module(
    resolved: ResolvedModule,
    interner: &mut Interner,
    options: SemaOptions<'_>,
) -> SemaModule {
    let mut checker = Checker::new(resolved, interner, options);
    checker.collect_module();
    checker.check_root();
    checker.finish()
}

impl<'interner, 'env> Checker<'interner, 'env> {
    fn new(
        resolved: ResolvedModule,
        interner: &'interner mut Interner,
        options: SemaOptions<'env>,
    ) -> Self {
        let prelude = options.prelude.cloned();
        let (module, runtime, typing, decls, facts) = prepare_module(resolved, interner, options);
        let mut this = Self {
            module,
            runtime,
            typing,
            decls,
            facts,
        };
        this.seed_import_bindings();
        if let Some(prelude) = prelude.as_ref() {
            this.seed_prelude(prelude);
        }
        this
    }

    fn seed_import_bindings(&mut self) {
        let Self {
            module,
            runtime,
            typing,
            decls,
            facts,
        } = self;
        let base = PassBase::new(PassParts {
            module,
            runtime,
            typing,
            decls,
            facts,
        });
        let collect = CollectPass::new(base);
        let mut check = CheckPass::new(collect);
        decls::seed_import_bindings(&mut check);
    }

    fn seed_prelude(&mut self, prelude: &ModuleSurface) {
        let Self {
            module,
            runtime,
            typing,
            decls,
            facts,
        } = self;
        let base = PassBase::new(PassParts {
            module,
            runtime,
            typing,
            decls,
            facts,
        });
        let collect = CollectPass::new(base);
        let mut check = CheckPass::new(collect);
        decls::seed_prelude_bindings(&mut check, prelude);
    }

    fn collect_module(&mut self) {
        let Self {
            module,
            runtime,
            typing,
            decls,
            facts,
        } = self;
        let base = PassBase::new(PassParts {
            module,
            runtime,
            typing,
            decls,
            facts,
        });
        let mut collect = CollectPass::new(base);
        collect::collect_module(&mut collect);
    }

    fn check_root(&mut self) {
        let Self {
            module,
            runtime,
            typing,
            decls,
            facts,
        } = self;
        let base = PassBase::new(PassParts {
            module,
            runtime,
            typing,
            decls,
            facts,
        });
        let collect = CollectPass::new(base);
        let mut check = CheckPass::new(collect);
        let root = check.root_expr_id();
        let _root_facts = exprs::check_module_root(&mut check, root);
    }

    fn finish(self) -> SemaModule {
        let Self {
            module,
            runtime,
            typing,
            decls,
            facts,
        } = self;
        finish_module(module, &runtime, &typing, decls, facts)
    }
}
