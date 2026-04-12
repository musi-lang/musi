use music_names::Interner;
use music_resolve::ResolvedModule;

use crate::api::{SemaModule, SemaOptions};

mod attrs;
mod collect;
mod decls;
mod expr_aggregates;
mod expr_calls;
mod exprs;
mod normalize;
mod patterns;
pub mod schemes;
mod state;
mod surface;
mod surface_exports;
mod surface_types;

use state::{
    CheckPass, CollectPass, DataDef, DataVariantDef, DeclState, EffectDef, EffectOpDef, FactState,
    ModuleState, PassBase, PassParts, ResumeCtx, ResumeState, RuntimeEnv, TypingState,
    finish_module, prepare_module,
};

use crate::diag::SemaDiagKind as DiagKind;

struct Checker<'interner, 'env> {
    module: ModuleState,
    runtime: RuntimeEnv<'interner, 'env>,
    typing: TypingState,
    decls: DeclState,
    facts: FactState,
    resume: ResumeState,
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
    checker.check_instance_coherence();
    checker.finish()
}

impl<'interner, 'env> Checker<'interner, 'env> {
    fn new(
        resolved: ResolvedModule,
        interner: &'interner mut Interner,
        options: SemaOptions<'env>,
    ) -> Self {
        let (module, runtime, typing, decls, facts, resume) =
            prepare_module(resolved, interner, options);
        Self {
            module,
            runtime,
            typing,
            decls,
            facts,
            resume,
        }
    }

    fn collect_module(&mut self) {
        let Self {
            module,
            runtime,
            typing,
            decls,
            facts,
            ..
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
            resume,
        } = self;
        let base = PassBase::new(PassParts {
            module,
            runtime,
            typing,
            decls,
            facts,
        });
        let collect = CollectPass::new(base);
        let mut check = CheckPass::new(collect, resume);
        let root = check.root_expr_id();
        let _root_facts = exprs::check_module_root(&mut check, root);
    }

    fn check_instance_coherence(&mut self) {
        let Self {
            module,
            runtime,
            typing,
            decls,
            facts,
            resume,
        } = self;
        let base = PassBase::new(PassParts {
            module,
            runtime,
            typing,
            decls,
            facts,
        });
        let collect = CollectPass::new(base);
        let mut check = CheckPass::new(collect, resume);
        check.check_instance_coherence();
    }

    fn finish(self) -> SemaModule {
        let Self {
            module,
            runtime,
            typing,
            decls,
            facts,
            resume: _,
        } = self;
        finish_module(module, &runtime, &typing, decls, facts)
    }
}
