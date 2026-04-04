use music_names::Interner;
use music_resolve::ResolvedModule;

use crate::api::{SemaModule, SemaOptions};

mod attrs;
mod collect;
mod decls;
mod exprs;
mod normalize;
mod patterns;
mod schemes;
mod state;
mod surface;

pub(crate) use state::{
    CheckPass, CollectPass, DeclState, EffectDef, EffectOpDef, FactState, ModuleState, PassBase,
    ResumeCtx, ResumeState, RuntimeEnv, TypingState, finish_module, prepare_module,
};

pub(crate) struct Checker<'interner, 'env> {
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
        let mut collect = CollectPass::new(module, runtime, typing, decls, facts);
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
        let mut check = CheckPass::new(module, runtime, typing, decls, facts, resume);
        let root = check.root_expr_id();
        let _root_facts = exprs::check_expr(&mut check, root);
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
        let mut check = CheckPass::new(module, runtime, typing, decls, facts, resume);
        decls::check_instance_coherence(&mut check);
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
