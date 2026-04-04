use music_names::Interner;
use music_resolve::ResolvedModule;

use crate::api::{SemaModule, SemaOptions};
use crate::collect::collect_module;
use crate::context::{CheckPass, CollectPass, finish_module, prepare_module};
use crate::decls::check_instance_coherence;
use crate::exprs::check_expr;

#[must_use]
pub fn check_module(
    resolved: ResolvedModule,
    interner: &mut Interner,
    options: SemaOptions<'_>,
) -> SemaModule {
    let (mut module, mut runtime, mut typing, mut decls, mut facts, mut resume) =
        prepare_module(resolved, interner, options);

    {
        let mut collect = CollectPass::new(
            &mut module,
            &mut runtime,
            &mut typing,
            &mut decls,
            &mut facts,
        );
        collect_module(&mut collect);
    }

    {
        let mut check = CheckPass::new(
            &mut module,
            &mut runtime,
            &mut typing,
            &mut decls,
            &mut facts,
            &mut resume,
        );
        let root = check.root_expr_id();
        let _root_facts = check_expr(&mut check, root);
        check_instance_coherence(&mut check);
    }

    finish_module(module, &runtime, &typing, decls, facts)
}
