use std::collections::{BTreeMap, HashMap};

use music_module::ModuleKey;
use music_names::{Interner, KnownSymbols};
use music_resolve::ResolvedModule;

use crate::api::{
    DefinitionKey, ExprFacts, PatFacts, SemaDataDef, SemaDataVariantDef, SemaModule, SemaOptions,
};
use crate::checker::surface::build_module_surface;
use crate::effects::EffectRow;

use super::{
    Builtins, DeclState, FactState, ModuleState, ResumeState, RuntimeEnv, TypingState,
    host_target_info,
};

pub fn prepare_module<'interner, 'env>(
    mut resolved: ResolvedModule,
    interner: &'interner mut Interner,
    options: SemaOptions<'env>,
) -> (
    ModuleState,
    RuntimeEnv<'interner, 'env>,
    TypingState,
    DeclState,
    FactState,
    ResumeState,
) {
    let known = KnownSymbols::new(interner);
    let builtins = Builtins::from_resolved(&mut resolved, known);
    let binding_ids = resolved
        .names
        .bindings
        .iter()
        .map(|(id, binding)| (binding.site, id))
        .collect::<HashMap<_, _>>();
    let import_targets = resolved
        .imports
        .iter()
        .map(|import| (import.span, import.to.clone()))
        .collect::<HashMap<_, _>>();
    let expr_facts = vec![
        ExprFacts::new(builtins.unknown, EffectRow::empty());
        resolved.module.store.exprs.len()
    ];
    let pat_facts = vec![PatFacts::new(builtins.unknown); resolved.module.store.pats.len()];

    let mut decls = DeclState::new();
    let module_key = resolved.module_key.clone();
    seed_builtin_data_defs(&mut decls, &module_key);

    (
        ModuleState::new(resolved, binding_ids, import_targets),
        RuntimeEnv::new(interner, known, builtins)
            .with_target(options.target.unwrap_or_else(host_target_info))
            .with_env(options.env),
        TypingState::new(),
        decls,
        FactState::new(expr_facts, pat_facts),
        ResumeState::new(),
    )
}

fn seed_builtin_data_defs(decls: &mut DeclState, module: &ModuleKey) {
    let bool_variants = BTreeMap::from([
        (
            "False".into(),
            SemaDataVariantDef::new(0, None, None, Box::default(), Box::default()),
        ),
        (
            "True".into(),
            SemaDataVariantDef::new(1, None, None, Box::default(), Box::default()),
        ),
    ]);
    let _ = decls.data_defs.insert(
        "Bool".into(),
        SemaDataDef::new(
            DefinitionKey::new(module.clone(), "Bool"),
            bool_variants,
            None,
            None,
            None,
            false,
        ),
    );
}

pub fn finish_module(
    module: ModuleState,
    runtime: &RuntimeEnv<'_, '_>,
    typing: &TypingState,
    decls: DeclState,
    facts: FactState,
) -> SemaModule {
    let surface = build_module_surface(&module, runtime, typing, &decls);
    crate::build_sema_module(crate::SemaModuleBuild {
        resolved: module.resolved,
        context: crate::SemaContextBuild {
            target: runtime.target.clone(),
            gated_bindings: typing.gated_bindings.clone(),
            foreign_links: typing.foreign_links.clone(),
            binding_types: typing.binding_types().clone(),
            binding_schemes: typing.binding_schemes().clone(),
            binding_constraint_keys: typing.binding_constraint_keys().clone(),
            binding_import_record_targets: typing.binding_import_record_targets().clone(),
            binding_comptime_values: typing.binding_comptime_values().clone(),
        },
        facts: crate::SemaFactsBuild {
            expr_facts: facts.expr_facts,
            pat_facts: facts.pat_facts,
            expr_import_record_targets: facts.expr_import_record_targets,
            type_test_targets: facts.type_test_targets,
            expr_constraint_answers: facts.expr_constraint_answers,
            expr_dot_callable_bindings: facts.expr_dot_callable_bindings,
            expr_member_facts: facts.expr_member_facts,
            expr_comptime_values: facts.expr_comptime_values,
        },
        decls: crate::SemaDeclsBuild {
            effect_defs: decls.effect_defs,
            data_defs: decls.data_defs,
            shape_facts: decls.shape_facts,
            given_facts: decls.given_facts,
        },
        surface,
        diags: facts.diags,
    })
}
