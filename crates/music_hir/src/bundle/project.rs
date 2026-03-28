use std::collections::HashMap;
use std::path::PathBuf;

use music_owned::modules::is_compiler_owned_path;
use music_resolve::{ModuleId, ModuleLoader, ModuleResult, ProjectResolution};
use music_sema::{TypeEnv, type_check};

use super::diag::{
    apply_diag_policy, collect_diag_policy, finalize_diag_policy, validate_attributes,
};
use super::typed::{TypedModule, TypedProject};
use crate::lower;

/// Convert a resolved project into the canonical typed-project artifact.
#[must_use]
pub fn type_project(project: ProjectResolution, loader: ModuleLoader) -> TypedProject {
    let ProjectResolution {
        graph,
        modules,
        order,
    } = project;
    let mut typed_modules = HashMap::with_capacity(modules.len());

    for (module_id, module) in modules {
        let path = graph.path(module_id).to_path_buf();
        let typed = type_module(module, Some(module_id), path);
        let _ = typed_modules.insert(module_id, typed);
    }

    TypedProject {
        loader,
        graph,
        modules: typed_modules,
        order,
    }
}

/// Convert one resolved module into the canonical typed-module artifact.
#[must_use]
pub fn type_module(
    module: ModuleResult,
    module_id: Option<ModuleId>,
    path: PathBuf,
) -> TypedModule {
    let ModuleResult {
        mut db,
        resolution,
        errors: _,
        mut diagnostics,
        has_errors,
    } = module;

    let source_id = db.source.iter().next().map(|source| source.id());
    if let Some(source_id) = source_id {
        diagnostics.extend(validate_attributes(
            &db,
            is_compiler_owned_path(&path),
            source_id,
        ));
    }

    let mut diag_policy = collect_diag_policy(&db, &db.interner);
    if let Some(source_id) = source_id {
        apply_diag_policy(&mut diagnostics, &mut diag_policy, source_id);
    }

    let has_errors = has_errors || !diagnostics.is_empty();

    if has_errors {
        if let Some(source_id) = source_id {
            finalize_diag_policy(&mut diagnostics, &mut diag_policy, source_id);
        }
        return TypedModule::with_status(
            db,
            resolution,
            TypeEnv::new(),
            diagnostics,
            true,
            module_id,
            path,
        );
    }

    lower(&mut db.ast);
    let (db, resolution, type_env, sema_errors) = type_check(db, resolution, None);
    let has_sema_errors = !sema_errors.is_empty();
    let mut typed_diagnostics = diagnostics;
    let source_id = db.source.iter().next().map(|source| source.id());
    if let Some(source_id) = source_id {
        for err in &sema_errors {
            typed_diagnostics.push(err.diagnostic(&db.interner, source_id));
        }
    }

    if let Some(source_id) = source_id {
        apply_diag_policy(&mut typed_diagnostics, &mut diag_policy, source_id);
        finalize_diag_policy(&mut typed_diagnostics, &mut diag_policy, source_id);
    }

    TypedModule::with_status(
        db,
        resolution,
        type_env,
        typed_diagnostics,
        has_sema_errors,
        module_id,
        path,
    )
}
