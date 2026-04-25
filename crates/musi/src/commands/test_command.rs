use std::path::Path;
use std::slice::from_ref;

use crate::error::{MusiError, MusiResult};
use crate::workspace::uses_workspace_scope;
use musi_project::{
    PackageId, Project, ProjectOptions, ProjectTestTarget, ProjectTestTargetSource,
    load_project_ancestor,
};
use musi_rt::{Runtime, RuntimeOutputMode};
use music_session::SessionError;

use super::project_target::{
    normalized_project_target, project_anchor, reject_workspace_target, resolve_project_path_entry,
};
use super::runtime::project_runtime_with_output;
use super::test_report::{TestModuleReport, print_test_reports, test_target_label};

pub(super) fn test_project(target: Option<&Path>, workspace: u8) -> MusiResult {
    reject_workspace_target(workspace, target)?;
    let anchor = project_anchor(target)?;
    let project = load_project_ancestor(anchor, ProjectOptions::default())?;
    let target = normalized_project_target(&project, target)?;
    let tests = resolve_test_targets(&project, target.as_deref(), workspace)?;
    let mut reports = Vec::new();
    for test in &tests {
        let mut runtime = project_runtime_with_output(&project, RuntimeOutputMode::Capture);
        register_synthetic_test_modules(&project, from_ref(test), &mut runtime)?;
        let report = runtime.run_test_export(test.module_key.as_str(), &test.export_name)?;
        let label = test_target_label(&project, test);
        reports.push(TestModuleReport { label, report });
    }
    let failed = print_test_reports(&reports);
    if failed > 0 {
        return Err(MusiError::TaskFailed {
            name: format!("{failed} test case(s) failed"),
        });
    }
    Ok(())
}
fn resolve_test_targets(
    project: &Project,
    target: Option<&Path>,
    workspace: u8,
) -> MusiResult<Vec<ProjectTestTarget>> {
    reject_workspace_target(workspace, target)?;
    if uses_workspace_scope(project, target, workspace) {
        return Ok(project.test_targets()?);
    }
    let Some(target) = target else {
        let package = project.root_package()?;
        return package_test_targets(project, &package.id);
    };
    if target.extension().is_some_and(|ext| ext == "ms") || target.components().count() > 1 {
        let entry = resolve_project_path_entry(project, target)?;
        return Ok(vec![ProjectTestTarget::module(
            entry.package,
            entry.module_key,
            entry.path,
        )]);
    }
    let package_name = target.to_string_lossy();
    let Some(package) = project.package(package_name.as_ref()) else {
        let entry = resolve_project_path_entry(project, target)?;
        return Ok(vec![ProjectTestTarget::module(
            entry.package,
            entry.module_key,
            entry.path,
        )]);
    };
    Ok(project
        .test_targets()?
        .into_iter()
        .filter(|test| test.package == package.id)
        .collect())
}

fn package_test_targets(
    project: &Project,
    package: &PackageId,
) -> MusiResult<Vec<ProjectTestTarget>> {
    Ok(project
        .test_targets()?
        .into_iter()
        .filter(|test| &test.package == package)
        .collect())
}

fn register_synthetic_test_modules(
    project: &Project,
    tests: &[ProjectTestTarget],
    runtime: &mut Runtime,
) -> MusiResult {
    if !tests
        .iter()
        .any(|target| matches!(target.source, ProjectTestTargetSource::SyntheticModule))
    {
        return Ok(());
    }
    let mut session = project.build_session()?;
    let _ = session.law_suite_modules()?;
    let mut missing_module_key = None;
    for target in tests {
        if !matches!(target.source, ProjectTestTargetSource::SyntheticModule) {
            continue;
        }
        let Some(source) = session.module_text(&target.module_key) else {
            missing_module_key = Some(&target.module_key);
            break;
        };
        runtime.register_module_text(target.module_key.as_str(), String::from(source))?;
    }
    if let Some(module_key) = missing_module_key {
        return Err(MusiError::SessionCompilationFailed(
            SessionError::ModuleNotRegistered {
                key: module_key.clone(),
            },
        ));
    }
    Ok(())
}
