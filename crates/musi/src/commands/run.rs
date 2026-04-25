use std::path::Path;

use crate::error::MusiResult;
use musi_project::{ProjectOptions, load_project_ancestor};
use musi_rt::RuntimeOutputMode;

use super::project_target::{project_anchor, resolve_project_entry};
use super::runtime::{mvm_options_from_args, project_runtime_with_vm};

pub(super) fn run_project(target: Option<&Path>, args: &[String]) -> MusiResult {
    let vm_options = mvm_options_from_args(args)?;
    let anchor = project_anchor(target)?;
    let project = load_project_ancestor(anchor, ProjectOptions::default())?;
    let entry = resolve_project_entry(&project, target)?;
    let mut runtime = project_runtime_with_vm(&project, RuntimeOutputMode::Inherit, vm_options);
    runtime.load_root(entry.module_key.as_str())?;
    Ok(())
}
