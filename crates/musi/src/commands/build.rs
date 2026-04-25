use std::path::Path;

use crate::error::{MusiError, MusiResult};
use musi_project::{ProjectOptions, load_project_ancestor};
use musi_tooling::write_artifact_bytes;
use music_sema::TargetInfo;

use super::project_target::{
    manifest_output_path, project_anchor, reject_workspace_target, resolve_project_entries,
};

pub(super) fn build(
    target: Option<&Path>,
    workspace: u8,
    out: Option<&Path>,
    target_name: Option<&str>,
) -> MusiResult {
    reject_workspace_target(workspace, target)?;
    if workspace > 0 && out.is_some() {
        return Err(MusiError::IncompatibleCommandArgs {
            left: "--workspace".to_owned(),
            right: "--out".to_owned(),
        });
    }
    let mut options = ProjectOptions::default();
    if let Some(target_name) = target_name {
        options.target = Some(target_info(target_name));
    }
    let anchor = project_anchor(target)?;
    let project = load_project_ancestor(anchor, options)?;
    let mut session = project.build_session()?;
    for entry in resolve_project_entries(&project, target, workspace)? {
        let output = session.compile_entry(&entry.module_key)?;
        let out_path = out
            .map(Path::to_path_buf)
            .or_else(|| manifest_output_path(&project, &entry))
            .unwrap_or_else(|| entry.path.with_extension("seam"));
        write_artifact_bytes(&out_path, &output.bytes)?;
        println!("{}", out_path.display());
    }
    Ok(())
}

fn target_info(target_name: &str) -> TargetInfo {
    TargetInfo::new().with_os(target_name)
}
