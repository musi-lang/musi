use std::path::Path;

use crate::error::MusiResult;
use musi_project::{ProjectOptions, load_project_ancestor};
use musi_tooling::read_artifact_bytes;
use music_seam::{decode_binary, format_decomp, format_disasm};

use super::project_target::{project_anchor, resolve_project_entry};

pub(super) fn disasm(target: &Path) -> MusiResult {
    let bytes = artifact_bytes_for(target)?;
    let artifact = decode_binary(&bytes)?;
    print!("{}", format_disasm(&artifact));
    Ok(())
}

pub(super) fn decomp(target: &Path) -> MusiResult {
    let bytes = artifact_bytes_for(target)?;
    let artifact = decode_binary(&bytes)?;
    print!("{}", format_decomp(&artifact));
    Ok(())
}

fn artifact_bytes_for(target: &Path) -> MusiResult<Vec<u8>> {
    if target.extension().is_some_and(|ext| ext == "seam") {
        return Ok(read_artifact_bytes(target)?);
    }
    let anchor = project_anchor(Some(target))?;
    let project = load_project_ancestor(anchor, ProjectOptions::default())?;
    let entry = resolve_project_entry(&project, Some(target))?;
    let mut session = project.build_session()?;
    Ok(session.compile_entry(&entry.module_key)?.bytes)
}
