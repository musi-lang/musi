use std::collections::BTreeSet;
use std::path::{Path, PathBuf};

use crate::cli::{BuildPackageArg, BuildProfileArg};
use crate::error::{MusiError, MusiResult};
use musi_project::{Project, ProjectEntry, ProjectOptions, load_project_ancestor};
use musi_tooling::write_artifact_bytes;
use music_base::{Source, SourceMap};
use music_module::ModuleKey;
use music_seam::descriptor::{ExportTarget, ProcedureVisibility};
use music_seam::{
    Artifact, MarArchive, MarManifest, MarModuleEntry, MarOptimizationPolicy, MarPackageKind,
    MarProfile, StringId, decode_binary, encode_binary, encode_mar_archive,
};
use music_sema::TargetInfo;
use music_session::Session;
use serde_json::{Map, Value};

use super::project_target::{
    manifest_output_path, project_anchor, reject_workspace_target, resolve_project_entries,
};

const BUILD_SOURCE_MAP_VERSION: u64 = 1;

pub(super) fn build(
    target: Option<&Path>,
    workspace: u8,
    out: Option<&Path>,
    target_name: Option<&str>,
    archive: u8,
    profile: Option<BuildProfileArg>,
    package: Option<BuildPackageArg>,
) -> MusiResult {
    reject_workspace_target(workspace, target)?;
    validate_archive_options(archive, profile, package)?;
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
        let out_path = build_output_path(out, &project, &entry, archive > 0);
        if archive > 0 {
            reject_archive_output_extension(&out_path)?;
            let archive_profile = profile.unwrap_or(BuildProfileArg::Debug);
            let archive_package = package.unwrap_or(BuildPackageArg::Fat);
            let manifest = MarManifest::new(
                entry.package.name.clone(),
                entry.package.version.clone(),
                archive_profile.into(),
                archive_package.into(),
            )
            .with_entry_module(entry.module_key.as_str());
            let manifest = if archive_profile == BuildProfileArg::Release
                && archive_package == BuildPackageArg::Fat
            {
                manifest.with_optimization_policy(MarOptimizationPolicy::release_fat())
            } else {
                manifest
            };
            let modules = archive_modules(
                &mut session,
                &entry.module_key,
                archive_profile,
                archive_package,
            )?;
            let archive = MarArchive::new(manifest, modules);
            let bytes = encode_mar_archive(&archive)?;
            write_artifact_bytes(&out_path, &bytes)?;
            if archive_profile == BuildProfileArg::Debug {
                write_source_map_sidecar(
                    &out_path,
                    entry.module_key.as_str(),
                    session.source_map(),
                )?;
            }
        } else {
            let output = session.compile_entry(&entry.module_key)?;
            write_artifact_bytes(&out_path, &output.bytes)?;
            write_source_map_sidecar(&out_path, entry.module_key.as_str(), session.source_map())?;
        }
        println!("{}", out_path.display());
    }
    Ok(())
}

fn archive_modules(
    session: &mut Session,
    entry_module_key: &ModuleKey,
    profile: BuildProfileArg,
    package: BuildPackageArg,
) -> MusiResult<Vec<MarModuleEntry>> {
    if profile == BuildProfileArg::Release && package == BuildPackageArg::Fat {
        let output = session.compile_entry(entry_module_key)?;
        let blob = apply_release_fat_module_policy(&output.bytes)?;
        return Ok(vec![MarModuleEntry::new(entry_module_key.as_str(), blob)]);
    }
    let outputs = if package == BuildPackageArg::Fat {
        session.compile_entry_modules(entry_module_key)?
    } else {
        vec![(
            entry_module_key.clone(),
            session.compile_entry(entry_module_key)?,
        )]
    };
    Ok(outputs
        .into_iter()
        .map(|(module_key, output)| MarModuleEntry::new(module_key.as_str(), output.bytes))
        .collect())
}

fn apply_release_fat_module_policy(bytes: &[u8]) -> MusiResult<Vec<u8>> {
    let mut artifact = decode_binary(bytes)?;
    mangle_private_symbol_names(&mut artifact);
    Ok(encode_binary(&artifact)?)
}

fn mangle_private_symbol_names(artifact: &mut Artifact) {
    let preserved_name_ids = preserved_public_name_ids(artifact);
    let mut used_names = artifact
        .strings
        .as_slice()
        .iter()
        .map(|record| record.text.to_string())
        .collect::<BTreeSet<_>>();
    let private_procedure_names = private_procedure_name_ids(artifact, &preserved_name_ids);
    let private_global_names = private_global_name_ids(artifact, &preserved_name_ids);
    rename_private_procedure_names(artifact, private_procedure_names, "__p", &mut used_names);
    rename_symbol_names(artifact, private_global_names, "__g", &mut used_names);
}

fn preserved_public_name_ids(artifact: &Artifact) -> BTreeSet<StringId> {
    let mut preserved_name_ids = BTreeSet::new();
    for (_, export_descriptor) in artifact.exports.iter() {
        let _ = preserved_name_ids.insert(export_descriptor.name);
        match export_descriptor.target {
            ExportTarget::Procedure(procedure_id) => {
                let _ = preserved_name_ids.insert(artifact.procedures.get(procedure_id).name);
            }
            ExportTarget::Global(global_id) => {
                let _ = preserved_name_ids.insert(artifact.globals.get(global_id).name);
            }
            ExportTarget::Foreign(foreign_id) => {
                let foreign_descriptor = artifact.foreigns.get(foreign_id);
                let _ = preserved_name_ids.insert(foreign_descriptor.name);
                let _ = preserved_name_ids.insert(foreign_descriptor.symbol);
            }
            ExportTarget::Type(_) | ExportTarget::Shape(_) => {}
        }
    }
    for (_, procedure_descriptor) in artifact.procedures.iter() {
        if procedure_descriptor.visibility != ProcedureVisibility::Private
            || procedure_descriptor.export
        {
            let _ = preserved_name_ids.insert(procedure_descriptor.name);
        }
    }
    for (_, global_descriptor) in artifact.globals.iter() {
        if global_descriptor.export {
            let _ = preserved_name_ids.insert(global_descriptor.name);
        }
    }
    preserved_name_ids
}

fn private_procedure_name_ids(
    artifact: &Artifact,
    preserved_name_ids: &BTreeSet<StringId>,
) -> BTreeSet<StringId> {
    artifact
        .procedures
        .iter()
        .filter_map(|(_, procedure_descriptor)| {
            (procedure_descriptor.visibility == ProcedureVisibility::Private
                && !procedure_descriptor.export
                && !preserved_name_ids.contains(&procedure_descriptor.name))
            .then_some(procedure_descriptor.name)
        })
        .collect()
}

fn private_global_name_ids(
    artifact: &Artifact,
    preserved_name_ids: &BTreeSet<StringId>,
) -> BTreeSet<StringId> {
    artifact
        .globals
        .iter()
        .filter_map(|(_, global_descriptor)| {
            (!global_descriptor.export && !preserved_name_ids.contains(&global_descriptor.name))
                .then_some(global_descriptor.name)
        })
        .collect()
}

fn rename_symbol_names(
    artifact: &mut Artifact,
    name_ids: BTreeSet<StringId>,
    prefix: &str,
    used_names: &mut BTreeSet<String>,
) {
    let mut suffix = 0_u32;
    for name_id in name_ids {
        let mangled_name = next_mangled_name(prefix, &mut suffix, used_names);
        artifact.strings.get_mut(name_id).text = mangled_name.clone().into_boxed_str();
        let _ = used_names.insert(mangled_name);
    }
}

fn rename_private_procedure_names(
    artifact: &mut Artifact,
    name_ids: BTreeSet<StringId>,
    prefix: &str,
    used_names: &mut BTreeSet<String>,
) {
    let mut suffix = 0_u32;
    for name_id in name_ids {
        let old_name = artifact.strings.get(name_id).text.clone();
        let mangled_name = next_mangled_name(prefix, &mut suffix, used_names);
        artifact.strings.get_mut(name_id).text = mangled_name.clone().into_boxed_str();
        rename_root_map_safe_point_prefix(artifact, old_name.as_ref(), &mangled_name);
        let _ = used_names.insert(mangled_name);
    }
}

fn rename_root_map_safe_point_prefix(artifact: &mut Artifact, old_name: &str, new_name: &str) {
    let old_colon_prefix = format!("{old_name}:");
    let old_dot_prefix = format!("{old_name}.");
    let safe_point_ids = artifact
        .root_maps
        .iter()
        .map(|(_, root_map_descriptor)| root_map_descriptor.safe_point)
        .collect::<Vec<_>>();
    for safe_point_id in safe_point_ids {
        let safe_point_text = artifact.strings.get(safe_point_id).text.clone();
        let rewritten = safe_point_text
            .strip_prefix(&old_colon_prefix)
            .map(|suffix| format!("{new_name}:{suffix}"))
            .or_else(|| {
                safe_point_text
                    .strip_prefix(&old_dot_prefix)
                    .map(|suffix| format!("{new_name}.{suffix}"))
            });
        if let Some(text) = rewritten {
            artifact.strings.get_mut(safe_point_id).text = text.into_boxed_str();
        }
    }
}

fn next_mangled_name(prefix: &str, suffix: &mut u32, used_names: &BTreeSet<String>) -> String {
    loop {
        let candidate = format!("{prefix}{suffix}");
        *suffix = suffix.saturating_add(1);
        if !used_names.contains(&candidate) {
            return candidate;
        }
    }
}

fn write_source_map_sidecar(
    artifact_path: &Path,
    entry_module_key: &str,
    source_map: &SourceMap,
) -> MusiResult {
    let sidecar_path = source_map_sidecar_path(artifact_path);
    let payload = source_map_payload(artifact_path, entry_module_key, source_map);
    let bytes = serde_json::to_vec_pretty(&payload)?;
    write_artifact_bytes(&sidecar_path, &bytes)?;
    Ok(())
}

fn source_map_sidecar_path(artifact_path: &Path) -> PathBuf {
    let mut sidecar = artifact_path.as_os_str().to_os_string();
    sidecar.push(".map");
    PathBuf::from(sidecar)
}

fn source_map_payload(
    artifact_path: &Path,
    entry_module_key: &str,
    source_map: &SourceMap,
) -> Value {
    let mut payload = Map::new();
    let _ = payload.insert("version".to_owned(), Value::from(BUILD_SOURCE_MAP_VERSION));
    let _ = payload.insert(
        "artifact_file_name".to_owned(),
        Value::String(file_name_string(artifact_path)),
    );
    let _ = payload.insert(
        "artifact_path".to_owned(),
        Value::String(path_string(artifact_path)),
    );
    let _ = payload.insert(
        "entry_module_key".to_owned(),
        Value::String(entry_module_key.to_owned()),
    );
    let _ = payload.insert(
        "sources".to_owned(),
        Value::Array(source_map.iter().map(source_map_source_payload).collect()),
    );
    Value::Object(payload)
}

fn source_map_source_payload(source: &Source) -> Value {
    let mut payload = Map::new();
    let _ = payload.insert("id".to_owned(), Value::from(u64::from(source.id().raw())));
    let _ = payload.insert("path".to_owned(), Value::String(path_string(source.path())));
    let _ = payload.insert(
        "line_count".to_owned(),
        Value::from(u64::try_from(source.line_count()).expect("source line count overflow")),
    );
    let _ = payload.insert(
        "byte_length".to_owned(),
        Value::from(u64::try_from(source.text().len()).expect("source byte length overflow")),
    );
    let _ = payload.insert("text".to_owned(), Value::String(source.text().to_owned()));
    Value::Object(payload)
}

fn file_name_string(path: &Path) -> String {
    path.file_name().map_or_else(
        || path_string(path),
        |name| name.to_string_lossy().into_owned(),
    )
}

fn path_string(path: &Path) -> String {
    path.to_string_lossy().into_owned()
}

fn reject_archive_output_extension(path: &Path) -> MusiResult {
    if path.extension().is_some_and(|ext| ext != "mar") {
        return Err(MusiError::IncompatibleCommandArgs {
            left: "--archive".to_owned(),
            right: path.display().to_string(),
        });
    }
    Ok(())
}

fn validate_archive_options(
    archive: u8,
    profile: Option<BuildProfileArg>,
    package: Option<BuildPackageArg>,
) -> MusiResult {
    if archive == 0 {
        if profile.is_some() {
            return Err(MusiError::IncompatibleCommandArgs {
                left: "--profile".to_owned(),
                right: "missing --archive".to_owned(),
            });
        }
        if package.is_some() {
            return Err(MusiError::IncompatibleCommandArgs {
                left: "--package".to_owned(),
                right: "missing --archive".to_owned(),
            });
        }
    }
    Ok(())
}

fn build_output_path(
    out: Option<&Path>,
    project: &Project,
    entry: &ProjectEntry,
    archive: bool,
) -> PathBuf {
    if let Some(out) = out {
        return out.to_path_buf();
    }
    let path = manifest_output_path(project, entry).unwrap_or_else(|| {
        entry
            .path
            .with_extension(if archive { "mar" } else { "seam" })
    });
    if archive && path.extension().is_some_and(|ext| ext == "seam") {
        path.with_extension("mar")
    } else {
        path
    }
}

fn target_info(target_name: &str) -> TargetInfo {
    TargetInfo::new().with_os(target_name)
}

impl From<BuildProfileArg> for MarProfile {
    fn from(profile: BuildProfileArg) -> Self {
        match profile {
            BuildProfileArg::Debug => Self::Debug,
            BuildProfileArg::Release => Self::Release,
        }
    }
}

impl From<BuildPackageArg> for MarPackageKind {
    fn from(package: BuildPackageArg) -> Self {
        match package {
            BuildPackageArg::Thin => Self::Thin,
            BuildPackageArg::Fat => Self::Fat,
        }
    }
}
