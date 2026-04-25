use std::collections::{BTreeMap, BTreeSet};
use std::fs;
use std::path::{Path, PathBuf};

use music_base::SourceId;
use music_base::diag::DiagContext;
use music_module::{ImportSiteKind, ModuleKey, collect_import_sites};
use music_syntax::{Lexer, parse};

use crate::builtin_std::{
    STD_FILES, STD_MANIFEST, STD_MANIFEST_PATH, STD_PACKAGE_NAME, STD_ROOT_DIR,
};
use crate::errors::ProjectError;
use crate::lock::Lockfile;
use crate::manifest::PackageManifest;
use crate::manifest_source::ManifestSource;
use crate::project::dependencies::{DependencyResolutionInputs, resolve_package_dependencies};
use crate::project::manifest::{manifest_path_for, read_manifest, validate_manifest};
use crate::project::model::{
    LoadedImportSite, LoadedManifest, LoadedModule, LocalPackage, LocalPackageMap, PackageId,
    PackageRecord, PackageSource, ProjectEntry, ProjectOptions, ResolvedPackage,
    ResolvedWorkspaceState, WorkspaceGraph, WorkspaceSeed,
};
use crate::project::module_graph::{
    build_import_map, build_lockfile, discover_modules, module_key_for, resolve_module_target,
};
use crate::project::storage::ProjectStorage;
use crate::{ProjectDiagKind, ProjectResult};

pub(super) fn resolve_workspace_state(
    root_dir: &Path,
    root_manifest_path: &Path,
    root_manifest_source: &ManifestSource,
    manifest: &PackageManifest,
    options: &ProjectOptions,
    storage: &ProjectStorage,
    loaded_lockfile: &Lockfile,
) -> ProjectResult<ResolvedWorkspaceState> {
    let local_packages =
        load_local_packages(root_dir, manifest, root_manifest_path, root_manifest_source)?;
    let WorkspaceSeed {
        mut package_records,
        mut package_name_index,
    } = seed_workspace_packages(&local_packages)?;
    seed_builtin_std_package(
        &local_packages,
        &mut package_records,
        &mut package_name_index,
    )?;
    let mut resolving = BTreeSet::<String>::new();

    let root_package = manifest
        .name
        .as_ref()
        .and_then(|name| package_name_index.get(name).cloned());
    let members = manifest
        .workspace_members()
        .iter()
        .map(|member| member_manifest_name(root_dir, member))
        .collect::<Result<Vec<_>, _>>()?
        .into_iter()
        .filter_map(|name| package_name_index.get(&name).cloned())
        .collect::<Vec<_>>();

    let package_ids = package_records.keys().cloned().collect::<Vec<_>>();
    let dep_inputs = DependencyResolutionInputs {
        local_packages: &local_packages,
        options,
        storage,
        lockfile: loaded_lockfile,
    };
    for package_id in package_ids {
        resolve_package_dependencies(
            &package_id,
            &dep_inputs,
            &mut package_records,
            &mut package_name_index,
            &mut resolving,
        )?;
    }

    let import_map = build_import_map(&package_records, &package_name_index)?;
    let module_texts = package_records
        .values()
        .flat_map(|record| {
            record
                .modules
                .values()
                .map(|module| (module.key.clone(), module.text.clone()))
        })
        .collect::<BTreeMap<_, _>>();
    let resolved_lockfile = build_lockfile(&package_records);
    let packages = package_records
        .into_iter()
        .map(|(id, record)| (id, record.package))
        .collect();
    let workspace = if let Some(root_package) = root_package {
        WorkspaceGraph::new(members, packages).with_root_package(root_package)
    } else {
        WorkspaceGraph::new(members, packages)
    };

    Ok(ResolvedWorkspaceState {
        workspace,
        package_name_index,
        import_map,
        module_texts,
        resolved_lockfile,
    })
}

fn seed_builtin_std_package(
    local_packages: &LocalPackageMap,
    package_records: &mut BTreeMap<PackageId, PackageRecord>,
    package_name_index: &mut BTreeMap<String, PackageId>,
) -> ProjectResult {
    if package_name_index.contains_key(STD_PACKAGE_NAME)
        || !local_packages
            .values()
            .any(|package| package_uses_std(&package.manifest))
    {
        return Ok(());
    }
    let record = load_builtin_std_record()?;
    let _ = package_name_index.insert(STD_PACKAGE_NAME.into(), record.package.id.clone());
    let _ = package_records.insert(record.package.id.clone(), record);
    Ok(())
}

fn package_uses_std(manifest: &PackageManifest) -> bool {
    manifest.enabled_libs().contains(&"std")
}

fn load_builtin_std_record() -> ProjectResult<PackageRecord> {
    let manifest_path = PathBuf::from(STD_MANIFEST_PATH);
    let manifest_source = ManifestSource::from_text(manifest_path.clone(), STD_MANIFEST.into());
    let manifest = serde_json::from_str::<PackageManifest>(STD_MANIFEST)
        .map_err(|source| manifest_source.parse_error(&source))?;
    validate_manifest(&manifest, &manifest_source)?;
    load_embedded_package_record(
        PathBuf::from(STD_ROOT_DIR),
        manifest_path,
        &manifest_source,
        manifest,
        PackageSource::Builtin,
        STD_FILES,
    )
}

fn load_embedded_package_record(
    root_dir: PathBuf,
    manifest_path: PathBuf,
    manifest_source: &ManifestSource,
    manifest: PackageManifest,
    source: PackageSource,
    files: &[(&str, &str)],
) -> ProjectResult<PackageRecord> {
    let id = PackageId::new(
        manifest.name.clone().ok_or_else(|| {
            manifest_source.catalog_error_with_hint(
                ProjectDiagKind::ManifestPackageNameMissing,
                DiagContext::new().with("path", manifest_source.path().display()),
                manifest_source.insertion_span(),
                "add `name` to this package manifest",
            )
        })?,
        manifest.version.clone().ok_or_else(|| {
            manifest_source.catalog_error_with_hint(
                ProjectDiagKind::ManifestPackageVersionMissing,
                DiagContext::new().with("name", manifest.name.as_deref().unwrap_or("<unknown>")),
                manifest_source.insertion_span(),
                "add `version` to this package manifest",
            )
        })?,
    );
    let modules = files
        .iter()
        .map(|(relative, text)| embedded_module(&id, &root_dir, relative, text))
        .collect::<BTreeMap<_, _>>();
    package_record_from_modules(
        id,
        root_dir,
        manifest_path,
        source,
        manifest_source,
        manifest,
        modules,
    )
}

fn embedded_module(
    package_id: &PackageId,
    root_dir: &Path,
    relative: &str,
    text: &str,
) -> (ModuleKey, LoadedModule) {
    let key = module_key_for(package_id, relative);
    let path = root_dir.join(relative);
    let parsed = parse(Lexer::new(text).lex());
    let imports = collect_import_sites(SourceId::from_raw(0), parsed.tree())
        .into_iter()
        .filter_map(|site| match site.kind {
            ImportSiteKind::Static { spec } => Some(LoadedImportSite {
                spec: spec.as_str().to_owned(),
                span: site.span,
            }),
            ImportSiteKind::NonLiteral | ImportSiteKind::InvalidStringLit => None,
        })
        .collect::<Vec<_>>();
    let module = LoadedModule {
        key: key.clone(),
        path,
        package_relative: relative.into(),
        local_scope_key: ModuleKey::new(format!("./{relative}")),
        text: text.into(),
        imports,
    };
    (key, module)
}

pub(super) fn load_lockfile(path: &Path) -> ProjectResult<Lockfile> {
    if !path.exists() {
        return Ok(Lockfile {
            version: 1,
            packages: Vec::new(),
        });
    }
    let text = fs::read_to_string(path).map_err(|source| ProjectError::ProjectIoFailed {
        path: path.to_path_buf(),
        source,
    })?;
    serde_json::from_str(&text).map_err(|source| ProjectError::InvalidManifestJson {
        path: path.to_path_buf(),
        source,
    })
}

fn seed_workspace_packages(local_packages: &LocalPackageMap) -> ProjectResult<WorkspaceSeed> {
    let mut package_records = BTreeMap::<PackageId, PackageRecord>::new();
    let mut package_name_index = BTreeMap::<String, PackageId>::new();
    for (name, package) in local_packages {
        let package_record = load_package_record(
            package.root_dir.clone(),
            package.manifest_path.clone(),
            &package.source,
            package.manifest.clone(),
            PackageSource::Workspace,
        )?;
        let _ = package_name_index.insert(name.clone(), package_record.package.id.clone());
        let _ = package_records.insert(package_record.package.id.clone(), package_record);
    }
    Ok(WorkspaceSeed {
        package_records,
        package_name_index,
    })
}

fn load_local_packages(
    root_dir: &Path,
    manifest: &PackageManifest,
    root_manifest_path: &Path,
    root_manifest_source: &ManifestSource,
) -> ProjectResult<LocalPackageMap> {
    let mut out = BTreeMap::new();
    if let Some(name) = manifest.name.clone() {
        let _ = manifest
            .version
            .clone()
            .ok_or_else(|| ProjectError::MissingPackageVersion { name: name.clone() })?;
        let package = LocalPackage {
            manifest_path: root_manifest_path.to_path_buf(),
            root_dir: root_dir.to_path_buf(),
            source: root_manifest_source.clone(),
            manifest: manifest.clone(),
        };
        if out.insert(name.clone(), package).is_some() {
            return Err(ProjectError::DuplicatePackageName { name });
        }
    }

    let mut member_roots = BTreeSet::new();
    for member in manifest.workspace_members() {
        let member_root = root_dir.join(member);
        if !member_roots.insert(member_root.clone()) {
            return Err(ProjectError::DuplicateWorkspaceMember {
                member: member.clone(),
            });
        }
        let member_manifest_path = manifest_path_for(&member_root)?;
        let LoadedManifest {
            manifest: member_manifest,
            source: member_source,
        } = read_manifest(&member_manifest_path)?;
        validate_manifest(&member_manifest, &member_source)?;
        let name = member_manifest.name.clone().ok_or_else(|| {
            member_source.catalog_error_with_hint(
                ProjectDiagKind::ManifestPackageNameMissing,
                DiagContext::new().with("path", member_source.path().display()),
                member_source.insertion_span(),
                "add `name` to this workspace member manifest",
            )
        })?;
        let _ = member_manifest
            .version
            .clone()
            .ok_or_else(|| ProjectError::MissingPackageVersion { name: name.clone() })?;
        let package = LocalPackage {
            manifest_path: member_manifest_path,
            root_dir: member_root,
            source: member_source,
            manifest: member_manifest,
        };
        if out.insert(name.clone(), package).is_some() {
            return Err(ProjectError::DuplicatePackageName { name });
        }
    }
    Ok(out)
}

fn member_manifest_name(root_dir: &Path, member: &str) -> ProjectResult<String> {
    let manifest_path = manifest_path_for(&root_dir.join(member))?;
    let LoadedManifest { manifest, source } = read_manifest(&manifest_path)?;
    manifest.name.ok_or_else(|| {
        source.catalog_error_with_hint(
            ProjectDiagKind::ManifestPackageNameMissing,
            DiagContext::new().with("path", source.path().display()),
            source.insertion_span(),
            "add `name` to this workspace member manifest",
        )
    })
}

pub(super) fn load_package_record(
    root_dir: PathBuf,
    manifest_path: PathBuf,
    manifest_source: &ManifestSource,
    manifest: PackageManifest,
    source: PackageSource,
) -> ProjectResult<PackageRecord> {
    let id = PackageId::new(
        manifest.name.clone().ok_or_else(|| {
            manifest_source.catalog_error_with_hint(
                ProjectDiagKind::ManifestPackageNameMissing,
                DiagContext::new().with("path", manifest_source.path().display()),
                manifest_source.insertion_span(),
                "add `name` to this package manifest",
            )
        })?,
        manifest.version.clone().ok_or_else(|| {
            manifest_source.catalog_error_with_hint(
                ProjectDiagKind::ManifestPackageVersionMissing,
                DiagContext::new().with("name", manifest.name.as_deref().unwrap_or("<unknown>")),
                manifest_source.insertion_span(),
                "add `version` to this package manifest",
            )
        })?,
    );
    let modules = discover_modules(&id, &root_dir)?;
    package_record_from_modules(
        id,
        root_dir,
        manifest_path,
        source,
        manifest_source,
        manifest,
        modules,
    )
}

fn package_record_from_modules(
    id: PackageId,
    root_dir: PathBuf,
    manifest_path: PathBuf,
    source: PackageSource,
    manifest_source: &ManifestSource,
    manifest: PackageManifest,
    modules: BTreeMap<ModuleKey, LoadedModule>,
) -> ProjectResult<PackageRecord> {
    let relative_modules = modules
        .values()
        .map(|module| (module.package_relative.clone(), module.key.clone()))
        .collect::<BTreeMap<_, _>>();
    let module_keys = modules
        .values()
        .map(|module| (module.key.clone(), module.path.clone()))
        .collect::<BTreeMap<_, _>>();
    let entry_key =
        resolve_module_target(&root_dir, &relative_modules, None, manifest.entry_path())
            .ok_or_else(|| {
                package_entry_missing(manifest_source, &id.name, manifest.entry.as_deref())
            })?;
    let entry_path = module_keys.get(&entry_key).cloned().ok_or_else(|| {
        package_entry_missing(manifest_source, &id.name, manifest.entry.as_deref())
    })?;
    let entry = ProjectEntry::new(id.clone(), entry_key, entry_path);

    let mut exports = BTreeMap::new();
    for (name, target) in manifest.export_map() {
        let export_key = resolve_module_target(&root_dir, &relative_modules, None, &target)
            .ok_or_else(|| missing_export_target(manifest_source, &id.name, &name, &target))?;
        let _ = exports.insert(name, export_key);
    }

    let package = ResolvedPackage::new(id, manifest_path, root_dir, source, manifest, entry)
        .with_exports(exports)
        .with_module_keys(module_keys);

    Ok(PackageRecord {
        package,
        modules,
        relative_modules,
    })
}

fn package_entry_missing(
    manifest_source: &ManifestSource,
    package_name: &str,
    entry_target: Option<&str>,
) -> ProjectError {
    let span = entry_target
        .and_then(|_| manifest_source.value_span(&json_pointer(&["entry"])))
        .unwrap_or_else(|| manifest_source.insertion_span());
    entry_target.map_or_else(
        || {
            manifest_source.catalog_error(
                ProjectDiagKind::ManifestEntryDefaultMissing,
                DiagContext::new().with("package", package_name),
                span,
            )
        },
        |target| {
            manifest_source.catalog_error(
                ProjectDiagKind::ManifestEntryTargetMissing,
                DiagContext::new()
                    .with("package", package_name)
                    .with("target", target),
                span,
            )
        },
    )
}

fn json_pointer(segments: &[&str]) -> String {
    let mut pointer = String::new();
    for segment in segments {
        pointer.push('/');
        pointer.push_str(segment);
    }
    pointer
}

fn missing_export_target(
    manifest_source: &ManifestSource,
    package_name: &str,
    export_name: &str,
    target: &str,
) -> ProjectError {
    let pointer = format!("/exports/{}", escape_pointer_segment(export_name));
    let span = manifest_source
        .value_span(&pointer)
        .or_else(|| manifest_source.key_span(&pointer))
        .unwrap_or_else(|| manifest_source.insertion_span());
    manifest_source.catalog_error_with_hint(
        ProjectDiagKind::ManifestExportTargetMissing,
        DiagContext::new()
            .with("package", package_name)
            .with("export", export_name)
            .with("target", target),
        span,
        "update export target or add target module",
    )
}

fn escape_pointer_segment(segment: &str) -> String {
    segment.replace('~', "~0").replace('/', "~1")
}
