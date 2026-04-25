use std::collections::{BTreeMap, BTreeSet};
use std::path::{Path, PathBuf};

use crate::ProjectResult;
use crate::errors::ProjectError;
use crate::lock::{LockedPackageSource, Lockfile};
use crate::manifest::PackageManifest;
use crate::manifest_source::ManifestSource;
use crate::project::git::{GitRequirement, locked_or_latest_git_package};
use crate::project::manifest::{manifest_path_for, read_manifest, validate_manifest};
use crate::project::model::{
    LoadedManifest, LocalPackageMap, PackageId, PackageRecord, PackageSource, ProjectOptions,
};
use crate::project::storage::ProjectStorage;
use crate::project::workspace::load_package_record;
use crate::registry::{RegistryPackage, copy_dir_recursive, resolve_registry_package};

pub(super) struct DependencyResolutionInputs<'a> {
    pub(super) local_packages: &'a LocalPackageMap,
    pub(super) options: &'a ProjectOptions,
    pub(super) storage: &'a ProjectStorage,
    pub(super) lockfile: &'a Lockfile,
}

pub(super) fn resolve_package_dependencies(
    package_id: &PackageId,
    inputs: &DependencyResolutionInputs<'_>,
    package_records: &mut BTreeMap<PackageId, PackageRecord>,
    package_name_index: &mut BTreeMap<String, PackageId>,
    resolving: &mut BTreeSet<String>,
) -> ProjectResult {
    if !resolving.insert(package_id.name.clone()) {
        return Err(ProjectError::PackageDependencyCycle {
            name: package_id.name.clone(),
        });
    }

    let manifest = package_records
        .get(package_id)
        .map(|record| record.package.manifest.clone())
        .ok_or_else(|| ProjectError::PackageGraphEntryMissing {
            name: package_id.name.clone(),
        })?;

    for (section, deps) in manifest.dependency_maps() {
        let mut dependency_ids = BTreeMap::new();
        let mut dep_ctx = ResolveDepCtx {
            inputs,
            package_records,
            package_name_index,
            resolving,
        };
        for (name, requirement) in deps {
            if let Some(id) = resolve_dependency(
                &mut dep_ctx,
                name,
                requirement,
                section == "optionalDependencies",
            )? {
                let _ = dependency_ids.insert(name.clone(), id);
            }
        }
        let record = package_records.get_mut(package_id).ok_or_else(|| {
            ProjectError::PackageGraphEntryMissing {
                name: package_id.name.clone(),
            }
        })?;
        match section {
            "dependencies" => record.package.dependencies = dependency_ids,
            "devDependencies" => record.package.dev_dependencies = dependency_ids,
            "peerDependencies" => record.package.peer_dependencies = dependency_ids,
            "optionalDependencies" => record.package.optional_dependencies = dependency_ids,
            _ => {}
        }
    }

    let _ = resolving.remove(&package_id.name);
    Ok(())
}

struct ResolveDepCtx<'a> {
    inputs: &'a DependencyResolutionInputs<'a>,
    package_records: &'a mut BTreeMap<PackageId, PackageRecord>,
    package_name_index: &'a mut BTreeMap<String, PackageId>,
    resolving: &'a mut BTreeSet<String>,
}

fn resolve_dependency(
    ctx: &mut ResolveDepCtx<'_>,
    name: &str,
    requirement: &str,
    optional: bool,
) -> ProjectResult<Option<PackageId>> {
    let ResolveDepCtx {
        inputs,
        package_records,
        package_name_index,
        resolving,
    } = ctx;
    let DependencyResolutionInputs {
        local_packages,
        options,
        storage,
        lockfile,
    } = *inputs;

    if let Some(package) = local_packages.get(name) {
        let package_id = PackageId::new(
            name,
            package
                .manifest
                .version
                .clone()
                .ok_or_else(|| ProjectError::MissingPackageVersion { name: name.into() })?,
        );
        if !package_records.contains_key(&package_id) {
            let record = load_package_record(
                package.root_dir.clone(),
                package.manifest_path.clone(),
                &package.source,
                package.manifest.clone(),
                PackageSource::Workspace,
            )?;
            let _ = package_name_index.insert(name.into(), record.package.id.clone());
            let _ = package_records.insert(record.package.id.clone(), record);
        }
        resolve_package_dependencies(
            &package_id,
            inputs,
            package_records,
            package_name_index,
            resolving,
        )?;
        return Ok(Some(package_id));
    }

    if let Some(package_id) = package_name_index.get(name).cloned()
        && package_records.contains_key(&package_id)
    {
        return Ok(Some(package_id));
    }

    let dependency_result = if requirement.trim().starts_with("git+") {
        resolve_git_dependency(name, requirement, storage, lockfile)
    } else {
        let Some(registry_root) = options.registry_root.as_deref() else {
            return if optional {
                Ok(None)
            } else {
                Err(ProjectError::MissingRegistryRoot { name: name.into() })
            };
        };
        resolve_registry_dependency(name, requirement, registry_root, storage, lockfile)
    };
    let dependency = match dependency_result {
        Ok(dependency) => dependency,
        Err(_) if optional => return Ok(None),
        Err(error) => return Err(error),
    };

    if !package_records.contains_key(&dependency.package_id) {
        let record = load_package_record(
            dependency.root_dir,
            dependency.manifest_path,
            &dependency.manifest_source,
            dependency.manifest,
            dependency.source,
        )?;
        let _ = package_name_index.insert(name.into(), record.package.id.clone());
        let _ = package_records.insert(record.package.id.clone(), record);
    }
    resolve_package_dependencies(
        &dependency.package_id,
        inputs,
        package_records,
        package_name_index,
        resolving,
    )?;
    Ok(Some(dependency.package_id))
}

struct ResolvedDependency {
    package_id: PackageId,
    root_dir: PathBuf,
    manifest_path: PathBuf,
    manifest_source: ManifestSource,
    manifest: PackageManifest,
    source: PackageSource,
}

fn resolve_registry_dependency(
    name: &str,
    requirement: &str,
    registry_root: &Path,
    storage: &ProjectStorage,
    lockfile: &Lockfile,
) -> ProjectResult<ResolvedDependency> {
    let registry_package = locked_or_latest_registry_package(
        registry_root,
        &storage.global_cache_dir,
        lockfile,
        name,
        requirement,
    )?;
    let root_dir = hydrate_package_dir(
        &registry_package.global_cache_dir,
        storage.modules_dir.as_deref(),
        name,
        &registry_package.version,
    )?;
    let manifest_path = manifest_path_for(&root_dir)?;
    let LoadedManifest {
        manifest,
        source: manifest_source,
    } = read_manifest(&manifest_path)?;
    validate_manifest(&manifest, &manifest_source)?;
    let version = manifest
        .version
        .clone()
        .unwrap_or_else(|| registry_package.version.clone());
    Ok(ResolvedDependency {
        package_id: PackageId::new(name, version),
        root_dir,
        manifest_path,
        manifest_source,
        manifest,
        source: PackageSource::Registry {
            registry_dir: registry_package.registry_dir,
            global_cache_dir: registry_package.global_cache_dir,
            modules_dir: storage.modules_dir.clone(),
        },
    })
}

fn resolve_git_dependency(
    name: &str,
    requirement: &str,
    storage: &ProjectStorage,
    lockfile: &Lockfile,
) -> ProjectResult<ResolvedDependency> {
    let git_requirement = GitRequirement::parse(name, requirement)?;
    let git_package = locked_or_latest_git_package(name, &git_requirement, storage, lockfile)?;
    let root_dir = hydrate_package_dir(
        &git_package.checkout_dir,
        storage.modules_dir.as_deref(),
        name,
        &git_package.commit,
    )?;
    let manifest_path = manifest_path_for(&root_dir)?;
    let LoadedManifest {
        manifest,
        source: manifest_source,
    } = read_manifest(&manifest_path)?;
    validate_manifest(&manifest, &manifest_source)?;
    let version = manifest
        .version
        .clone()
        .ok_or_else(|| ProjectError::MissingPackageVersion { name: name.into() })?;
    Ok(ResolvedDependency {
        package_id: PackageId::new(name, version),
        root_dir,
        manifest_path,
        manifest_source,
        manifest,
        source: PackageSource::Git {
            url: git_requirement.url,
            reference: git_requirement.reference,
            commit: git_package.commit,
            global_cache_dir: git_package.checkout_dir,
            modules_dir: storage.modules_dir.clone(),
        },
    })
}

fn hydrate_package_dir(
    source_dir: &Path,
    modules_dir: Option<&Path>,
    name: &str,
    version_or_commit: &str,
) -> ProjectResult<PathBuf> {
    let Some(modules_dir) = modules_dir else {
        return Ok(source_dir.to_path_buf());
    };
    let package_dir = modules_dir.join(name).join(version_or_commit);
    if !package_dir.exists() {
        copy_dir_recursive(source_dir, &package_dir)?;
    }
    Ok(package_dir)
}

fn locked_or_latest_registry_package(
    registry_root: &Path,
    global_cache_root: &Path,
    lockfile: &Lockfile,
    name: &str,
    requirement: &str,
) -> ProjectResult<RegistryPackage> {
    if let Some(locked) = lockfile.packages.iter().find(|package| {
        matches!(&package.source, LockedPackageSource::Registry { .. }) && package.name == name
    }) {
        let exact = format!("={}", locked.version);
        if let Ok(package) =
            resolve_registry_package(registry_root, global_cache_root, name, &exact)
        {
            return Ok(package);
        }
    }
    resolve_registry_package(registry_root, global_cache_root, name, requirement)
}
