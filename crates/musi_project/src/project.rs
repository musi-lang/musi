use std::collections::{BTreeMap, BTreeSet};
use std::ffi::OsStr;
use std::fs;
use std::path::{Component, Path, PathBuf};

use musi_foundation::{extend_import_map, register_modules, resolve_spec};
use music_base::SourceId;
use music_bc::Artifact;
use music_emit::EmitOptions;
use music_module::{ImportMap, ImportSiteKind, ModuleKey, ModuleSpecifier, collect_import_sites};
use music_sema::TargetInfo;
use music_session::{CompiledOutput, Session, SessionError, SessionOptions};
use music_syntax::{Lexer, parse};

use crate::ProjectResult;
use crate::errors::ProjectError;
use crate::lock::{LockedPackage, LockedPackageSource, Lockfile};
use crate::manifest::{CompilerOptions, PackageManifest, TaskConfig};
use crate::registry::{RegistryPackage, resolve_registry_package};

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct PackageId {
    pub name: String,
    pub version: String,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum PackageSource {
    Workspace,
    Registry {
        registry_dir: PathBuf,
        cache_dir: PathBuf,
    },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ProjectEntry {
    pub package: PackageId,
    pub module_key: ModuleKey,
    pub path: PathBuf,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TaskSpec {
    pub description: Option<String>,
    pub command: String,
    pub dependencies: Vec<String>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ResolvedPackage {
    pub id: PackageId,
    pub manifest_path: PathBuf,
    pub root_dir: PathBuf,
    pub source: PackageSource,
    pub manifest: PackageManifest,
    pub entry: ProjectEntry,
    pub exports: BTreeMap<String, ModuleKey>,
    pub module_keys: BTreeMap<ModuleKey, PathBuf>,
    pub dependencies: BTreeMap<String, PackageId>,
    pub dev_dependencies: BTreeMap<String, PackageId>,
    pub peer_dependencies: BTreeMap<String, PackageId>,
    pub optional_dependencies: BTreeMap<String, PackageId>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct WorkspaceGraph {
    pub root_package: Option<PackageId>,
    pub members: Vec<PackageId>,
    pub packages: BTreeMap<PackageId, ResolvedPackage>,
}

#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct ProjectOptions {
    pub registry_root: Option<PathBuf>,
    pub cache_root: Option<PathBuf>,
    pub emit: EmitOptions,
    pub target: Option<TargetInfo>,
}

#[derive(Debug)]
pub struct Project {
    options: ProjectOptions,
    root_dir: PathBuf,
    root_manifest_path: PathBuf,
    manifest: PackageManifest,
    workspace: WorkspaceGraph,
    import_map: ImportMap,
    module_texts: BTreeMap<ModuleKey, String>,
    package_name_index: BTreeMap<String, PackageId>,
    lockfile_path: PathBuf,
    loaded_lockfile: Lockfile,
    resolved_lockfile: Lockfile,
}

#[derive(Debug, Clone)]
struct LoadedModule {
    key: ModuleKey,
    path: PathBuf,
    package_relative: String,
    local_scope_key: ModuleKey,
    text: String,
    imports: Vec<String>,
}

#[derive(Debug, Clone)]
struct PackageRecord {
    package: ResolvedPackage,
    modules: BTreeMap<ModuleKey, LoadedModule>,
    relative_modules: BTreeMap<String, ModuleKey>,
}

#[derive(Debug, Clone)]
struct LocalPackage {
    manifest_path: PathBuf,
    root_dir: PathBuf,
    manifest: PackageManifest,
}

/// # Errors
///
/// Returns [`ProjectError`] when the project manifest, workspace, or dependency graph cannot be
/// loaded from `path`.
pub fn load_project(path: impl AsRef<Path>, options: ProjectOptions) -> ProjectResult<Project> {
    Project::load(path, options)
}

/// # Errors
///
/// Returns [`ProjectError`] when no ancestor `musi.json` can be found from `path`, or the
/// discovered project cannot be loaded.
pub fn load_project_ancestor(
    path: impl AsRef<Path>,
    options: ProjectOptions,
) -> ProjectResult<Project> {
    Project::load_ancestor(path, options)
}

impl Project {
    /// # Errors
    ///
    /// Returns [`ProjectError`] when the manifest, workspace, dependency graph, or registry/cache
    /// state cannot be loaded into a project model.
    pub fn load(path: impl AsRef<Path>, options: ProjectOptions) -> ProjectResult<Self> {
        let root_manifest_path = manifest_path_for(path.as_ref())?;
        let root_dir = root_manifest_path
            .parent()
            .map(Path::to_path_buf)
            .ok_or_else(|| ProjectError::MissingPackageRoot {
                path: root_manifest_path.clone(),
            })?;
        let manifest = read_manifest(&root_manifest_path)?;
        validate_manifest(&manifest, &root_manifest_path)?;

        let lockfile_path = root_dir.join(manifest.lock_path());
        if manifest.is_lock_frozen() && !lockfile_path.exists() {
            return Err(ProjectError::MissingFrozenLockfile {
                path: lockfile_path,
            });
        }
        let loaded_lockfile = load_lockfile(&lockfile_path)?;

        let local_packages = load_local_packages(&root_dir, &manifest, &root_manifest_path)?;
        let mut package_records = BTreeMap::<PackageId, PackageRecord>::new();
        let mut package_name_index = BTreeMap::<String, PackageId>::new();
        let mut resolving = BTreeSet::<String>::new();

        for (name, package) in &local_packages {
            let package_record = load_package_record(
                package.root_dir.clone(),
                package.manifest_path.clone(),
                package.manifest.clone(),
                PackageSource::Workspace,
            )?;
            let _ = package_name_index.insert(name.clone(), package_record.package.id.clone());
            let _ = package_records.insert(package_record.package.id.clone(), package_record);
        }

        let root_package = manifest
            .name
            .as_ref()
            .and_then(|name| package_name_index.get(name).cloned());
        let members = manifest
            .workspace_members()
            .iter()
            .map(|member| member_manifest_name(&root_dir, member))
            .collect::<Result<Vec<_>, _>>()?
            .into_iter()
            .filter_map(|name| package_name_index.get(&name).cloned())
            .collect::<Vec<_>>();

        let package_ids = package_records.keys().cloned().collect::<Vec<_>>();
        for package_id in package_ids {
            resolve_package_dependencies(
                &package_id,
                &local_packages,
                &options,
                &loaded_lockfile,
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
        if manifest.is_lock_frozen()
            && loaded_lockfile.clone().normalized() != resolved_lockfile.clone().normalized()
        {
            return Err(ProjectError::FrozenLockfileOutOfDate {
                path: lockfile_path,
            });
        }

        let packages = package_records
            .into_iter()
            .map(|(id, record)| (id, record.package))
            .collect();
        let workspace = WorkspaceGraph {
            root_package,
            members,
            packages,
        };

        Ok(Self {
            options,
            root_dir,
            root_manifest_path,
            manifest,
            workspace,
            import_map,
            module_texts,
            package_name_index,
            lockfile_path,
            loaded_lockfile,
            resolved_lockfile,
        })
    }

    /// # Errors
    ///
    /// Returns [`ProjectError`] when no ancestor `musi.json` can be found from `path`, or the
    /// discovered project cannot be loaded.
    pub fn load_ancestor(path: impl AsRef<Path>, options: ProjectOptions) -> ProjectResult<Self> {
        let manifest_path = manifest_ancestor_path_for(path.as_ref())?;
        Self::load(manifest_path, options)
    }

    #[must_use]
    pub const fn manifest(&self) -> &PackageManifest {
        &self.manifest
    }

    #[must_use]
    pub fn root_dir(&self) -> &Path {
        &self.root_dir
    }

    #[must_use]
    pub fn root_manifest_path(&self) -> &Path {
        &self.root_manifest_path
    }

    #[must_use]
    pub fn module_text(&self, key: &ModuleKey) -> Option<&str> {
        self.module_texts.get(key).map(String::as_str)
    }

    pub fn module_texts(&self) -> impl Iterator<Item = (&ModuleKey, &str)> {
        self.module_texts
            .iter()
            .map(|(key, text)| (key, text.as_str()))
    }

    #[must_use]
    pub const fn workspace(&self) -> &WorkspaceGraph {
        &self.workspace
    }

    #[must_use]
    pub const fn import_map(&self) -> &ImportMap {
        &self.import_map
    }

    #[must_use]
    pub fn package(&self, name: &str) -> Option<&ResolvedPackage> {
        let id = self.package_name_index.get(name)?;
        self.workspace.packages.get(id)
    }

    /// # Errors
    ///
    /// Returns [`ProjectError::NoRootPackage`] when the loaded manifest is workspace-only.
    pub fn root_package(&self) -> ProjectResult<&ResolvedPackage> {
        let Some(id) = &self.workspace.root_package else {
            return Err(ProjectError::NoRootPackage);
        };
        self.workspace
            .packages
            .get(id)
            .ok_or(ProjectError::NoRootPackage)
    }

    /// # Errors
    ///
    /// Returns [`ProjectError`] when the project has no root package entry.
    pub fn root_entry(&self) -> ProjectResult<&ProjectEntry> {
        Ok(&self.root_package()?.entry)
    }

    /// # Errors
    ///
    /// Returns [`ProjectError`] when the named package is not part of the loaded project graph.
    pub fn package_entry(&self, name: &str) -> ProjectResult<&ProjectEntry> {
        let package = self
            .package(name)
            .ok_or_else(|| ProjectError::UnresolvedDependency { name: name.into() })?;
        Ok(&package.entry)
    }

    #[must_use]
    pub const fn lockfile(&self) -> &Lockfile {
        &self.resolved_lockfile
    }

    #[must_use]
    pub fn lockfile_path(&self) -> &Path {
        &self.lockfile_path
    }

    #[must_use]
    pub fn lockfile_needs_write(&self) -> bool {
        self.loaded_lockfile.clone().normalized() != self.resolved_lockfile.clone().normalized()
    }

    /// # Errors
    ///
    /// Returns [`ProjectError`] when the resolved lockfile cannot be serialized or written.
    pub fn write_lockfile(&self) -> ProjectResult {
        let text = serde_json::to_string_pretty(&self.resolved_lockfile).map_err(|source| {
            ProjectError::ManifestJson {
                path: self.lockfile_path.clone(),
                source,
            }
        })?;
        fs::write(&self.lockfile_path, text).map_err(|source| ProjectError::Io {
            path: self.lockfile_path.clone(),
            source,
        })
    }

    #[must_use]
    pub fn task(&self, name: &str) -> Option<TaskSpec> {
        let TaskConfig {
            description,
            command,
            dependencies,
        } = self.manifest.task_config(name)?;
        Some(TaskSpec {
            description,
            command,
            dependencies,
        })
    }

    /// # Errors
    ///
    /// Returns [`ProjectError`] when the named task does not exist or the task graph contains a
    /// dependency cycle.
    pub fn task_plan(&self, name: &str) -> ProjectResult<Vec<TaskSpec>> {
        let mut order = Vec::new();
        let mut seen = BTreeSet::new();
        let mut active = BTreeSet::new();
        self.collect_task_plan(name, &mut seen, &mut active, &mut order)?;
        Ok(order)
    }

    fn collect_task_plan(
        &self,
        name: &str,
        seen: &mut BTreeSet<String>,
        active: &mut BTreeSet<String>,
        out: &mut Vec<TaskSpec>,
    ) -> ProjectResult {
        if !seen.insert(name.into()) {
            return Ok(());
        }
        if !active.insert(name.into()) {
            return Err(ProjectError::TaskCycle { name: name.into() });
        }
        let task = self
            .task(name)
            .ok_or_else(|| ProjectError::UnknownTask { name: name.into() })?;
        for dependency in &task.dependencies {
            self.collect_task_plan(dependency, seen, active, out)?;
        }
        let _ = active.remove(name);
        out.push(task);
        Ok(())
    }

    /// # Errors
    ///
    /// Returns [`ProjectError`] when the project modules cannot be registered into a configured
    /// [`Session`].
    pub fn build_session(&self) -> ProjectResult<Session> {
        let mut import_map = self.import_map.clone();
        extend_import_map(&mut import_map);
        let mut session = Session::new(SessionOptions {
            emit: self.options.emit,
            import_map,
            target: self.options.target.clone(),
        });
        register_modules(&mut session)?;
        for (key, text) in &self.module_texts {
            session.set_module_text(key, text.clone())?;
        }
        Ok(session)
    }

    fn with_entry_session<T, F>(&self, entry: &ModuleKey, compile: F) -> ProjectResult<T>
    where
        F: FnOnce(&mut Session, &ModuleKey) -> ProjectResult<T>,
    {
        let mut session = self.build_session()?;
        compile(&mut session, entry)
    }

    fn with_root_entry_session<T, F>(&self, compile: F) -> ProjectResult<T>
    where
        F: FnOnce(&mut Session, &ModuleKey) -> ProjectResult<T>,
    {
        let entry = self.root_entry()?;
        self.with_entry_session(&entry.module_key, compile)
    }

    fn compile_root_entry_with<T, F>(&self, compile: F) -> ProjectResult<T>
    where
        F: FnOnce(&mut Session, &ModuleKey) -> Result<T, SessionError>,
    {
        self.with_root_entry_session(|session, entry| Ok(compile(session, entry)?))
    }

    /// # Errors
    ///
    /// Returns [`ProjectError`] when the root package entry cannot be compiled through
    /// [`Session`].
    pub fn compile_root_entry(&self) -> ProjectResult<CompiledOutput> {
        self.compile_root_entry_with(Session::compile_entry)
    }

    /// # Errors
    ///
    /// Returns [`ProjectError`] when the root package entry cannot be emitted to an artifact.
    pub fn compile_root_entry_artifact(&self) -> ProjectResult<Artifact> {
        self.compile_root_entry_with(Session::compile_entry_artifact)
    }

    /// # Errors
    ///
    /// Returns [`ProjectError`] when the root package entry cannot be compiled to bytes.
    pub fn compile_root_entry_bytes(&self) -> ProjectResult<Vec<u8>> {
        self.compile_root_entry_with(Session::compile_entry_bytes)
    }

    /// # Errors
    ///
    /// Returns [`ProjectError`] when the root package entry cannot be compiled to text.
    pub fn compile_root_entry_text(&self) -> ProjectResult<String> {
        self.compile_root_entry_with(Session::compile_entry_text)
    }

    /// # Errors
    ///
    /// Returns [`ProjectError`] when the named package entry cannot be compiled.
    pub fn compile_package_entry(&self, name: &str) -> ProjectResult<CompiledOutput> {
        let entry = self.package_entry(name)?;
        self.with_entry_session(&entry.module_key, |session, module_key| {
            Ok(session.compile_entry(module_key)?)
        })
    }

    #[cfg(test)]
    pub(crate) fn compile_module(&self, module_key: &ModuleKey) -> ProjectResult<CompiledOutput> {
        self.with_entry_session(module_key, |session, entry| {
            Ok(session.compile_entry(entry)?)
        })
    }
}

fn manifest_path_for(path: &Path) -> ProjectResult<PathBuf> {
    let manifest_path = if path.file_name() == Some(OsStr::new("musi.json")) {
        path.to_path_buf()
    } else {
        path.join("musi.json")
    };
    if manifest_path.is_file() {
        Ok(manifest_path)
    } else {
        Err(ProjectError::MissingManifest {
            path: manifest_path,
        })
    }
}

fn manifest_ancestor_path_for(path: &Path) -> ProjectResult<PathBuf> {
    let start_dir = if path.file_name() == Some(OsStr::new("musi.json")) {
        path.parent()
            .map(Path::to_path_buf)
            .ok_or_else(|| ProjectError::MissingPackageRoot {
                path: path.to_path_buf(),
            })?
    } else if path.is_dir() {
        path.to_path_buf()
    } else {
        path.parent()
            .map(Path::to_path_buf)
            .ok_or_else(|| ProjectError::MissingPackageRoot {
                path: path.to_path_buf(),
            })?
    };

    for ancestor in start_dir.ancestors() {
        let manifest_path = ancestor.join("musi.json");
        if manifest_path.is_file() {
            return Ok(manifest_path);
        }
    }

    Err(ProjectError::MissingManifestAncestor {
        path: path.to_path_buf(),
    })
}

fn read_manifest(path: &Path) -> ProjectResult<PackageManifest> {
    let text = fs::read_to_string(path).map_err(|source| ProjectError::Io {
        path: path.to_path_buf(),
        source,
    })?;
    serde_json::from_str(&text).map_err(|source| ProjectError::ManifestJson {
        path: path.to_path_buf(),
        source,
    })
}

fn validate_manifest(manifest: &PackageManifest, path: &Path) -> ProjectResult {
    if let Some(name) = &manifest.name {
        if name.trim().is_empty() {
            return Err(ProjectError::Validation {
                message: format!("package name in `{}` must not be empty", path.display()),
            });
        }
    }
    for (export_name, export_path) in manifest.export_map() {
        if export_name != "." && !export_name.starts_with("./") {
            return Err(ProjectError::Validation {
                message: format!(
                    "export key `{export_name}` in `{}` must be `.` or start with `./`",
                    path.display()
                ),
            });
        }
        if !export_path.starts_with("./") {
            return Err(ProjectError::Validation {
                message: format!(
                    "export target `{export_path}` in `{}` must start with `./`",
                    path.display()
                ),
            });
        }
    }
    validate_task_graph(manifest)?;
    Ok(())
}

fn validate_task_graph(manifest: &PackageManifest) -> ProjectResult {
    let mut seen = BTreeSet::new();
    let mut active = BTreeSet::new();
    for name in manifest.tasks.keys() {
        validate_task_node(name, manifest, &mut seen, &mut active)?;
    }
    Ok(())
}

fn validate_task_node(
    name: &str,
    manifest: &PackageManifest,
    seen: &mut BTreeSet<String>,
    active: &mut BTreeSet<String>,
) -> ProjectResult {
    if !seen.insert(name.into()) {
        return Ok(());
    }
    if !active.insert(name.into()) {
        return Err(ProjectError::TaskCycle { name: name.into() });
    }
    if let Some(task) = manifest.task_config(name) {
        for dependency in task.dependencies {
            if manifest.task_config(&dependency).is_none() {
                return Err(ProjectError::Validation {
                    message: format!("task `{name}` depends on unknown task `{dependency}`"),
                });
            }
            validate_task_node(&dependency, manifest, seen, active)?;
        }
    }
    let _ = active.remove(name);
    Ok(())
}

fn load_lockfile(path: &Path) -> ProjectResult<Lockfile> {
    if !path.exists() {
        return Ok(Lockfile {
            version: 1,
            packages: Vec::new(),
        });
    }
    let text = fs::read_to_string(path).map_err(|source| ProjectError::Io {
        path: path.to_path_buf(),
        source,
    })?;
    serde_json::from_str(&text).map_err(|source| ProjectError::ManifestJson {
        path: path.to_path_buf(),
        source,
    })
}

fn load_local_packages(
    root_dir: &Path,
    manifest: &PackageManifest,
    root_manifest_path: &Path,
) -> ProjectResult<BTreeMap<String, LocalPackage>> {
    let mut out = BTreeMap::new();
    if let Some(name) = manifest.name.clone() {
        let _ = manifest
            .version
            .clone()
            .ok_or_else(|| ProjectError::MissingPackageVersion { name: name.clone() })?;
        let package = LocalPackage {
            manifest_path: root_manifest_path.to_path_buf(),
            root_dir: root_dir.to_path_buf(),
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
        let member_manifest = read_manifest(&member_manifest_path)?;
        validate_manifest(&member_manifest, &member_manifest_path)?;
        let name = member_manifest
            .name
            .clone()
            .ok_or_else(|| ProjectError::Validation {
                message: format!(
                    "workspace member `{}` must declare `name`",
                    member_manifest_path.display()
                ),
            })?;
        let _ = member_manifest
            .version
            .clone()
            .ok_or_else(|| ProjectError::MissingPackageVersion { name: name.clone() })?;
        let package = LocalPackage {
            manifest_path: member_manifest_path,
            root_dir: member_root,
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
    let manifest = read_manifest(&manifest_path)?;
    manifest.name.ok_or_else(|| ProjectError::Validation {
        message: format!(
            "workspace member `{}` must declare `name`",
            manifest_path.display()
        ),
    })
}

fn resolve_package_dependencies(
    package_id: &PackageId,
    local_packages: &BTreeMap<String, LocalPackage>,
    options: &ProjectOptions,
    lockfile: &Lockfile,
    package_records: &mut BTreeMap<PackageId, PackageRecord>,
    package_name_index: &mut BTreeMap<String, PackageId>,
    resolving: &mut BTreeSet<String>,
) -> ProjectResult {
    if !resolving.insert(package_id.name.clone()) {
        return Err(ProjectError::DependencyCycle {
            name: package_id.name.clone(),
        });
    }

    let manifest = package_records
        .get(package_id)
        .map(|record| record.package.manifest.clone())
        .ok_or_else(|| ProjectError::UnresolvedDependency {
            name: package_id.name.clone(),
        })?;

    for (section, deps) in manifest.dependency_maps() {
        let mut dependency_ids = BTreeMap::new();
        let mut dep_ctx = ResolveDepCtx {
            local_packages,
            options,
            lockfile,
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
            ProjectError::UnresolvedDependency {
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
    local_packages: &'a BTreeMap<String, LocalPackage>,
    options: &'a ProjectOptions,
    lockfile: &'a Lockfile,
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
        local_packages,
        options,
        lockfile,
        package_records,
        package_name_index,
        resolving,
    } = ctx;

    if let Some(package) = local_packages.get(name) {
        let package_id = PackageId {
            name: name.into(),
            version: package
                .manifest
                .version
                .clone()
                .ok_or_else(|| ProjectError::MissingPackageVersion { name: name.into() })?,
        };
        if !package_records.contains_key(&package_id) {
            let record = load_package_record(
                package.root_dir.clone(),
                package.manifest_path.clone(),
                package.manifest.clone(),
                PackageSource::Workspace,
            )?;
            let _ = package_name_index.insert(name.into(), record.package.id.clone());
            let _ = package_records.insert(record.package.id.clone(), record);
        }
        resolve_package_dependencies(
            &package_id,
            local_packages,
            options,
            lockfile,
            package_records,
            package_name_index,
            resolving,
        )?;
        return Ok(Some(package_id));
    }

    let Some(registry_root) = options.registry_root.as_deref() else {
        return if optional {
            Ok(None)
        } else {
            Err(ProjectError::MissingRegistryRoot { name: name.into() })
        };
    };
    let Some(cache_root) = options.cache_root.as_deref() else {
        return if optional {
            Ok(None)
        } else {
            Err(ProjectError::MissingCacheRoot { name: name.into() })
        };
    };

    let registry_package_result =
        locked_or_latest_registry_package(registry_root, cache_root, lockfile, name, requirement);
    let registry_package = match registry_package_result {
        Ok(package) => package,
        Err(_) if optional => return Ok(None),
        Err(error) => return Err(error),
    };

    let manifest_path = manifest_path_for(&registry_package.cache_dir)?;
    let manifest = read_manifest(&manifest_path)?;
    validate_manifest(&manifest, &manifest_path)?;
    let version = manifest
        .version
        .clone()
        .unwrap_or_else(|| registry_package.version.clone());
    let package_id = PackageId {
        name: name.into(),
        version,
    };
    if !package_records.contains_key(&package_id) {
        let record = load_package_record(
            registry_package.cache_dir.clone(),
            manifest_path,
            manifest,
            PackageSource::Registry {
                registry_dir: registry_package.registry_dir,
                cache_dir: registry_package.cache_dir,
            },
        )?;
        let _ = package_name_index.insert(name.into(), record.package.id.clone());
        let _ = package_records.insert(record.package.id.clone(), record);
    }
    resolve_package_dependencies(
        &package_id,
        local_packages,
        options,
        lockfile,
        package_records,
        package_name_index,
        resolving,
    )?;
    Ok(Some(package_id))
}

fn locked_or_latest_registry_package(
    registry_root: &Path,
    cache_root: &Path,
    lockfile: &Lockfile,
    name: &str,
    requirement: &str,
) -> ProjectResult<RegistryPackage> {
    if let Some(locked) = lockfile
        .packages
        .iter()
        .find(|package| package.name == name && package.source == LockedPackageSource::Registry)
    {
        let exact = format!("={}", locked.version);
        if let Ok(package) = resolve_registry_package(registry_root, cache_root, name, &exact) {
            return Ok(package);
        }
    }
    resolve_registry_package(registry_root, cache_root, name, requirement)
}

fn load_package_record(
    root_dir: PathBuf,
    manifest_path: PathBuf,
    manifest: PackageManifest,
    source: PackageSource,
) -> ProjectResult<PackageRecord> {
    let id = PackageId {
        name: manifest
            .name
            .clone()
            .ok_or_else(|| ProjectError::Validation {
                message: format!("package `{}` must declare `name`", manifest_path.display()),
            })?,
        version: manifest
            .version
            .clone()
            .ok_or_else(|| ProjectError::Validation {
                message: format!(
                    "package `{}` must declare `version`",
                    manifest_path.display()
                ),
            })?,
    };

    let modules = discover_modules(&id, &root_dir)?;
    let relative_modules = modules
        .values()
        .map(|module| (module.package_relative.clone(), module.key.clone()))
        .collect::<BTreeMap<_, _>>();
    let module_keys = modules
        .values()
        .map(|module| (module.key.clone(), module.path.clone()))
        .collect::<BTreeMap<_, _>>();
    let entry_key =
        resolve_module_target(&root_dir, &relative_modules, None, manifest.main_entry())
            .ok_or_else(|| ProjectError::MissingEntry {
                package: id.name.clone(),
            })?;
    let entry_path =
        module_keys
            .get(&entry_key)
            .cloned()
            .ok_or_else(|| ProjectError::MissingEntry {
                package: id.name.clone(),
            })?;
    let entry = ProjectEntry {
        package: id.clone(),
        module_key: entry_key,
        path: entry_path,
    };

    let mut exports = BTreeMap::new();
    for (name, target) in manifest.export_map() {
        let export_key = resolve_module_target(&root_dir, &relative_modules, None, &target)
            .ok_or_else(|| ProjectError::Validation {
                message: format!(
                    "export target `{target}` does not exist for package `{}`",
                    id.name
                ),
            })?;
        let _ = exports.insert(name, export_key);
    }

    let package = ResolvedPackage {
        id,
        manifest_path,
        root_dir,
        source,
        manifest,
        entry,
        exports,
        module_keys,
        dependencies: BTreeMap::new(),
        dev_dependencies: BTreeMap::new(),
        peer_dependencies: BTreeMap::new(),
        optional_dependencies: BTreeMap::new(),
    };

    Ok(PackageRecord {
        package,
        modules,
        relative_modules,
    })
}

fn discover_modules(
    package_id: &PackageId,
    root_dir: &Path,
) -> ProjectResult<BTreeMap<ModuleKey, LoadedModule>> {
    let mut out = BTreeMap::new();
    discover_modules_recursive(package_id, root_dir, root_dir, &mut out)?;
    Ok(out)
}

fn discover_modules_recursive(
    package_id: &PackageId,
    root_dir: &Path,
    dir: &Path,
    out: &mut BTreeMap<ModuleKey, LoadedModule>,
) -> ProjectResult {
    let entries = fs::read_dir(dir).map_err(|source| ProjectError::Io {
        path: dir.to_path_buf(),
        source,
    })?;
    for entry in entries {
        let entry = entry.map_err(|source| ProjectError::Io {
            path: dir.to_path_buf(),
            source,
        })?;
        let path = entry.path();
        if path.is_dir() {
            discover_modules_recursive(package_id, root_dir, &path, out)?;
            continue;
        }
        if path.extension() != Some(OsStr::new("ms")) {
            continue;
        }
        let text = fs::read_to_string(&path).map_err(|source| ProjectError::Io {
            path: path.clone(),
            source,
        })?;
        let relative = normalize_relative_path(
            path.strip_prefix(root_dir)
                .map_err(|_| ProjectError::MissingModule { path: path.clone() })?,
        );
        let key = module_key_for(package_id, &relative);
        let scope_key = ModuleKey::new(format!("./{relative}"));
        let parsed = parse(Lexer::new(&text).lex());
        let imports = collect_import_sites(SourceId::from_raw(0), parsed.tree())
            .into_iter()
            .filter_map(|site| match site.kind {
                ImportSiteKind::Static { spec } => Some(spec.as_str().to_owned()),
                ImportSiteKind::Dynamic | ImportSiteKind::InvalidStringLit => None,
            })
            .collect::<Vec<_>>();
        let module = LoadedModule {
            key: key.clone(),
            path: path.clone(),
            package_relative: relative,
            local_scope_key: scope_key,
            text,
            imports,
        };
        let _ = out.insert(key, module);
    }
    Ok(())
}

fn build_import_map(
    package_records: &BTreeMap<PackageId, PackageRecord>,
    package_name_index: &BTreeMap<String, PackageId>,
) -> ProjectResult<ImportMap> {
    let mut import_map = ImportMap::default();
    for record in package_records.values() {
        let manifest_map = ImportMap {
            imports: record.package.manifest.imports.clone(),
            scopes: record.package.manifest.scopes.clone(),
        };
        for module in record.modules.values() {
            let scope = import_map
                .scopes
                .entry(module.key.as_str().to_owned())
                .or_default();
            for import_spec in &module.imports {
                let remapped = manifest_map
                    .resolve(
                        &module.local_scope_key,
                        &ModuleSpecifier::new(import_spec.as_str()),
                    )
                    .map_or_else(|| import_spec.clone(), |spec| spec.as_str().to_owned());
                let target = resolve_import_spec(
                    record,
                    module,
                    &remapped,
                    package_records,
                    package_name_index,
                )
                .ok_or_else(|| ProjectError::UnresolvedDependency {
                    name: import_spec.clone(),
                })?;
                let _ = scope.insert(import_spec.clone(), target.as_str().to_owned());
            }
        }
    }
    Ok(import_map)
}

fn resolve_import_spec(
    package: &PackageRecord,
    module: &LoadedModule,
    spec: &str,
    package_records: &BTreeMap<PackageId, PackageRecord>,
    package_name_index: &BTreeMap<String, PackageId>,
) -> Option<ModuleKey> {
    if let Some(target) = resolve_spec(spec) {
        return Some(target);
    }
    if let Some(target) = resolve_compiler_path(
        &package.package.root_dir,
        &package.relative_modules,
        package.package.manifest.compiler_options.as_ref(),
        spec,
    ) {
        return Some(target);
    }
    if let Some(target) = resolve_module_target(
        &package.package.root_dir,
        &package.relative_modules,
        Some(Path::new(&module.package_relative)),
        spec,
    ) {
        return Some(target);
    }

    let (package_name, export_key) = split_package_spec(spec);
    let target_id = package_name_index.get(package_name.as_str())?;
    let target_package = package_records.get(target_id)?;
    target_package
        .package
        .exports
        .get(export_key.as_str())
        .cloned()
}

fn resolve_compiler_path(
    root_dir: &Path,
    relative_modules: &BTreeMap<String, ModuleKey>,
    compiler_options: Option<&CompilerOptions>,
    spec: &str,
) -> Option<ModuleKey> {
    let compiler_options = compiler_options?;

    if let Some(candidates) = compiler_options.paths.get(spec) {
        for candidate in candidates {
            if let Some(target) = resolve_module_target(root_dir, relative_modules, None, candidate)
            {
                return Some(target);
            }
        }
    }

    for (pattern, candidates) in &compiler_options.paths {
        let Some(pattern_prefix) = pattern.strip_suffix('*') else {
            continue;
        };
        let Some(rest) = spec.strip_prefix(pattern_prefix) else {
            continue;
        };
        for candidate in candidates {
            let candidate = candidate.replace('*', rest);
            if let Some(target) =
                resolve_module_target(root_dir, relative_modules, None, &candidate)
            {
                return Some(target);
            }
        }
    }

    if let Some(base_url) = compiler_options.base_url.as_deref() {
        let base = root_dir.join(base_url);
        let candidate = base.join(spec);
        if let Some(target) = resolve_candidate_path(relative_modules, root_dir, &candidate) {
            return Some(target);
        }
    }

    None
}

fn resolve_module_target(
    root_dir: &Path,
    relative_modules: &BTreeMap<String, ModuleKey>,
    from_module: Option<&Path>,
    spec: &str,
) -> Option<ModuleKey> {
    let spec_path = Path::new(spec);
    if spec_path.is_absolute() {
        return None;
    }
    let base_dir = from_module
        .and_then(Path::parent)
        .map(Path::to_path_buf)
        .unwrap_or_default();
    let candidate = if spec.starts_with("./") || spec.starts_with("../") {
        root_dir.join(base_dir).join(spec_path)
    } else if spec.starts_with('/') {
        root_dir.join(
            normalize_spec_path(spec)
                .strip_prefix(Path::new("/"))
                .unwrap_or_else(|_| Path::new(spec)),
        )
    } else if spec.starts_with('.') {
        root_dir.join(base_dir).join(spec_path)
    } else {
        root_dir.join(normalize_spec_path(spec))
    };
    resolve_candidate_path(relative_modules, root_dir, &candidate)
}

fn resolve_candidate_path(
    relative_modules: &BTreeMap<String, ModuleKey>,
    root_dir: &Path,
    candidate: &Path,
) -> Option<ModuleKey> {
    for path in candidate_paths(candidate) {
        if let Ok(relative) = path.strip_prefix(root_dir) {
            let relative = normalize_relative_path(relative);
            if let Some(target) = relative_modules.get(&relative) {
                return Some(target.clone());
            }
        }
    }
    None
}

fn candidate_paths(path: &Path) -> Vec<PathBuf> {
    if path.extension() == Some(OsStr::new("ms")) {
        return vec![path.to_path_buf()];
    }
    vec![
        path.with_extension("ms"),
        path.join("index.ms"),
        path.to_path_buf(),
    ]
}

fn split_package_spec(spec: &str) -> (String, String) {
    match spec.split_once('/') {
        Some((name, subpath)) => (name.into(), format_export_key(subpath)),
        None => (spec.into(), ".".into()),
    }
}

fn format_export_key(subpath: &str) -> String {
    format!("./{subpath}")
}

fn module_key_for(package_id: &PackageId, relative: &str) -> ModuleKey {
    ModuleKey::new(format!(
        "@{}@{}/{}",
        package_id.name, package_id.version, relative
    ))
}

fn normalize_relative_path(path: &Path) -> String {
    path.components()
        .filter_map(|component| match component {
            Component::Normal(part) => Some(part.to_string_lossy().into_owned()),
            _ => None,
        })
        .collect::<Vec<_>>()
        .join("/")
}

fn normalize_spec_path(spec: &str) -> PathBuf {
    let mut out = PathBuf::new();
    for component in Path::new(spec).components() {
        match component {
            Component::ParentDir => {
                let _ = out.pop();
            }
            Component::Normal(part) => out.push(part),
            Component::CurDir | Component::RootDir | Component::Prefix(_) => {}
        }
    }
    out
}

fn build_lockfile(package_records: &BTreeMap<PackageId, PackageRecord>) -> Lockfile {
    let packages = package_records
        .values()
        .map(|record| LockedPackage {
            name: record.package.id.name.clone(),
            version: record.package.id.version.clone(),
            source: match record.package.source {
                PackageSource::Workspace => LockedPackageSource::Workspace,
                PackageSource::Registry { .. } => LockedPackageSource::Registry,
            },
        })
        .collect::<Vec<_>>();
    Lockfile {
        version: 1,
        packages,
    }
    .normalized()
}
