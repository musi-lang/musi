use std::collections::{BTreeMap, BTreeSet};
use std::ffi::OsStr;
use std::fs;
use std::path::{Path, PathBuf};

use musi_foundation::{extend_import_map, register_modules};
use music_base::diag::DiagCode;
use music_base::{SourceId, Span};
use music_emit::EmitOptions;
use music_module::{ImportMap, ImportSiteKind, ModuleKey, collect_import_sites};
use music_seam::Artifact;
use music_sema::TargetInfo;
use music_session::{CompiledOutput, Session, SessionError, SessionOptions};
use music_syntax::{Lexer, parse};

use crate::ProjectResult;
use crate::builtin_std::{
    STD_FILES, STD_MANIFEST, STD_MANIFEST_PATH, STD_PACKAGE_NAME, STD_ROOT_DIR,
};
use crate::errors::ProjectError;
use crate::lock::{LockedPackageSource, Lockfile};
use crate::manifest::{MusiModulesDir, PackageManifest, PublishConfig, TaskConfig};
use crate::manifest_source::ManifestSource;
use crate::project::module_graph::{
    build_import_map, build_lockfile, discover_modules, module_key_for, normalize_lookup_path,
    resolve_module_target,
};
use crate::registry::{RegistryPackage, copy_dir_recursive, resolve_registry_package};

mod git;
mod module_graph;
mod storage;

use git::{GitRequirement, locked_or_latest_git_package};
use storage::ProjectStorage;

type ExportModuleMap = BTreeMap<String, ModuleKey>;
type DependencyPackageMap = BTreeMap<String, PackageId>;
type LocalPackageMap = BTreeMap<String, LocalPackage>;
type VisitedPackageNames = BTreeSet<String>;
type TaskNameSet = BTreeSet<String>;
type CompiledOutputResult = ProjectResult<CompiledOutput>;

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct PackageId {
    pub name: String,
    pub version: String,
}

impl PackageId {
    #[must_use]
    pub fn new(name: impl Into<String>, version: impl Into<String>) -> Self {
        Self {
            name: name.into(),
            version: version.into(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum PackageSource {
    Workspace,
    Builtin,
    Registry {
        registry_dir: PathBuf,
        global_cache_dir: PathBuf,
        modules_dir: Option<PathBuf>,
    },
    Git {
        url: String,
        reference: String,
        commit: String,
        global_cache_dir: PathBuf,
        modules_dir: Option<PathBuf>,
    },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ProjectEntry {
    pub package: PackageId,
    pub module_key: ModuleKey,
    pub path: PathBuf,
}

impl ProjectEntry {
    #[must_use]
    pub const fn new(package: PackageId, module_key: ModuleKey, path: PathBuf) -> Self {
        Self {
            package,
            module_key,
            path,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TaskSpec {
    pub description: Option<String>,
    pub command: String,
    pub dependencies: Vec<String>,
}

impl TaskSpec {
    #[must_use]
    pub fn new(command: impl Into<String>) -> Self {
        Self {
            description: None,
            command: command.into(),
            dependencies: Vec::new(),
        }
    }

    #[must_use]
    pub fn with_description(mut self, description: impl Into<String>) -> Self {
        self.description = Some(description.into());
        self
    }

    #[must_use]
    pub fn with_dependencies(mut self, dependencies: Vec<String>) -> Self {
        self.dependencies = dependencies;
        self
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ResolvedPackage {
    pub id: PackageId,
    pub manifest_path: PathBuf,
    pub root_dir: PathBuf,
    pub source: PackageSource,
    pub manifest: PackageManifest,
    pub entry: ProjectEntry,
    pub exports: ExportModuleMap,
    pub module_keys: BTreeMap<ModuleKey, PathBuf>,
    pub dependencies: DependencyPackageMap,
    pub dev_dependencies: DependencyPackageMap,
    pub peer_dependencies: DependencyPackageMap,
    pub optional_dependencies: DependencyPackageMap,
}

impl ResolvedPackage {
    #[must_use]
    pub const fn new(
        id: PackageId,
        manifest_path: PathBuf,
        root_dir: PathBuf,
        source: PackageSource,
        manifest: PackageManifest,
        entry: ProjectEntry,
    ) -> Self {
        Self {
            id,
            manifest_path,
            root_dir,
            source,
            manifest,
            entry,
            exports: BTreeMap::new(),
            module_keys: BTreeMap::new(),
            dependencies: BTreeMap::new(),
            dev_dependencies: BTreeMap::new(),
            peer_dependencies: BTreeMap::new(),
            optional_dependencies: BTreeMap::new(),
        }
    }

    #[must_use]
    pub fn with_exports(mut self, exports: ExportModuleMap) -> Self {
        self.exports = exports;
        self
    }

    #[must_use]
    pub fn with_module_keys(mut self, module_keys: BTreeMap<ModuleKey, PathBuf>) -> Self {
        self.module_keys = module_keys;
        self
    }

    #[must_use]
    pub fn with_dependencies(mut self, dependencies: DependencyPackageMap) -> Self {
        self.dependencies = dependencies;
        self
    }

    #[must_use]
    pub fn with_dev_dependencies(mut self, dev_dependencies: DependencyPackageMap) -> Self {
        self.dev_dependencies = dev_dependencies;
        self
    }

    #[must_use]
    pub fn with_peer_dependencies(mut self, peer_dependencies: DependencyPackageMap) -> Self {
        self.peer_dependencies = peer_dependencies;
        self
    }

    #[must_use]
    pub fn with_optional_dependencies(
        mut self,
        optional_dependencies: DependencyPackageMap,
    ) -> Self {
        self.optional_dependencies = optional_dependencies;
        self
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct WorkspaceGraph {
    pub root_package: Option<PackageId>,
    pub members: Vec<PackageId>,
    pub packages: BTreeMap<PackageId, ResolvedPackage>,
}

impl WorkspaceGraph {
    #[must_use]
    pub const fn new(
        members: Vec<PackageId>,
        packages: BTreeMap<PackageId, ResolvedPackage>,
    ) -> Self {
        Self {
            root_package: None,
            members,
            packages,
        }
    }

    #[must_use]
    pub fn with_root_package(mut self, root_package: PackageId) -> Self {
        self.root_package = Some(root_package);
        self
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct ProjectOptions {
    pub registry_root: Option<PathBuf>,
    pub global_cache_root: Option<PathBuf>,
    pub emit: EmitOptions,
    pub target: Option<TargetInfo>,
}

impl ProjectOptions {
    #[must_use]
    pub const fn new() -> Self {
        Self {
            registry_root: None,
            global_cache_root: None,
            emit: EmitOptions,
            target: None,
        }
    }

    #[must_use]
    pub fn with_registry_root(mut self, registry_root: PathBuf) -> Self {
        self.registry_root = Some(registry_root);
        self
    }

    #[must_use]
    pub fn with_global_cache_root(mut self, global_cache_root: PathBuf) -> Self {
        self.global_cache_root = Some(global_cache_root);
        self
    }

    #[must_use]
    pub const fn with_emit(mut self, emit: EmitOptions) -> Self {
        self.emit = emit;
        self
    }

    #[must_use]
    pub fn with_target(mut self, target: TargetInfo) -> Self {
        self.target = Some(target);
        self
    }
}

#[derive(Debug)]
pub struct Project {
    options: ProjectOptions,
    root_dir: PathBuf,
    root_manifest_path: PathBuf,
    root_manifest_source: ManifestSource,
    manifest: PackageManifest,
    workspace: WorkspaceGraph,
    import_map: ImportMap,
    module_texts: BTreeMap<ModuleKey, String>,
    package_name_index: BTreeMap<String, PackageId>,
    lockfile_path: PathBuf,
    global_cache_dir: PathBuf,
    modules_dir: Option<PathBuf>,
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
    imports: Vec<LoadedImportSite>,
}

#[derive(Debug, Clone)]
struct LoadedImportSite {
    spec: String,
    span: Span,
}

#[derive(Debug, Clone)]
struct PackageRecord {
    package: ResolvedPackage,
    modules: BTreeMap<ModuleKey, LoadedModule>,
    relative_modules: ExportModuleMap,
}

#[derive(Debug, Clone)]
struct LocalPackage {
    manifest_path: PathBuf,
    root_dir: PathBuf,
    source: ManifestSource,
    manifest: PackageManifest,
}

#[derive(Debug, Clone)]
struct LoadedManifest {
    manifest: PackageManifest,
    source: ManifestSource,
}

struct WorkspaceSeed {
    package_records: BTreeMap<PackageId, PackageRecord>,
    package_name_index: DependencyPackageMap,
}

struct ResolvedWorkspaceState {
    workspace: WorkspaceGraph,
    package_name_index: DependencyPackageMap,
    import_map: ImportMap,
    module_texts: BTreeMap<ModuleKey, String>,
    resolved_lockfile: Lockfile,
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
        let LoadedManifest {
            manifest,
            source: root_manifest_source,
        } = read_manifest(&root_manifest_path)?;
        validate_manifest(&manifest, &root_manifest_source)?;
        let storage = ProjectStorage::new(&root_dir, &manifest, &options)?;

        let lockfile_path = root_dir.join(manifest.lock_path());
        if manifest.is_lock_frozen() && !lockfile_path.exists() {
            return Err(ProjectError::MissingFrozenLockfile {
                path: lockfile_path,
            });
        }
        let loaded_lockfile = load_lockfile(&lockfile_path)?;
        let ResolvedWorkspaceState {
            workspace,
            package_name_index,
            import_map,
            module_texts,
            resolved_lockfile,
        } = resolve_workspace_state(
            &root_dir,
            &root_manifest_path,
            &root_manifest_source,
            &manifest,
            &options,
            &storage,
            &loaded_lockfile,
        )?;
        if manifest.is_lock_frozen()
            && loaded_lockfile.clone().normalized() != resolved_lockfile.clone().normalized()
        {
            return Err(ProjectError::FrozenLockfileOutOfDate {
                path: lockfile_path,
            });
        }

        Ok(Self {
            options,
            root_dir,
            root_manifest_path,
            root_manifest_source,
            manifest,
            workspace,
            import_map,
            module_texts,
            package_name_index,
            lockfile_path,
            global_cache_dir: storage.global_cache_dir,
            modules_dir: storage.modules_dir,
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
}

impl Project {
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
    pub fn global_cache_dir(&self) -> &Path {
        &self.global_cache_dir
    }

    #[must_use]
    pub fn modules_dir(&self) -> Option<&Path> {
        self.modules_dir.as_deref()
    }

    #[must_use]
    pub fn module_text(&self, key: &ModuleKey) -> Option<&str> {
        self.module_texts.get(key).map(String::as_str)
    }

    #[must_use]
    pub fn module_key_for_path(&self, path: &Path) -> Option<ModuleKey> {
        let target = normalize_lookup_path(path);
        self.workspace
            .packages
            .values()
            .flat_map(|package| package.module_keys.iter())
            .find_map(|(key, module_path)| {
                (normalize_lookup_path(module_path) == target).then(|| key.clone())
            })
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
            return Err(self.root_manifest_source.error_with_hint(
                DiagCode::new(3610),
                "missing root package entry",
                self.root_manifest_source.insertion_span(),
                "missing `name` field",
                "add `name` and `version`, or target workspace member explicitly",
            ));
        };
        self.workspace.packages.get(id).ok_or_else(|| {
            self.root_manifest_source.error_with_hint(
                DiagCode::new(3610),
                "missing root package entry",
                self.root_manifest_source.insertion_span(),
                "missing root package record",
                "declare `name` and `version` in `musi.json`",
            )
        })
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
            .ok_or_else(|| ProjectError::UnknownPackage { name: name.into() })?;
        Ok(&package.entry)
    }
}

impl Project {
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
            ProjectError::InvalidManifestJson {
                path: self.lockfile_path.clone(),
                source,
            }
        })?;
        fs::write(&self.lockfile_path, text).map_err(|source| ProjectError::ProjectIoFailed {
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
        let task = if let Some(description) = description {
            TaskSpec::new(command).with_description(description)
        } else {
            TaskSpec::new(command)
        };
        Some(task.with_dependencies(dependencies))
    }

    /// # Errors
    ///
    /// Returns [`ProjectError`] when the named task does not exist or the task graph contains a
    /// dependency cycle.
    pub fn task_plan(&self, name: &str) -> ProjectResult<Vec<TaskSpec>> {
        let mut order = Vec::new();
        let mut seen = VisitedPackageNames::new();
        let mut active = VisitedPackageNames::new();
        self.collect_task_plan(name, &mut seen, &mut active, &mut order)?;
        Ok(order)
    }

    fn collect_task_plan(
        &self,
        name: &str,
        seen: &mut VisitedPackageNames,
        active: &mut VisitedPackageNames,
        out: &mut Vec<TaskSpec>,
    ) -> ProjectResult {
        if !seen.insert(name.into()) {
            return Ok(());
        }
        if !active.insert(name.into()) {
            return Err(ProjectError::TaskDependencyCycle { name: name.into() });
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
}

impl Project {
    /// # Errors
    ///
    /// Returns [`ProjectError`] when the project modules cannot be registered into a configured
    /// [`Session`].
    pub fn build_session(&self) -> ProjectResult<Session> {
        let mut import_map = self.import_map.clone();
        extend_import_map(&mut import_map);
        let mut session_options = SessionOptions::new()
            .with_emit(self.options.emit)
            .with_import_map(import_map);
        if let Some(target) = self.options.target.clone() {
            session_options = session_options.with_target(target);
        }
        let mut session = Session::new(session_options);
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
    pub fn compile_root_entry(&self) -> CompiledOutputResult {
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

fn resolve_workspace_state(
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

fn manifest_path_for(path: &Path) -> ProjectResult<PathBuf> {
    let manifest_path = if path.file_name() == Some(OsStr::new("musi.json")) {
        path.to_path_buf()
    } else {
        path.join("musi.json")
    };
    if manifest_path.is_file() {
        Ok(normalize_lookup_path(&manifest_path))
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
            return Ok(normalize_lookup_path(&manifest_path));
        }
    }

    Err(ProjectError::MissingManifestAncestor {
        path: path.to_path_buf(),
    })
}

fn read_manifest(path: &Path) -> ProjectResult<LoadedManifest> {
    let text = fs::read_to_string(path).map_err(|source| ProjectError::ProjectIoFailed {
        path: path.to_path_buf(),
        source,
    })?;
    let source = ManifestSource::from_text(path.to_path_buf(), text.clone());
    let manifest = serde_json::from_str(&text).map_err(|error| source.parse_error(&error))?;
    Ok(LoadedManifest { manifest, source })
}

fn validate_manifest(manifest: &PackageManifest, source: &ManifestSource) -> ProjectResult {
    if let Some(name) = &manifest.name {
        if name.trim().is_empty() {
            let span = source
                .value_span(&json_pointer(&["name"]))
                .unwrap_or_else(|| source.insertion_span());
            return Err(source.error(
                DiagCode::new(3606),
                "package name empty",
                span,
                "`name` must not be empty",
            ));
        }
    }
    for (index, lib) in manifest.enabled_libs().into_iter().enumerate() {
        if lib != "std" {
            let pointer = format!("/lib/{index}");
            let span = source
                .value_span(&pointer)
                .or_else(|| source.value_span(&json_pointer(&["lib"])))
                .unwrap_or_else(|| source.insertion_span());
            return Err(source.error(
                DiagCode::new(3606),
                format!("unknown lib `{lib}`"),
                span,
                format!("lib `{lib}` is not supported"),
            ));
        }
    }
    if matches!(manifest.publish, Some(PublishConfig::Disabled(true))) {
        let span = source
            .value_span(&json_pointer(&["publish"]))
            .unwrap_or_else(|| source.insertion_span());
        return Err(source.error(
            DiagCode::new(3606),
            "invalid publish value",
            span,
            "`publish` boolean must be false",
        ));
    }
    if matches!(
        manifest.musi_modules_dir,
        Some(MusiModulesDir::Disabled(true))
    ) {
        let span = source
            .value_span(&json_pointer(&["musiModulesDir"]))
            .unwrap_or_else(|| source.insertion_span());
        return Err(source.error(
            DiagCode::new(3606),
            "invalid musiModulesDir value",
            span,
            "`musiModulesDir` boolean must be false",
        ));
    }
    for (export_name, export_path) in manifest.export_map() {
        if export_name != "." && !export_name.starts_with("./") {
            let pointer = format!("/exports/{}", escape_pointer_segment(&export_name));
            let span = source
                .key_span(&pointer)
                .or_else(|| source.value_span(&json_pointer(&["exports"])))
                .unwrap_or_else(|| source.insertion_span());
            return Err(source.error(
                DiagCode::new(3606),
                format!("invalid export key `{export_name}`"),
                span,
                "export key must be `.` or start with `./`",
            ));
        }
        if !export_path.starts_with("./") {
            let pointer = format!("/exports/{}", escape_pointer_segment(&export_name));
            let span = source
                .value_span(&pointer)
                .or_else(|| source.value_span(&json_pointer(&["exports"])))
                .unwrap_or_else(|| source.insertion_span());
            return Err(source.error(
                DiagCode::new(3606),
                format!("invalid export target `{export_path}`"),
                span,
                "export target must start with `./`",
            ));
        }
    }
    validate_task_graph(manifest, source)?;
    Ok(())
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
            manifest_source.error_with_hint(
                DiagCode::new(3606),
                "package name missing",
                manifest_source.insertion_span(),
                "`name` field missing",
                "add `name` to this package manifest",
            )
        })?,
        manifest.version.clone().ok_or_else(|| {
            manifest_source.error_with_hint(
                DiagCode::new(3606),
                "package version missing",
                manifest_source.insertion_span(),
                "`version` field missing",
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

fn validate_task_graph(manifest: &PackageManifest, source: &ManifestSource) -> ProjectResult {
    let mut seen = BTreeSet::new();
    let mut active = BTreeSet::new();
    for name in manifest.tasks.keys() {
        validate_task_node(name, manifest, source, &mut seen, &mut active)?;
    }
    Ok(())
}

fn validate_task_node(
    name: &str,
    manifest: &PackageManifest,
    source: &ManifestSource,
    seen: &mut TaskNameSet,
    active: &mut TaskNameSet,
) -> ProjectResult {
    if !seen.insert(name.into()) {
        return Ok(());
    }
    if !active.insert(name.into()) {
        return Err(ProjectError::TaskDependencyCycle { name: name.into() });
    }
    if let Some(task) = manifest.task_config(name) {
        for (index, dependency) in task.dependencies.into_iter().enumerate() {
            if manifest.task_config(&dependency).is_none() {
                let pointer = format!(
                    "/tasks/{}/dependencies/{}",
                    escape_pointer_segment(name),
                    index
                );
                let span = source
                    .value_span(&pointer)
                    .unwrap_or_else(|| source.insertion_span());
                return Err(source.error(
                    DiagCode::new(3606),
                    format!("task dependency `{dependency}` not found"),
                    span,
                    format!("task `{name}` depends on unknown task `{dependency}`"),
                ));
            }
            validate_task_node(&dependency, manifest, source, seen, active)?;
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
            member_source.error_with_hint(
                DiagCode::new(3606),
                "workspace member name missing",
                member_source.insertion_span(),
                "`name` field missing",
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
        source.error_with_hint(
            DiagCode::new(3606),
            "workspace member name missing",
            source.insertion_span(),
            "`name` field missing",
            "add `name` to this workspace member manifest",
        )
    })
}

#[derive(Debug, Clone, Copy)]
struct DependencyResolutionInputs<'a> {
    local_packages: &'a LocalPackageMap,
    options: &'a ProjectOptions,
    storage: &'a ProjectStorage,
    lockfile: &'a Lockfile,
}

fn resolve_package_dependencies(
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

fn load_package_record(
    root_dir: PathBuf,
    manifest_path: PathBuf,
    manifest_source: &ManifestSource,
    manifest: PackageManifest,
    source: PackageSource,
) -> ProjectResult<PackageRecord> {
    let id = PackageId::new(
        manifest.name.clone().ok_or_else(|| {
            manifest_source.error_with_hint(
                DiagCode::new(3606),
                "package name missing",
                manifest_source.insertion_span(),
                "`name` field missing",
                "add `name` to this package manifest",
            )
        })?,
        manifest.version.clone().ok_or_else(|| {
            manifest_source.error_with_hint(
                DiagCode::new(3606),
                "package version missing",
                manifest_source.insertion_span(),
                "`version` field missing",
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
    let label = entry_target.map_or_else(
        || "missing `entry` field and root `index.ms`".into(),
        |target| format!("entry target `{target}` does not resolve"),
    );
    let hint = match entry_target {
        Some(_) => "update `entry` to existing module path",
        None => "add `entry`, or create `index.ms` at package root",
    };
    manifest_source.error_with_hint(
        DiagCode::new(3611),
        format!("missing entry module for package `{package_name}`"),
        span,
        label,
        hint,
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
    manifest_source.error_with_hint(
        DiagCode::new(3606),
        format!("export target `{target}` missing"),
        span,
        format!("export `{export_name}` in package `{package_name}` points to missing module"),
        "update export target or add target module",
    )
}

fn escape_pointer_segment(segment: &str) -> String {
    segment.replace('~', "~0").replace('/', "~1")
}
