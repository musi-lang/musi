use std::collections::{BTreeMap, BTreeSet};
use std::path::PathBuf;

use music_base::Span;
use music_emit::EmitOptions;
use music_module::{ImportMap, ModuleKey};
use music_sema::TargetInfo;
use music_session::CompiledOutput;

use crate::ProjectResult;
use crate::lock::Lockfile;
use crate::manifest::PackageManifest;
use crate::manifest_source::ManifestSource;

pub(super) type ExportModuleMap = BTreeMap<String, ModuleKey>;
pub(super) type DependencyPackageMap = BTreeMap<String, PackageId>;
pub(super) type LocalPackageMap = BTreeMap<String, LocalPackage>;
pub(super) type VisitedPackageNames = BTreeSet<String>;
pub(super) type TaskNameSet = BTreeSet<String>;
pub(super) type CompiledOutputResult = ProjectResult<CompiledOutput>;

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
    pub(super) options: ProjectOptions,
    pub(super) root_dir: PathBuf,
    pub(super) root_manifest_path: PathBuf,
    pub(super) root_manifest_source: ManifestSource,
    pub(super) manifest: PackageManifest,
    pub(super) workspace: WorkspaceGraph,
    pub(super) import_map: ImportMap,
    pub(super) module_texts: BTreeMap<ModuleKey, String>,
    pub(super) package_name_index: BTreeMap<String, PackageId>,
    pub(super) lockfile_path: PathBuf,
    pub(super) global_cache_dir: PathBuf,
    pub(super) modules_dir: Option<PathBuf>,
    pub(super) loaded_lockfile: Lockfile,
    pub(super) resolved_lockfile: Lockfile,
}

#[derive(Debug, Clone)]
pub(super) struct LoadedModule {
    pub(super) key: ModuleKey,
    pub(super) path: PathBuf,
    pub(super) package_relative: String,
    pub(super) local_scope_key: ModuleKey,
    pub(super) text: String,
    pub(super) imports: Vec<LoadedImportSite>,
}

#[derive(Debug, Clone)]
pub(super) struct LoadedImportSite {
    pub(super) spec: String,
    pub(super) span: Span,
}

#[derive(Debug, Clone)]
pub(super) struct PackageRecord {
    pub(super) package: ResolvedPackage,
    pub(super) modules: BTreeMap<ModuleKey, LoadedModule>,
    pub(super) relative_modules: ExportModuleMap,
}

#[derive(Debug, Clone)]
pub(super) struct LocalPackage {
    pub(super) manifest_path: PathBuf,
    pub(super) root_dir: PathBuf,
    pub(super) source: ManifestSource,
    pub(super) manifest: PackageManifest,
}

#[derive(Debug, Clone)]
pub(super) struct LoadedManifest {
    pub(super) manifest: PackageManifest,
    pub(super) source: ManifestSource,
}

pub(super) struct WorkspaceSeed {
    pub(super) package_records: BTreeMap<PackageId, PackageRecord>,
    pub(super) package_name_index: DependencyPackageMap,
}

pub(super) struct ResolvedWorkspaceState {
    pub(super) workspace: WorkspaceGraph,
    pub(super) package_name_index: DependencyPackageMap,
    pub(super) import_map: ImportMap,
    pub(super) module_texts: BTreeMap<ModuleKey, String>,
    pub(super) resolved_lockfile: Lockfile,
}
