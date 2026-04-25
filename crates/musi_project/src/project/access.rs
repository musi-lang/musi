use std::fs;
use std::path::Path;

use music_base::diag::DiagContext;
use music_module::{ImportMap, ModuleKey};

use crate::errors::ProjectError;
use crate::lock::Lockfile;
use crate::manifest::PackageManifest;
use crate::project::model::{Project, ProjectEntry, ResolvedPackage, WorkspaceGraph};
use crate::project::module_graph::normalize_lookup_path;
use crate::{ProjectDiagKind, ProjectResult};

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
            return Err(self.root_manifest_source.catalog_error_with_hint(
                ProjectDiagKind::ManifestPackageNameMissing,
                DiagContext::new().with("path", self.root_manifest_path.display()),
                self.root_manifest_source.insertion_span(),
                "add `name` and `version`, or target workspace member explicitly",
            ));
        };
        self.workspace.packages.get(id).ok_or_else(|| {
            self.root_manifest_source.catalog_error_with_hint(
                ProjectDiagKind::NoRootPackage,
                DiagContext::new(),
                self.root_manifest_source.insertion_span(),
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
}
