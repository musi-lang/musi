use std::collections::BTreeMap;

use serde::Deserialize;

pub type ManifestStringMap = BTreeMap<String, String>;
pub type ManifestScopeMap = BTreeMap<String, ManifestStringMap>;
pub type ManifestMetadata = BTreeMap<String, serde_json::Value>;
pub type ManifestLibList = Vec<String>;

#[derive(Debug, Clone, Deserialize, PartialEq, Eq, Default)]
#[serde(default, deny_unknown_fields, rename_all = "camelCase")]
pub struct PackageManifest {
    #[serde(rename = "$schema")]
    pub schema: Option<String>,
    pub name: Option<String>,
    pub version: Option<String>,
    pub description: Option<String>,
    pub docs: Option<String>,
    pub readme: Option<Readme>,
    pub entry: Option<String>,
    pub exports: Option<Exports>,
    pub license: Option<License>,
    pub author: Option<Author>,
    pub contributors: Vec<Author>,
    pub repository: Option<Repository>,
    pub homepage: Option<String>,
    pub bugs: Option<Bugs>,
    pub keywords: Vec<String>,
    pub categories: Vec<String>,
    pub metadata: ManifestMetadata,
    pub lib: Option<ManifestLibList>,
    pub imports: ManifestStringMap,
    pub scopes: ManifestScopeMap,
    pub dependencies: ManifestStringMap,
    #[serde(rename = "devDependencies")]
    pub dev_dependencies: ManifestStringMap,
    #[serde(rename = "peerDependencies")]
    pub peer_dependencies: ManifestStringMap,
    #[serde(rename = "optionalDependencies")]
    pub optional_dependencies: ManifestStringMap,
    pub overrides: ManifestStringMap,
    #[serde(rename = "musiModulesDir")]
    pub musi_modules_dir: Option<MusiModulesDir>,
    #[serde(rename = "compilerOptions")]
    pub compiler_options: Option<CompilerOptions>,
    pub tasks: BTreeMap<String, TaskDefinition>,
    pub fmt: Option<FmtConfig>,
    pub lint: Option<LintConfig>,
    pub lints: ManifestStringMap,
    pub test: Option<TestConfig>,
    pub bench: Option<BenchConfig>,
    pub compile: Option<CompileConfig>,
    pub publish: Option<PublishConfig>,
    pub lock: Option<LockConfig>,
    pub workspace: Option<WorkspaceConfig>,
    pub exclude: Vec<String>,
}

#[derive(Debug, Clone, Deserialize, PartialEq, Eq)]
#[serde(untagged)]
pub enum Exports {
    Entry(String),
    Map(ManifestStringMap),
}

#[derive(Debug, Clone, Deserialize, PartialEq, Eq)]
#[serde(untagged)]
pub enum License {
    Spdx(String),
    File(LicenseFile),
}

#[derive(Debug, Clone, Deserialize, PartialEq, Eq)]
#[serde(untagged)]
pub enum Readme {
    Path(String),
    Enabled(bool),
}

#[derive(Debug, Clone, Deserialize, PartialEq, Eq)]
#[serde(deny_unknown_fields)]
pub struct LicenseFile {
    pub file: String,
}

#[derive(Debug, Clone, Deserialize, PartialEq, Eq)]
#[serde(untagged)]
pub enum Author {
    Name(String),
    Detailed(AuthorObject),
}

#[derive(Debug, Clone, Deserialize, PartialEq, Eq)]
#[serde(deny_unknown_fields)]
pub struct AuthorObject {
    pub name: String,
    pub email: Option<String>,
    pub url: Option<String>,
}

#[derive(Debug, Clone, Deserialize, PartialEq, Eq)]
#[serde(untagged)]
pub enum Repository {
    Url(String),
    Detailed(RepositoryObject),
}

#[derive(Debug, Clone, Deserialize, PartialEq, Eq)]
#[serde(deny_unknown_fields)]
pub struct RepositoryObject {
    #[serde(rename = "type")]
    pub kind: String,
    pub url: String,
    pub directory: Option<String>,
}

#[derive(Debug, Clone, Deserialize, PartialEq, Eq)]
#[serde(untagged)]
pub enum Bugs {
    Url(String),
    Detailed(BugsObject),
}

#[derive(Debug, Clone, Deserialize, PartialEq, Eq)]
#[serde(deny_unknown_fields)]
pub struct BugsObject {
    pub url: Option<String>,
    pub email: Option<String>,
}

#[derive(Debug, Clone, Deserialize, PartialEq, Eq, Default)]
#[serde(default, deny_unknown_fields, rename_all = "camelCase")]
pub struct CompilerOptions {
    pub strict: Option<bool>,
    pub no_unused_locals: Option<bool>,
    pub no_unused_parameters: Option<bool>,
    pub no_error_truncation: Option<bool>,
    pub base_url: Option<String>,
    pub paths: BTreeMap<String, Vec<String>>,
}

#[derive(Debug, Clone, Deserialize, PartialEq, Eq)]
#[serde(untagged)]
pub enum TaskDefinition {
    Command(String),
    Object(TaskDefinitionObject),
}

#[derive(Debug, Clone, Deserialize, PartialEq, Eq, Default)]
#[serde(default, deny_unknown_fields)]
pub struct TaskDefinitionObject {
    pub description: Option<String>,
    pub command: String,
    pub dependencies: Vec<String>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TaskConfig {
    pub description: Option<String>,
    pub command: String,
    pub dependencies: Vec<String>,
}

impl TaskConfig {
    #[must_use]
    pub const fn new(
        description: Option<String>,
        command: String,
        dependencies: Vec<String>,
    ) -> Self {
        Self {
            description,
            command,
            dependencies,
        }
    }

    #[must_use]
    pub const fn from_command(command: String) -> Self {
        Self::new(None, command, Vec::new())
    }

    #[must_use]
    pub fn from_object(object: &TaskDefinitionObject) -> Self {
        Self::new(
            object.description.clone(),
            object.command.clone(),
            object.dependencies.clone(),
        )
    }

    #[must_use]
    pub fn from_definition(definition: &TaskDefinition) -> Self {
        match definition {
            TaskDefinition::Command(command) => Self::from_command(command.clone()),
            TaskDefinition::Object(object) => Self::from_object(object),
        }
    }
}

#[derive(Debug, Clone, Deserialize, PartialEq, Eq, Default)]
#[serde(default, deny_unknown_fields, rename_all = "camelCase")]
pub struct FmtConfig {
    pub include: Vec<String>,
    pub exclude: Vec<String>,
    pub use_tabs: Option<bool>,
    pub line_width: Option<u32>,
    pub indent_width: Option<u32>,
    pub trailing_commas: Option<String>,
    pub brace_position: Option<String>,
}

#[derive(Debug, Clone, Deserialize, PartialEq, Eq, Default)]
#[serde(default, deny_unknown_fields)]
pub struct TestConfig {
    pub include: Vec<String>,
    pub exclude: Vec<String>,
}

#[derive(Debug, Clone, Deserialize, PartialEq, Eq, Default)]
#[serde(default, deny_unknown_fields)]
pub struct BenchConfig {
    pub include: Vec<String>,
    pub exclude: Vec<String>,
}

#[derive(Debug, Clone, Deserialize, PartialEq, Eq, Default)]
#[serde(default, deny_unknown_fields, rename_all = "camelCase")]
pub struct LintConfig {
    pub include: Vec<String>,
    pub exclude: Vec<String>,
    pub rules: Option<LintRules>,
    pub report: Option<String>,
}

#[derive(Debug, Clone, Deserialize, PartialEq, Eq, Default)]
#[serde(default, deny_unknown_fields)]
pub struct LintRules {
    pub tags: Vec<String>,
    pub exclude: Vec<String>,
    pub include: Vec<String>,
}

#[derive(Debug, Clone, Deserialize, PartialEq, Eq, Default)]
#[serde(default, deny_unknown_fields)]
pub struct CompileConfig {
    pub target: Option<String>,
    pub output: Option<String>,
}

#[derive(Debug, Clone, Deserialize, PartialEq, Eq)]
#[serde(untagged)]
pub enum PublishConfig {
    Settings(PublishSettings),
    Disabled(bool),
}

#[derive(Debug, Clone, Deserialize, PartialEq, Eq, Default)]
#[serde(default, deny_unknown_fields)]
pub struct PublishSettings {
    pub registry: Option<String>,
    pub include: Vec<String>,
    pub exclude: Vec<String>,
}

#[derive(Debug, Clone, Deserialize, PartialEq, Eq)]
#[serde(untagged)]
pub enum LockConfig {
    Flag(bool),
    Path(String),
    Object(LockConfigObject),
}

#[derive(Debug, Clone, Deserialize, PartialEq, Eq, Default)]
#[serde(default, deny_unknown_fields)]
pub struct LockConfigObject {
    pub path: Option<String>,
    pub frozen: Option<bool>,
}

#[derive(Debug, Clone, Deserialize, PartialEq, Eq)]
#[serde(untagged)]
pub enum WorkspaceConfig {
    Members(Vec<String>),
    Object(WorkspaceMembersObject),
}

#[derive(Debug, Clone, Deserialize, PartialEq, Eq)]
#[serde(untagged)]
pub enum MusiModulesDir {
    Enabled(String),
    Disabled(bool),
}

#[derive(Debug, Clone, Deserialize, PartialEq, Eq, Default)]
#[serde(default, deny_unknown_fields)]
pub struct WorkspaceMembersObject {
    pub members: Vec<String>,
}

impl PackageManifest {
    #[must_use]
    pub fn new() -> Self {
        Self::default()
    }

    #[must_use]
    pub fn entry_path(&self) -> &str {
        self.entry.as_deref().unwrap_or("index.ms")
    }

    #[must_use]
    pub fn enabled_libs(&self) -> Vec<&str> {
        self.lib.as_ref().map_or_else(
            || vec!["std"],
            |libs| libs.iter().map(String::as_str).collect(),
        )
    }

    #[must_use]
    pub fn workspace_members(&self) -> &[String] {
        match &self.workspace {
            Some(WorkspaceConfig::Members(members)) => members,
            Some(WorkspaceConfig::Object(object)) => &object.members,
            None => &[],
        }
    }

    #[must_use]
    pub const fn modules_dir(&self) -> Option<&str> {
        match &self.musi_modules_dir {
            Some(MusiModulesDir::Enabled(path)) => Some(path.as_str()),
            Some(MusiModulesDir::Disabled(false)) => None,
            Some(MusiModulesDir::Disabled(true)) | None => Some("musi_modules"),
        }
    }

    #[must_use]
    pub fn lock_path(&self) -> &str {
        match &self.lock {
            Some(LockConfig::Path(path)) => path,
            Some(LockConfig::Object(object)) => object.path.as_deref().unwrap_or("musi.lock"),
            _ => "musi.lock",
        }
    }

    #[must_use]
    pub const fn is_lock_enabled(&self) -> bool {
        !matches!(self.lock, Some(LockConfig::Flag(false)))
    }

    #[must_use]
    pub const fn is_lock_frozen(&self) -> bool {
        matches!(
            self.lock,
            Some(LockConfig::Object(LockConfigObject {
                frozen: Some(true),
                ..
            }))
        )
    }

    #[must_use]
    pub fn export_map(&self) -> ManifestStringMap {
        match &self.exports {
            Some(Exports::Entry(path)) => {
                let mut map = ManifestStringMap::new();
                let _ = map.insert(".".into(), path.clone());
                map
            }
            Some(Exports::Map(map)) => map.clone(),
            None => {
                let mut map = ManifestStringMap::new();
                let _ = map.insert(".".into(), format!("./{}", self.entry_path()));
                map
            }
        }
    }

    #[must_use]
    pub const fn dependency_maps(&self) -> [(&str, &ManifestStringMap); 4] {
        [
            ("dependencies", &self.dependencies),
            ("devDependencies", &self.dev_dependencies),
            ("peerDependencies", &self.peer_dependencies),
            ("optionalDependencies", &self.optional_dependencies),
        ]
    }

    #[must_use]
    pub fn task_config(&self, name: &str) -> Option<TaskConfig> {
        self.tasks.get(name).map(TaskConfig::from_definition)
    }
}
