use std::collections::HashMap;
use std::error::Error;
use std::fmt;
use std::fs;
use std::io;
use std::path::Path;

use serde::Deserialize;

/// Error returned when loading or parsing a `musi.json` configuration file.
#[derive(Debug)]
pub enum ConfigError {
    Io(io::Error),
    Parse(serde_json::Error),
}

impl fmt::Display for ConfigError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Io(err) => write!(f, "failed to read config: {err}"),
            Self::Parse(err) => write!(f, "failed to parse config: {err}"),
        }
    }
}

impl Error for ConfigError {
    fn source(&self) -> Option<&(dyn Error + 'static)> {
        match self {
            Self::Io(err) => Some(err),
            Self::Parse(err) => Some(err),
        }
    }
}

impl From<io::Error> for ConfigError {
    fn from(err: io::Error) -> Self {
        Self::Io(err)
    }
}

impl From<serde_json::Error> for ConfigError {
    fn from(err: serde_json::Error) -> Self {
        Self::Parse(err)
    }
}

/// Load a `MusiConfig` from a file path.
///
/// # Errors
///
/// Returns `ConfigError::Io` if the file cannot be read, or
/// `ConfigError::Parse` if the JSON is invalid.
pub fn load_config(path: &Path) -> Result<MusiConfig, ConfigError> {
    let contents = fs::read_to_string(path)?;
    load_config_from_str(&contents)
}

/// Parse a `MusiConfig` from a JSON string.
///
/// # Errors
///
/// Returns `ConfigError::Parse` if the JSON is invalid.
pub fn load_config_from_str(json: &str) -> Result<MusiConfig, ConfigError> {
    let config: MusiConfig = serde_json::from_str(json)?;
    Ok(config)
}

/// Top-level Musi configuration, parsed from `musi.json`.
#[derive(Debug, Clone, Default, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct MusiConfig {
    pub name: Option<String>,
    pub version: Option<String>,
    pub description: Option<String>,
    pub main: Option<String>,
    pub exports: Option<Exports>,
    pub license: Option<String>,
    pub author: Option<Author>,
    pub contributors: Option<Vec<Author>>,
    pub private: Option<bool>,
    pub repository: Option<Repository>,
    pub homepage: Option<String>,
    pub bugs: Option<Bugs>,
    pub keywords: Option<Vec<String>>,
    pub imports: Option<HashMap<String, String>>,
    pub scopes: Option<HashMap<String, HashMap<String, String>>>,
    pub dependencies: Option<HashMap<String, String>>,
    pub dev_dependencies: Option<HashMap<String, String>>,
    pub peer_dependencies: Option<HashMap<String, String>>,
    pub optional_dependencies: Option<HashMap<String, String>>,
    pub overrides: Option<HashMap<String, String>>,
    pub compiler_options: Option<CompilerOptions>,
    pub tasks: Option<HashMap<String, Task>>,
    pub fmt: Option<FmtConfig>,
    pub lint: Option<LintConfig>,
    pub test: Option<TestConfig>,
    pub bench: Option<BenchConfig>,
    pub compile: Option<CompileConfig>,
    pub publish: Option<PublishConfig>,
    pub lock: Option<LockConfig>,
    pub workspace: Option<Workspace>,
    pub exclude: Option<Vec<String>>,
}

/// Package exports: either a single path string or a map of export names to paths.
#[derive(Debug, Clone, Deserialize)]
#[serde(untagged)]
pub enum Exports {
    Path(String),
    Map(HashMap<String, String>),
}

/// Package author: either a string like `"Name <email>"` or a structured object.
#[derive(Debug, Clone, Deserialize)]
#[serde(untagged)]
pub enum Author {
    Inline(String),
    Object(AuthorObject),
}

/// Structured author with name, optional email, and optional URL.
#[derive(Debug, Clone, Deserialize)]
pub struct AuthorObject {
    pub name: String,
    pub email: Option<String>,
    pub url: Option<String>,
}

/// Repository location: either a URL string or a structured object.
#[derive(Debug, Clone, Deserialize)]
#[serde(untagged)]
pub enum Repository {
    Url(String),
    Object(RepositoryObject),
}

/// Structured repository with VCS type, URL, and optional directory.
#[derive(Debug, Clone, Deserialize)]
pub struct RepositoryObject {
    #[serde(rename = "type")]
    pub vcs_type: String,
    pub url: String,
    pub directory: Option<String>,
}

/// Bug tracker: either a URL string or a structured object.
#[derive(Debug, Clone, Deserialize)]
#[serde(untagged)]
pub enum Bugs {
    Url(String),
    Object(BugsObject),
}

/// Structured bug tracker with optional URL and email.
#[derive(Debug, Clone, Deserialize)]
pub struct BugsObject {
    pub url: Option<String>,
    pub email: Option<String>,
}

/// Compiler options controlling type checking behavior.
#[derive(Debug, Clone, Default, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct CompilerOptions {
    pub strict: Option<bool>,
    pub no_implicit_any: Option<bool>,
    pub no_unused_locals: Option<bool>,
    pub no_unused_parameters: Option<bool>,
    pub no_implicit_returns: Option<bool>,
    pub allow_unreachable_code: Option<bool>,
    pub no_error_truncation: Option<bool>,
    pub base_url: Option<String>,
    pub paths: Option<HashMap<String, Vec<String>>>,
}

/// Task definition: either a command string or a structured object.
#[derive(Debug, Clone, Deserialize)]
#[serde(untagged)]
pub enum Task {
    Command(String),
    Object(TaskObject),
}

/// Structured task with command, description, and dependencies.
#[derive(Debug, Clone, Deserialize)]
pub struct TaskObject {
    pub command: Option<String>,
    pub description: Option<String>,
    pub dependencies: Option<Vec<String>>,
}

/// Formatter configuration.
#[derive(Debug, Clone, Default, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct FmtConfig {
    pub include: Option<Vec<String>>,
    pub exclude: Option<Vec<String>>,
    pub use_tabs: Option<bool>,
    pub line_width: Option<u32>,
    pub indent_width: Option<u32>,
    pub semi_colons: Option<bool>,
    pub trailing_commas: Option<TrailingCommas>,
    pub brace_position: Option<BracePosition>,
}

/// Trailing comma style for the formatter.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Deserialize)]
#[serde(rename_all = "camelCase")]
pub enum TrailingCommas {
    Never,
    Always,
    MultiLine,
}

/// Opening brace position for the formatter.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Deserialize)]
#[serde(rename_all = "camelCase")]
pub enum BracePosition {
    SameLine,
    NextLine,
}

/// Linter configuration.
#[derive(Debug, Clone, Default, Deserialize)]
pub struct LintConfig {
    pub include: Option<Vec<String>>,
    pub exclude: Option<Vec<String>>,
    pub rules: Option<LintRules>,
    pub report: Option<LintReport>,
}

/// Lint rule selection.
#[derive(Debug, Clone, Default, Deserialize)]
pub struct LintRules {
    pub tags: Option<Vec<String>>,
    pub exclude: Option<Vec<String>>,
    pub include: Option<Vec<String>>,
}

/// Lint report format.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Deserialize)]
#[serde(rename_all = "camelCase")]
pub enum LintReport {
    Pretty,
    Json,
    Compact,
}

/// Test runner configuration.
#[derive(Debug, Clone, Default, Deserialize)]
pub struct TestConfig {
    pub include: Option<Vec<String>>,
    pub exclude: Option<Vec<String>>,
}

/// Benchmark runner configuration.
#[derive(Debug, Clone, Default, Deserialize)]
pub struct BenchConfig {
    pub include: Option<Vec<String>>,
    pub exclude: Option<Vec<String>>,
}

/// AOT compilation configuration.
#[derive(Debug, Clone, Default, Deserialize)]
pub struct CompileConfig {
    pub target: Option<String>,
    pub output: Option<String>,
}

/// Publish configuration: either structured include/exclude or `false` to disable.
#[derive(Debug, Clone, Deserialize)]
#[serde(untagged)]
pub enum PublishConfig {
    Disabled(bool),
    Options(PublishOptions),
}

/// Structured publish options.
#[derive(Debug, Clone, Default, Deserialize)]
pub struct PublishOptions {
    pub include: Option<Vec<String>>,
    pub exclude: Option<Vec<String>>,
}

/// Lock file configuration: a path string, a boolean, or a structured object.
#[derive(Debug, Clone, Deserialize)]
#[serde(untagged)]
pub enum LockConfig {
    Enabled(bool),
    Path(String),
    Object(LockObject),
}

/// Structured lock file options.
#[derive(Debug, Clone, Default, Deserialize)]
pub struct LockObject {
    pub path: Option<String>,
    pub frozen: Option<bool>,
}

/// Workspace configuration: either an array of member paths or a structured object.
#[derive(Debug, Clone, Deserialize)]
#[serde(untagged)]
pub enum Workspace {
    Members(Vec<String>),
    Object(WorkspaceObject),
}

/// Structured workspace with a members list.
#[derive(Debug, Clone, Default, Deserialize)]
pub struct WorkspaceObject {
    pub members: Option<Vec<String>>,
}

#[cfg(test)]
#[allow(clippy::unwrap_used, clippy::panic)]
mod tests;
