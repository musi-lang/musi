use std::collections::HashMap;
use std::fs;
use std::path::Path;

use serde::Deserialize;

mod error;

pub use error::ManifestError;

/// Loads and parses a `musi.json` manifest from the given path.
///
/// # Errors
///
/// Returns `ManifestError::Read` if the file cannot be read, or
/// `ManifestError::Parse` if the JSON content is malformed.
pub fn load(path: &Path) -> Result<MusiManifest, ManifestError> {
    let content = fs::read_to_string(path).map_err(|e| ManifestError::Read {
        path: path.to_path_buf(),
        source: e,
    })?;
    let manifest: MusiManifest =
        serde_json::from_str(&content).map_err(|e| ManifestError::Parse {
            path: path.to_path_buf(),
            source: e,
        })?;
    Ok(manifest)
}

#[derive(Debug, Default, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct MusiManifest {
    #[serde(default)]
    pub name: Option<String>,
    #[serde(default)]
    pub version: Option<String>,
    #[serde(default)]
    pub description: Option<String>,
    #[serde(default)]
    pub main: Option<String>,
    #[serde(default)]
    pub exports: Option<Exports>,
    #[serde(default)]
    pub license: Option<String>,
    #[serde(default)]
    pub author: Option<Author>,
    #[serde(default)]
    pub contributors: Vec<Author>,
    #[serde(default)]
    pub private: bool,
    #[serde(default)]
    pub repository: Option<Repository>,
    #[serde(default)]
    pub homepage: Option<String>,
    #[serde(default)]
    pub bugs: Option<Bugs>,
    #[serde(default)]
    pub keywords: Vec<String>,
    #[serde(default)]
    pub imports: HashMap<String, String>,
    #[serde(default)]
    pub scopes: HashMap<String, HashMap<String, String>>,
    #[serde(default)]
    pub dependencies: HashMap<String, String>,
    #[serde(default)]
    pub dev_dependencies: HashMap<String, String>,
    #[serde(default)]
    pub peer_dependencies: HashMap<String, String>,
    #[serde(default)]
    pub optional_dependencies: HashMap<String, String>,
    #[serde(default)]
    pub overrides: HashMap<String, String>,
    #[serde(default)]
    pub compiler_options: CompilerOptions,
    #[serde(default)]
    pub tasks: HashMap<String, TaskDef>,
    #[serde(default)]
    pub fmt: FmtConfig,
    #[serde(default)]
    pub lint: LintConfig,
    #[serde(default)]
    pub test: FilterConfig,
    #[serde(default)]
    pub bench: FilterConfig,
    #[serde(default)]
    pub compile: CompileConfig,
    #[serde(default)]
    pub publish: Option<PublishConfig>,
    #[serde(default)]
    pub lock: LockConfig,
    #[serde(default)]
    pub workspace: Option<Workspace>,
    #[serde(default)]
    pub exclude: Vec<String>,
}

impl MusiManifest {
    /// Returns the package entry point, defaulting to `"index.ms"` when `main` is absent.
    #[must_use]
    pub fn entry_point(&self) -> &str {
        self.main.as_deref().unwrap_or("index.ms")
    }
}

/// Package author - either a plain string or a structured object.
#[derive(Debug, Deserialize)]
#[serde(untagged)]
pub enum Author {
    String(String),
    Object {
        name: String,
        #[serde(default)]
        email: Option<String>,
        #[serde(default)]
        url: Option<String>,
    },
}

/// Repository location - either a URI string or a structured object.
#[derive(Debug, Deserialize)]
#[serde(untagged)]
pub enum Repository {
    Url(String),
    Object {
        r#type: String,
        url: String,
        #[serde(default)]
        directory: Option<String>,
    },
}

/// Bug tracker - either a URI string or a structured object.
#[derive(Debug, Deserialize)]
#[serde(untagged)]
pub enum Bugs {
    Url(String),
    Object {
        #[serde(default)]
        url: Option<String>,
        #[serde(default)]
        email: Option<String>,
    },
}

/// Package exports - either a single path string or an export map.
#[derive(Debug, Deserialize)]
#[serde(untagged)]
pub enum Exports {
    Single(String),
    Map(HashMap<String, String>),
}

/// Workspace members - either an array of globs or an object with a `members` field.
#[derive(Debug, Deserialize)]
#[serde(untagged)]
pub enum Workspace {
    Members(Vec<String>),
    Object { members: Vec<String> },
}

/// Publish configuration - either include/exclude lists or `false` to disable publishing.
#[derive(Debug, Deserialize)]
#[serde(untagged)]
pub enum PublishConfig {
    Config(FilterConfig),
    Disabled(bool),
}

/// Lock file configuration - path string, boolean, or a detailed object.
#[derive(Debug, Deserialize)]
#[serde(untagged)]
pub enum LockConfig {
    Path(String),
    Enabled(bool),
    Object {
        #[serde(default)]
        path: Option<String>,
        #[serde(default)]
        frozen: bool,
    },
}

impl Default for LockConfig {
    fn default() -> Self {
        Self::Enabled(true)
    }
}

/// Task definition - either a plain command string or a structured task object.
#[derive(Debug, Deserialize)]
#[serde(untagged)]
pub enum TaskDef {
    Simple(String),
    Complex(TaskDefObject),
}

#[derive(Debug, Deserialize)]
pub struct TaskDefObject {
    #[serde(default)]
    pub description: Option<String>,
    pub command: String,
    #[serde(default)]
    pub dependencies: Vec<String>,
}

const fn default_true() -> bool {
    true
}

// CompilerOptions fields map 1:1 to the musi.json schema's `compilerOptions` object.
// The schema mandates these as individual boolean properties; they cannot be grouped
// without breaking JSON deserialization of the flat schema shape.
// False positive: bools are schema-mandated, not a design choice — grouping would break serde.
#[allow(clippy::struct_excessive_bools)]
#[derive(Debug, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct CompilerOptions {
    #[serde(default = "default_true")]
    pub strict: bool,
    #[serde(default = "default_true")]
    pub no_implicit_any: bool,
    #[serde(default)]
    pub no_unused_locals: bool,
    #[serde(default)]
    pub no_unused_parameters: bool,
    #[serde(default)]
    pub no_implicit_returns: bool,
    #[serde(default)]
    pub allow_unreachable_code: bool,
    #[serde(default)]
    pub no_error_truncation: bool,
    #[serde(default)]
    pub base_url: Option<String>,
    #[serde(default)]
    pub paths: Option<HashMap<String, Vec<Option<String>>>>,
}

impl Default for CompilerOptions {
    fn default() -> Self {
        Self {
            strict: true,
            no_implicit_any: true,
            no_unused_locals: false,
            no_unused_parameters: false,
            no_implicit_returns: false,
            allow_unreachable_code: false,
            no_error_truncation: false,
            base_url: None,
            paths: None,
        }
    }
}

#[derive(Debug, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct FmtConfig {
    #[serde(default)]
    pub include: Vec<String>,
    #[serde(default)]
    pub exclude: Vec<String>,
    #[serde(default)]
    pub use_tabs: bool,
    #[serde(default = "default_line_width")]
    pub line_width: u32,
    #[serde(default = "default_indent_width")]
    pub indent_width: u32,
    #[serde(default = "default_semi_colons")]
    pub semi_colons: bool,
    #[serde(default = "default_trailing_commas")]
    pub trailing_commas: String,
    #[serde(default = "default_brace_position")]
    pub brace_position: String,
}

const fn default_line_width() -> u32 {
    100
}
const fn default_indent_width() -> u32 {
    4
}
const fn default_semi_colons() -> bool {
    true
}
fn default_trailing_commas() -> String {
    String::from("multiLine")
}
fn default_brace_position() -> String {
    String::from("sameLine")
}

impl Default for FmtConfig {
    fn default() -> Self {
        Self {
            include: vec![],
            exclude: vec![],
            use_tabs: false,
            line_width: default_line_width(),
            indent_width: default_indent_width(),
            semi_colons: default_semi_colons(),
            trailing_commas: default_trailing_commas(),
            brace_position: default_brace_position(),
        }
    }
}

#[derive(Debug, Default, Deserialize)]
pub struct LintConfig {
    #[serde(default)]
    pub include: Vec<String>,
    #[serde(default)]
    pub exclude: Vec<String>,
    #[serde(default)]
    pub rules: LintRules,
    #[serde(default)]
    pub report: Option<String>,
}

#[derive(Debug, Default, Deserialize)]
pub struct LintRules {
    #[serde(default)]
    pub tags: Vec<String>,
    #[serde(default)]
    pub include: Vec<String>,
    #[serde(default)]
    pub exclude: Vec<String>,
}

#[derive(Debug, Default, Deserialize)]
pub struct FilterConfig {
    #[serde(default)]
    pub include: Vec<String>,
    #[serde(default)]
    pub exclude: Vec<String>,
}

#[derive(Debug, Default, Deserialize)]
pub struct CompileConfig {
    #[serde(default)]
    pub target: Option<String>,
    #[serde(default)]
    pub output: Option<String>,
}
