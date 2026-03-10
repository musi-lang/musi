use std::collections::HashMap;
use std::fs;
use std::path::Path;

use serde::Deserialize;

mod error;

pub use error::ManifestError;

/// Loads and parses an `mspackage.toml` manifest from the given path.
///
/// # Errors
///
/// Returns `ManifestError::Read` if the file cannot be read, or
/// `ManifestError::Parse` if the TOML content is malformed.
pub fn load(path: &Path) -> Result<MusiManifest, ManifestError> {
    let content = fs::read_to_string(path).map_err(|e| ManifestError::Read {
        path: path.to_path_buf(),
        source: e,
    })?;
    let manifest: MusiManifest = toml::from_str(&content).map_err(|e| ManifestError::Parse {
        path: path.to_path_buf(),
        source: e,
    })?;
    Ok(manifest)
}

#[derive(Debug, Default, Deserialize)]
#[serde(rename_all = "kebab-case")]
pub struct MusiManifest {
    #[serde(default)]
    pub package: PackageInfo,
    #[serde(default)]
    pub imports: HashMap<String, String>,
    #[serde(default)]
    pub dependencies: HashMap<String, String>,
    #[serde(default)]
    pub dev_dependencies: HashMap<String, String>,
    #[serde(default)]
    pub peer_dependencies: HashMap<String, String>,
    #[serde(default)]
    pub optional_dependencies: HashMap<String, String>,
    #[serde(default)]
    pub compiler_options: CompilerOptions,
    #[serde(default)]
    pub tasks: HashMap<String, TaskDef>,
    #[serde(default)]
    pub lock: LockConfig,
    #[serde(default)]
    pub fmt: FmtConfig,
    #[serde(default)]
    pub lint: LintConfig,
    #[serde(default)]
    pub test: FilterConfig,
    #[serde(default)]
    pub bench: FilterConfig,
    #[serde(default)]
    pub publish: FilterConfig,
    #[serde(default)]
    pub workspaces: WorkspacesConfig,
    #[serde(default)]
    pub watch_options: WatchOptions,
    #[serde(default)]
    pub references: Vec<Reference>,
}

#[derive(Debug, Default, Deserialize)]
#[serde(rename_all = "kebab-case")]
pub struct PackageInfo {
    #[serde(default)]
    pub name: String,
    #[serde(default)]
    pub version: String,
    #[serde(default)]
    pub description: String,
    #[serde(default = "default_main")]
    pub main: String,
    #[serde(default)]
    pub license: String,
    #[serde(default)]
    pub homepage: String,
    #[serde(default)]
    pub keywords: Vec<String>,
    #[serde(default)]
    pub files: Vec<String>,
    #[serde(default)]
    pub private: bool,
    #[serde(default = "default_publish_registry")]
    pub publish_registry: String,
    #[serde(default)]
    pub exclude: Vec<String>,
    #[serde(default)]
    pub include: Vec<String>,
    #[serde(default)]
    pub compile_files: Vec<String>,
    #[serde(default = "default_deps_dir")]
    pub deps_dir: String,
    #[serde(default)]
    pub vendor: bool,
    #[serde(default)]
    pub author: Option<AuthorInfo>,
    #[serde(default)]
    pub repository: Option<RepositoryInfo>,
    #[serde(default)]
    pub bugs: Option<BugsInfo>,
    #[serde(default)]
    pub exports: Option<Exports>,
    #[serde(default)]
    pub bin: Option<BinConfig>,
}

fn default_main() -> String {
    String::from("./index.ms")
}
fn default_publish_registry() -> String {
    String::from("https://msr.musi-lang.org")
}
fn default_deps_dir() -> String {
    String::from("auto")
}

#[derive(Debug, Default, Deserialize)]
pub struct AuthorInfo {
    #[serde(default)]
    pub name: String,
    #[serde(default)]
    pub email: String,
    #[serde(default)]
    pub url: String,
}

#[derive(Debug, Default, Deserialize)]
pub struct RepositoryInfo {
    #[serde(default = "default_repo_type")]
    pub r#type: String,
    #[serde(default)]
    pub url: String,
    #[serde(default)]
    pub directory: String,
}

fn default_repo_type() -> String {
    String::from("git")
}

#[derive(Debug, Default, Deserialize)]
pub struct BugsInfo {
    #[serde(default)]
    pub url: String,
    #[serde(default)]
    pub email: String,
}

#[derive(Debug, Deserialize)]
#[serde(untagged)]
pub enum Exports {
    Single(String),
    Map(HashMap<String, String>),
}

#[derive(Debug, Deserialize)]
#[serde(untagged)]
pub enum BinConfig {
    Single(String),
    Map(HashMap<String, String>),
}

#[allow(clippy::struct_excessive_bools)]
#[derive(Debug, Deserialize)]
#[serde(rename_all = "kebab-case")]
pub struct CompilerOptions {
    #[serde(default = "default_target")]
    pub target: String,
    #[serde(default)]
    pub lib: Vec<String>,
    #[serde(default = "default_out_dir")]
    pub out_dir: String,
    #[serde(default = "default_root_dir")]
    pub root_dir: String,
    #[serde(default = "default_base_url")]
    pub base_url: String,
    #[serde(default)]
    pub strict: bool,
    #[serde(default)]
    pub no_implicit_any: bool,
    #[serde(default)]
    pub no_implicit_unit: bool,
    #[serde(default)]
    pub strict_function_types: bool,
    #[serde(default)]
    pub strict_optional_checks: bool,
    #[serde(default)]
    pub no_missing_cases_in_match: bool,
    #[serde(default)]
    pub exact_optional_property_types: bool,
    #[serde(default)]
    pub no_unused_locals: bool,
    #[serde(default)]
    pub no_unused_parameters: bool,
    #[serde(default)]
    pub no_implicit_returns: bool,
    #[serde(default)]
    pub allow_unreachable_code: bool,
    #[serde(default)]
    pub allow_unused_labels: bool,
    #[serde(default = "true_default")]
    pub force_consistent_casing_in_file_names: bool,
    #[serde(default = "true_default")]
    pub skip_lib_check: bool,
    #[serde(default)]
    pub no_emit: bool,
    #[serde(default = "true_default")]
    pub no_emit_on_error: bool,
    #[serde(default = "true_default")]
    pub remove_comments: bool,
    #[serde(default)]
    pub composite: bool,
    #[serde(default)]
    pub incremental: bool,
    #[serde(default = "default_build_info_file")]
    pub build_info_file: String,
    #[serde(default)]
    pub paths: HashMap<String, Vec<String>>,
}

fn default_target() -> String {
    String::from("MS2025")
}
fn default_out_dir() -> String {
    String::from("./dist")
}
fn default_root_dir() -> String {
    String::from("./src")
}
fn default_base_url() -> String {
    String::from("./")
}
fn default_build_info_file() -> String {
    String::from(".msbuildinfo")
}
const fn true_default() -> bool {
    true
}

impl Default for CompilerOptions {
    fn default() -> Self {
        Self {
            target: default_target(),
            lib: Vec::new(),
            out_dir: default_out_dir(),
            root_dir: default_root_dir(),
            base_url: default_base_url(),
            strict: false,
            no_implicit_any: false,
            no_implicit_unit: false,
            strict_function_types: false,
            strict_optional_checks: false,
            no_missing_cases_in_match: false,
            exact_optional_property_types: false,
            no_unused_locals: false,
            no_unused_parameters: false,
            no_implicit_returns: false,
            allow_unreachable_code: false,
            allow_unused_labels: false,
            force_consistent_casing_in_file_names: true,
            skip_lib_check: true,
            no_emit: false,
            no_emit_on_error: true,
            remove_comments: true,
            composite: false,
            incremental: false,
            build_info_file: default_build_info_file(),
            paths: HashMap::new(),
        }
    }
}

#[derive(Debug, Deserialize)]
#[serde(untagged)]
pub enum TaskDef {
    Simple(String),
    Complex(TaskDefTable),
}

#[derive(Debug, Deserialize)]
pub struct TaskDefTable {
    #[serde(default)]
    pub description: String,
    pub command: String,
    #[serde(default)]
    pub dependencies: Vec<String>,
}

#[derive(Debug, Deserialize)]
#[serde(rename_all = "kebab-case")]
pub struct LockConfig {
    #[serde(default = "default_lock_path")]
    pub path: String,
    #[serde(default)]
    pub frozen: bool,
}

fn default_lock_path() -> String {
    String::from("musi.lock")
}

impl Default for LockConfig {
    fn default() -> Self {
        Self {
            path: default_lock_path(),
            frozen: false,
        }
    }
}

#[derive(Debug, Deserialize)]
#[serde(rename_all = "kebab-case")]
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
    #[serde(default = "default_prose_wrap")]
    pub prose_wrap: String,
}

const fn default_line_width() -> u32 {
    80
}
const fn default_indent_width() -> u32 {
    2
}
fn default_prose_wrap() -> String {
    String::from("preserve")
}

impl Default for FmtConfig {
    fn default() -> Self {
        Self {
            include: Vec::new(),
            exclude: Vec::new(),
            use_tabs: false,
            line_width: default_line_width(),
            indent_width: default_indent_width(),
            prose_wrap: default_prose_wrap(),
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
pub struct WorkspacesConfig {
    #[serde(default)]
    pub packages: Vec<String>,
}

#[derive(Debug, Default, Deserialize)]
#[serde(rename_all = "kebab-case")]
pub struct WatchOptions {
    #[serde(default)]
    pub watch_file: String,
    #[serde(default)]
    pub watch_directory: String,
    #[serde(default)]
    pub exclude_files: Vec<String>,
    #[serde(default)]
    pub exclude_directories: Vec<String>,
}

#[derive(Debug, Default, Deserialize)]
pub struct Reference {
    #[serde(default)]
    pub path: String,
}
