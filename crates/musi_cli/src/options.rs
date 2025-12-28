use serde::Deserialize;
use std::path::PathBuf;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Default, Deserialize)]
#[serde(rename_all = "UPPERCASE")]
pub enum Target {
    #[default]
    MS2025,
    Native,
    Wasm,
    Embedded,
}

#[allow(clippy::struct_excessive_bools)]
#[derive(Debug, Clone, Copy, Default, Deserialize)]
#[serde(rename_all = "camelCase", default)]
pub struct StrictOptions {
    pub strict: bool,
    pub no_implicit_any: bool,
    pub no_implicit_unit: bool,
    pub strict_function_types: bool,
    pub strict_optional_checks: bool,
    pub no_missing_cases_in_match: bool,
    pub exact_optional_property_types: bool,
    pub no_implicit_returns: bool,
}

#[allow(clippy::struct_excessive_bools)]
#[derive(Debug, Clone, Copy, Default, Deserialize)]
#[serde(rename_all = "camelCase", default)]
pub struct UnusedCodeOptions {
    pub no_unused_locals: bool,
    pub no_unused_parameters: bool,
    pub allow_unreachable_code: bool,
    pub allow_unused_labels: bool,
}

#[allow(clippy::struct_excessive_bools)]
#[derive(Debug, Clone, Copy, Default, Deserialize)]
#[serde(rename_all = "camelCase", default)]
pub struct EmitOptions {
    pub no_emit: bool,
    pub no_emit_on_error: bool,
    pub remove_comments: bool,
    pub composite: bool,
    pub incremental: bool,
}

#[derive(Debug, Clone, Copy, Default, Deserialize)]
#[serde(rename_all = "camelCase", default)]
pub struct ModuleResolutionOptions {
    pub force_consistent_casing_in_file_names: bool,
    pub skip_lib_check: bool,
}

#[derive(Debug, Clone, Default, Deserialize)]
#[serde(rename_all = "camelCase", default)]
pub struct CompilerOptions {
    pub target: Target,
    pub lib: Vec<String>,
    pub out_dir: Option<PathBuf>,
    pub root_dir: Option<PathBuf>,
    pub base_url: Option<PathBuf>,
    #[serde(flatten)]
    pub strict: StrictOptions,
    #[serde(flatten)]
    pub unused: UnusedCodeOptions,
    #[serde(flatten)]
    pub emit: EmitOptions,
    #[serde(flatten)]
    pub module: ModuleResolutionOptions,
    pub build_info_file: Option<PathBuf>,
}

impl CompilerOptions {
    pub const fn apply_strict(&mut self) {
        if self.strict.strict {
            self.strict.no_implicit_any = true;
            self.strict.no_implicit_unit = true;
            self.strict.strict_function_types = true;
            self.strict.strict_optional_checks = true;
            self.strict.no_missing_cases_in_match = true;
            self.strict.exact_optional_property_types = true;
            self.unused.no_unused_locals = true;
            self.unused.no_unused_parameters = true;
            self.strict.no_implicit_returns = true;
        }
    }
}
