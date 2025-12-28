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

#[derive(Debug, Clone, Default, Deserialize)]
#[serde(rename_all = "camelCase", default)]
pub struct CompilerOptions {
    pub target: Target,
    pub lib: Vec<String>,
    pub out_dir: Option<PathBuf>,
    pub root_dir: Option<PathBuf>,
    pub base_url: Option<PathBuf>,
    pub strict: bool,
    pub no_implicit_any: bool,
    pub no_implicit_unit: bool,
    pub strict_function_types: bool,
    pub strict_optional_checks: bool,
    pub no_missing_cases_in_match: bool,
    pub exact_optional_property_types: bool,
    pub no_unused_locals: bool,
    pub no_unused_parameters: bool,
    pub no_implicit_returns: bool,
    pub allow_unreachable_code: bool,
    pub allow_unused_labels: bool,
    pub force_consistent_casing_in_file_names: bool,
    pub skip_lib_check: bool,
    pub no_emit: bool,
    pub no_emit_on_error: bool,
    pub remove_comments: bool,
    pub composite: bool,
    pub incremental: bool,
    pub build_info_file: Option<PathBuf>,
}

impl CompilerOptions {
    pub fn apply_strict(&mut self) {
        if self.strict {
            self.no_implicit_any = true;
            self.no_implicit_unit = true;
            self.strict_function_types = true;
            self.strict_optional_checks = true;
            self.no_missing_cases_in_match = true;
            self.exact_optional_property_types = true;
            self.no_unused_locals = true;
            self.no_unused_parameters = true;
            self.no_implicit_returns = true;
        }
    }
}
