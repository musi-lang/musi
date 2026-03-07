//! mspackage.json configuration loader.

use std::collections::HashMap;
use std::path::{Path, PathBuf};

use serde::Deserialize;

/// Compiler options subset (only what the CLI uses today).
#[allow(dead_code)] // fields consumed in Wave 2 (build_cmd)
#[derive(Debug, Default, Deserialize)]
#[serde(rename_all = "camelCase", default)]
pub(crate) struct CompilerOptions {
    /// Output directory for compiled .mso files (default: "./dist").
    pub(crate) out_dir: Option<String>,
    /// Root source directory (default: "./src").
    pub(crate) root_dir: Option<String>,
    /// Emit no output -- check only.
    pub(crate) no_emit: bool,
}

/// A task definition from the `tasks` field in mspackage.json.
#[derive(Debug, Deserialize)]
#[serde(untagged)]
pub(crate) enum TaskEntry {
    /// Simple form: just a shell command string.
    Simple(String),
    /// Full form: command + optional description + optional dependencies.
    Full {
        command: String,
        #[serde(default)]
        #[allow(dead_code)]
        description: String,
        #[serde(default)]
        dependencies: Vec<String>,
    },
}

impl TaskEntry {
    pub(crate) fn command(&self) -> &str {
        match self {
            Self::Simple(cmd) | Self::Full { command: cmd, .. } => cmd,
        }
    }

    pub(crate) fn dependencies(&self) -> &[String] {
        match self {
            Self::Simple(_) => &[],
            Self::Full { dependencies, .. } => dependencies,
        }
    }
}

/// Parsed representation of mspackage.json.
#[derive(Debug, Default, Deserialize)]
#[serde(rename_all = "camelCase", default)]
pub(crate) struct MusiConfig {
    pub(crate) name: Option<String>,
    pub(crate) version: Option<String>,
    pub(crate) description: Option<String>,
    /// Main entry point (default: "./index.ms").
    pub(crate) main: Option<String>,
    pub(crate) compiler_options: CompilerOptions,
    #[serde(default)]
    pub(crate) tasks: HashMap<String, TaskEntry>,
}

/// Name of the package manifest file.
pub(crate) const CONFIG_FILENAME: &str = "mspackage.json";

/// Walk up from `start` looking for `mspackage.json`.
/// Returns `(config, config_dir)` if found, else `None`.
pub(crate) fn find_and_load(start: &Path) -> Option<(MusiConfig, PathBuf)> {
    let mut dir = start.to_path_buf();
    loop {
        let candidate = dir.join(CONFIG_FILENAME);
        if candidate.exists() {
            let text = std::fs::read_to_string(&candidate).ok()?;
            let cfg: MusiConfig = serde_json::from_str(&text).unwrap_or_else(|e| {
                eprintln!("warning: failed to parse {}: {e}", candidate.display());
                MusiConfig::default()
            });
            return Some((cfg, dir));
        }
        if !dir.pop() {
            return None;
        }
    }
}
