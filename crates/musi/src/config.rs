//! mspackage.json configuration loader.

use std::collections::HashMap;
use std::fs;
use std::io;
use std::path::{Path, PathBuf};

use serde::{Deserialize, Serialize};

/// Compiler options subset (only what the CLI uses today).
#[allow(dead_code)] // fields consumed in Wave 2 (build_cmd)
#[derive(Debug, Default, Deserialize, Serialize)]
#[serde(rename_all = "camelCase", default)]
pub struct CompilerOptions {
    /// Output directory for compiled .mso files (default: "./dist").
    pub out_dir: Option<String>,
    /// Root source directory (default: "./src").
    pub root_dir: Option<String>,
    /// Emit no output -- check only.
    pub no_emit: bool,
}

/// A task definition from the `tasks` field in mspackage.json.
#[derive(Debug, Deserialize, Serialize)]
#[serde(untagged)]
pub enum TaskEntry {
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
    pub fn command(&self) -> &str {
        match self {
            Self::Simple(cmd) | Self::Full { command: cmd, .. } => cmd,
        }
    }

    pub fn dependencies(&self) -> &[String] {
        match self {
            Self::Simple(_) => &[],
            Self::Full { dependencies, .. } => dependencies,
        }
    }
}

/// Parsed representation of mspackage.json.
#[derive(Debug, Default, Deserialize, Serialize)]
#[serde(rename_all = "camelCase", default)]
pub struct MusiConfig {
    pub name: Option<String>,
    pub version: Option<String>,
    pub description: Option<String>,
    /// Main entry point (default: "./index.ms").
    pub main: Option<String>,
    pub compiler_options: CompilerOptions,
    #[serde(default)]
    pub tasks: HashMap<String, TaskEntry>,
    /// Package dependencies: `"@scope/name": "^0.1.0"` or `"local": "file:./path"`.
    #[serde(default)]
    pub dependencies: HashMap<String, String>,
    /// Development-only dependencies (not included in production builds).
    #[serde(default)]
    pub dev_dependencies: HashMap<String, String>,
}

/// Name of the package manifest file.
pub const CONFIG_FILENAME: &str = "mspackage.json";

/// Walk up from `start` looking for `mspackage.json`.
/// Returns `(config, config_dir)` if found, else `None`.
pub fn find_and_load(start: &Path) -> Option<(MusiConfig, PathBuf)> {
    let mut dir = start.to_path_buf();
    loop {
        let candidate = dir.join(CONFIG_FILENAME);
        if candidate.exists() {
            let text = fs::read_to_string(&candidate).ok()?;
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

/// Load config from a specific directory.
#[allow(dead_code)]
pub fn load_from_dir(dir: &Path) -> Option<MusiConfig> {
    let path = dir.join(CONFIG_FILENAME);
    let text = fs::read_to_string(&path).ok()?;
    serde_json::from_str(&text).ok()
}

/// Save config to a specific directory.
pub fn save_to_dir(dir: &Path, config: &MusiConfig) -> io::Result<()> {
    let path = dir.join(CONFIG_FILENAME);
    // serde_json serialization of MusiConfig is infallible since all fields are serializable
    let text = serde_json::to_string_pretty(config)
        .unwrap_or_else(|_| "{}".to_owned());
    fs::write(&path, text)
}
