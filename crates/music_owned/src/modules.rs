use std::path::{Path, PathBuf};

const BUILTIN_MODULES_DIR: &str = "modules";

#[must_use]
pub fn prelude_source_path() -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("prelude.ms")
}

/// Resolve a public `musi:` intrinsic module to its canonical source file.
#[must_use]
pub fn resolve_module(specifier: &str) -> Option<PathBuf> {
    let module_name = specifier.strip_prefix("musi:")?;
    if module_name.is_empty() {
        return None;
    }

    let candidate = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join(BUILTIN_MODULES_DIR)
        .join(format!("{module_name}.ms"));

    candidate.exists().then_some(candidate)
}

#[must_use]
pub fn is_compiler_owned_path(path: &Path) -> bool {
    path == prelude_source_path() || path.starts_with(PathBuf::from(env!("CARGO_MANIFEST_DIR")).join(BUILTIN_MODULES_DIR))
}

#[cfg(test)]
mod tests;
