use std::path::PathBuf;

const BUILTIN_MODULES_DIR: &str = "modules";

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

#[cfg(test)]
mod tests;
