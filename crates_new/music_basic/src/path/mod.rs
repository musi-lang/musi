use std::path::{Component, Path, PathBuf};

#[must_use]
pub fn normalize_path(path: &Path) -> PathBuf {
    let mut out = PathBuf::new();
    for c in path.components() {
        match c {
            Component::CurDir => {}
            Component::ParentDir => {
                let _ = out.pop();
            }
            Component::RootDir | Component::Prefix(_) | Component::Normal(_) => {
                out.push(c.as_os_str());
            }
        }
    }
    out
}

#[must_use]
pub fn resolve_import_path(from_path: &Path, raw: &str) -> PathBuf {
    let mut path = PathBuf::from(raw);
    if !path.is_absolute() {
        let base = from_path.parent().unwrap_or_else(|| Path::new(""));
        path = base.join(path);
    }

    if path.extension().is_none() {
        let _did_set = path.set_extension("ms");
    }

    normalize_path(&path)
}

#[cfg(test)]
#[allow(clippy::unwrap_used, clippy::panic)]
mod tests;
