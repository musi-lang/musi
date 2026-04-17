use std::fs;
use std::path::{Path, PathBuf};

use crate::ProjectResult;
use crate::errors::ProjectError;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RegistryPackage {
    pub version: String,
    pub registry_dir: PathBuf,
    pub global_cache_dir: PathBuf,
}

impl RegistryPackage {
    #[must_use]
    pub const fn new(version: String, registry_dir: PathBuf, global_cache_dir: PathBuf) -> Self {
        Self {
            version,
            registry_dir,
            global_cache_dir,
        }
    }
}

pub fn resolve_registry_package(
    registry_root: &Path,
    global_cache_root: &Path,
    name: &str,
    requirement: &str,
) -> ProjectResult<RegistryPackage> {
    let package_root = registry_root.join(name);
    let mut best: Option<(VersionKey, PathBuf)> = None;

    let entries = fs::read_dir(&package_root).map_err(|source| ProjectError::ProjectIoFailed {
        path: package_root.clone(),
        source,
    })?;
    for entry in entries {
        let entry = entry.map_err(|source| ProjectError::ProjectIoFailed {
            path: package_root.clone(),
            source,
        })?;
        let path = entry.path();
        if !path.is_dir() {
            continue;
        }
        let file_name = entry.file_name();
        let Some(raw) = file_name.to_str() else {
            continue;
        };
        let Some(version) = VersionKey::parse(raw) else {
            continue;
        };
        if !version_matches(requirement, &version) {
            continue;
        }
        match &best {
            Some((current, _)) if &version <= current => {}
            _ => best = Some((version, path)),
        }
    }

    let Some((version, registry_dir)) = best else {
        return Err(ProjectError::RegistryVersionNotFound {
            name: name.into(),
            requirement: requirement.into(),
        });
    };

    let global_cache_dir = global_cache_root
        .join("registry")
        .join(path_cache_key(registry_root))
        .join(name)
        .join(version.raw.clone());
    if !global_cache_dir.exists() {
        copy_dir_recursive(&registry_dir, &global_cache_dir)?;
    }

    Ok(RegistryPackage::new(
        version.raw,
        registry_dir,
        global_cache_dir,
    ))
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
struct VersionKey {
    raw: String,
    parts: Vec<u32>,
}

impl VersionKey {
    const fn new(raw: String, parts: Vec<u32>) -> Self {
        Self { raw, parts }
    }

    fn parse(raw: &str) -> Option<Self> {
        let parts = raw
            .split('.')
            .map(str::parse::<u32>)
            .collect::<Result<Vec<_>, _>>()
            .ok()?;
        Some(Self::new(raw.into(), parts))
    }
}

fn version_matches(requirement: &str, version: &VersionKey) -> bool {
    let requirement = requirement.trim();
    if requirement.is_empty() || requirement == "*" {
        return true;
    }

    if let Some(exact) = requirement.strip_prefix('=') {
        return VersionKey::parse(exact).is_some_and(|required| version == &required);
    }
    if let Some(minimum) = requirement.strip_prefix(">=") {
        return VersionKey::parse(minimum).is_some_and(|required| version >= &required);
    }
    if let Some(compatible) = requirement.strip_prefix('^') {
        let Some(required) = VersionKey::parse(compatible) else {
            return false;
        };
        return version.parts.first() == required.parts.first() && version >= &required;
    }

    VersionKey::parse(requirement).is_some_and(|required| version == &required)
}

pub fn copy_dir_recursive(from: &Path, to: &Path) -> ProjectResult {
    fs::create_dir_all(to).map_err(|source| ProjectError::ProjectIoFailed {
        path: to.to_path_buf(),
        source,
    })?;
    let entries = fs::read_dir(from).map_err(|source| ProjectError::ProjectIoFailed {
        path: from.to_path_buf(),
        source,
    })?;
    for entry in entries {
        let entry = entry.map_err(|source| ProjectError::ProjectIoFailed {
            path: from.to_path_buf(),
            source,
        })?;
        let src_path = entry.path();
        let dst_path = to.join(entry.file_name());
        if src_path.is_dir() {
            copy_dir_recursive(&src_path, &dst_path)?;
        } else {
            let _ =
                fs::copy(&src_path, &dst_path).map_err(|source| ProjectError::ProjectIoFailed {
                    path: dst_path.clone(),
                    source,
                })?;
        }
    }
    Ok(())
}

pub fn path_cache_key(path: &Path) -> String {
    stable_cache_key(path.to_string_lossy().as_ref())
}

pub fn stable_cache_key(text: &str) -> String {
    const OFFSET: u64 = 0xcbf2_9ce4_8422_2325;
    const PRIME: u64 = 0x0000_0100_0000_01b3;
    let mut hash = OFFSET;
    for byte in text.as_bytes() {
        hash ^= u64::from(*byte);
        hash = hash.wrapping_mul(PRIME);
    }
    format!("{hash:016x}")
}
