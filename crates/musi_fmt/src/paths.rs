use std::fs;
use std::path::{Component, Path, PathBuf};

use crate::{
    FormatError, FormatInputKind, FormatOptions, FormatResultOf, format_markdown, format_source,
};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FormatPathChange {
    pub path: PathBuf,
    pub changed: bool,
}

#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct FormatPathSummary {
    pub files: Vec<FormatPathChange>,
}

impl FormatPathSummary {
    #[must_use]
    pub fn changed_paths(&self) -> Vec<PathBuf> {
        self.files
            .iter()
            .filter(|file| file.changed)
            .map(|file| file.path.clone())
            .collect()
    }

    #[must_use]
    pub const fn is_empty(&self) -> bool {
        self.files.is_empty()
    }
}

/// Formats one text buffer using the format kind implied by `path`.
///
/// # Errors
///
/// Returns [`FormatError`] when parsing or formatting fails.
pub fn format_text_for_path(path: &Path, text: &str, options: &FormatOptions) -> FormatResultOf {
    match input_kind_for_path(path, options) {
        Some(FormatInputKind::Musi) => format_source(text, options),
        Some(FormatInputKind::Markdown) => format_markdown(text, options),
        None => Ok(crate::FormatResult {
            text: text.to_owned(),
            changed: false,
        }),
    }
}

/// Formats one file.
///
/// # Errors
///
/// Returns [`FormatError`] when reading, parsing, or writing fails.
pub fn format_file(
    path: &Path,
    options: &FormatOptions,
    check: bool,
) -> FormatResultOf<FormatPathChange> {
    let text = fs::read_to_string(path).map_err(|source| FormatError::IoFailed {
        path: path.to_path_buf(),
        source,
    })?;
    let formatted = format_text_for_path(path, &text, options)?;
    if formatted.changed && !check {
        fs::write(path, formatted.text).map_err(|source| FormatError::IoFailed {
            path: path.to_path_buf(),
            source,
        })?;
    }
    Ok(FormatPathChange {
        path: path.to_path_buf(),
        changed: formatted.changed,
    })
}

/// Formats Musi source files under the given paths.
///
/// # Errors
///
/// Returns [`FormatError`] when walking, reading, parsing, or writing fails.
pub fn format_paths(
    roots: &[PathBuf],
    base_dir: &Path,
    options: &FormatOptions,
    check: bool,
) -> FormatResultOf<FormatPathSummary> {
    let mut files = Vec::new();
    for root in roots {
        collect_format_files(root, base_dir, options, &mut files)?;
    }
    files.sort();
    files.dedup();

    let mut summary = FormatPathSummary::default();
    for file in files {
        summary.files.push(format_file(&file, options, check)?);
    }
    Ok(summary)
}

fn collect_format_files(
    path: &Path,
    base_dir: &Path,
    options: &FormatOptions,
    out: &mut Vec<PathBuf>,
) -> FormatResultOf<()> {
    if should_skip_path(path, base_dir, options) {
        return Ok(());
    }
    if path.is_file() {
        if input_kind_for_path(path, options).is_some() {
            out.push(path.to_path_buf());
        }
        return Ok(());
    }
    if !path.is_dir() {
        return Ok(());
    }
    let entries = fs::read_dir(path).map_err(|source| FormatError::IoFailed {
        path: path.to_path_buf(),
        source,
    })?;
    for entry in entries {
        let entry = entry.map_err(|source| FormatError::IoFailed {
            path: path.to_path_buf(),
            source,
        })?;
        collect_format_files(&entry.path(), base_dir, options, out)?;
    }
    Ok(())
}

fn input_kind_for_path(path: &Path, options: &FormatOptions) -> Option<FormatInputKind> {
    let extension = path.extension().and_then(|extension| extension.to_str());
    extension
        .and_then(FormatInputKind::from_extension)
        .or_else(|| {
            extension
                .is_none()
                .then_some(options.assume_extension)
                .flatten()
        })
}

fn should_skip_path(path: &Path, base_dir: &Path, options: &FormatOptions) -> bool {
    if path.components().any(is_ignored_component) {
        return true;
    }
    let relative = path.strip_prefix(base_dir).unwrap_or(path);
    let relative_text = relative.to_string_lossy().replace('\\', "/");
    if !options.include.is_empty()
        && path.is_file()
        && !options
            .include
            .iter()
            .any(|pattern| matches_pattern(pattern, &relative_text))
    {
        return true;
    }
    options
        .exclude
        .iter()
        .any(|pattern| matches_pattern(pattern, &relative_text))
}

fn is_ignored_component(component: Component<'_>) -> bool {
    let text = component.as_os_str().to_string_lossy();
    matches!(
        text.as_ref(),
        ".git" | ".cache" | ".musi" | "musi_modules" | "node_modules" | "target"
    )
}

fn matches_pattern(pattern: &str, value: &str) -> bool {
    if pattern == "**" || pattern == "**/*" {
        return true;
    }
    if let Some(prefix) = pattern.strip_suffix("/**") {
        return value == prefix || value.starts_with(&format!("{prefix}/"));
    }
    if let Some(suffix) = pattern.strip_prefix("**/*") {
        return value.ends_with(suffix);
    }
    if let Some(suffix) = pattern.strip_prefix('*') {
        return value.ends_with(suffix);
    }
    if let Some(prefix) = pattern.strip_suffix('*') {
        return value.starts_with(prefix);
    }
    pattern == value
}
