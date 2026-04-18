mod imports;
mod line_width;
mod markdown;
mod paths;
pub mod pretty;
mod protected;
mod roles;
mod source;
mod token_class;

use std::io::Error as IoError;
use std::path::PathBuf;

pub use markdown::format_markdown;
pub use paths::{FormatPathChange, FormatPathSummary, format_file, format_paths};
pub use source::{FormatResult, format_source};

use musi_project::manifest::FmtConfig;
use thiserror::Error;

pub type FormatResultOf<T = FormatResult> = Result<T, FormatError>;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TrailingCommas {
    Never,
    Always,
    MultiLine,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BracePosition {
    SameLine,
    NextLine,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FormatOptions {
    pub use_tabs: bool,
    pub line_width: usize,
    pub indent_width: usize,
    pub trailing_commas: TrailingCommas,
    pub brace_position: BracePosition,
    pub include: Vec<String>,
    pub exclude: Vec<String>,
    pub assume_extension: Option<FormatInputKind>,
}

impl Default for FormatOptions {
    fn default() -> Self {
        Self {
            use_tabs: false,
            line_width: 80,
            indent_width: 2,
            trailing_commas: TrailingCommas::MultiLine,
            brace_position: BracePosition::SameLine,
            include: Vec::new(),
            exclude: Vec::new(),
            assume_extension: None,
        }
    }
}

impl FormatOptions {
    #[must_use]
    pub fn from_manifest(config: Option<&FmtConfig>) -> Self {
        let mut options = Self::default();
        if let Some(config) = config {
            options.include.clone_from(&config.include);
            options.exclude.clone_from(&config.exclude);
            if let Some(use_tabs) = config.use_tabs {
                options.use_tabs = use_tabs;
            }
            if let Some(line_width) = config
                .line_width
                .and_then(|value| usize::try_from(value).ok())
            {
                options.line_width = line_width;
            }
            if let Some(indent_width) = config
                .indent_width
                .and_then(|value| usize::try_from(value).ok())
            {
                options.indent_width = indent_width;
            }
            if let Some(trailing_commas) = config.trailing_commas.as_deref() {
                options.trailing_commas = TrailingCommas::from_manifest_value(trailing_commas);
            }
            if let Some(brace_position) = config.brace_position.as_deref() {
                options.brace_position = BracePosition::from_manifest_value(brace_position);
            }
        }
        options
    }

    #[must_use]
    pub fn indent_unit(&self) -> String {
        if self.use_tabs {
            "\t".to_owned()
        } else {
            " ".repeat(self.indent_width)
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum FormatInputKind {
    Musi,
    Markdown,
}

impl FormatInputKind {
    #[must_use]
    pub fn from_extension(extension: &str) -> Option<Self> {
        match extension.to_ascii_lowercase().as_str() {
            "ms" => Some(Self::Musi),
            "md" | "mkd" | "mkdn" | "mdwn" | "mdown" | "markdown" => Some(Self::Markdown),
            _ => None,
        }
    }
}

impl TrailingCommas {
    #[must_use]
    pub const fn from_manifest_value(value: &str) -> Self {
        match value.as_bytes() {
            b"never" => Self::Never,
            b"always" => Self::Always,
            _ => Self::MultiLine,
        }
    }
}

impl BracePosition {
    #[must_use]
    pub const fn from_manifest_value(value: &str) -> Self {
        match value.as_bytes() {
            b"nextLine" => Self::NextLine,
            _ => Self::SameLine,
        }
    }
}

#[derive(Debug, Error)]
pub enum FormatError {
    #[error("format input has syntax errors")]
    SyntaxErrors,
    #[error("no Musi source files found")]
    NoFiles,
    #[error("formatter I/O failed at `{path}`")]
    IoFailed {
        path: PathBuf,
        #[source]
        source: IoError,
    },
    #[error("unsupported formatter extension `{extension}`")]
    UnsupportedExtension { extension: String },
}

#[cfg(test)]
#[allow(clippy::unwrap_used, clippy::panic)]
mod tests;
