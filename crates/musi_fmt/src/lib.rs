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

use musi_project::manifest::{
    FmtBracePosition, FmtConfig, FmtGroupLayout, FmtMatchArmArrowAlignment, FmtMatchArmIndent,
    FmtOperatorBreak, FmtProfile, FmtTrailingCommas,
};
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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum MatchArmIndent {
    PipeAligned,
    Block,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum MatchArmArrowAlignment {
    None,
    Consecutive,
    Block,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum GroupLayout {
    Auto,
    Block,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum OperatorBreak {
    Before,
    After,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FormatOptions {
    pub use_tabs: bool,
    pub line_width: usize,
    pub indent_width: usize,
    pub trailing_commas: TrailingCommas,
    pub brace_position: BracePosition,
    pub match_arm_indent: MatchArmIndent,
    pub match_arm_arrow_alignment: MatchArmArrowAlignment,
    pub call_argument_layout: GroupLayout,
    pub declaration_parameter_layout: GroupLayout,
    pub record_field_layout: GroupLayout,
    pub effect_member_parameter_layout: GroupLayout,
    pub operator_break: OperatorBreak,
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
            match_arm_indent: MatchArmIndent::PipeAligned,
            match_arm_arrow_alignment: MatchArmArrowAlignment::None,
            call_argument_layout: GroupLayout::Auto,
            declaration_parameter_layout: GroupLayout::Auto,
            record_field_layout: GroupLayout::Block,
            effect_member_parameter_layout: GroupLayout::Auto,
            operator_break: OperatorBreak::Before,
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
            if let Some(profile) = config.profile {
                options.apply_profile(profile);
            }
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
            if let Some(trailing_commas) = config.trailing_commas {
                options.trailing_commas = TrailingCommas::from_manifest_value(trailing_commas);
            }
            if let Some(brace_position) = config.brace_position {
                options.brace_position = BracePosition::from_manifest_value(brace_position);
            }
            if let Some(match_arm_indent) = config.match_arm_indent {
                options.match_arm_indent = MatchArmIndent::from_manifest_value(match_arm_indent);
            }
            if let Some(alignment) = config.match_arm_arrow_alignment {
                options.match_arm_arrow_alignment =
                    MatchArmArrowAlignment::from_manifest_value(alignment);
            }
            if let Some(layout) = config.call_argument_layout {
                options.call_argument_layout = GroupLayout::from_manifest_value(layout);
            }
            if let Some(layout) = config.declaration_parameter_layout {
                options.declaration_parameter_layout = GroupLayout::from_manifest_value(layout);
            }
            if let Some(layout) = config.record_field_layout {
                options.record_field_layout = GroupLayout::from_manifest_value(layout);
            }
            if let Some(layout) = config.effect_member_parameter_layout {
                options.effect_member_parameter_layout = GroupLayout::from_manifest_value(layout);
            }
            if let Some(operator_break) = config.operator_break {
                options.operator_break = OperatorBreak::from_manifest_value(operator_break);
            }
        }
        options
    }

    pub fn apply_profile(&mut self, profile: FmtProfile) {
        match profile {
            FmtProfile::Standard => {}
            FmtProfile::Compact => {
                self.line_width = 100;
                self.match_arm_indent = MatchArmIndent::PipeAligned;
                self.match_arm_arrow_alignment = MatchArmArrowAlignment::None;
                self.record_field_layout = GroupLayout::Auto;
            }
            FmtProfile::Expanded => {
                self.line_width = 80;
                self.match_arm_indent = MatchArmIndent::Block;
                self.match_arm_arrow_alignment = MatchArmArrowAlignment::Block;
                self.call_argument_layout = GroupLayout::Block;
                self.declaration_parameter_layout = GroupLayout::Block;
                self.record_field_layout = GroupLayout::Block;
                self.effect_member_parameter_layout = GroupLayout::Block;
            }
        }
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
    pub const fn from_manifest_value(value: FmtTrailingCommas) -> Self {
        match value {
            FmtTrailingCommas::Never => Self::Never,
            FmtTrailingCommas::Always => Self::Always,
            FmtTrailingCommas::MultiLine => Self::MultiLine,
        }
    }
}

impl BracePosition {
    #[must_use]
    pub const fn from_manifest_value(value: FmtBracePosition) -> Self {
        match value {
            FmtBracePosition::SameLine => Self::SameLine,
            FmtBracePosition::NextLine => Self::NextLine,
        }
    }
}

impl MatchArmIndent {
    #[must_use]
    pub const fn from_manifest_value(value: FmtMatchArmIndent) -> Self {
        match value {
            FmtMatchArmIndent::PipeAligned => Self::PipeAligned,
            FmtMatchArmIndent::Block => Self::Block,
        }
    }
}

impl MatchArmArrowAlignment {
    #[must_use]
    pub const fn from_manifest_value(value: FmtMatchArmArrowAlignment) -> Self {
        match value {
            FmtMatchArmArrowAlignment::None => Self::None,
            FmtMatchArmArrowAlignment::Consecutive => Self::Consecutive,
            FmtMatchArmArrowAlignment::Block => Self::Block,
        }
    }
}

impl GroupLayout {
    #[must_use]
    pub const fn from_manifest_value(value: FmtGroupLayout) -> Self {
        match value {
            FmtGroupLayout::Auto => Self::Auto,
            FmtGroupLayout::Block => Self::Block,
        }
    }
}

impl OperatorBreak {
    #[must_use]
    pub const fn from_manifest_value(value: FmtOperatorBreak) -> Self {
        match value {
            FmtOperatorBreak::Before => Self::Before,
            FmtOperatorBreak::After => Self::After,
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
