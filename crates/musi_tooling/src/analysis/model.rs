use music_base::Span;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ToolHover {
    pub span: Span,
    pub range: ToolRange,
    pub contents: String,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ToolSymbolKind {
    Module,
    Function,
    Procedure,
    Variable,
    Parameter,
    TypeParameter,
    Type,
    Namespace,
    Alias,
    Property,
    EnumMember,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ToolMemberShape {
    Function,
    Procedure,
    Property,
    Type,
}

impl ToolSymbolKind {
    #[must_use]
    pub const fn label(self) -> &'static str {
        match self {
            Self::Module => "module",
            Self::Function => "function",
            Self::Procedure => "procedure",
            Self::Variable => "variable",
            Self::Parameter => "parameter",
            Self::TypeParameter => "type parameter",
            Self::Type => "type",
            Self::Namespace => "namespace",
            Self::Alias => "alias",
            Self::Property => "property",
            Self::EnumMember => "enum member",
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ToolInlayHintKind {
    Type,
    Parameter,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct ToolPosition {
    pub line: usize,
    pub col: usize,
}

impl ToolPosition {
    #[must_use]
    pub const fn new(line: usize, col: usize) -> Self {
        Self { line, col }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ToolInlayHint {
    pub position: ToolPosition,
    pub label: String,
    pub kind: ToolInlayHintKind,
    pub tooltip: Option<String>,
    pub is_literal_argument: bool,
}

impl ToolInlayHint {
    #[must_use]
    pub fn new(position: ToolPosition, label: impl Into<String>, kind: ToolInlayHintKind) -> Self {
        Self {
            position,
            label: label.into(),
            kind,
            tooltip: None,
            is_literal_argument: false,
        }
    }
}

impl ToolHover {
    #[must_use]
    pub fn new(span: Span, range: ToolRange, contents: impl Into<String>) -> Self {
        Self {
            span,
            range,
            contents: contents.into(),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct ToolRange {
    pub start_line: usize,
    pub start_col: usize,
    pub end_line: usize,
    pub end_col: usize,
}

impl ToolRange {
    #[must_use]
    pub const fn new(start_line: usize, start_col: usize, end_line: usize, end_col: usize) -> Self {
        Self {
            start_line,
            start_col,
            end_line,
            end_col,
        }
    }
}
