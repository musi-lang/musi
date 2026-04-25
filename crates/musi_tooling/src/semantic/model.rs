use crate::analysis::ToolRange;

pub type ToolSemanticModifierList = Vec<ToolSemanticModifier>;
pub type ToolSemanticTokenList = Vec<ToolSemanticToken>;
pub(super) type SemanticTokenSink<'a> = &'a mut ToolSemanticTokenList;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u32)]
pub enum ToolSemanticTokenKind {
    Namespace = 0,
    Type = 1,
    TypeParameter = 2,
    Parameter = 3,
    Variable = 4,
    Property = 5,
    EnumMember = 6,
    Function = 7,
    Procedure = 8,
    Macro = 9,
    Keyword = 10,
    Modifier = 11,
    Comment = 12,
    String = 13,
    Number = 14,
    Operator = 15,
    Decorator = 16,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u32)]
pub enum ToolSemanticModifier {
    Declaration = 0,
    Definition = 1,
    Readonly = 2,
    Static = 3,
    Deprecated = 4,
    Documentation = 5,
    DefaultLibrary = 6,
    Modification = 7,
    Module = 8,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ToolSemanticToken {
    pub range: ToolRange,
    pub kind: ToolSemanticTokenKind,
    pub modifiers: ToolSemanticModifierList,
}

impl ToolSemanticToken {
    #[must_use]
    pub const fn new(
        range: ToolRange,
        kind: ToolSemanticTokenKind,
        modifiers: ToolSemanticModifierList,
    ) -> Self {
        Self {
            range,
            kind,
            modifiers,
        }
    }
}

impl ToolSemanticTokenKind {
    pub(super) const fn is_callable(self) -> bool {
        matches!(self, Self::Function | Self::Procedure)
    }
}
