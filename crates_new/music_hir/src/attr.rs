use music_ast::SyntaxTokenId;
use music_basic::Span;
use music_names::Ident;
use music_storage::Idx;

use super::*;

pub type HirAttrId = Idx<HirAttr>;

/// Attribute path such as `@musi.lang` or `@diag.allow`.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct HirAttrPath {
    pub segments: HirIdents,
}

/// Attribute, stored in an arena and referenced by `HirAttrId`.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct HirAttr {
    pub origin: HirOrigin,
    pub path: HirAttrPath,
    pub args: Box<[HirAttrArg]>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct HirAttrArg {
    pub origin: HirOrigin,
    pub kind: HirAttrArgKind,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum HirAttrArgKind {
    Named { name: Ident, value: HirExprId },
    Positional { value: HirExprId },
}

/// A string literal surface used by attrs and other declarations.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct HirStringLit {
    pub span: Span,
    pub syntax: Option<SyntaxTokenId>,
}

impl HirStringLit {
    #[must_use]
    pub const fn new(span: Span, syntax: Option<SyntaxTokenId>) -> Self {
        Self { span, syntax }
    }
}

#[cfg(test)]
#[allow(clippy::unwrap_used, clippy::panic)]
mod tests;
