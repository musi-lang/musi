use music_ast::SyntaxNodeId;
use music_basic::Span;

/// Source provenance for a HIR node.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct HirOrigin {
    pub span: Span,
    pub syntax: Option<SyntaxNodeId>,
}

impl HirOrigin {
    #[must_use]
    pub const fn new(span: Span, syntax: Option<SyntaxNodeId>) -> Self {
        Self { span, syntax }
    }

    #[must_use]
    pub const fn dummy() -> Self {
        Self {
            span: Span::DUMMY,
            syntax: None,
        }
    }
}

#[cfg(test)]
#[allow(clippy::unwrap_used, clippy::panic)]
mod tests;
