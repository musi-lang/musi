use music_base::{SourceId, Span};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct HirOrigin {
    pub source_id: SourceId,
    pub span: Span,
}

impl HirOrigin {
    #[must_use]
    pub const fn new(source_id: SourceId, span: Span) -> Self {
        Self { source_id, span }
    }

    #[must_use]
    pub const fn dummy() -> Self {
        Self {
            source_id: SourceId::from_raw(0),
            span: Span::DUMMY,
        }
    }
}
