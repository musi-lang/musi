use crate::artifact::StringId;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ImportDescriptor {
    pub spec: StringId,
    pub resolved: StringId,
}

impl ImportDescriptor {
    #[must_use]
    pub const fn new(spec: StringId, resolved: StringId) -> Self {
        Self { spec, resolved }
    }
}
