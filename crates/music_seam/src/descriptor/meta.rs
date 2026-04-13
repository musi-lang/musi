use crate::artifact::StringId;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MetaDescriptor {
    pub target: StringId,
    pub key: StringId,
    pub values: Box<[StringId]>,
}

impl MetaDescriptor {
    #[must_use]
    pub const fn new(target: StringId, key: StringId, values: Box<[StringId]>) -> Self {
        Self {
            target,
            key,
            values,
        }
    }
}
