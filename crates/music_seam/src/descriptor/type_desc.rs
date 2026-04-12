use crate::artifact::StringId;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TypeDescriptor {
    pub name: StringId,
    pub term: StringId,
}

impl TypeDescriptor {
    #[must_use]
    pub const fn new(name: StringId, term: StringId) -> Self {
        Self { name, term }
    }
}
