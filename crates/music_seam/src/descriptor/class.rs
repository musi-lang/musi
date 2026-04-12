use crate::artifact::StringId;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ClassDescriptor {
    pub name: StringId,
}

impl ClassDescriptor {
    #[must_use]
    pub const fn new(name: StringId) -> Self {
        Self { name }
    }
}
