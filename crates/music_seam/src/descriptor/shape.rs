use crate::artifact::StringId;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ShapeDescriptor {
    pub name: StringId,
}

impl ShapeDescriptor {
    #[must_use]
    pub const fn new(name: StringId) -> Self {
        Self { name }
    }
}
