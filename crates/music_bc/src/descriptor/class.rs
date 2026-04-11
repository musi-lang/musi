use crate::artifact::StringId;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ClassDescriptor {
    pub name: StringId,
}
