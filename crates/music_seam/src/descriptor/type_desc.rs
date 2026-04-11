use crate::artifact::StringId;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TypeDescriptor {
    pub name: StringId,
    pub term: StringId,
}
