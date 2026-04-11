use crate::artifact::StringId;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MetaDescriptor {
    pub target: StringId,
    pub key: StringId,
    pub values: Box<[StringId]>,
}
