use crate::artifact::StringId;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DataDescriptor {
    pub name: StringId,
    pub variant_count: u32,
    pub field_count: u32,
    pub repr_kind: Option<StringId>,
    pub layout_align: Option<u32>,
    pub layout_pack: Option<u32>,
}
