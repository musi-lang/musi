use music_arena::Idx;

use crate::artifact::StringRecord;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DataDescriptor {
    pub name: Idx<StringRecord>,
    pub variant_count: u32,
    pub field_count: u32,
    pub repr_kind: Option<Idx<StringRecord>>,
    pub layout_align: Option<u32>,
    pub layout_pack: Option<u32>,
}
