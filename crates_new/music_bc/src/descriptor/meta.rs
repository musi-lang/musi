use music_arena::Idx;

use crate::artifact::StringRecord;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MetaDescriptor {
    pub target: Idx<StringRecord>,
    pub key: Idx<StringRecord>,
    pub values: Box<[Idx<StringRecord>]>,
}

