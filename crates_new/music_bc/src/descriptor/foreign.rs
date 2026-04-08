use music_arena::Idx;

use crate::artifact::StringRecord;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ForeignDescriptor {
    pub name: Idx<StringRecord>,
    pub params: u16,
    pub abi: Idx<StringRecord>,
    pub symbol: Idx<StringRecord>,
    pub link: Option<Idx<StringRecord>>,
    pub export: bool,
}
