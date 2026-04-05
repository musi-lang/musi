use music_arena::Idx;

use crate::artifact::StringRecord;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ForeignDescriptor {
    pub name: Idx<StringRecord>,
    pub abi: Idx<StringRecord>,
    pub symbol: Idx<StringRecord>,
    pub export: bool,
}
