use music_arena::Idx;

use crate::artifact::StringRecord;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TypeDescriptor {
    pub name: Idx<StringRecord>,
}
