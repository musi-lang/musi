use music_arena::Idx;

use crate::artifact::StringRecord;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ClassDescriptor {
    pub name: Idx<StringRecord>,
}
