use music_arena::Idx;

use crate::artifact::StringRecord;
use crate::descriptor::MethodDescriptor;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct GlobalDescriptor {
    pub name: Idx<StringRecord>,
    pub export: bool,
    pub initializer: Option<Idx<MethodDescriptor>>,
}
