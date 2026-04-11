use music_arena::Idx;

use crate::artifact::StringId;
use crate::descriptor::MethodDescriptor;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct GlobalDescriptor {
    pub name: StringId,
    pub export: bool,
    pub initializer: Option<Idx<MethodDescriptor>>,
}
