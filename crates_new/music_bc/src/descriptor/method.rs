use music_arena::Idx;

use crate::artifact::StringRecord;
use crate::instruction::CodeEntry;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MethodDescriptor {
    pub name: Idx<StringRecord>,
    pub params: u16,
    pub locals: u16,
    pub export: bool,
    pub labels: Box<[Idx<StringRecord>]>,
    pub code: Box<[CodeEntry]>,
}
