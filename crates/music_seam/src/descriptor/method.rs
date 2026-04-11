use crate::artifact::StringId;
use crate::instruction::CodeEntry;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MethodDescriptor {
    pub name: StringId,
    pub params: u16,
    pub locals: u16,
    pub export: bool,
    pub labels: Box<[StringId]>,
    pub code: Box<[CodeEntry]>,
}
