use crate::artifact::{ProcedureId, TypeId};
use crate::instruction::LabelId;

pub type BlockSignatureTypeIdList = Box<[TypeId]>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct BlockSignatureDescriptor {
    pub procedure: ProcedureId,
    pub label: LabelId,
    pub incoming_tys: BlockSignatureTypeIdList,
}

impl BlockSignatureDescriptor {
    #[must_use]
    pub const fn new(
        procedure: ProcedureId,
        label: LabelId,
        incoming_tys: BlockSignatureTypeIdList,
    ) -> Self {
        Self {
            procedure,
            label,
            incoming_tys,
        }
    }
}
