pub type BranchOffsets = Vec<i16>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Operand {
    None,
    U8(u8),
    U16(u16),
    I16(i16),
    Wide(u16, u8),
    TypeLen(u16, u16),
    Effect(u16, u16),
    EffectJump(u16, u16, i16),
    Table(BranchOffsets),
}
