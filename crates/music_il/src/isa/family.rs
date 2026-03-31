#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum OpcodeFamily {
    Data,
    Stack,
    Scalar,
    Logic,
    Compare,
    Branch,
    Call,
    Sequence,
    Aggregate,
    Type,
    Effect,
    Runtime,
}
