//! IR constant values.

use music_shared::{Idx, Symbol};

use crate::types::IrType;

/// A top-level constant in the IR module.
#[derive(Debug, Clone)]
pub struct IrConst {
    pub value: IrConstValue,
    pub ty: Idx<IrType>,
}

/// A concrete compile-time value.
#[derive(Debug, Clone, PartialEq)]
pub enum IrConstValue {
    Int(i64),
    Float(f64),
    Bool(bool),
    /// Rune (Unicode scalar value).
    Rune(u32),
    /// Interned string constant.
    Str(Symbol),
    Unit,
    /// Function reference by id (NaN-box tag `0x7FF7`).
    FnRef(u32),
}
