//! IR constant values.

use music_shared::{Idx, Symbol};

use crate::types::IrType;

/// A top-level constant in the IR module.
#[derive(Debug, Clone)]
pub struct IrConst {
    /// The constant value.
    pub value: IrConstValue,
    /// The type of the constant.
    pub ty: Idx<IrType>,
}

/// A concrete compile-time value.
#[derive(Debug, Clone, PartialEq)]
pub enum IrConstValue {
    /// Integer constant (up to 64-bit signed).
    Int(i64),
    /// Floating-point constant (64-bit).
    Float(f64),
    /// Boolean constant.
    Bool(bool),
    /// Rune (Unicode scalar value).
    Rune(u32),
    /// Interned string constant.
    Str(Symbol),
    /// Unit value.
    Unit,
    /// Function reference by id (NaN-box tag `0x7FF7`).
    FnRef(u32),
}
