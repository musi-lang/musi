//! IR effect definitions and identifiers.

use music_shared::{Idx, Symbol};

use crate::types::IrType;

/// Unique identifier for an effect definition in the IR.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct IrEffectId(pub u32);

/// Unique identifier for an effect operation.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct IrEffectOpId(pub u32);

/// An effect definition in the IR module.
#[derive(Debug, Clone)]
pub struct IrEffectDef {
    /// Unique identifier for this effect.
    pub id: IrEffectId,
    /// Name of the effect.
    pub name: Symbol,
    /// Operations provided by this effect.
    pub ops: Vec<IrEffectOpDef>,
}

/// A single operation within an effect definition.
#[derive(Debug, Clone)]
pub struct IrEffectOpDef {
    /// Unique identifier for this operation.
    pub id: IrEffectOpId,
    /// Name of the operation.
    pub name: Symbol,
    /// Parameter types.
    pub param_tys: Vec<Idx<IrType>>,
    /// Return type.
    pub ret_ty: Idx<IrType>,
}
