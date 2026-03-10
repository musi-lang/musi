//! IR function representation.
//!
//! Each function is a flat sequence of [`IrInst`] instructions in A-normal form.
//! Every local variable is declared in `locals` with its type; parameters are a
//! prefix of the locals.

use music_sema::DefId;
use music_shared::{Idx, Span, Symbol};

use crate::inst::IrInst;
use crate::types::{IrEffectMask, IrType};

/// Unique identifier for a function in the IR module (stable across passes).
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct IrFnId(pub u32);

/// A local variable slot within a function body.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct IrLocal(pub u32);

#[derive(Debug, Clone)]
pub struct IrFunction {
    /// Stable function identifier.
    pub id: IrFnId,
    /// Traceability back to the semantic definition, if any.
    pub source_def: Option<DefId>,
    /// Function name (for diagnostics and the pretty-printer).
    pub name: Symbol,
    /// Formal parameters.
    pub params: Vec<IrParam>,
    /// Return type.
    pub ret_ty: Idx<IrType>,
    /// Effect mask for this function.
    pub effects: IrEffectMask,
    /// The flat instruction sequence (ANF bindings + control flow).
    pub body: Vec<IrInst>,
    /// All local variable declarations (parameters are a prefix).
    pub locals: Vec<IrLocalDecl>,
    /// Whether this function is a closure (has a captured environment).
    pub is_closure: bool,
    /// Source span for diagnostics.
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct IrParam {
    /// The local slot this parameter occupies.
    pub local: IrLocal,
    /// The type of the parameter.
    pub ty: Idx<IrType>,
    /// Passing mode (by value or by pointer for `inout`).
    pub mode: IrParamMode,
    /// Source span for diagnostics.
    pub span: Span,
}

/// Parameter passing mode.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum IrParamMode {
    /// Pass by value (the default).
    Value,
    /// Pass by pointer (lowered from `inout`).
    Ptr,
}

#[derive(Debug, Clone)]
pub struct IrLocalDecl {
    /// The local slot.
    pub local: IrLocal,
    /// The type of the local.
    pub ty: Idx<IrType>,
    /// Whether this local is mutable.
    pub mutable: bool,
    /// Source span for diagnostics.
    pub span: Span,
}

/// A foreign (FFI) function declaration in the IR module.
#[derive(Debug, Clone)]
pub struct IrForeignFn {
    /// Musi-side binding name.
    pub name: Symbol,
    /// C-side symbol name (may differ from `name` via `as "..."`).
    pub ext_name: Symbol,
    /// Library to link against (`None` = libc/default).
    pub library: Option<Symbol>,
    /// Parameter types.
    pub param_tys: Vec<Idx<IrType>>,
    /// Return type.
    pub ret_ty: Idx<IrType>,
    /// Whether this function is variadic.
    pub variadic: bool,
}
