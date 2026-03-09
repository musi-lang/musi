//! MSIR — the Musi intermediate representation.
//!
//! MSIR is an A-normal form IR sitting between semantic analysis (`music_sema`)
//! and bytecode generation.  Every non-trivial subexpression is bound to a local
//! variable, making evaluation order explicit and simplifying code generation for
//! the stack-machine target.
//!
//! # Module layout
//!
//! - [`types`] — IR type representation (monomorphized or existentialized)
//! - [`inst`] — instructions, rvalues, operands, places
//! - [`func`] — function definitions and local declarations
//! - [`constant`] — compile-time constant values
//! - [`effect`] — effect definitions and identifiers
//! - [`error`] — structured lowering errors
//! - [`lower`] — lowering passes (AST+sema -> IR)
//! - [`pretty`] — IR pretty-printer for debugging

pub mod constant;
pub mod effect;
pub mod error;
pub mod func;
pub mod inst;
pub mod lower;
pub mod pretty;
pub mod types;

pub use constant::{IrConst, IrConstValue};
pub use effect::{IrEffectDef, IrEffectId, IrEffectOpDef, IrEffectOpId};
pub use error::IrError;
pub use func::{IrFnId, IrFunction, IrLocal, IrLocalDecl, IrParam, IrParamMode};
pub use inst::{
    IrBinOp, IrCallee, IrInst, IrLabel, IrOperand, IrPlace, IrRvalue, IrSwitchArm, IrUnaryOp,
};
pub use types::{GenericStrategy, IrEffectMask, IrSumVariant, IrType, TypeMetadata, WitnessEntry};

use music_shared::{Arena, Idx};

/// The top-level IR module produced by lowering.
pub struct IrModule {
    /// All functions in the module (arena-allocated).
    pub functions: Arena<IrFunction>,
    /// All IR types (arena-allocated).
    pub types: Arena<IrType>,
    /// Top-level constants.
    pub constants: Vec<IrConst>,
    /// The module entry point, if any.
    pub entry: Option<Idx<IrFunction>>,
    /// Effect definitions.
    pub effects: Vec<IrEffectDef>,
}

impl IrModule {
    /// Creates an empty IR module.
    #[must_use]
    pub const fn new() -> Self {
        Self {
            functions: Arena::new(),
            types: Arena::new(),
            constants: Vec::new(),
            entry: None,
            effects: Vec::new(),
        }
    }
}

impl Default for IrModule {
    fn default() -> Self {
        Self::new()
    }
}
