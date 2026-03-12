//! IR instructions, rvalues, operands, and places.
//!
//! Instructions form a flat sequence in A-normal form: every non-trivial
//! subexpression is bound to a local via `Assign`, and control flow uses
//! structured `Branch`/`Switch`/`Label`/`Goto` instead of a full CFG.

use music_sema::DefId;
use music_shared::{Span, Symbol};

use crate::constant::IrConstValue;
use crate::effect::{IrEffectId, IrEffectOpId};
use crate::func::{IrFnIdx, IrLocal};
use crate::types::IrTypeIdx;

/// A label for control-flow targets within a function body.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct IrLabel(pub u32);

#[derive(Debug, Clone)]
pub enum IrInst {
    // ── Binding ─────────────────────────────────────────────────────────
    /// `let dst = rvalue;`
    Assign {
        dst: IrLocal,
        rvalue: IrRvalue,
        span: Span,
    },

    /// `*dst <- value;`  (mutation through a place)
    Store {
        dst: IrPlace,
        value: IrOperand,
        span: Span,
    },

    // ── Control flow ────────────────────────────────────────────────────
    /// Unconditional jump.
    Goto(IrLabel),

    /// Conditional branch.
    Branch {
        cond: IrOperand,
        then_label: IrLabel,
        else_label: IrLabel,
        span: Span,
    },

    /// Multi-way branch (lowered from `match`/piecewise).
    Switch {
        scrutinee: IrOperand,
        arms: Vec<IrSwitchArm>,
        default: IrLabel,
        span: Span,
    },

    /// Target for `Goto`, `Branch`, or `Switch`.
    Label(IrLabel),

    /// Return from the current function.
    Return {
        value: Option<IrOperand>,
        span: Span,
    },

    // ── Effects ─────────────────────────────────────────────────────────
    /// Push an effect handler onto the handler stack.
    EffectPush {
        effect: IrEffectId,
        handler_fn: IrFnIdx,
        span: Span,
    },

    /// Pop an effect handler from the handler stack.
    EffectPop { effect: IrEffectId, span: Span },

    /// Perform an effect operation (suspend to the nearest handler).
    EffectDo {
        op: IrEffectOpId,
        args: Vec<IrOperand>,
        dst: IrLocal,
        span: Span,
    },

    /// Resume a suspended effect handler with a value.
    EffectResume { value: IrOperand, span: Span },

    /// No operation (placeholder, removed by later passes).
    Nop,
}

#[derive(Debug, Clone)]
pub struct IrSwitchArm {
    /// The constant value to match against.
    pub value: IrConstValue,
    /// The label to jump to on match.
    pub label: IrLabel,
}

/// The right-hand side of an `Assign` instruction.
///
/// All operands are either locals or constants — no nested computations
/// (this is the ANF invariant).
#[derive(Debug, Clone)]
pub enum IrRvalue {
    /// Simple use of an operand (copy/move).
    Use(IrOperand),

    /// Binary operation.
    BinOp {
        op: IrBinOp,
        left: IrOperand,
        right: IrOperand,
    },

    /// Unary operation.
    UnaryOp { op: IrUnaryOp, operand: IrOperand },

    /// Function call.
    Call {
        callee: IrCallee,
        args: Vec<IrOperand>,
        tail: bool,
    },

    // ── Construction ────────────────────────────────────────────────────
    /// Construct a product value (tuple or record).
    MakeProduct {
        ty: IrTypeIdx,
        fields: Vec<IrOperand>,
    },

    /// Construct a tagged variant.
    MakeVariant {
        ty: IrTypeIdx,
        tag: u32,
        payload: Vec<IrOperand>,
    },

    /// Construct an array from elements.
    MakeArray {
        elem_ty: IrTypeIdx,
        elems: Vec<IrOperand>,
    },

    /// Construct a closure (function + captured environment).
    MakeClosure {
        fn_id: IrFnIdx,
        captures: Vec<IrOperand>,
    },

    // ── Access ──────────────────────────────────────────────────────────
    /// Read a field from a product value.
    FieldGet { object: IrOperand, index: u32 },

    /// Read an element from an array.
    IndexGet { array: IrOperand, index: IrOperand },

    /// Extract the tag of a sum value.
    GetTag { value: IrOperand },

    /// Extract a payload field from a sum variant.
    GetPayload {
        value: IrOperand,
        tag: u32,
        field: u32,
    },

    // ── Memory ──────────────────────────────────────────────────────────
    /// Allocate a heap reference.
    AllocRef { ty: IrTypeIdx },

    /// Allocate in an arena.
    AllocArena { ty: IrTypeIdx },

    /// Dereference a pointer/reference.
    Deref { ptr: IrOperand },

    // ── Misc ────────────────────────────────────────────────────────────
    /// Type cast.
    Cast {
        operand: IrOperand,
        from: IrTypeIdx,
        to: IrTypeIdx,
    },

    /// Inline constant value (including unit via `IrConstValue::Unit`).
    Const(IrConstValue),

    // ── Concurrency ─────────────────────────────────────────────────────
    /// Spawn an async task.
    Spawn {
        callee: IrCallee,
        args: Vec<IrOperand>,
    },

    /// Await a task result.
    Await { task: IrOperand },

    /// Create a new channel.
    ChannelMake,

    /// Send a value into a channel.
    ChannelSend { chan: IrOperand, value: IrOperand },

    /// Receive a value from a channel (blocking).
    ChannelRecv { chan: IrOperand },

    /// Call a foreign (FFI) function by index.
    ForeignCall { fn_idx: u32, args: Vec<IrOperand> },
}

/// An operand — either a local variable or a constant.
///
/// This is the leaf of every rvalue; the ANF invariant guarantees no nesting.
#[derive(Debug, Clone)]
pub enum IrOperand {
    /// A local variable.
    Local(IrLocal),
    /// An inline constant.
    Const(IrConstValue),
}

/// An assignable place (l-value).
#[derive(Debug, Clone)]
pub enum IrPlace {
    /// A local variable.
    Local(IrLocal),
    /// A field of a product value.
    Field { base: IrLocal, index: u32 },
    /// An element of an array.
    Index { base: IrLocal, index: IrOperand },
    /// Through a pointer/reference.
    Deref { ptr: IrLocal },
}

/// The target of a function call.
#[derive(Debug, Clone)]
pub enum IrCallee {
    /// A statically known function.
    Direct(IrFnIdx),
    /// An indirect call through a closure or function value.
    Indirect(IrLocal),
    /// A typeclass instance method call.
    ///
    /// `instance_fn` is the resolved function for codegen; `class_def` and
    /// `method` are retained for diagnostics and the pretty-printer.
    Instance {
        class_def: DefId,
        method: Symbol,
        instance_fn: IrFnIdx,
    },
}

/// Type-specialized binary operators.
///
/// Logical `&&`/`||` are desugared to `Branch` chains during lowering,
/// so only bitwise `And`/`Or`/`Xor` appear here.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum IrBinOp {
    // ── Signed integer arithmetic ───────────────────────────────────────
    IAdd,
    ISub,
    IMul,
    IDiv,
    IRem,
    // ── Unsigned integer arithmetic ─────────────────────────────────────
    UAdd,
    USub,
    UMul,
    UDiv,
    URem,
    // ── Floating-point arithmetic ───────────────────────────────────────
    FAdd,
    FSub,
    FMul,
    FDiv,
    FRem,
    // ── Signed integer comparison ───────────────────────────────────────
    IEq,
    INe,
    ILt,
    ILe,
    IGt,
    IGe,
    // ── Unsigned integer comparison ─────────────────────────────────────
    ULt,
    ULe,
    UGt,
    UGe,
    // ── Floating-point comparison ───────────────────────────────────────
    FEq,
    FNe,
    FLt,
    FLe,
    FGt,
    FGe,
    // ── Bitwise ─────────────────────────────────────────────────────────
    And,
    Or,
    Xor,
    Shl,
    Shr,
    /// Unsigned (logical) shift right.
    ShrUn,
}

/// Unary operators (type-specialized where needed).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum IrUnaryOp {
    /// Signed integer negation.
    INeg,
    /// Floating-point negation.
    FNeg,
    /// Bitwise NOT.
    Not,
}
