//! IR type representation.
//!
//! IR types are monomorphized (or existentialized) — no unification variables
//! remain after lowering from semantic types.  Types are arena-allocated via
//! `Arena<IrType>` and addressed by `IrTypeIdx`.

use music_sema::DefId;
use music_shared::{Idx, Symbol};

use crate::func::IrFnIdx;

/// Index into the IR type arena.
pub type IrTypeIdx = Idx<IrType>;

/// A lowered, fully resolved type in the IR.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum IrType {
    /// The unit type (zero-sized).
    Unit,
    /// Boolean.
    Bool,
    /// Signed integers by width.
    Int8,
    Int16,
    Int32,
    Int64,
    /// Unsigned integers by width.
    UInt8,
    UInt16,
    UInt32,
    UInt64,
    /// Floating-point by width.
    Float32,
    Float64,
    /// A Unicode scalar value.
    Rune,
    /// The top type — unifies with everything.
    Any,
    /// A product type (tuple or struct — fields accessed by index).
    Product {
        fields: Vec<Idx<Self>>,
    },
    /// A tagged sum type (ADT / choice).
    Sum {
        variants: Vec<IrSumVariant>,
    },
    /// A homogeneous array.
    Array {
        elem: Idx<Self>,
    },
    /// A function type.
    Fn {
        params: Vec<Idx<Self>>,
        ret: Idx<Self>,
        effect_mask: IrEffectMask,
    },
    /// A heap reference.
    Ref {
        inner: Idx<Self>,
    },
    /// A raw pointer (lowered from `inout`).
    Ptr {
        inner: Idx<Self>,
    },
    /// A closure: a function bundled with its captured environment.
    Closure {
        fn_ty: Idx<Self>,
        env_ty: Idx<Self>,
    },
    /// An opaque foreign type — zero-sized, only used behind `Ptr`.
    Opaque {
        name: Symbol,
    },
    /// Opaque type parameter — used in existentialized generic functions.
    /// Size/alignment/operations accessed via the associated witness table.
    TypeParam {
        index: u32,
    },
    /// Witness table (typeclass dictionary) — passed as a runtime value
    /// when a generic function is existentialized rather than monomorphized.
    WitnessTable {
        class_def: DefId,
    },
}

/// A variant in an IR sum type.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct IrSumVariant {
    /// Name of the variant (for diagnostics and debugging).
    pub name: Symbol,
    /// Payload field types.
    pub fields: Vec<IrTypeIdx>,
}

/// Bit-mask encoding which effects a function may perform.
///
/// Each bit position corresponds to a well-known effect (IO = bit 0,
/// Async = bit 1, State = bit 2, etc.).  Pure functions have mask `0`.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default)]
pub struct IrEffectMask(pub u16);

impl IrEffectMask {
    /// Pure: no effects.
    pub const PURE: Self = Self(0);

    /// Returns `true` if the mask represents a pure (effect-free) function.
    #[must_use]
    pub const fn is_pure(self) -> bool {
        self.0 == 0
    }

    /// Returns the union of two effect masks.
    #[must_use]
    pub const fn union(self, other: Self) -> Self {
        Self(self.0 | other.0)
    }

    /// Returns `true` if `self` contains all effects in `other`.
    #[must_use]
    pub const fn contains(self, other: Self) -> bool {
        (self.0 & other.0) == other.0
    }
}

/// How a generic function is compiled in the IR.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum GenericStrategy {
    /// Specialized copy per concrete type argument set.
    Mono,
    /// Single copy; type params become `TypeParam`, constraints become
    /// `WitnessTable` params.
    Existential,
}

/// A witness table entry — one per typeclass method.
#[derive(Debug, Clone)]
pub struct WitnessEntry {
    /// The method name in the typeclass.
    pub method: Symbol,
    /// The IR function implementing this method for the concrete type.
    pub fn_id: IrFnIdx,
}

/// Runtime type metadata for existentialized generics.
#[derive(Debug, Clone)]
pub struct TypeMetadata {
    /// Size in bytes.
    pub size: u32,
    /// Alignment in bytes.
    pub align: u32,
    /// Witness table entries for typeclass constraints.
    pub witnesses: Vec<WitnessEntry>,
}
