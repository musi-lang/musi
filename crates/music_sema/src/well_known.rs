//! Prelude-registered well-known type definitions.
//!
//! Instead of a `PrimTy` enum, all types including `Int`, `Bool`, `String`
//! are `Type::Named` with a `DefId` looked up from this registry.

#[cfg(test)]
mod tests;

use music_shared::{Interner, Span};

use crate::def::{DefId, DefKind, DefTable};
use crate::scope::{ScopeId, ScopeTree};

/// Well-known signed integer types.
#[derive(Debug, Clone)]
pub struct WellKnownInts {
    pub int: DefId,
    pub int8: DefId,
    pub int16: DefId,
    pub int32: DefId,
    pub int64: DefId,
}

/// Well-known unsigned integer types.
#[derive(Debug, Clone)]
pub struct WellKnownUInts {
    pub uint8: DefId,
    pub uint16: DefId,
    pub uint32: DefId,
    pub uint64: DefId,
}

/// Well-known floating-point types.
#[derive(Debug, Clone)]
pub struct WellKnownFloats {
    pub float32: DefId,
    pub float64: DefId,
}

/// `DefId` handles for all well-known (prelude) types.
///
/// Populated once by [`init_well_known`] at the start of analysis.
#[derive(Debug, Clone)]
pub struct WellKnown {
    pub ints: WellKnownInts,
    pub uints: WellKnownUInts,
    pub floats: WellKnownFloats,
    // Text
    pub string: DefId,
    pub rune: DefId,
    // Core
    pub bool: DefId,
    pub unit: DefId,
    // Special
    pub any: DefId,
    pub never: DefId,
    pub option: DefId,
}

/// Registers all well-known types in the def table and scope, returning
/// a [`WellKnown`] handle struct.
///
/// Each type is interned, allocated as a `DefKind::Type` with a dummy span,
/// and defined in the given scope so user code can refer to them.
#[must_use]
pub fn init_well_known(
    interner: &mut Interner,
    defs: &mut DefTable,
    scope: ScopeId,
    scopes: &mut ScopeTree,
) -> WellKnown {
    let mut register = |name: &str| -> DefId {
        let sym = interner.intern(name);
        let id = defs.alloc(sym, DefKind::Type, Span::DUMMY);
        let _prev = scopes.define(scope, sym, id);
        id
    };

    WellKnown {
        ints: WellKnownInts {
            int: register("Int"),
            int8: register("Int8"),
            int16: register("Int16"),
            int32: register("Int32"),
            int64: register("Int64"),
        },
        uints: WellKnownUInts {
            uint8: register("UInt8"),
            uint16: register("UInt16"),
            uint32: register("UInt32"),
            uint64: register("UInt64"),
        },
        floats: WellKnownFloats {
            float32: register("Float32"),
            float64: register("Float64"),
        },
        string: register("String"),
        rune: register("Rune"),
        bool: register("Bool"),
        unit: register("Unit"),
        any: register("Any"),
        never: register("Never"),
        option: register("Option"),
    }
}
