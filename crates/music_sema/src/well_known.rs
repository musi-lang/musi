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

/// Well-known FFI C-compatible types.
#[derive(Debug, Clone)]
pub struct WellKnownFfi {
    pub c_string: DefId,
    pub ptr: DefId,
}

/// Well-known typeclass definitions.
#[derive(Debug, Clone)]
pub struct WellKnownClasses {
    pub eq: DefId,
    pub ord: DefId,
    pub show: DefId,
    pub add: DefId,
    pub into: DefId,
    pub iterable: DefId,
    pub propagate: DefId,
}

/// Well-known effect definitions.
#[derive(Debug, Clone)]
pub struct WellKnownEffects {
    pub io: DefId,
    pub async_eff: DefId,
    pub state: DefId,
    pub throw: DefId,
}

/// Well-known container/ADT types.
#[derive(Debug, Clone)]
pub struct WellKnownContainers {
    pub result: DefId,
    pub ordering: DefId,
    pub list: DefId,
    pub map: DefId,
    pub set: DefId,
}

/// Well-known prelude functions (typeclass methods and utilities).
#[derive(Debug, Clone)]
pub struct WellKnownFns {
    pub compare: DefId,
    pub show: DefId,
    pub reverse: DefId,
    pub append: DefId,
    pub is_some: DefId,
    pub is_none: DefId,
    pub next: DefId,
    pub write: DefId,
    pub writeln: DefId,
}

/// `DefId` handles for all well-known (prelude) types.
///
/// Populated once by [`init_well_known`] at the start of analysis.
#[derive(Debug, Clone)]
pub struct WellKnown {
    pub ints: WellKnownInts,
    pub uints: WellKnownUInts,
    pub floats: WellKnownFloats,
    pub ffi: WellKnownFfi,
    pub classes: WellKnownClasses,
    pub effects: WellKnownEffects,
    pub containers: WellKnownContainers,
    pub fns: WellKnownFns,
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
    let mut register = |name: &str, kind: DefKind| -> DefId {
        let sym = interner.intern(name);
        let id = defs.alloc(sym, kind, Span::DUMMY);
        let _prev = scopes.define(scope, sym, id);
        id
    };

    let ints = WellKnownInts {
        int: register("Int", DefKind::Type),
        int8: register("Int8", DefKind::Type),
        int16: register("Int16", DefKind::Type),
        int32: register("Int32", DefKind::Type),
        int64: register("Int64", DefKind::Type),
    };
    let uints = WellKnownUInts {
        uint8: register("UInt8", DefKind::Type),
        uint16: register("UInt16", DefKind::Type),
        uint32: register("UInt32", DefKind::Type),
        uint64: register("UInt64", DefKind::Type),
    };
    let floats = WellKnownFloats {
        float32: register("Float32", DefKind::Type),
        float64: register("Float64", DefKind::Type),
    };
    let ffi = WellKnownFfi {
        c_string: register("CString", DefKind::Type),
        ptr: register("Ptr", DefKind::Type),
    };

    let classes = WellKnownClasses {
        eq: register("Eq", DefKind::Class),
        ord: register("Ord", DefKind::Class),
        show: register("Show", DefKind::Class),
        add: register("Add", DefKind::Class),
        into: register("Into", DefKind::Class),
        iterable: register("Iterable", DefKind::Class),
        propagate: register("Propagate", DefKind::Class),
    };

    let effects = WellKnownEffects {
        io: register("IO", DefKind::Effect),
        async_eff: register("Async", DefKind::Effect),
        state: register("State", DefKind::Effect),
        throw: register("Throw", DefKind::Effect),
    };

    let containers = WellKnownContainers {
        result: register("Result", DefKind::Type),
        ordering: register("Ordering", DefKind::Type),
        list: register("List", DefKind::Type),
        map: register("Map", DefKind::Type),
        set: register("Set", DefKind::Type),
    };

    let fns = WellKnownFns {
        compare: register("compare", DefKind::Fn),
        show: register("show", DefKind::Fn),
        reverse: register("reverse", DefKind::Fn),
        append: register("append", DefKind::Fn),
        is_some: register("is_some", DefKind::Fn),
        is_none: register("is_none", DefKind::Fn),
        next: register("next", DefKind::Fn),
        write: register("write", DefKind::Fn),
        writeln: register("writeln", DefKind::Fn),
    };

    let string = register("String", DefKind::Type);
    let rune = register("Rune", DefKind::Type);
    let bool = register("Bool", DefKind::Type);
    let unit = register("Unit", DefKind::Type);
    let any = register("Any", DefKind::Type);
    let never = register("Never", DefKind::Type);
    let option = register("Option", DefKind::Type);

    WellKnown {
        ints,
        uints,
        floats,
        ffi,
        classes,
        effects,
        containers,
        fns,
        string,
        rune,
        bool,
        unit,
        any,
        never,
        option,
    }
}
