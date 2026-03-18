//! Prelude-registered well-known type definitions.
//!
//! Instead of a `PrimTy` enum, all types including `Int`, `Bool`, `String`
//! are `Type::Named` with a `DefId` looked up from this registry.

#[cfg(test)]
mod tests;

use music_shared::{Arena, FileId, Interner, Span};

use crate::def::{DefId, DefKind, DefTable};
use crate::scope::{ScopeId, ScopeTree};
use crate::types::Type;

/// Well-known signed integer types.
#[derive(Debug, Clone)]
pub struct WellKnownInts {
    pub int: DefId,
    pub int8: DefId,
    pub int16: DefId,
    pub int32: DefId,
    pub int64: DefId,
}

/// Well-known natural number (unsigned integer) types.
#[derive(Debug, Clone)]
pub struct WellKnownNats {
    pub nat: DefId,
    pub nat8: DefId,
    pub nat16: DefId,
    pub nat32: DefId,
    pub nat64: DefId,
}

/// Compiler-internal phantom primitive types (not user-visible).
#[derive(Debug, Clone)]
pub struct WellKnownPrimitives {
    pub isize_: DefId,
    pub i8_: DefId,
    pub i16_: DefId,
    pub i32_: DefId,
    pub i64_: DefId,
    pub usize_: DefId,
    pub u8_: DefId,
    pub u16_: DefId,
    pub u32_: DefId,
    pub u64_: DefId,
    pub fsize_: DefId,
    pub f32_: DefId,
    pub f64_: DefId,
    pub bool_: DefId,
    pub char_: DefId,
    pub str_: DefId,
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

/// Well-known effect definitions.
#[derive(Debug, Clone)]
pub struct WellKnownEffects {
    pub io: DefId,
    pub async_eff: DefId,
    pub state: DefId,
    pub throw: DefId,
}

/// `DefId` handles for all well-known (prelude) types.
///
/// Populated once by [`init_well_known`] at the start of analysis.
#[derive(Debug, Clone)]
pub struct WellKnown {
    pub ints: WellKnownInts,
    pub nats: WellKnownNats,
    pub floats: WellKnownFloats,
    pub ffi: WellKnownFfi,
    pub effects: WellKnownEffects,
    pub primitives: WellKnownPrimitives,
    pub float: DefId,
    pub string: DefId,
    pub rune: DefId,
    pub bool: DefId,
    pub unit: DefId,
    pub option: DefId,
    pub any: DefId,
    pub never: DefId,
    pub unknown: DefId,
    pub type_: DefId,
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
    let compiler_file = FileId(0);
    let mut register = |name: &str, kind: DefKind| -> DefId {
        let sym = interner.intern(name);
        let id = defs.alloc(sym, kind, Span::DUMMY, compiler_file);
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
    let nats = WellKnownNats {
        nat: register("Nat", DefKind::Type),
        nat8: register("Nat8", DefKind::Type),
        nat16: register("Nat16", DefKind::Type),
        nat32: register("Nat32", DefKind::Type),
        nat64: register("Nat64", DefKind::Type),
    };
    let primitives = WellKnownPrimitives {
        isize_: register("__isize", DefKind::Primitive),
        i8_: register("__i8", DefKind::Primitive),
        i16_: register("__i16", DefKind::Primitive),
        i32_: register("__i32", DefKind::Primitive),
        i64_: register("__i64", DefKind::Primitive),
        usize_: register("__usize", DefKind::Primitive),
        u8_: register("__u8", DefKind::Primitive),
        u16_: register("__u16", DefKind::Primitive),
        u32_: register("__u32", DefKind::Primitive),
        u64_: register("__u64", DefKind::Primitive),
        fsize_: register("__fsize", DefKind::Primitive),
        f32_: register("__f32", DefKind::Primitive),
        f64_: register("__f64", DefKind::Primitive),
        bool_: register("__bool", DefKind::Primitive),
        char_: register("__char", DefKind::Primitive),
        str_: register("__str", DefKind::Primitive),
    };
    let floats = WellKnownFloats {
        float32: register("Float32", DefKind::Type),
        float64: register("Float64", DefKind::Type),
    };
    let ffi = WellKnownFfi {
        c_string: register("CString", DefKind::Type),
        ptr: register("Ptr", DefKind::Type),
    };

    let effects = WellKnownEffects {
        io: register("IO", DefKind::Effect),
        async_eff: register("Async", DefKind::Effect),
        state: register("State", DefKind::Effect),
        throw: register("Throw", DefKind::Effect),
    };

    let option = register("Option", DefKind::Type);
    let float = register("Float", DefKind::Type);
    let string = register("String", DefKind::Type);
    let rune = register("Rune", DefKind::Type);
    let bool = register("Bool", DefKind::Type);
    let unit = register("Unit", DefKind::Type);
    let any = register("Any", DefKind::Type);
    let never = register("Never", DefKind::Type);
    let unknown = register("Unknown", DefKind::Type);
    let type_ = register("Type", DefKind::Type);

    WellKnown {
        ints,
        nats,
        floats,
        ffi,
        effects,
        primitives,
        float,
        string,
        rune,
        bool,
        unit,
        option,
        any,
        never,
        unknown,
        type_,
    }
}

pub fn assign_well_known_types(defs: &mut DefTable, wk: &WellKnown, types: &mut Arena<Type>) {
    let u0 = types.alloc(Type::Universe { level: 0 });
    let u1 = types.alloc(Type::Universe { level: 1 });

    // Type : U₁ (the type of types lives one universe up)
    defs.get_mut(wk.type_).ty_info.ty = Some(u1);

    // All concrete types inhabit U₀
    let concrete_types = [
        wk.unknown,
        wk.any,
        wk.never,
        wk.unit,
        wk.bool,
        wk.string,
        wk.rune,
        wk.float,
        wk.option,
        wk.ints.int,
        wk.ints.int8,
        wk.ints.int16,
        wk.ints.int32,
        wk.ints.int64,
        wk.nats.nat,
        wk.nats.nat8,
        wk.nats.nat16,
        wk.nats.nat32,
        wk.nats.nat64,
        wk.floats.float32,
        wk.floats.float64,
    ];
    for &def_id in &concrete_types {
        defs.get_mut(def_id).ty_info.ty = Some(u0);
    }
}
