//! Prelude-registered well-known type definitions.
//!
//! Instead of a `PrimTy` enum, all types including `Int`, `Bool`, `String`
//! are `Type::Named` with a `DefId` looked up from this registry.

#[cfg(test)]
mod tests;

use music_shared::{Arena, Interner, Span};

use crate::def::{DefId, DefKind, DefTable};
use crate::scope::{ScopeId, ScopeTree};
use crate::types::{EffectEntry, EffectRow, Type};

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

/// Well-known `musi:core` builtin functions.
#[derive(Debug, Clone)]
pub struct WellKnownCore {
    pub int_abs: DefId,
    pub int_min: DefId,
    pub int_max: DefId,
    pub int_clamp: DefId,
    pub int_pow: DefId,
    pub str_len: DefId,
    pub str_contains: DefId,
    pub str_starts_with: DefId,
    pub str_ends_with: DefId,
    pub arr_len: DefId,
    pub arr_push: DefId,
    pub arr_pop: DefId,
    pub arr_reverse: DefId,
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
    pub core: WellKnownCore,
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

    let core = WellKnownCore {
        int_abs: register("int_abs", DefKind::ForeignFn),
        int_min: register("int_min", DefKind::ForeignFn),
        int_max: register("int_max", DefKind::ForeignFn),
        int_clamp: register("int_clamp", DefKind::ForeignFn),
        int_pow: register("int_pow", DefKind::ForeignFn),
        str_len: register("str_len", DefKind::ForeignFn),
        str_contains: register("str_contains", DefKind::ForeignFn),
        str_starts_with: register("str_starts_with", DefKind::ForeignFn),
        str_ends_with: register("str_ends_with", DefKind::ForeignFn),
        arr_len: register("arr_len", DefKind::ForeignFn),
        arr_push: register("arr_push", DefKind::ForeignFn),
        arr_pop: register("arr_pop", DefKind::ForeignFn),
        arr_reverse: register("arr_reverse", DefKind::ForeignFn),
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
        core,
        string,
        rune,
        bool,
        unit,
        any,
        never,
        option,
    }
}

/// Assigns proper `Type::Fn` signatures to well-known prelude functions.
///
/// Must be called after `init_well_known` and after the type arena is available.
/// This ensures that calls to `writeln`, `write`, etc. get real types instead
/// of fresh unification variables.
pub fn assign_well_known_types(defs: &mut DefTable, wk: &WellKnown, types: &mut Arena<Type>) {
    let string_ty = types.alloc(Type::Named {
        def: wk.string,
        args: vec![],
    });
    let bool_ty = types.alloc(Type::Named {
        def: wk.bool,
        args: vec![],
    });
    let unit_ty = types.alloc(Type::Named {
        def: wk.unit,
        args: vec![],
    });
    let any_ty = types.alloc(Type::Named {
        def: wk.any,
        args: vec![],
    });
    let ordering_ty = types.alloc(Type::Named {
        def: wk.containers.ordering,
        args: vec![],
    });

    let io_effect = EffectRow {
        effects: vec![EffectEntry {
            def: wk.effects.io,
            args: vec![],
        }],
        row_var: None,
    };

    assign_fn_type(
        defs,
        types,
        wk.fns.write,
        &[string_ty],
        unit_ty,
        Some(io_effect.clone()),
    );
    assign_fn_type(
        defs,
        types,
        wk.fns.writeln,
        &[string_ty],
        unit_ty,
        Some(io_effect),
    );

    assign_fn_type(defs, types, wk.fns.show, &[any_ty], string_ty, None);
    assign_fn_type(
        defs,
        types,
        wk.fns.compare,
        &[any_ty, any_ty],
        ordering_ty,
        None,
    );
    assign_fn_type(defs, types, wk.fns.is_some, &[any_ty], bool_ty, None);
    assign_fn_type(defs, types, wk.fns.is_none, &[any_ty], bool_ty, None);

    // Core builtins
    let int_ty = types.alloc(Type::Named {
        def: wk.ints.int,
        args: vec![],
    });

    // int_abs: Int -> Int
    assign_fn_type(defs, types, wk.core.int_abs, &[int_ty], int_ty, None);
    // int_min: Int -> Int -> Int
    assign_fn_type(defs, types, wk.core.int_min, &[int_ty, int_ty], int_ty, None);
    // int_max: Int -> Int -> Int
    assign_fn_type(defs, types, wk.core.int_max, &[int_ty, int_ty], int_ty, None);
    // int_clamp: Int -> Int -> Int -> Int
    assign_fn_type(defs, types, wk.core.int_clamp, &[int_ty, int_ty, int_ty], int_ty, None);
    // int_pow: Int -> Int -> Int
    assign_fn_type(defs, types, wk.core.int_pow, &[int_ty, int_ty], int_ty, None);
    // str_len: String -> Int
    assign_fn_type(defs, types, wk.core.str_len, &[string_ty], int_ty, None);
    // str_contains: String -> String -> Bool
    assign_fn_type(defs, types, wk.core.str_contains, &[string_ty, string_ty], bool_ty, None);
    // str_starts_with: String -> String -> Bool
    assign_fn_type(defs, types, wk.core.str_starts_with, &[string_ty, string_ty], bool_ty, None);
    // str_ends_with: String -> String -> Bool
    assign_fn_type(defs, types, wk.core.str_ends_with, &[string_ty, string_ty], bool_ty, None);
    // arr_len: [Any] -> Int (polymorphic, use any_ty)
    assign_fn_type(defs, types, wk.core.arr_len, &[any_ty], int_ty, None);
    // arr_push: [Any] -> Any -> Unit
    assign_fn_type(defs, types, wk.core.arr_push, &[any_ty, any_ty], unit_ty, None);
    // arr_pop: [Any] -> Any (polymorphic)
    assign_fn_type(defs, types, wk.core.arr_pop, &[any_ty], any_ty, None);
    // arr_reverse: [Any] -> [Any] (polymorphic)
    assign_fn_type(defs, types, wk.core.arr_reverse, &[any_ty], any_ty, None);
}

fn assign_fn_type(
    defs: &mut DefTable,
    types: &mut Arena<Type>,
    def_id: DefId,
    params: &[crate::TypeIdx],
    ret: crate::TypeIdx,
    effect: Option<EffectRow>,
) {
    let fn_ty = types.alloc(Type::Fn {
        params: params.to_vec(),
        ret,
        effects: effect.unwrap_or(EffectRow::PURE),
    });
    defs.get_mut(def_id).ty_info.ty = Some(fn_ty);
}
