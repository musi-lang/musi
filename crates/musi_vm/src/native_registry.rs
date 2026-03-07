//! Runtime-registered native module table.
//!
//! The CLI builds a [`NativeRegistry`] from [`musi_native::REGISTRY`] and
//! passes it into [`crate::vm::Vm::new`].  The VM uses it to dispatch
//! intrinsic calls whose IDs are not handled by the legacy `native::dispatch`
//! match (i.e. every intrinsic added after the initial 24).

use std::collections::HashMap;

use musi_codegen::intrinsics::Intrinsic;

use crate::value::Value;

/// A native function: receives the argument slice, returns a `Value`.
pub type NativeFn = fn(&[Value]) -> Value;

/// One `musi:*` module entry: specifier, compiled-in Musi source, function table.
pub struct NativeModuleEntry {
    pub specifier: &'static str,
    pub source:    &'static str,
    pub functions: &'static [(&'static str, NativeFn)],
}

/// Lookup table built once at startup from a slice of [`NativeModuleEntry`]s.
pub struct NativeRegistry {
    by_id:   HashMap<u16, NativeFn>,
    by_spec: HashMap<&'static str, &'static str>,
}

impl NativeRegistry {
    /// Build a registry from a static slice of module entries.
    #[must_use]
    pub fn new(entries: &[NativeModuleEntry]) -> Self {
        let mut by_id:   HashMap<u16, NativeFn>              = HashMap::new();
        let mut by_spec: HashMap<&'static str, &'static str> = HashMap::new();

        for entry in entries {
            let _ = by_spec.insert(entry.specifier, entry.source);
            for &(name, fn_ptr) in entry.functions {
                if let Some(intr) = Intrinsic::from_name(name) {
                    let _ = by_id.insert(intr.id(), fn_ptr);
                }
            }
        }

        Self { by_id, by_spec }
    }

    /// Empty registry (used in tests / sandboxed contexts).
    #[must_use]
    pub fn empty() -> Self {
        Self { by_id: HashMap::new(), by_spec: HashMap::new() }
    }

    /// Look up a native function by intrinsic ID.
    #[must_use]
    pub fn lookup_id(&self, id: u16) -> Option<NativeFn> {
        self.by_id.get(&id).copied()
    }

    /// Return the embedded Musi source for a `musi:*` specifier.
    #[must_use]
    pub fn source_for(&self, specifier: &str) -> Option<&'static str> {
        self.by_spec.get(specifier).copied()
    }
}
