//! Registry of built-in `musi:*` modules provided by the runtime.
//!
//! Built-in modules have no `.ms` source file; their exports are injected
//! directly by the compilation pipeline.

/// Returns `true` if `name` identifies a built-in `musi:` module.
///
/// Built-in modules are intercepted before filesystem resolution so they
/// do not need a corresponding `.ms` file in the standard library root.
pub fn is_builtin_module(name: &str) -> bool {
    matches!(name, "ffi")
}
