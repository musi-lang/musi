//! Extrinsic function interface: dlopen/dlsym dispatch for `extrin fn` declarations.

#![allow(unsafe_code)]

use std::collections::HashMap;
use std::mem::transmute;

use musi_codegen::Module;

use crate::error::VmError;
use crate::value::Value;

/// Cached FFI state: loaded libraries and resolved function pointers.
pub struct FfiState {
    /// Loaded shared libraries keyed by name. `None` key = default namespace.
    libs: HashMap<Option<Box<str>>, libloading::Library>,
    /// Resolved function pointers keyed by (`fn_idx`).
    symbols: HashMap<u16, FfiSymbol>,
}

struct FfiSymbol {
    /// Raw function pointer obtained from dlsym.
    ptr: *const (),
    /// Number of parameters (used to pick calling convention shim).
    param_count: u8,
}

impl Default for FfiState {
    fn default() -> Self {
        Self::new()
    }
}

impl FfiState {
    #[must_use]
    pub fn new() -> Self {
        Self {
            libs: HashMap::new(),
            symbols: HashMap::new(),
        }
    }

    /// Resolves the function pointer for an extrin function, loading the library if needed.
    fn resolve(&mut self, fn_idx: u16, module: &Module) -> Result<&FfiSymbol, VmError> {
        if self.symbols.contains_key(&fn_idx) {
            return self
                .symbols
                .get(&fn_idx)
                .ok_or_else(|| VmError::FfiFailed("symbol vanished".into()));
        }

        let func = module
            .function_table
            .get(usize::from(fn_idx))
            .ok_or(VmError::FunctionOutOfBounds(fn_idx))?;
        let sym = module
            .symbol_table
            .get(usize::from(func.symbol_idx))
            .ok_or(VmError::SymbolOutOfBounds(func.symbol_idx))?;

        let c_name: &str = sym.link_name.as_deref().unwrap_or(&sym.name);
        let lib_key = sym.link_lib.clone();

        // Load library if not cached
        if !self.libs.contains_key(&lib_key) {
            let lib = load_library(lib_key.as_deref())?;
            let _prev = self.libs.insert(lib_key.clone(), lib);
        }

        let lib = self
            .libs
            .get(&lib_key)
            .ok_or_else(|| VmError::FfiFailed("library not loaded".into()))?;

        // SAFETY: lib.get resolves the named symbol from a loaded shared library.
        let sym_result: Result<libloading::Symbol<'_, *const ()>, _> =
            unsafe { lib.get(c_name.as_bytes()) };
        let sym_ref =
            sym_result.map_err(|e| VmError::FfiFailed(format!("dlsym '{c_name}': {e}").into()))?;
        // Symbol<*const ()> wraps a raw fn pointer; Deref copies the pointer value (safe).
        let ptr: *const () = *sym_ref;

        let _prev = self.symbols.insert(
            fn_idx,
            FfiSymbol {
                ptr,
                param_count: func.param_count,
            },
        );

        self.symbols
            .get(&fn_idx)
            .ok_or_else(|| VmError::FfiFailed("symbol vanished".into()))
    }

    /// Calls an extrin function with the given arguments, returning its result as a `Value`.
    ///
    /// For V1, supports these signatures based on parameter count and types:
    /// - `(f64) -> f64` (e.g. sqrt, floor, ceil)
    /// - `(f64, f64) -> f64` (e.g. pow)
    /// - `(i64) -> i64`
    /// - `(i64, i64) -> i64`
    /// - `() -> f64`
    /// - `() -> i64`
    ///
    /// # Errors
    /// Returns `VmError` if the symbol cannot be resolved or the argument types do not match.
    pub fn call(&mut self, fn_idx: u16, args: &[Value], module: &Module) -> Result<Value, VmError> {
        let sym = self.resolve(fn_idx, module)?;
        let ptr = sym.ptr;
        let param_count = sym.param_count;

        match (param_count, args) {
            // () -> f64
            (0, []) => {
                // SAFETY: extrin fn declared with matching signature
                let f: unsafe extern "C" fn() -> f64 = unsafe { transmute(ptr) };
                // SAFETY: calling the resolved fn with the declared signature
                let result = unsafe { f() };
                Ok(Value::Float(result))
            }
            // (f64) -> f64
            (1, [Value::Float(a)]) => {
                // SAFETY: extrin fn declared with matching signature
                let f: unsafe extern "C" fn(f64) -> f64 = unsafe { transmute(ptr) };
                // SAFETY: calling the resolved fn with the declared signature
                let result = unsafe { f(*a) };
                Ok(Value::Float(result))
            }
            // (i64) -> i64
            (1, [Value::Int(a)]) => {
                // SAFETY: extrin fn declared with matching signature
                let f: unsafe extern "C" fn(i64) -> i64 = unsafe { transmute(ptr) };
                // SAFETY: calling the resolved fn with the declared signature
                let result = unsafe { f(*a) };
                Ok(Value::Int(result))
            }
            // (f64, f64) -> f64
            (2, [Value::Float(a), Value::Float(b)]) => {
                // SAFETY: extrin fn declared with matching signature
                let f: unsafe extern "C" fn(f64, f64) -> f64 = unsafe { transmute(ptr) };
                // SAFETY: calling the resolved fn with the declared signature
                let result = unsafe { f(*a, *b) };
                Ok(Value::Float(result))
            }
            // (i64, i64) -> i64
            (2, [Value::Int(a), Value::Int(b)]) => {
                // SAFETY: extrin fn declared with matching signature
                let f: unsafe extern "C" fn(i64, i64) -> i64 = unsafe { transmute(ptr) };
                // SAFETY: calling the resolved fn with the declared signature
                let result = unsafe { f(*a, *b) };
                Ok(Value::Int(result))
            }
            _ => Err(VmError::FfiFailed(
                format!(
                    "unsupported FFI signature: {} params, arg types {:?}",
                    param_count,
                    args.iter().map(type_name_of).collect::<Vec<_>>()
                )
                .into(),
            )),
        }
    }
}

const fn type_name_of(v: &Value) -> &'static str {
    match v {
        Value::Int(_) => "Int",
        Value::Float(_) => "Float",
        Value::String(_) => "String",
        Value::Unit => "Unit",
        Value::Function(_) => "Function",
        Value::Object { .. } => "Object",
        Value::Array(_) => "Array",
        Value::Map(_) => "Map",
    }
}

/// Loads a shared library by name, or the default namespace if `None`.
fn load_library(name: Option<&str>) -> Result<libloading::Library, VmError> {
    match name {
        Some(lib_name) => {
            let candidates = library_candidates(lib_name);
            for candidate in &candidates {
                // SAFETY: Loading a shared library is inherently unsafe.
                // We trust the user's #[link] attribute specifies a legitimate library.
                if let Ok(lib) = unsafe { libloading::Library::new(candidate) } {
                    return Ok(lib);
                }
            }
            Err(VmError::FfiFailed(
                format!("could not load library '{lib_name}' (tried: {candidates:?})").into(),
            ))
        }
        None => {
            // Load the default namespace (contains libc/libm on macOS, libc on Linux)
            // On macOS, pass null to dlopen to get the default handle.
            // libloading doesn't directly support null, so we load libc/libSystem explicitly.
            #[cfg(target_os = "macos")]
            {
                // SAFETY: libSystem.B.dylib is always available on macOS
                unsafe { libloading::Library::new("libSystem.B.dylib") }
                    .map_err(|e| VmError::FfiFailed(format!("default namespace: {e}").into()))
            }
            #[cfg(target_os = "linux")]
            {
                // SAFETY: libc.so.6 is always available on Linux; also try libm
                unsafe { libloading::Library::new("libm.so.6") }
                    .or_else(|_| unsafe { libloading::Library::new("libm.so") })
                    .map_err(|e| VmError::FfiFailed(format!("default namespace: {}", e).into()))
            }
            #[cfg(not(any(target_os = "macos", target_os = "linux")))]
            {
                Err(VmError::FfiFailed(
                    "FFI not supported on this platform".into(),
                ))
            }
        }
    }
}

/// Returns platform-specific candidate paths for a library name.
fn library_candidates(name: &str) -> Vec<String> {
    let mut candidates = Vec::new();

    // If the name already has a path separator or extension, try it as-is first
    if name.contains('/') || name.contains('.') {
        candidates.push(name.to_owned());
    }

    #[cfg(target_os = "macos")]
    {
        candidates.push(format!("lib{name}.dylib"));
        candidates.push(format!("{name}.dylib"));
        candidates.push(format!("lib{name}.so"));
    }

    #[cfg(target_os = "linux")]
    {
        candidates.push(format!("lib{name}.so"));
        candidates.push(format!("{name}.so"));
        candidates.push(format!("lib{name}.so.6"));
    }

    #[cfg(not(any(target_os = "macos", target_os = "linux")))]
    {
        candidates.push(name.to_owned());
    }

    candidates
}
