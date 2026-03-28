use crate::errors::{VmError, VmResult};
use crate::ffi::FfiRuntime;
use crate::value::Value;
use crate::vm::Vm;

pub trait RuntimeHost {
    fn handle_effect(
        &mut self,
        _vm: &Vm,
        _effect_id: u16,
        _op_id: u16,
        _payload: Value,
    ) -> VmResult<Option<Value>> {
        Ok(None)
    }

    fn load_library(&mut self, name: &str) -> VmResult {
        Err(VmError::FfiLibraryNotFound(name.into()))
    }

    fn resolve_symbol(&mut self, lib_name: &str, symbol: &str) -> Result<*const (), VmError> {
        let _ = lib_name;
        Err(VmError::FfiSymbolNotFound(symbol.into()))
    }
}

pub struct NativeHost {
    ffi_runtime: FfiRuntime,
}

impl Default for NativeHost {
    fn default() -> Self {
        Self::new()
    }
}

impl NativeHost {
    #[must_use]
    pub fn new() -> Self {
        Self {
            ffi_runtime: FfiRuntime::new(),
        }
    }
}

impl RuntimeHost for NativeHost {
    fn load_library(&mut self, name: &str) -> VmResult {
        self.ffi_runtime.load_library(name)
    }

    fn resolve_symbol(&mut self, lib_name: &str, symbol: &str) -> Result<*const (), VmError> {
        self.ffi_runtime.resolve_symbol(lib_name, symbol)
    }
}
