use std::sync::Arc;

use musi_vm::{Value, Vm};

use super::Runtime;
use super::compile::SessionLoader;
use crate::error::{RuntimeError, RuntimeErrorKind, RuntimeResult};

impl Runtime {
    /// Loads one root module into one fresh VM runtime and initializes it.
    ///
    /// # Errors
    ///
    /// Returns [`crate::RuntimeError`] if source or program lookup fails, compilation fails, or VM initialization fails.
    pub fn load_root(&mut self, spec: &str) -> RuntimeResult {
        let program = self.compile_registered_program(spec)?;
        let loader = SessionLoader::new(Arc::clone(&self.store));
        let host = self.host.clone();
        let mut vm = Vm::new(program, loader, host, self.options.vm.clone());
        vm.initialize()?;
        self.root_spec = Some(spec.into());
        self.vm = Some(vm);
        Ok(())
    }

    /// Looks up one root export from loaded runtime state.
    ///
    /// # Errors
    ///
    /// Returns [`crate::RuntimeError`] if root runtime state is missing or export lookup fails.
    pub fn lookup_export(&mut self, name: &str) -> RuntimeResult<Value> {
        Ok(self.vm_mut()?.lookup_export(name)?)
    }

    /// Calls one root export from loaded runtime state.
    ///
    /// # Errors
    ///
    /// Returns [`crate::RuntimeError`] if root runtime state is missing or export call fails.
    pub fn call_export(&mut self, name: &str, args: &[Value]) -> RuntimeResult<Value> {
        Ok(self.vm_mut()?.call_export(name, args)?)
    }

    /// Loads one runtime module through registered source or program state.
    ///
    /// # Errors
    ///
    /// Returns [`crate::RuntimeError`] if root runtime state is missing or module loading fails.
    pub fn load_module(&mut self, spec: &str) -> RuntimeResult<Value> {
        Ok(self.vm_mut()?.load_module(spec)?)
    }

    /// Calls one export from one loaded module handle.
    ///
    /// # Errors
    ///
    /// Returns [`crate::RuntimeError`] if root runtime state is missing or module export call fails.
    pub fn call_module_export(
        &mut self,
        module: &Value,
        name: &str,
        args: &[Value],
    ) -> RuntimeResult<Value> {
        Ok(self.vm_mut()?.call_module_export(module, name, args)?)
    }

    /// Calls one runtime value from loaded root runtime state.
    ///
    /// # Errors
    ///
    /// Returns [`crate::RuntimeError`] if root runtime state is missing or the value call fails.
    pub fn call_value(&mut self, value: Value, args: &[Value]) -> RuntimeResult<Value> {
        Ok(self.vm_mut()?.call_value(value, args)?)
    }

    #[must_use]
    pub fn root_spec(&self) -> Option<&str> {
        self.root_spec.as_deref()
    }

    pub(crate) fn vm_mut(&mut self) -> RuntimeResult<&mut Vm> {
        self.vm
            .as_mut()
            .ok_or_else(|| RuntimeError::new(RuntimeErrorKind::RootModuleRequired))
    }
}
