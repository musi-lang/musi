use musi_vm::{HeapCollectionStats, Value, ValueView};

use super::Runtime;
use crate::error::{RuntimeError, RuntimeErrorKind, RuntimeResult};

impl Runtime {
    /// Inspects one runtime value through one stable VM view.
    ///
    /// # Errors
    ///
    /// Returns [`crate::RuntimeError`] if root runtime state is missing.
    pub fn inspect<'a>(&'a self, value: &'a Value) -> RuntimeResult<ValueView<'a>> {
        let vm = self
            .vm
            .as_ref()
            .ok_or_else(|| RuntimeError::new(RuntimeErrorKind::RootModuleRequired))?;
        Ok(vm.inspect(value))
    }

    /// Returns estimated live heap bytes for loaded runtime state.
    ///
    /// # Errors
    ///
    /// Returns [`crate::RuntimeError`] if root runtime state is missing.
    pub fn heap_allocated_bytes(&self) -> RuntimeResult<usize> {
        let vm = self
            .vm
            .as_ref()
            .ok_or_else(|| RuntimeError::new(RuntimeErrorKind::RootModuleRequired))?;
        Ok(vm.heap_allocated_bytes())
    }

    /// Returns executed VM instruction count for loaded runtime state.
    ///
    /// # Errors
    ///
    /// Returns [`crate::RuntimeError`] if root runtime state is missing.
    pub fn executed_instructions(&self) -> RuntimeResult<u64> {
        let vm = self
            .vm
            .as_ref()
            .ok_or_else(|| RuntimeError::new(RuntimeErrorKind::RootModuleRequired))?;
        Ok(vm.executed_instructions())
    }

    /// Runs one explicit collection for loaded runtime state.
    ///
    /// # Errors
    ///
    /// Returns [`crate::RuntimeError`] if root runtime state is missing.
    pub fn collect_garbage(&mut self) -> RuntimeResult<HeapCollectionStats> {
        Ok(self.vm_mut()?.collect_garbage())
    }
}
