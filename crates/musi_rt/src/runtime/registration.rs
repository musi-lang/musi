use musi_vm::Program;

use super::Runtime;
use crate::error::RuntimeResult;

impl Runtime {
    /// Registers source text for one runtime module spec.
    ///
    /// # Errors
    ///
    /// Returns [`crate::RuntimeError`] if runtime state cannot accept updated module text.
    pub fn register_module_text(
        &mut self,
        spec: impl Into<Box<str>>,
        text: impl Into<String>,
    ) -> RuntimeResult {
        let spec = spec.into();
        let _ = self
            .store
            .borrow_mut()
            .module_texts
            .insert(spec, text.into());
        self.invalidate_loaded_state();
        Ok(())
    }

    /// Registers one precompiled runtime program for one runtime module spec.
    ///
    /// # Errors
    ///
    /// Returns [`crate::RuntimeError`] if runtime state cannot accept updated program bytes.
    pub fn register_program(
        &mut self,
        spec: impl Into<Box<str>>,
        program: Program,
    ) -> RuntimeResult {
        let spec = spec.into();
        let _ = self.store.borrow_mut().programs.insert(spec, program);
        self.invalidate_loaded_state();
        Ok(())
    }

    pub(super) fn invalidate_loaded_state(&mut self) {
        self.store.borrow_mut().programs.clear();
        self.vm = None;
        self.root_spec = None;
    }
}
