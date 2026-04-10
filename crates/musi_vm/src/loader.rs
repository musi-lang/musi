use super::{Program, VmError, VmErrorKind, VmResult};

pub trait VmLoader {
    /// Loads one runtime module by specifier text.
    ///
    /// # Errors
    ///
    /// Returns [`VmError`] if loading rejects one requested specifier.
    fn load_program(&mut self, spec: &str) -> VmResult<Program>;
}

#[derive(Debug, Default)]
pub struct NativeLoader;

impl VmLoader for NativeLoader {
    fn load_program(&mut self, spec: &str) -> VmResult<Program> {
        Err(VmError::new(VmErrorKind::ModuleLoadRejected {
            spec: spec.into(),
        }))
    }
}
