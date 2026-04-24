use super::{Program, VmError, VmErrorKind, VmResult};

pub trait VmLoader: Send {
    /// Loads one runtime module by specifier text.
    ///
    /// # Errors
    ///
    /// Returns [`VmError`] if loading rejects one requested specifier.
    fn load_program(&mut self, spec: &str) -> VmResult<Program>;
}

#[derive(Debug, Default)]
pub struct RejectingLoader;

impl VmLoader for RejectingLoader {
    fn load_program(&mut self, spec: &str) -> VmResult<Program> {
        Err(VmError::new(VmErrorKind::ModuleLoadRejected {
            spec: spec.into(),
        }))
    }
}
