use super::{Program, Vm, VmHost, VmLoader, VmOptions};

#[derive(Debug, Default, Clone, Copy)]
pub struct VmRuntime;

impl VmRuntime {
    #[must_use]
    pub const fn new() -> Self {
        Self
    }

    #[must_use]
    pub fn isolate(
        self,
        program: Program,
        loader: impl VmLoader + 'static,
        host: impl VmHost + 'static,
        options: VmOptions,
    ) -> Vm {
        Vm::new(program, loader, host, options)
    }
}
