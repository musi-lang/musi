use std::sync::Arc;

use musi_native::NativeTestReport;
use musi_vm::Vm;

use super::Runtime;
use super::compile::SessionLoader;
use crate::error::RuntimeResult;
use crate::output::RuntimeOutput;

impl Runtime {
    /// Runs one registered test module and returns structured match results.
    ///
    /// # Errors
    ///
    /// Returns [`crate::RuntimeError`] if compilation, intrinsic test-event collection, or test-body execution fails.
    pub fn run_test_module(&mut self, spec: &str) -> RuntimeResult<NativeTestReport> {
        self.run_test_export(spec, "test")
    }

    /// Runs one registered test export and returns structured match results.
    ///
    /// # Errors
    ///
    /// Returns [`crate::RuntimeError`] if compilation, intrinsic test-event collection, or test-body execution fails.
    pub fn run_test_export(
        &mut self,
        spec: &str,
        export_name: &str,
    ) -> RuntimeResult<NativeTestReport> {
        let program = self.compile_registered_program(spec)?;
        let loader = SessionLoader::new(Arc::clone(&self.store));
        let host = self.host.clone();
        let mut vm = Vm::new(program, loader, host, self.options.vm.clone());
        vm.initialize()?;
        self.host.begin_test_session();
        self.clear_output();
        let export_result = vm.call_export(export_name, &[]);
        let output = self.take_output();
        let report = self
            .host
            .finish_test_session(spec)
            .with_output(output.stdout, output.stderr);
        let _ = export_result?;
        Ok(report)
    }

    fn clear_output(&self) {
        if let Ok(mut output) = self.output.lock() {
            output.clear();
        }
    }

    fn take_output(&self) -> RuntimeOutput {
        self.output
            .lock()
            .map_or_else(|_| RuntimeOutput::default(), |mut output| output.take())
    }
}
