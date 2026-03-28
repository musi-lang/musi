use std::cell::RefCell;
use std::path::Path;
use std::rc::Rc;

use musi::driver::{compile_project, emit_project_diagnostics};
use musi_vm::Vm;
use music_emit::{emit_project, write_seam};

use super::collector::{new_handler, TestCollector};
use super::discovery::{default_suite_name, is_test_file};
use super::model::TestSuite;

pub(super) fn run_test_file(path: &Path) -> Result<(Vm, TestSuite), String> {
    if !is_test_file(path) {
        return Err("expected a *.test.ms file".into());
    }

    let analysis = compile_project(path).map_err(|e| e.to_string())?;
    emit_project_diagnostics(&analysis);
    if analysis.has_errors {
        return Err("compilation failed".into());
    }

    let canonical = path.canonicalize().unwrap_or_else(|_| path.to_path_buf());
    let _module_id = analysis
        .project
        .graph
        .lookup(&canonical)
        .ok_or_else(|| "entry test module missing from project graph".to_owned())?;

    let emitted = emit_project(analysis.project).map_err(|e| e.to_string())?;
    let bytes = write_seam(&emitted.module);
    let program = musi_vm::load(&bytes).map_err(|e| format!("load failed; {e}"))?;

    let effect_id = program
        .effect_id("musi:test", "Test")
        .ok_or_else(|| "missing emitted `musi:test::Test` effect metadata".to_owned())?;
    let emit_op_id = program
        .effect_op_id(effect_id, "emit")
        .ok_or_else(|| "missing emitted `musi:test::Test.emit` operation metadata".to_owned())?;
    let collector = Rc::new(RefCell::new(TestCollector::new(default_suite_name(path))));
    let mut vm = Vm::with_host(
        program,
        Box::new(new_handler(Rc::clone(&collector), effect_id, emit_op_id)),
    );
    vm.initialize().map_err(|e| e.to_string())?;
    if let Err(error) = vm.invoke_export("test", &[]) {
        let collector = collector.borrow();
        return Err(collector
            .error
            .clone()
            .unwrap_or_else(|| format!("failed to invoke exported `test`; {error}")));
    }
    let suite = collector.borrow_mut().finish()?;
    Ok((vm, suite))
}
