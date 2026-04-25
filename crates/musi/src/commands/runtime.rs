use std::env::var;

use crate::error::{MusiError, MusiResult};
use musi_native::NativeHost;
use musi_project::Project;
use musi_rt::{Runtime, RuntimeOptions, RuntimeOutputMode};
use musi_vm::VmOptions;

pub(super) fn project_runtime_with_output(project: &Project, output: RuntimeOutputMode) -> Runtime {
    project_runtime_with_vm(project, output, VmOptions)
}

pub(super) fn project_runtime_with_vm(
    project: &Project,
    output: RuntimeOutputMode,
    vm: VmOptions,
) -> Runtime {
    let mut options = RuntimeOptions::default();
    options.session.import_map = project.import_map().clone();
    options.vm = vm;
    options.output = output;
    let mut runtime = Runtime::new(NativeHost::new(), options);
    for (key, text) in project.module_texts() {
        runtime
            .register_module_text(key.as_str(), text)
            .expect("project module texts should register into runtime");
    }
    runtime
}

pub(super) fn mvm_options_from_args(args: &[String]) -> MusiResult<VmOptions> {
    let env_options = var("MVM_OPTIONS").ok();
    VmOptions::parse_mvm_options(env_options.as_deref(), args).map_err(|error| {
        MusiError::UnsupportedRunArgs {
            argument: error.message().to_owned(),
        }
    })
}
