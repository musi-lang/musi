use std::path::PathBuf;
use std::process;

use clap::Args;
use musi_native::REGISTRY;
use musi_vm::{NativeRegistry, Vm};

use crate::compiler;

#[derive(Args)]
pub(crate) struct RunArgs {
    /// Musi source file to compile and execute
    pub(crate) file: PathBuf,
}

pub(crate) fn run(args: RunArgs) {
    let path = args.file.to_string_lossy();
    let module = compiler::compile_file(&path);

    let main_fn_idx =
        u16::try_from(module.function_table.len() - 1).expect("function table fits u16");

    let registry = NativeRegistry::new(REGISTRY);
    let mut vm = Vm::new(module, registry);
    match vm.run(main_fn_idx) {
        Ok(_) => {}
        Err(e) => {
            eprintln!("runtime error: {e}");
            process::exit(1);
        }
    }
}
