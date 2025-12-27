use std::path::{Path, PathBuf};

use crate::types::OptEmitKind;

pub fn compile(files: &[PathBuf], _out_dir: &Path, emit: OptEmitKind) {
    for path in files {
        if let Err(e) = super::util::check_file(path, emit) {
            super::util::error(&e);
            continue;
        }
        eprintln!("note: bytecode emission not yet implemented");
    }
}
