use std::path::PathBuf;

use crate::types::OptEmitKind;

pub fn check(files: &[PathBuf], emit: OptEmitKind) {
    for path in files {
        if let Err(e) = super::util::check_file(path, emit) {
            super::util::error(&e);
        }
    }
}
