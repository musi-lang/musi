use std::path::PathBuf;

use crate::types::OptEmitKind;

pub fn watch(files: &[PathBuf], _emit: OptEmitKind) {
    if files.is_empty() {
        super::util::error("no file(s) specified to watch");
        return;
    }
    eprintln!("note: watch mode not yet implemented");
}
