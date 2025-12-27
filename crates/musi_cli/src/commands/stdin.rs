use std::io::{self, Read};

use musi_basic::interner::Interner;
use musi_basic::source::SourceMap;

use crate::types::OptEmitKind;

pub fn stdin(emit: OptEmitKind) {
    let mut buf = String::new();
    if io::stdin().read_to_string(&mut buf).is_err() {
        super::util::error("failed to read stdin");
        return;
    }

    let mut source_map = SourceMap::new();
    let mut interner = Interner::new();

    let file_id = source_map.add_file("<stdin>".into(), buf);
    let Some(file) = source_map.get(file_id).cloned() else {
        super::util::error("internal: failed to add source file");
        return;
    };

    let _ = super::util::run_phases(&file, &mut interner, &source_map, emit);
}
