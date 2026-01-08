use std::io::{self, Read};

use musi_core::{Interner, SourceFile};

use crate::types::OptEmitKind;

pub fn stdin(emit: OptEmitKind) {
    let mut buf = String::new();
    if io::stdin().read_to_string(&mut buf).is_err() {
        super::util::error("failed to read stdin");
        return;
    }

    let mut interner = Interner::new();
    let file = SourceFile::new("<stdin>".into(), buf, 0);

    let _ = super::util::run_phases(&file, &mut interner, emit);
}
