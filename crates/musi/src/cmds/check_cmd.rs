use std::path::PathBuf;

use clap::Args;
use musi_shared::{DiagnosticBag, Interner, SourceDb};

use crate::compiler::{
    self, PRELUDE_FILENAME, PRELUDE_SRC, collect_and_parse_deps, parse_file, print_diags_and_exit,
    read_file,
};

#[derive(Args)]
pub struct CheckArgs {
    /// Musi source file to type-check
    pub file: PathBuf,
}

pub fn run(args: &CheckArgs) {
    let file_path_str = args.file.to_string_lossy();
    let file_path = file_path_str.as_ref();
    let src = read_file(file_path);

    let mut interner = Interner::new();
    let mut source_db = SourceDb::new();
    let mut diags = DiagnosticBag::new();

    // Parse prelude.
    let prelude_file_id = source_db.add(PRELUDE_FILENAME, PRELUDE_SRC);
    let prelude_lexed = musi_lex::lex(PRELUDE_SRC, prelude_file_id, &mut interner, &mut diags);
    let prelude_module = musi_parse::parse(
        &prelude_lexed.tokens,
        prelude_file_id,
        &mut diags,
        &interner,
    );

    if diags.has_errors() {
        print_diags_and_exit(&diags, &source_db);
    }

    // Parse user file.
    let (file_id, user_module) =
        parse_file(file_path, &src, &mut interner, &mut source_db, &mut diags);

    if diags.has_errors() {
        print_diags_and_exit(&diags, &source_db);
    }

    // Parse deps (best-effort: tolerate missing files).
    let dep_modules = collect_and_parse_deps(
        &user_module,
        args.file.as_path(),
        &mut interner,
        &mut source_db,
        &mut diags,
        true,
    );

    // Run sema.
    if !compiler::run_sema(
        &prelude_module,
        prelude_file_id,
        &dep_modules,
        &user_module,
        file_id,
        &interner,
        &mut diags,
    ) {
        print_diags_and_exit(&diags, &source_db);
    }

    println!("\u{2713} no type errors");
}
