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

    // Skip prelude injection when checking the prelude itself.
    let is_prelude = file_path.ends_with("std/prelude.ms") || file_path.ends_with("prelude.ms");

    // Parse prelude (unless we're checking the prelude itself).
    let (prelude_file_id, prelude_module) = if is_prelude {
        // Use a dummy empty module for prelude when checking prelude.ms
        let dummy_id = source_db.add("<prelude-skip>", "");
        let dummy_lexed = musi_lex::lex("", dummy_id, &mut interner, &mut diags);
        let dummy_module = musi_parse::parse(&dummy_lexed.tokens, dummy_id, &mut diags, &interner);
        (dummy_id, dummy_module)
    } else {
        let prelude_file_id = source_db.add(PRELUDE_FILENAME, PRELUDE_SRC);
        let prelude_lexed = musi_lex::lex(PRELUDE_SRC, prelude_file_id, &mut interner, &mut diags);
        let prelude_module = musi_parse::parse(
            &prelude_lexed.tokens,
            prelude_file_id,
            &mut diags,
            &interner,
        );
        (prelude_file_id, prelude_module)
    };

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
