//! The `musi` command-line driver.
//!
//! Usage: `musi run <file.ms>`

#![allow(clippy::module_name_repetitions)]
#![allow(clippy::exhaustive_structs)]
#![allow(clippy::exhaustive_enums)]

use std::env;
use std::fs;
use std::process;

use musi_codegen::emit;
use musi_lex::lex;
use musi_parse::parse;
use musi_shared::{DiagnosticBag, Interner, SourceDb};
use musi_vm::Vm;

const PRELUDE_SRC: &str = include_str!("../../../std/prelude.ms");
const PRELUDE_FILENAME: &str = "<prelude>";

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 3 || args[1] != "run" {
        eprintln!("Usage: musi run <file.ms>");
        process::exit(2);
    }

    let file_path = &args[2];

    let src = match fs::read_to_string(file_path) {
        Ok(s) => s,
        Err(e) => {
            eprintln!("error: cannot read '{file_path}': {e}");
            process::exit(1);
        }
    };

    let mut interner = Interner::new();
    let mut source_db = SourceDb::new();
    let mut diags = DiagnosticBag::new();

    let user_file_id = source_db.add(file_path.as_str(), src.as_str());
    let user_lexed = lex(&src, user_file_id, &mut interner, &mut diags);
    let user_module = parse(&user_lexed.tokens, user_file_id, &mut diags, &interner);

    if diags.has_errors() {
        for diag in diags.iter() {
            eprintln!("{}", diag.render_simple(&source_db));
        }
        process::exit(1);
    }

    let prelude_file_id = source_db.add(PRELUDE_FILENAME, PRELUDE_SRC);
    let prelude_lexed = lex(PRELUDE_SRC, prelude_file_id, &mut interner, &mut diags);
    let prelude_module = parse(&prelude_lexed.tokens, prelude_file_id, &mut diags, &interner);

    if diags.has_errors() {
        for diag in diags.iter() {
            eprintln!("{}", diag.render_simple(&source_db));
        }
        process::exit(1);
    }

    let module = match emit(&prelude_module, &user_module, &interner) {
        Ok(m) => m,
        Err(e) => {
            eprintln!("codegen error: {e}");
            process::exit(1);
        }
    };

    let main_fn_idx =
        u16::try_from(module.function_table.len() - 1).expect("function table index fits u16");

    let mut vm = Vm::new(module);
    match vm.run(main_fn_idx) {
        Ok(_) => {}
        Err(e) => {
            eprintln!("runtime error: {e}");
            process::exit(1);
        }
    }
}
