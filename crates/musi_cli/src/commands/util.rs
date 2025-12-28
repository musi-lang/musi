use std::fs;
use std::io::{self, Write as _};
use std::path::Path;

use musi_basic::source::{SourceFile, SourceMap};
use musi_basic::{diagnostic, interner::Interner};
use musi_lex::lexer;
use musi_sema::{binder, builtins::Builtins};

use crate::cli::EmitKind;
use crate::types::OptEmitKind;

pub fn check_file(path: &Path, emit: OptEmitKind) -> Result<(), String> {
    let contents =
        fs::read_to_string(path).map_err(|e| format!("cannot read '{}': {e}", path.display()))?;

    let mut source_map = SourceMap::new();
    let mut interner = Interner::new();

    let file_id = source_map.add_file(path.display().to_string(), contents);
    let file = source_map
        .get(file_id)
        .cloned()
        .ok_or_else(|| "internal: failed to add source file".to_owned())?;

    let errs = run_phases(&file, &mut interner, &source_map, emit);

    if errs > 0 {
        Err(format!("{errs} error(s) in {}", path.display()))
    } else {
        Ok(())
    }
}

pub fn run_phases(
    file: &SourceFile,
    interner: &mut Interner,
    source_map: &SourceMap,
    emit: OptEmitKind,
) -> usize {
    let (tokens, mut bag) = lexer::tokenize(file, interner);

    if emit == Some(EmitKind::Tokens) {
        for token in &tokens {
            eprintln!("{}", token.kind.display(interner));
        }
    }

    let parse_result = musi_parse::parse(&tokens);
    bag.merge(parse_result.diagnostics);

    if emit == Some(EmitKind::Ast) {
        eprintln!("{:#?}", parse_result.prog);
    }

    if bag.errors == 0 {
        let builtins = Builtins::from_interner(interner);
        let (_, _, sema_bag) =
            binder::bind(&parse_result.arena, &mut interner, &parse_result.prog, &builtins);
        bag.merge(sema_bag);
    }

    diagnostic::emit_all(&bag, source_map);
    bag.errors
}

pub fn error(msg: &str) {
    let mut stderr = io::stderr();
    writeln!(stderr, "\x1b[1;31merror:\x1b[0m {msg}").unwrap_or(());
}
