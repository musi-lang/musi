use musi_basic::{diagnostic, interner::Interner, source::SourceMap};
use musi_lex::lexer;

use std::fs;
use std::io::{self, Read, Write as _};
use std::path::PathBuf;

use crate::EmitKind;

pub fn check(files: &[PathBuf], emit: Option<EmitKind>) {
    for path in files {
        if let Err(e) = check_file(path, emit) {
            error(&e);
        }
    }
}

pub fn compile(files: &[PathBuf], _out_dir: &PathBuf, emit: Option<EmitKind>) {
    for path in files {
        if let Err(e) = check_file(path, emit) {
            error(&e);
            continue;
        }
        // TODO: emit .mso bytecode to out_dir
        eprintln!("note: bytecode emission not yet implemented");
    }
}

pub fn build(project: &PathBuf, _emit: Option<EmitKind>) {
    let config_path = project.join("mspackage.json");
    if !config_path.exists() {
        error(&format!(
            "no 'mspackage.json' found in {}",
            project.display()
        ));
        return;
    }
    eprintln!("note: project build not yet implemented");
}

pub fn init(path: &PathBuf) {
    if let Err(e) = fs::create_dir_all(path) {
        error(&format!(
            "unable to create directory '{}': {e}",
            path.display()
        ));
        return;
    }

    let config_path = path.join("mspackage.json");
    if config_path.exists() {
        error("'mspackage.json' already exists");
        return;
    }

    let template = r#"{
  "name": "my-package",
  "version": "0.1.0",
  "main": "./src/index.ms",
  "compilerOptions": {
    "target": "MS2025",
    "strict": true,
    "outDir": "./dist"
  }
}
"#;

    match fs::write(&config_path, template) {
        Ok(()) => eprintln!("created {}", config_path.display()),
        Err(e) => error(&format!("unable to create 'mspackage.json': {e}")),
    }
}

pub fn watch(files: &[PathBuf], _emit: Option<EmitKind>) {
    if files.is_empty() {
        error("no file(s) specified to watch");
        return;
    }
    eprintln!("note: watch mode not yet implemented");
}

pub fn stdin(emit: Option<EmitKind>) {
    let mut buf = String::new();
    if io::stdin().read_to_string(&mut buf).is_err() {
        error("unable to read stdin");
        return;
    }

    let mut source_map = SourceMap::new();
    let mut interner = Interner::new();

    let file_id = source_map.add_file("<stdin>".into(), buf);
    let Some(file) = source_map.get(file_id).cloned() else {
        error("internal: unable to add source file");
        return;
    };

    let (tokens, bag) = lexer::tokenize(&file, &mut interner);

    if bag.errors > 0 {
        diagnostic::emit_all(&bag, &source_map);
    }

    if emit == Some(EmitKind::Tokens) {
        for token in &tokens {
            eprintln!("{}", token.kind.display(&interner));
        }
    }
}

fn check_file(path: &PathBuf, emit: Option<EmitKind>) -> Result<(), String> {
    let contents =
        fs::read_to_string(path).map_err(|e| format!("cannot read '{}': {e}", path.display()))?;

    let mut source_map = SourceMap::new();
    let mut interner = Interner::new();

    let file_id = source_map.add_file(path.display().to_string(), contents);
    let file = source_map
        .get(file_id)
        .cloned()
        .ok_or_else(|| "internal: unable to add source file".to_owned())?;

    let (tokens, bag) = lexer::tokenize(&file, &mut interner);

    if bag.errors > 0 {
        diagnostic::emit_all(&bag, &source_map);
    }

    if emit == Some(EmitKind::Tokens) {
        for token in &tokens {
            eprintln!("{}", token.kind.display(&interner));
        }
    }

    // TODO: parse, semantic analysis

    if bag.errors > 0 {
        Err(format!("{} error(s) in {}", bag.errors, path.display()))
    } else {
        Ok(())
    }
}

fn error(msg: &str) {
    let mut stderr = io::stderr();
    writeln!(stderr, "\x1b[1;31merror:\x1b[0m {msg}").unwrap_or(());
}
