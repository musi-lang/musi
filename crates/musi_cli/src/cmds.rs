use musi_basic::source::SourceFile;
use musi_basic::{diagnostic, interner::Interner, source::SourceMap};
use musi_lex::lexer;
use musi_sema::{binder, builtins::Builtins};

use std::fs;
use std::io::{self, Read, Write as _};
use std::path::{Path, PathBuf};

use crate::EmitKind;

pub fn check(files: &[PathBuf], emit: Option<EmitKind>) {
    for path in files {
        if let Err(e) = check_file(path, emit) {
            error(&e);
        }
    }
}

pub fn compile(files: &[PathBuf], _out_dir: &Path, emit: Option<EmitKind>) {
    for path in files {
        if let Err(e) = check_file(path, emit) {
            error(&e);
            continue;
        }
        // TODO: emit .mso bytecode to out_dir
        eprintln!("note: bytecode emission not yet implemented");
    }
}

pub fn build(project: &Path, _emit: Option<EmitKind>) {
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

pub fn init(path: &Path) {
    if let Err(e) = fs::create_dir_all(path) {
        error(&format!(
            "failed to create directory '{}': {e}",
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
        Err(e) => error(&format!("failed to create 'mspackage.json': {e}")),
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
        error("failed to read stdin");
        return;
    }

    let mut source_map = SourceMap::new();
    let mut interner = Interner::new();

    let file_id = source_map.add_file("<stdin>".into(), buf);
    let Some(file) = source_map.get(file_id).cloned() else {
        error("internal: failed to add source file");
        return;
    };

    let _ = run_phases(&file, &mut interner, &source_map, emit);
}

fn check_file(path: &Path, emit: Option<EmitKind>) -> Result<(), String> {
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

fn run_phases(
    file: &SourceFile,
    interner: &mut Interner,
    source_map: &SourceMap,
    emit: Option<EmitKind>,
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
        let (_, sema_bag) =
            binder::bind(&parse_result.arena, interner, &parse_result.prog, &builtins);
        bag.merge(sema_bag);
    }

    diagnostic::emit_all(&bag, source_map);
    bag.errors
}

fn error(msg: &str) {
    let mut stderr = io::stderr();
    writeln!(stderr, "\x1b[1;31merror:\x1b[0m {msg}").unwrap_or(());
}
