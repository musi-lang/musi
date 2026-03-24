use std::fs;
use std::io;
use std::path::Path;

use music_db::Db;
use music_found::{Interner, SourceMap};
use music_hir::lower;
use music_lex::Lexer;
use music_parse::parse;
use music_resolve::queries::{ResolutionMap, ResolveDb};
use music_sema::{TypeEnv, type_check};

use crate::diagnostic::{Diagnostic, Severity};

pub struct CompileResult {
    pub db: Db,
    pub resolution: ResolutionMap,
    pub type_env: TypeEnv,
    pub diagnostics: Vec<Diagnostic>,
    pub has_errors: bool,
}

/// # Errors
///
/// Returns `Err` if the source file cannot be read (e.g. not found, permission denied).
pub fn compile(path: &Path) -> Result<CompileResult, io::Error> {
    let source = fs::read_to_string(path)?;

    let mut source_map = SourceMap::default();
    let source_id = source_map.add(path, source.as_str());

    let mut interner = Interner::new();

    let (tokens, lex_errors) = Lexer::new(&source).lex();
    let (mut ast, parse_errors) = parse(&tokens, &source, &mut interner);
    lower(&mut ast);

    let db = Db::new(ast, interner, source_map);

    let root = path
        .parent()
        .unwrap_or_else(|| Path::new("."))
        .to_path_buf();
    let mut rdb = ResolveDb::new(db, root);
    rdb.set_current_file(path.to_path_buf());
    rdb.seed_builtins();
    rdb.resolve_module();
    let (db, resolution, resolve_errors) = rdb.finish();

    let (db, resolution, type_env, sema_errors) = type_check(db, resolution, None);

    let has_errors = !lex_errors.is_empty()
        || !parse_errors.is_empty()
        || !resolve_errors.is_empty()
        || !sema_errors.is_empty();

    let mut diagnostics: Vec<Diagnostic> = Vec::new();
    for e in lex_errors {
        diagnostics.push(Diagnostic {
            severity: Severity::Error,
            message: e.kind.to_string(),
            span: e.span,
            source_id,
        });
    }
    for e in parse_errors {
        diagnostics.push(Diagnostic {
            severity: Severity::Error,
            message: e.to_string(),
            span: e.span,
            source_id,
        });
    }
    for e in resolve_errors {
        diagnostics.push(Diagnostic {
            severity: Severity::Error,
            message: e.to_string(),
            span: e.span,
            source_id,
        });
    }
    for e in sema_errors {
        diagnostics.push(Diagnostic {
            severity: Severity::Error,
            message: e.to_string(),
            span: e.span,
            source_id,
        });
    }

    Ok(CompileResult {
        db,
        resolution,
        type_env,
        diagnostics,
        has_errors,
    })
}
