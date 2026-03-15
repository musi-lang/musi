//! Typecheck all `stdlib/` stub files and report diagnostics.
//!
//! Run with: `cargo test -p music_sema --test std_typecheck -- --nocapture`
#![allow(clippy::tests_outside_test_module)]

use std::fs;
use std::path::{Path, PathBuf};

use music_lex::lex as lex_source;
use music_parse::parse;
use music_sema::analyze;
use music_shared::{DiagnosticBag, Interner, Severity, SourceDb};

fn collect_ms_files(dir: &Path) -> Vec<PathBuf> {
    let mut files = Vec::new();
    let mut stack = vec![dir.to_path_buf()];
    while let Some(current) = stack.pop() {
        let Ok(entries) = fs::read_dir(&current) else {
            continue;
        };
        for entry in entries.flatten() {
            let path = entry.path();
            if path.is_dir() {
                stack.push(path);
            } else if path.extension().is_some_and(|e| e == "ms") {
                files.push(path);
            }
        }
    }
    files.sort();
    files
}

#[test]
fn typecheck_all_std_files() {
    let manifest_dir = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let std_dir = manifest_dir.join("../../stdlib");
    let std_dir = std_dir.canonicalize().expect("stdlib/ directory should exist");

    let files = collect_ms_files(&std_dir);
    if files.is_empty() {
        eprintln!("no .ms files found under stdlib/ — skipping");
        return;
    }

    let mut total_errors = 0u32;
    let mut total_warnings = 0u32;
    let mut file_count = 0u32;

    for path in &files {
        let source = fs::read_to_string(path).expect("failed to read file");
        let rel_path = path
            .strip_prefix(&std_dir)
            .unwrap_or(path)
            .display()
            .to_string();

        let mut interner = Interner::new();
        let mut source_db = SourceDb::new();
        let file_id = source_db.add(format!("stdlib/{rel_path}"), &source);

        // Lex
        let mut lex_diags = DiagnosticBag::new();
        let lexed = lex_source(&source, file_id, &mut interner, &mut lex_diags);

        // Parse
        let mut parse_diags = DiagnosticBag::new();
        let module = parse(&lexed.tokens, file_id, &mut parse_diags, &mut interner);

        // Analyze
        let mut sema_diags = DiagnosticBag::new();
        let _result = analyze(&module, &mut interner, file_id, &mut sema_diags);

        // Collect all diagnostics
        let all_diags: Vec<_> = lex_diags
            .iter()
            .chain(parse_diags.iter())
            .chain(sema_diags.iter())
            .collect();

        let errors = all_diags
            .iter()
            .filter(|d| d.severity == Severity::Error)
            .count();
        let warnings = all_diags
            .iter()
            .filter(|d| d.severity == Severity::Warning)
            .count();

        if !all_diags.is_empty() {
            eprintln!("\n=== stdlib/{rel_path} ===");
            for d in &all_diags {
                let rendered = d.render_simple(&source_db);
                eprintln!("  {rendered}");
            }
            eprintln!("  {errors} error(s), {warnings} warning(s)");
        }

        total_errors += u32::try_from(errors).expect("count fits");
        total_warnings += u32::try_from(warnings).expect("count fits");
        file_count += 1;
    }

    eprintln!("\n========================================");
    eprintln!(
        "Checked {file_count} files: {total_errors} total error(s), {total_warnings} total warning(s)"
    );
    eprintln!("========================================");

    // This test intentionally does NOT assert zero errors.
    // It catalogues the current state of stdlib/ file compatibility.
}
