use std::fs;
use std::path::{Path, PathBuf};
use std::process;

use clap::Args;
use musi_native::REGISTRY;
use musi_vm::{NativeRegistry, Vm};

use crate::compiler;

#[derive(Args)]
pub struct TestArgs {
    /// File, or directory to search recursively for *.test.ms / *.spec.ms.
    /// Defaults to the current directory.
    pub target: Option<PathBuf>,
}

pub fn run(args: TestArgs) {
    let target = args.target.unwrap_or_else(|| PathBuf::from("."));

    let files = if target.is_file() {
        vec![target]
    } else {
        collect_test_files(&target)
    };

    if files.is_empty() {
        println!("no test files found");
        return;
    }

    let mut total_passed = 0usize;
    let mut total_failed = 0usize;

    for file in &files {
        let path = file.to_string_lossy();
        let module = compiler::compile_file(&path);
        let main_fn_idx =
            u16::try_from(module.function_table.len() - 1).expect("function table fits u16");

        let registry = NativeRegistry::new(REGISTRY);
        let mut vm = Vm::new(module, registry);
        match vm.run_tests(main_fn_idx) {
            Ok(()) => {}
            Err(e) => {
                eprintln!("runtime error in {path}: {e}");
                process::exit(1);
            }
        }

        let results = &vm.test_results;
        if !results.is_empty() {
            println!("{path}");
        }
        for r in results {
            if r.passed {
                println!("  PASS  {}", r.label);
                total_passed += 1;
            } else {
                let msg = r.error.as_deref().unwrap_or("assertion failed");
                println!("  FAIL  {}: {msg}", r.label);
                total_failed += 1;
            }
        }
    }

    println!();
    println!("{total_passed} passed, {total_failed} failed");

    if total_failed > 0 {
        process::exit(1);
    }
}

/// Recursively collect all `*.test.ms` and `*.spec.ms` files under `dir`.
fn collect_test_files(dir: &Path) -> Vec<PathBuf> {
    let mut found = Vec::new();
    collect_recursive(dir, &mut found);
    found.sort();
    found
}

fn collect_recursive(dir: &Path, out: &mut Vec<PathBuf>) {
    let Ok(entries) = fs::read_dir(dir) else { return };
    for entry in entries.flatten() {
        let path = entry.path();
        if path.is_dir() {
            collect_recursive(&path, out);
        } else if is_test_file(&path) {
            out.push(path);
        }
    }
}

fn is_test_file(path: &Path) -> bool {
    let name = path.file_name().and_then(|n| n.to_str()).unwrap_or("");
    (name.ends_with(".test.ms") || name.ends_with(".spec.ms")) && path.is_file()
}
