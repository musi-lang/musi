//! `musi test` - discover and run `*.test.ms` files.

use std::fmt::Write as FmtWrite;
use std::path::{Path, PathBuf};
use std::time::Instant;
use std::{fs, process};

use msc::pipeline;
use msc_builtins::StdHost;
use msc_manifest::MusiManifest;
use msc_vm::{Vm, load, verify};

enum TestOutcome {
    Passed,
    Failed(i64),
    Error(String),
}

pub fn run(
    filter: Option<&str>,
    manifest: Option<&MusiManifest>,
    project_root: Option<&Path>,
) -> ! {
    let Some(project_root) = project_root else {
        eprintln!("error: no musi.json found - `musi test` requires a project");
        process::exit(1);
    };

    let manifest = manifest.expect("manifest must exist when project_root exists");

    let mut test_files = discover_test_files(project_root);
    test_files.sort();

    if let Some(f) = filter {
        let canonical_filter = Path::new(f)
            .canonicalize()
            .map_or_else(|_| f.to_owned(), |p| p.display().to_string());
        test_files.retain(|p| p.display().to_string().contains(&canonical_filter));
        if test_files.is_empty() {
            eprintln!("no test files matched filter '{f}'");
            process::exit(0);
        }
    } else if test_files.is_empty() {
        eprintln!("no test files found");
        process::exit(0);
    }

    let total = test_files.len();
    let mut passed = 0usize;
    let mut failed = 0usize;
    let mut errors = 0usize;

    let start = Instant::now();

    for path in &test_files {
        let display = path.strip_prefix(project_root).unwrap_or(path).display();

        match run_test_file(path, manifest, project_root) {
            TestOutcome::Passed => {
                println!("[PASS] {display}");
                passed += 1;
            }
            TestOutcome::Failed(n) => {
                println!("[FAIL] {display} ({n} failed)");
                failed += 1;
            }
            TestOutcome::Error(msg) => {
                println!("[ERR ] {display} - {msg}");
                errors += 1;
            }
        }
    }

    let elapsed = start.elapsed();
    println!("─────────────────────────────");

    let mut summary = format!("{passed}/{total} passed");
    if failed > 0 {
        write!(summary, ", {failed} failed").unwrap();
    }
    if errors > 0 {
        write!(
            summary,
            ", {errors} error{}",
            if errors == 1 { "" } else { "s" }
        )
        .unwrap();
    }
    write!(summary, " ({:.2}s)", elapsed.as_secs_f64()).unwrap();
    println!("{summary}");

    if failed > 0 || errors > 0 {
        process::exit(1);
    }
    process::exit(0);
}

fn run_test_file(path: &Path, manifest: &MusiManifest, project_root: &Path) -> TestOutcome {
    let Ok(mut out) = pipeline::run_frontend_multi(path, Some(manifest), Some(project_root)) else {
        return TestOutcome::Error("compilation error".into());
    };

    let Ok(bytes) = pipeline::run_backend(&mut out, true) else {
        return TestOutcome::Error("codegen error".into());
    };

    let module = match load(&bytes) {
        Ok(m) => m,
        Err(e) => return TestOutcome::Error(format!("load error: {e}")),
    };

    if let Err(e) = verify(&module) {
        return TestOutcome::Error(format!("verify error: {e}"));
    }

    let Ok(host) = StdHost::new(&module.foreign_fns) else {
        return TestOutcome::Error("host error".into());
    };

    let mut vm = Vm::new(module);
    vm.set_host(Box::new(host));

    match vm.run() {
        Ok(value) => match value.as_int() {
            Ok(0) | Err(_) => TestOutcome::Passed,
            Ok(n) => TestOutcome::Failed(n),
        },
        Err(e) => TestOutcome::Error(format!("runtime error: {e}")),
    }
}

fn discover_test_files(root: &Path) -> Vec<PathBuf> {
    let root = root.canonicalize().unwrap_or_else(|_| root.to_path_buf());
    let mut files = Vec::new();
    collect_test_files(&root, &mut files);
    files
}

fn collect_test_files(dir: &Path, out: &mut Vec<PathBuf>) {
    let Ok(entries) = fs::read_dir(dir) else {
        return;
    };
    for entry in entries.flatten() {
        let path = entry.path();
        if path.is_dir() {
            collect_test_files(&path, out);
        } else if path
            .extension()
            .is_some_and(|e| e.eq_ignore_ascii_case("ms"))
            && path
                .file_stem()
                .and_then(|s| s.to_str())
                .is_some_and(|stem| {
                    Path::new(stem)
                        .extension()
                        .is_some_and(|ext| ext.eq_ignore_ascii_case("test"))
                })
        {
            out.push(path);
        }
    }
}
