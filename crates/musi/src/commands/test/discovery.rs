use std::fs;
use std::path::{Path, PathBuf};

pub(super) fn default_suite_name(path: &Path) -> String {
    path.file_stem()
        .and_then(|name| name.to_str())
        .map(str::to_owned)
        .unwrap_or_else(|| "tests".to_owned())
}

pub(super) fn collect_test_files(target: &Path) -> Result<Vec<PathBuf>, std::io::Error> {
    if target.is_file() {
        return Ok(is_test_file(target)
            .then_some(target.to_path_buf())
            .into_iter()
            .collect());
    }

    let mut files = Vec::new();
    collect_test_files_recursive(target, &mut files)?;
    files.sort();
    Ok(files)
}

pub(super) fn is_test_file(path: &Path) -> bool {
    path.file_name()
        .and_then(|name| name.to_str())
        .is_some_and(|name| name.ends_with(".test.ms"))
}

fn collect_test_files_recursive(dir: &Path, out: &mut Vec<PathBuf>) -> Result<(), std::io::Error> {
    for entry in fs::read_dir(dir)? {
        let entry = entry?;
        let path = entry.path();
        if path.is_dir() {
            collect_test_files_recursive(&path, out)?;
            continue;
        }
        if is_test_file(&path) {
            out.push(path);
        }
    }
    Ok(())
}
