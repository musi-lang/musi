#![allow(clippy::panic, clippy::unwrap_used, clippy::tests_outside_test_module)]

use std::fs;
use std::path::{Path, PathBuf};
use std::process::Command;

const MAX_LOC: usize = 2000;

fn repo_root() -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("../..")
        .canonicalize()
        .unwrap()
}

fn tracked_source_files(root: &Path) -> Vec<PathBuf> {
    let output = Command::new("git")
        .arg("ls-files")
        .current_dir(root)
        .output()
        .unwrap();
    assert!(output.status.success(), "git ls-files failed");

    String::from_utf8(output.stdout)
        .unwrap()
        .lines()
        .filter(|path| {
            matches!(
                Path::new(path).extension().and_then(|ext| ext.to_str()),
                Some("rs" | "ms" | "abnf")
            )
        })
        .map(|path| root.join(path))
        .filter(|path| path.exists())
        .collect()
}

#[test]
fn tracked_source_files_stay_under_loc_cap() {
    let root = repo_root();
    let offenders: Vec<_> = tracked_source_files(&root)
        .into_iter()
        .filter_map(|path| {
            let contents = fs::read_to_string(&path).unwrap();
            let loc = contents.lines().count();
            (loc > MAX_LOC).then_some((path, loc))
        })
        .collect();

    assert!(
        offenders.is_empty(),
        "source file LOC cap exceeded: {:?}",
        offenders
    );
}
