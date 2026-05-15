use std::collections::BTreeMap;
use std::fs;
use std::path::{Path, PathBuf};

const CRATE_DOMINATION_MIN_TOTAL: usize = 1_500;
const CRATE_DOMINATION_MAX_SHARE_NUMERATOR: usize = 3;
const CRATE_DOMINATION_MAX_SHARE_DENOMINATOR: usize = 4;

fn repo_root() -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .and_then(Path::parent)
        .expect("music crate should live under crates/music")
        .to_path_buf()
}

fn collect_files(root: &Path, extension: &str, out: &mut Vec<PathBuf>) {
    let entries = fs::read_dir(root).expect("directory should be readable");
    for entry in entries {
        let path = entry.expect("directory entry should be readable").path();
        if path.file_name().is_some_and(|name| {
            matches!(
                name.to_str(),
                Some(".astro" | ".git" | ".vscode-test" | "dist" | "node_modules" | "target")
            )
        }) {
            continue;
        }
        if path.is_dir() {
            collect_files(&path, extension, out);
        } else if path.extension().is_some_and(|actual| actual == extension) {
            out.push(path);
        }
    }
}

fn read_files_under(root: &Path, out: &mut Vec<PathBuf>) {
    let entries = fs::read_dir(root).expect("directory should be readable");
    for entry in entries {
        let path = entry.expect("directory entry should be readable").path();
        if path.file_name().is_some_and(|name| {
            matches!(
                name.to_str(),
                Some(".astro" | ".git" | ".vscode-test" | "dist" | "node_modules" | "target")
            )
        }) {
            continue;
        }
        if path.is_dir() {
            read_files_under(&path, out);
        } else {
            out.push(path);
        }
    }
}

fn rust_files_under(root: &Path) -> Vec<PathBuf> {
    let mut files = Vec::new();
    collect_files(root, "rs", &mut files);
    files.sort();
    files
}

fn relative_to_repo(path: &Path) -> String {
    path.strip_prefix(repo_root())
        .expect("path should be inside repo")
        .display()
        .to_string()
}

fn is_test_rust_file(path: &Path, text: &str) -> bool {
    path.file_name().is_some_and(|name| name == "tests.rs")
        || path
            .components()
            .any(|component| component.as_os_str() == "tests")
        || text.contains("#[test]")
}

fn non_test_source_files() -> Vec<PathBuf> {
    rust_files_under(&repo_root().join("crates"))
        .into_iter()
        .filter(|path| {
            !path
                .components()
                .any(|component| component.as_os_str() == "benches")
        })
        .filter(|path| {
            let text = fs::read_to_string(path).expect("Rust file should be UTF-8");
            !is_test_rust_file(path, &text)
        })
        .collect()
}

#[cfg(test)]
mod success {
    use super::*;

    #[test]
    fn public_install_shell_script_is_only_remaining_shell_script() {
        let mut scripts = Vec::new();
        collect_files(&repo_root(), "sh", &mut scripts);
        let scripts = scripts
            .iter()
            .map(|path| relative_to_repo(path))
            .collect::<Vec<_>>();

        assert_eq!(scripts, ["install.sh"]);
    }

    #[test]
    fn crate_source_is_not_dominated_by_single_file() {
        let mut totals = BTreeMap::<String, usize>::new();
        let mut largest = BTreeMap::<String, (usize, String)>::new();
        for path in non_test_source_files() {
            let relative = relative_to_repo(&path);
            let crate_name = relative
                .split('/')
                .nth(1)
                .expect("crate path should include crate name")
                .to_owned();
            let line_count = fs::read_to_string(&path)
                .expect("Rust file should be UTF-8")
                .lines()
                .count();
            *totals.entry(crate_name.clone()).or_default() += line_count;
            let current = largest.entry(crate_name).or_default();
            if line_count > current.0 {
                *current = (line_count, relative);
            }
        }

        let violations = totals
            .into_iter()
            .filter_map(|(crate_name, total)| {
                let (max_lines, file) = largest
                    .get(&crate_name)
                    .expect("crate should have largest file");
                let dominated = total >= CRATE_DOMINATION_MIN_TOTAL
                    && max_lines * CRATE_DOMINATION_MAX_SHARE_DENOMINATOR
                        >= total * CRATE_DOMINATION_MAX_SHARE_NUMERATOR;
                dominated
                    .then(|| format!("{crate_name}: total={total} max={max_lines} file={file}"))
            })
            .collect::<Vec<_>>();

        assert_eq!(violations, Vec::<String>::new());
    }
}

#[cfg(test)]
mod failure {
    use super::*;

    #[test]
    fn no_mjs_scripts_remain() {
        let mut scripts = Vec::new();
        collect_files(&repo_root(), "mjs", &mut scripts);

        assert_eq!(scripts, Vec::<PathBuf>::new());
    }

    #[test]
    fn rust_tests_are_categorized_by_success_and_failure() {
        let missing = rust_files_under(&repo_root().join("crates"))
            .into_iter()
            .filter_map(|path| {
                let text = fs::read_to_string(&path).expect("Rust file should be UTF-8");
                (text.contains("#[test]")
                    && (!text.contains("mod success") || !text.contains("mod failure")))
                .then(|| relative_to_repo(&path))
            })
            .collect::<Vec<_>>();

        assert_eq!(missing, Vec::<String>::new());
    }

    #[test]
    fn integration_test_directories_use_success_and_failure_layout() {
        let missing = fs::read_dir(repo_root().join("crates"))
            .expect("crates directory should be readable")
            .filter_map(|entry| {
                let crate_dir = entry
                    .expect("crate directory entry should be readable")
                    .path();
                let tests_dir = crate_dir.join("tests");
                tests_dir.is_dir().then_some(tests_dir)
            })
            .filter_map(|tests_dir| {
                let has_success_dir = tests_dir.join("success").is_dir();
                let has_failure_dir = tests_dir.join("failure").is_dir();
                let has_success_mod = tests_dir.join("success.rs").is_file();
                let has_failure_mod = tests_dir.join("failure.rs").is_file();
                let has_success = has_success_dir || has_success_mod;
                let has_failure = has_failure_dir || has_failure_mod;

                (!has_success || !has_failure).then(|| relative_to_repo(&tests_dir))
            })
            .collect::<Vec<_>>();

        assert_eq!(missing, Vec::<String>::new());
    }

    #[test]
    fn musi_source_extension_is_ms_not_musi() {
        let mut extension_files = Vec::new();
        collect_files(&repo_root(), "musi", &mut extension_files);
        assert_eq!(extension_files, Vec::<PathBuf>::new());

        let source_extension = [".", "musi"].concat();
        let forbidden_needles = [
            ["*.", "musi"].concat(),
            ["`.", "musi", "` source"].concat(),
            ["source `.", "musi", "`"].concat(),
            ["extension `.", "musi", "`"].concat(),
            ["extension .", "musi"].concat(),
        ];
        let mut files = Vec::new();
        for root in ["README.md", "docs", "crates", "packages", "specs"] {
            let path = repo_root().join(root);
            if path.is_file() {
                files.push(path);
            } else if path.is_dir() {
                read_files_under(&path, &mut files);
            }
        }
        let violations = files
            .into_iter()
            .filter_map(|path| {
                let text = fs::read_to_string(&path).ok()?;
                let lower = text.to_ascii_lowercase();
                forbidden_needles
                    .iter()
                    .any(|needle| lower.contains(needle))
                    .then(|| relative_to_repo(&path))
            })
            .filter(|path| {
                path != "crates/music/tests/workspace_shape.rs"
                    && !path.ends_with("diag_catalog_gen.rs")
                    && !path.ends_with("target")
            })
            .collect::<Vec<_>>();

        assert_eq!(
            violations,
            Vec::<String>::new(),
            "{source_extension} must not be used as Musi source extension"
        );
    }

    #[test]
    fn generated_diagnostic_catalogs_are_rustfmt_skipped_at_module_boundary() {
        let missing = rust_files_under(&repo_root().join("crates"))
            .into_iter()
            .filter(|path| {
                path.file_name()
                    .is_some_and(|name| name == "diag_catalog_gen.rs")
            })
            .filter_map(|catalog| {
                let dir = catalog.parent()?;
                let owner = ["diag.rs", "errors.rs"]
                    .into_iter()
                    .map(|name| dir.join(name))
                    .find(|path| path.exists())?;
                let text = fs::read_to_string(&owner).ok()?;
                (!text.contains(
                    "#[path = \"diag_catalog_gen.rs\"]\n#[rustfmt::skip]\nmod diag_catalog_gen;",
                ))
                .then(|| relative_to_repo(&catalog))
            })
            .collect::<Vec<_>>();

        assert_eq!(missing, Vec::<String>::new());
    }
}
