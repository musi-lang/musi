use std::env::current_dir;
use std::ffi::OsStr;
use std::fs;
use std::path::Path;

use crate::error::{MusiError, MusiResult};
use musi_tooling::ToolingError;

const STARTER_INDEX: &str =
    "let io := import \"@std/io\";\n\nlet message := \"Hello, world!\";\nio.writeLine(message);\n";
const STARTER_TEST: &str = r#"let Testing := import "@std/testing";

let add (left : Int, right : Int) : Int := left + right;

export let test () :=
  (
    Testing.describe("add");
    Testing.it("adds values", Testing.toBe(add(2, 3), 5));
    Testing.endDescribe()
  );
"#;

pub(super) fn init_package(target: Option<&Path>) -> MusiResult {
    let root = match target {
        Some(path) => path.to_path_buf(),
        None => current_dir().map_err(|_| MusiError::MissingCurrentDirectory)?,
    };
    let name = package_name_for_path(&root)?;
    if package_marker_exists(&root) {
        return Err(MusiError::PackageAlreadyInitialized { path: root });
    }
    fs::create_dir_all(&root).map_err(|source| ToolingError::ToolingIoFailed {
        path: root.clone(),
        source,
    })?;
    fs::write(
        root.join("musi.json"),
        format!(
            "{{\n  \"name\": \"{name}\",\n  \"version\": \"0.1.0\",\n  \"entry\": \"index.ms\"\n}}\n"
        ),
    )
    .map_err(|source| ToolingError::ToolingIoFailed {
        path: root.join("musi.json"),
        source,
    })?;
    fs::write(root.join("index.ms"), STARTER_INDEX).map_err(|source| {
        ToolingError::ToolingIoFailed {
            path: root.join("index.ms"),
            source,
        }
    })?;
    let tests_dir = root.join("__tests__");
    fs::create_dir_all(&tests_dir).map_err(|source| ToolingError::ToolingIoFailed {
        path: tests_dir.clone(),
        source,
    })?;
    fs::write(tests_dir.join("add.test.ms"), STARTER_TEST).map_err(|source| {
        ToolingError::ToolingIoFailed {
            path: tests_dir.join("add.test.ms"),
            source,
        }
    })?;
    if !root.join(".gitignore").exists() {
        fs::write(root.join(".gitignore"), "musi_modules/\n").map_err(|source| {
            ToolingError::ToolingIoFailed {
                path: root.join(".gitignore"),
                source,
            }
        })?;
    }
    println!("{}", root.display());
    Ok(())
}

fn package_name_for_path(root: &Path) -> MusiResult<String> {
    let canonical_root = root.canonicalize().ok();
    let name_source = if root.file_name().is_some() {
        root
    } else {
        canonical_root.as_deref().unwrap_or(root)
    };
    name_source
        .file_name()
        .filter(|name| !name.is_empty())
        .unwrap_or_else(|| OsStr::new(""))
        .to_str()
        .filter(|name| !name.is_empty())
        .map(str::to_owned)
        .ok_or_else(|| MusiError::MissingPackageName {
            path: root.to_path_buf(),
        })
}

fn package_marker_exists(root: &Path) -> bool {
    root.join("musi.json").exists()
        || root.join("index.ms").exists()
        || root.join("__tests__/add.test.ms").exists()
        || root.join("add.test.ms").exists()
}
