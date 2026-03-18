use std::fs;
use std::path::Path;
use std::process;

use serde_json::json;

pub fn run(name: &str, template: &str) -> ! {
    if !is_valid_package_name(name) {
        eprintln!("error: invalid package name `{name}`");
        eprintln!("  names must match: [a-z0-9][a-z0-9._-]*");
        process::exit(1);
    }

    let dir = Path::new(name);
    if dir.exists() {
        eprintln!("error: directory `{name}` already exists");
        process::exit(1);
    }

    if let Err(e) = create_project(dir, name, template) {
        eprintln!("error: {e}");
        process::exit(1);
    }

    eprintln!("created `{name}` project in ./{name}");
    process::exit(0)
}

pub fn create_project(dir: &Path, name: &str, template: &str) -> Result<(), String> {
    fs::create_dir_all(dir).map_err(|e| format!("failed to create directory: {e}"))?;

    let main_file = if template == "lib" {
        "lib.ms"
    } else {
        "index.ms"
    };
    let manifest = serde_json::to_string_pretty(&json!({
        "name": name,
        "version": "0.1.0",
        "main": format!("./{main_file}")
    }))
    .map_err(|e| format!("failed to serialize musi.json: {e}"))?;
    fs::write(dir.join("musi.json"), format!("{manifest}\n"))
        .map_err(|e| format!("failed to write musi.json: {e}"))?;

    #[allow(clippy::literal_string_with_formatting_args)]
    let source = if template == "lib" {
        r#"export let greet : (String) -> String := (name) => f"hello, {name}!";
"#
    } else {
        r#"import "@std/rt" as rt;

rt.writeln("hello, world!");
"#
    };
    let source_file = if template == "lib" {
        "lib.ms"
    } else {
        "index.ms"
    };
    fs::write(dir.join(source_file), source)
        .map_err(|e| format!("failed to write {source_file}: {e}"))?;

    let gitignore = r"target/
dist/
*.seam
.msbuildinfo
";
    fs::write(dir.join(".gitignore"), gitignore)
        .map_err(|e| format!("failed to write .gitignore: {e}"))?;

    Ok(())
}

fn is_valid_package_name(name: &str) -> bool {
    if name.is_empty() {
        return false;
    }
    let bytes = name.as_bytes();
    let first = bytes[0];
    if !(first.is_ascii_lowercase() || first.is_ascii_digit()) {
        return false;
    }
    bytes[1..].iter().all(|&b| {
        b.is_ascii_lowercase() || b.is_ascii_digit() || b == b'.' || b == b'_' || b == b'-'
    })
}
