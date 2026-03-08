//! `musi add` command - Add a dependency to mspackage.json.

use std::env;
use std::process;

use clap::Args;

use crate::config::{self, find_and_load};
use crate::deps;
use crate::fetch;
use crate::lock::LockFile;

#[derive(Args)]
pub struct AddArgs {
    /// Package to add (e.g., `@scope/name` or `@scope/name@1.0.0`).
    package: String,

    /// Add as dev dependency.
    #[arg(long, short = 'D')]
    dev: bool,
}

/// Parse a package argument like `@scope/name` or `@scope/name@1.0.0`.
fn parse_package_arg(arg: &str) -> (String, Option<String>) {
    // Handle @scope/name@version format
    // The tricky part: @ can appear both as scope prefix and version separator
    if let Some(rest) = arg.strip_prefix('@') {
        // Find scope/name boundary
        if let Some(slash_pos) = rest.find('/') {
            let after_slash = rest.get(slash_pos + 1..).unwrap_or("");
            // Check for @version after the package name
            if let Some(at_pos) = after_slash.find('@') {
                let name_end = slash_pos + 1 + at_pos;
                let name = format!("@{}", rest.get(..name_end).unwrap_or(""));
                let version = after_slash.get(at_pos + 1..).unwrap_or("").to_owned();
                return (name, Some(version));
            }
        }
        // No version specified
        return (arg.to_owned(), None);
    }

    // Non-scoped package with version: name@version
    if let Some(at_pos) = arg.find('@') {
        let name = arg.get(..at_pos).unwrap_or("").to_owned();
        let version = arg.get(at_pos + 1..).unwrap_or("").to_owned();
        return (name, Some(version));
    }

    (arg.to_owned(), None)
}

pub fn run(args: &AddArgs) {
    let cwd = env::current_dir().unwrap_or_else(|e| {
        eprintln!("error: cannot get current directory: {e}");
        process::exit(1);
    });

    let Some((mut config, project_dir)) = find_and_load(&cwd) else {
        eprintln!("error: no mspackage.json found; run `musi init` first");
        process::exit(1);
    };

    let (name, specified_version) = parse_package_arg(&args.package);

    // Validate the package name
    if !name.starts_with('@') {
        eprintln!("error: package name must be scoped (@scope/name)");
        process::exit(1);
    }

    // Check if already installed
    let deps_map = if args.dev {
        &config.dev_dependencies
    } else {
        &config.dependencies
    };

    if deps_map.contains_key(&name) {
        eprintln!("Package {name} is already in dependencies.");
        eprintln!("Use `musi install` to update.");
        process::exit(1);
    }

    // Determine version to use
    let version_req = specified_version.unwrap_or_else(|| "*".to_owned());

    println!("Adding {name}@{version_req}...");

    // Parse and resolve the dependency
    let spec = match deps::parse_dep_spec(&name, &version_req) {
        Ok(s) => s,
        Err(e) => {
            eprintln!("error: invalid dependency: {e}");
            process::exit(1);
        }
    };

    // Fetch to verify it exists and get the resolved version
    let resolved = match fetch::ensure_installed(&spec) {
        Ok(r) => r,
        Err(e) => {
            eprintln!("error: failed to fetch {name}: {e}");
            process::exit(1);
        }
    };

    println!("Resolved to v{}", resolved.version);

    // Update config
    let version_to_save = if version_req == "*" {
        format!("^{}", resolved.version)
    } else {
        version_req
    };

    if args.dev {
        let _ = config
            .dev_dependencies
            .insert(name.clone(), version_to_save.clone());
    } else {
        let _ = config
            .dependencies
            .insert(name.clone(), version_to_save.clone());
    }

    // Save config
    if let Err(e) = config::save_to_dir(&project_dir, &config) {
        eprintln!("error: failed to update mspackage.json: {e}");
        process::exit(1);
    }

    // Update lock file
    let mut lock = LockFile::read_from(&project_dir).unwrap_or_default();
    lock.add_resolved(&resolved);
    if let Err(e) = lock.write_to(&project_dir) {
        eprintln!("warning: failed to write lock file: {e}");
    }

    println!();
    println!(
        "Added {name}@{version_to_save} to {}.",
        if args.dev {
            "devDependencies"
        } else {
            "dependencies"
        }
    );
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parse_scoped_package() {
        let (name, version) = parse_package_arg("@musi-lang/libc-ms");
        assert_eq!(name, "@musi-lang/libc-ms");
        assert_eq!(version, None);
    }

    #[test]
    fn parse_scoped_package_with_version() {
        let (name, version) = parse_package_arg("@musi-lang/libc-ms@0.1.0");
        assert_eq!(name, "@musi-lang/libc-ms");
        assert_eq!(version, Some("0.1.0".to_owned()));
    }

    #[test]
    fn parse_simple_package_with_version() {
        let (name, version) = parse_package_arg("pkg@1.2.3");
        assert_eq!(name, "pkg");
        assert_eq!(version, Some("1.2.3".to_owned()));
    }
}
