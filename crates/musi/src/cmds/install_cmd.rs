//! `musi install` command - Install all dependencies from mspackage.json.

use std::env;
use std::process;

use clap::Args;

use crate::config::find_and_load;
use crate::deps::{self, DepSpec};
use crate::fetch;
use crate::lock::LockFile;

#[derive(Args)]
pub struct InstallArgs {
    /// Include dev dependencies.
    #[arg(long, short = 'D')]
    dev: bool,
}

pub fn run(args: &InstallArgs) {
    let cwd = env::current_dir().unwrap_or_else(|e| {
        eprintln!("error: cannot get current directory: {e}");
        process::exit(1);
    });

    let Some((config, project_dir)) = find_and_load(&cwd) else {
        eprintln!("error: no mspackage.json found in current directory or parents");
        process::exit(1);
    };

    if config.dependencies.is_empty() && (!args.dev || config.dev_dependencies.is_empty()) {
        println!("No dependencies to install.");
        return;
    }

    // Parse all dependency specs
    let mut all_specs: Vec<DepSpec> = Vec::new();

    match deps::parse_all_deps(&config) {
        Ok(specs) => all_specs.extend(specs),
        Err(e) => {
            eprintln!("error: invalid dependency: {e}");
            process::exit(1);
        }
    }

    if args.dev {
        match deps::parse_dev_deps(&config) {
            Ok(specs) => all_specs.extend(specs),
            Err(e) => {
                eprintln!("error: invalid dev dependency: {e}");
                process::exit(1);
            }
        }
    }

    // Load or create lock file
    let mut lock = LockFile::read_from(&project_dir).unwrap_or_default();

    // Install each dependency
    let mut installed = 0;
    let mut cached = 0;
    let total = all_specs.len();

    for spec in &all_specs {
        print!("Installing {}...", spec.name);

        // Check if already locked
        if let Some(locked_ver) = lock.get_locked_version(&spec.name)
            && deps::is_cached(spec, locked_ver)
        {
            println!(" (cached v{locked_ver})");
            cached += 1;
            continue;
        }

        match fetch::ensure_installed(spec) {
            Ok(resolved) => {
                println!(" v{}", resolved.version);
                lock.add_resolved(&resolved);
                installed += 1;
            }
            Err(e) => {
                eprintln!("\nerror: failed to install {}: {e}", spec.name);
                process::exit(1);
            }
        }
    }

    // Write lock file
    if installed > 0
        && let Err(e) = lock.write_to(&project_dir)
    {
        eprintln!("warning: failed to write lock file: {e}");
    }

    println!();
    if installed > 0 {
        println!("Installed {installed} package(s).");
    }
    if cached > 0 {
        println!("Reused {cached} cached package(s).");
    }
    println!("Total: {total} package(s).");
}
