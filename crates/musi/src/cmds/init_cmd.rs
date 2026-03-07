use std::io::{self, Write};
use std::path::Path;
use std::process;
use std::fs;

use clap::Args;

use crate::config::CONFIG_FILENAME;

#[derive(Args)]
pub(crate) struct InitArgs {
    /// Package name (defaults to current directory name)
    #[arg(long)]
    pub(crate) name: Option<String>,
    /// Skip interactive prompts, accept defaults
    #[arg(short, long)]
    pub(crate) yes: bool,
}

pub(crate) fn run(args: InitArgs) {
    if Path::new(CONFIG_FILENAME).exists() {
        eprintln!("error: {CONFIG_FILENAME} already exists");
        process::exit(1);
    }

    let default_name = std::env::current_dir()
        .ok()
        .and_then(|p| p.file_name().map(|n| n.to_string_lossy().into_owned()))
        .unwrap_or_else(|| "musi-package".to_owned());

    let name = if let Some(n) = args.name {
        n
    } else if args.yes {
        default_name
    } else {
        print!("package name ({default_name}): ");
        io::stdout().flush().unwrap();
        let mut input = String::new();
        let _ = io::stdin().read_line(&mut input);
        let trimmed = input.trim();
        if trimmed.is_empty() { default_name } else { trimmed.to_owned() }
    };

    let config = serde_json::json!({
        "$schema": "https://musi-lang.org/schemas/mspackage-schema.v1.json",
        "name": name,
        "version": "0.1.0",
        "description": "",
        "main": "./index.ms"
    });

    fs::write(CONFIG_FILENAME, serde_json::to_string_pretty(&config).unwrap() + "\n")
        .unwrap_or_else(|e| {
            eprintln!("error: failed to write {CONFIG_FILENAME}: {e}");
            process::exit(1);
        });

    let index_path = Path::new("index.ms");
    if !index_path.exists() {
        fs::write(index_path, format!("writeln(\"Hello from {name}!\");\n"))
            .unwrap_or_else(|e| {
                eprintln!("error: failed to write index.ms: {e}");
                process::exit(1);
            });
    }

    println!("Initialized package {name}");
}
