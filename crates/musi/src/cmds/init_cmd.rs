use std::env;
use std::fs;
use std::io::{self, Write};
use std::path::Path;
use std::process;

use clap::Args;

use crate::config::CONFIG_FILENAME;

#[derive(Args)]
pub struct InitArgs {
    /// Package name (defaults to current directory name)
    #[arg(long)]
    pub name: Option<String>,
    /// Skip interactive prompts, accept defaults
    #[arg(short, long)]
    pub yes: bool,
}

pub fn run(args: InitArgs) {
    if Path::new(CONFIG_FILENAME).exists() {
        eprintln!("error: {CONFIG_FILENAME} already exists");
        process::exit(1);
    }

    let default_name = env::current_dir()
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
        let _bytes = io::stdin().read_line(&mut input).unwrap_or(0);
        let trimmed = input.trim();
        if trimmed.is_empty() {
            default_name
        } else {
            trimmed.to_owned()
        }
    };

    let config = serde_json::json!({
        "$schema": "https://musi-lang.org/schemas/mspackage-schema.v1.json",
        "name": name,
        "version": "0.1.0",
        "main": "./index.ms"
    });

    fs::write(
        CONFIG_FILENAME,
        serde_json::to_string_pretty(&config).unwrap() + "\n",
    )
    .unwrap_or_else(|e| {
        eprintln!("error: failed to write {CONFIG_FILENAME}: {e}");
        process::exit(1);
    });

    let index_path = Path::new("index.ms");
    if !index_path.exists() {
        fs::write(index_path, INDEX_TEMPLATE).unwrap_or_else(|e| {
            eprintln!("error: failed to write index.ms: {e}");
            process::exit(1);
        });
    }

    let test_path = Path::new("add.test.ms");
    if !test_path.exists() {
        fs::write(test_path, ADD_TEST_TEMPLATE).unwrap_or_else(|e| {
            eprintln!("error: failed to write add.test.ms: {e}");
            process::exit(1);
        });
    }

    println!("     Created package `{name}`");
}

const INDEX_TEMPLATE: &str = "writeln(\"Hello, world!\");\n";

const ADD_TEST_TEMPLATE: &str = r#"import { assert_eq_int } from "std/assert";

fn add(a: Int, b: Int): Int => a + b;

#[test("it works")]
fn it_works() => assert_eq_int(add(2, 2), 4);
"#;
