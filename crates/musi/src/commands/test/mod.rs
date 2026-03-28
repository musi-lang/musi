mod collector;
mod decode;
mod discovery;
mod load;
mod model;
mod runner;

use std::path::PathBuf;
use std::process::ExitCode;

use clap::Args;

use discovery::collect_test_files;
use model::TestStats;
use runner::run_test_file;

#[derive(Args)]
pub struct TestArgs {
    /// Optional file or directory to test. Defaults to the current directory.
    pub path: Option<PathBuf>,
    /// Substring filter over joined suite/test names.
    #[arg(long)]
    pub grep: Option<String>,
}

pub fn run(args: &TestArgs) -> ExitCode {
    let target = args.path.clone().unwrap_or_else(|| PathBuf::from("."));
    let files = match collect_test_files(&target) {
        Ok(files) => files,
        Err(e) => {
            eprintln!("error: {e}");
            return ExitCode::from(2);
        }
    };

    if files.is_empty() {
        eprintln!("error: no *.test.ms files found under {}", target.display());
        return ExitCode::FAILURE;
    }

    let mut total = TestStats::new();
    for file in files {
        match run_test_file(&file, args.grep.as_deref()) {
            Ok(stats) => {
                total.passed += stats.passed;
                total.failed += stats.failed;
            }
            Err(message) => {
                eprintln!("error: {}: {message}", file.display());
                total.failed += 1;
            }
        }
    }

    if total.failed == 0 {
        println!("\nPASS  {} passed", total.passed);
        ExitCode::SUCCESS
    } else {
        println!("\nFAIL  {} passed, {} failed", total.passed, total.failed);
        ExitCode::FAILURE
    }
}
