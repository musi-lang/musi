use crate::cli::Command;
use crate::error::{MusiError, MusiResult};

pub(super) fn reserved_command_for(command: Command) -> MusiResult {
    let (command, feature, args) = match command {
        Command::Compile(args) => ("compile", "native executable output", args.args),
        Command::Lint(args) => ("lint", "linter", args.args),
        Command::Bench(args) => ("bench", "benchmark runner", args.args),
        Command::Doc(args) => ("doc", "documentation generator", args.args),
        Command::Coverage(args) => ("coverage", "coverage reporter", args.args),
        Command::Serve(args) => ("serve", "HTTP server runtime", args.args),
        Command::Repl(args) => ("repl", "interactive runtime", args.args),
        Command::Eval(args) => ("eval", "inline evaluator", args.args),
        Command::Add(args) => ("add", "package dependency writer", args.args),
        Command::Remove(args) => ("remove", "package dependency remover", args.args),
        Command::Update(args) => ("update", "package updater", args.args),
        Command::Outdated(args) => ("outdated", "package version reporter", args.args),
        Command::Audit(args) => ("audit", "package audit", args.args),
        Command::Publish(args) => ("publish", "registry publisher", args.args),
        Command::Clean(args) => ("clean", "artifact cleaner", args.args),
        _ => return Ok(()),
    };
    let _argument_count = args.len();
    Err(MusiError::CommandUnavailable { command, feature })
}
