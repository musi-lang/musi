use std::collections::HashSet;
use std::process;

use musi_manifest::MusiManifest;
use musi_manifest::TaskDef;

pub fn run(name: Option<&str>, list: bool, manifest: Option<&MusiManifest>) -> ! {
    let manifest = manifest.unwrap_or_else(|| {
        eprintln!("error: no mspackage.toml found in current directory");
        process::exit(1);
    });

    if manifest.tasks.is_empty() {
        eprintln!("no tasks defined in mspackage.toml");
        process::exit(0);
    }

    if list || name.is_none() {
        list_tasks(manifest);
        process::exit(0);
    }

    let task_name = name.expect("task name required");
    run_task(manifest, task_name, &mut HashSet::new());
    process::exit(0)
}

fn list_tasks(manifest: &MusiManifest) {
    eprintln!("available tasks:");
    let mut names: Vec<&String> = manifest.tasks.keys().collect();
    names.sort();
    for name in names {
        if let Some(task) = manifest.tasks.get(name) {
            let desc = match task {
                TaskDef::Simple(cmd) => cmd.as_str(),
                TaskDef::Complex(t) => {
                    if t.description.is_empty() {
                        t.command.as_str()
                    } else {
                        t.description.as_str()
                    }
                }
            };
            eprintln!("  {name}: {desc}");
        }
    }
}

fn run_task(manifest: &MusiManifest, name: &str, visited: &mut HashSet<String>) {
    if !visited.insert(name.to_owned()) {
        eprintln!("error: circular task dependency detected: `{name}`");
        process::exit(1);
    }

    let Some(task) = manifest.tasks.get(name) else {
        eprintln!("error: unknown task `{name}`");
        eprintln!("run `music task --list` to see available tasks");
        process::exit(1);
    };

    let (cmd, deps) = match task {
        TaskDef::Simple(cmd) => (cmd.as_str(), vec![]),
        TaskDef::Complex(t) => (t.command.as_str(), t.dependencies.clone()),
    };

    for dep in &deps {
        run_task(manifest, dep, visited);
    }

    eprintln!("$ {cmd}");
    let status = process::Command::new("sh").arg("-c").arg(cmd).status();

    match status {
        Ok(s) if s.success() => {}
        Ok(s) => {
            let code = s.code().unwrap_or(1);
            eprintln!("error: task `{name}` failed with exit code {code}");
            process::exit(code);
        }
        Err(e) => {
            eprintln!("error: failed to execute task `{name}`: {e}");
            process::exit(1);
        }
    }
}
