use std::collections::{HashMap, HashSet, VecDeque};
use std::process::{self, Command, Stdio};

use clap::Args;

use crate::compiler;
use crate::config::TaskEntry;

#[derive(Args)]
pub(crate) struct TaskArgs {
    /// Name of the task to run
    pub(crate) name: String,
}

pub(crate) fn run(args: TaskArgs) {
    let (cfg, _) = compiler::load_project_config().unwrap_or_else(|| {
        eprintln!("error: no mspackage.json found");
        process::exit(1);
    });

    if cfg.tasks.is_empty() {
        eprintln!("error: no tasks defined in mspackage.json");
        process::exit(1);
    }

    let order = topo_sort(&args.name, &cfg.tasks);

    for task_name in &order {
        let entry = &cfg.tasks[task_name];
        let cmd_str = entry.command();
        println!("$ {cmd_str}");
        let status = Command::new("sh")
            .arg("-c")
            .arg(cmd_str)
            .stdin(Stdio::inherit())
            .stdout(Stdio::inherit())
            .stderr(Stdio::inherit())
            .status()
            .unwrap_or_else(|e| {
                eprintln!("error: failed to spawn task '{task_name}': {e}");
                process::exit(1);
            });
        if !status.success() {
            eprintln!(
                "error: task '{task_name}' failed (exit {})",
                status.code().unwrap_or(1)
            );
            process::exit(status.code().unwrap_or(1));
        }
    }
}

/// Topological sort via Kahn's algorithm. Returns tasks in dependency-first order.
fn topo_sort(start: &str, tasks: &HashMap<String, TaskEntry>) -> Vec<String> {
    if !tasks.contains_key(start) {
        let mut available: Vec<&str> = tasks.keys().map(String::as_str).collect();
        available.sort_unstable();
        eprintln!(
            "error: unknown task '{start}'. Available tasks: {}",
            available.join(", ")
        );
        process::exit(1);
    }

    // Collect all reachable tasks and build in-degree map.
    let mut in_degree: HashMap<&str, usize> = HashMap::new();
    let mut edges: HashMap<&str, Vec<&str>> = HashMap::new();
    let mut visited = HashSet::new();
    let mut stack = vec![start];

    while let Some(name) = stack.pop() {
        if !visited.insert(name) {
            continue;
        }
        let entry = tasks.get(name).unwrap_or_else(|| {
            eprintln!("error: task '{name}' (dependency) not found in mspackage.json");
            process::exit(1);
        });
        let _ = in_degree.entry(name).or_insert(0);
        for dep in entry.dependencies() {
            let dep_str = dep.as_str();
            // Edge: dep -> name (dep must run before name).
            edges.entry(dep_str).or_default().push(name);
            *in_degree.entry(name).or_insert(0) += 1;
            stack.push(dep_str);
        }
    }

    // Kahn's algorithm.
    let mut queue: VecDeque<&str> = in_degree
        .iter()
        .filter(|&(_, &deg)| deg == 0)
        .map(|(&name, _)| name)
        .collect();
    let mut result = Vec::with_capacity(in_degree.len());

    while let Some(name) = queue.pop_front() {
        result.push(name.to_owned());
        if let Some(dependents) = edges.get(name) {
            for &dep in dependents {
                let deg = in_degree.get_mut(dep).unwrap();
                *deg -= 1;
                if *deg == 0 {
                    queue.push_back(dep);
                }
            }
        }
    }

    if result.len() != in_degree.len() {
        // Find cycle participants for the error message.
        let in_cycle: Vec<&str> = in_degree
            .iter()
            .filter(|&(_, &deg)| deg > 0)
            .map(|(&name, _)| name)
            .collect();
        eprintln!("error: task cycle detected: {}", in_cycle.join(" -> "));
        process::exit(1);
    }

    result
}
