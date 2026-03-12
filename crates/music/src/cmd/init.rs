use std::env;
use std::process;

pub fn run(template: &str) -> ! {
    let dir = env::current_dir().unwrap_or_else(|e| {
        eprintln!("error: cannot determine current directory: {e}");
        process::exit(1);
    });

    if dir.join("mspackage.toml").exists() {
        eprintln!("error: mspackage.toml already exists in current directory");
        process::exit(1);
    }

    let name = dir
        .file_name()
        .and_then(|n| n.to_str())
        .unwrap_or("project")
        .to_owned();

    let name = sanitize_name(&name);

    if let Err(e) = super::new::create_project(&dir, &name, template) {
        eprintln!("error: {e}");
        process::exit(1);
    }

    eprintln!("initialized `{name}` project in current directory");
    process::exit(0)
}

fn sanitize_name(name: &str) -> String {
    let sanitized: String = name
        .chars()
        .map(|c| {
            if c.is_ascii_lowercase() || c.is_ascii_digit() || c == '.' || c == '_' || c == '-' {
                c
            } else if c.is_ascii_uppercase() {
                c.to_ascii_lowercase()
            } else {
                '-'
            }
        })
        .collect();
    if sanitized.is_empty() {
        String::from("project")
    } else {
        sanitized
    }
}
