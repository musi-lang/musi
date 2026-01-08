use std::fs;
use std::path::Path;

pub fn init(path: &Path) {
    if let Err(e) = fs::create_dir_all(path) {
        super::util::error(&format!(
            "failed to create directory '{}': {e}",
            path.display()
        ));
        return;
    }

    let config_path = path.join("mspackage.json");
    if config_path.exists() {
        super::util::error("'mspackage.json' already exists");
        return;
    }

    let template = r#"{
  "name": "my-package",
  "version": "0.1.0",
  "main": "./src/index.ms",
  "compilerOptions": {
    "target": "MS2025",
    "strict": true,
    "outDir": "./dist"
  }
}
"#;

    match fs::write(&config_path, template) {
        Ok(()) => eprintln!("created {}", config_path.display()),
        Err(e) => super::util::error(&format!("failed to create 'mspackage.json': {e}")),
    }
}
