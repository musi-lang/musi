use std::path::Path;

use crate::types::OptEmitKind;

pub fn build(project: &Path, _emit: OptEmitKind) {
    let config_path = project.join("mspackage.json");
    if !config_path.exists() {
        super::util::error(&format!(
            "no 'mspackage.json' found in {}",
            project.display()
        ));
        return;
    }
    eprintln!("note: project build not yet implemented");
}
