use std::path::PathBuf;
use std::process;

use musi_manifest::MusiManifest;

pub fn run(_files: &[PathBuf], _manifest: Option<&MusiManifest>) -> ! {
    eprintln!("music lint is planned but not yet available");
    process::exit(2)
}
