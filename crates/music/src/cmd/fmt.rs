use std::path::PathBuf;
use std::process;

use musi_manifest::MusiManifest;

pub fn run(_files: &[PathBuf], _check: bool, _manifest: Option<&MusiManifest>) -> ! {
    eprintln!("music fmt is planned but not yet available");
    process::exit(2)
}
