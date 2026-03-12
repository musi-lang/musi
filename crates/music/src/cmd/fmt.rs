use std::path::PathBuf;
use std::process;

use musi_manifest::MusiManifest;

pub fn run(_files: &[PathBuf], _check: bool, _manifest: Option<&MusiManifest>) -> ! {
    eprintln!("error: `music fmt` is not yet implemented");
    process::exit(1)
}
