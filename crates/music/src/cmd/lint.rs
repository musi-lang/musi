use std::path::PathBuf;
use std::process;

use musi_manifest::MusiManifest;

pub fn run(_files: &[PathBuf], _manifest: Option<&MusiManifest>) -> ! {
    eprintln!("error: `music lint` is not yet implemented");
    process::exit(1)
}
