use std::path::PathBuf;
use std::process;

use msc_manifest::MusiManifest;

pub fn run(_files: &[PathBuf], _manifest: Option<&MusiManifest>) -> ! {
    eprintln!("musi lint is planned but not yet available");
    process::exit(2)
}
