use std::process;

use musi_manifest::MusiManifest;

pub fn run(_filter: Option<&str>, _manifest: Option<&MusiManifest>) -> ! {
    eprintln!("error: `music test` is not yet implemented");
    process::exit(1)
}
