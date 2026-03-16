use std::process;

use musi_manifest::MusiManifest;

pub fn run(_filter: Option<&str>, _manifest: Option<&MusiManifest>) -> ! {
    eprintln!("music test is planned but not yet available");
    process::exit(2)
}
