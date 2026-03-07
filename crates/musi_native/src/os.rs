#![allow(clippy::absolute_paths)]
use musi_macros::musi_module;

#[musi_module]
pub mod os {
    pub fn os_platform() -> String {
        std::env::consts::OS.to_owned()
    }

    pub fn os_arch() -> String {
        std::env::consts::ARCH.to_owned()
    }

    pub fn os_home_dir() -> Option<String> {
        std::env::var("HOME")
            .or_else(|_| std::env::var("USERPROFILE"))
            .ok()
    }

    pub fn os_hostname() -> String {
        std::env::var("HOSTNAME")
            .or_else(|_| std::fs::read_to_string("/etc/hostname").map(|s| s.trim().to_owned()))
            .unwrap_or_else(|_| String::from("localhost"))
    }

    pub fn os_cpu_count() -> i64 {
        std::thread::available_parallelism()
            .ok()
            .and_then(|n| i64::try_from(n.get()).ok())
            .unwrap_or(1)
    }

    pub fn os_tmp_dir() -> String {
        std::env::var("TMPDIR")
            .or_else(|_| std::env::var("TMP"))
            .or_else(|_| std::env::var("TEMP"))
            .unwrap_or_else(|_| String::from("/tmp"))
    }
}
