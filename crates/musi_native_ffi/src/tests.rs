#![allow(unused_imports)]

use super::ffi::default_ffi_abi;
use super::loader::library_candidates;

mod success {
    use super::*;

    #[test]
    fn c_runtime_link_uses_platform_candidates() {
        let candidates = library_candidates("c");
        #[cfg(target_os = "macos")]
        assert_eq!(
            candidates,
            vec!["libSystem.B.dylib", "libc.dylib", "libc.so"]
        );
        #[cfg(target_os = "linux")]
        assert_eq!(candidates, vec!["libc.so.6", "libc.so"]);
        #[cfg(target_os = "windows")]
        assert_eq!(candidates, vec!["ucrtbase.dll", "msvcrt.dll"]);
    }

    #[test]
    fn generic_library_name_keeps_default_candidates() {
        assert_eq!(
            library_candidates("sqlite3"),
            vec![
                "sqlite3".to_owned(),
                "libsqlite3.dylib".to_owned(),
                "libsqlite3.so".to_owned(),
            ]
        );
    }

    #[test]
    fn explicit_path_is_preserved() {
        assert_eq!(
            library_candidates("/usr/lib/libSystem.B.dylib"),
            vec!["/usr/lib/libSystem.B.dylib".to_owned()]
        );
    }

    #[test]
    fn default_ffi_abi_matches_current_target() {
        #[cfg(all(target_arch = "x86_64", not(target_os = "windows")))]
        assert_eq!(default_ffi_abi(), 2);
        #[cfg(all(target_arch = "x86_64", target_os = "windows", target_env = "gnu"))]
        assert_eq!(default_ffi_abi(), 2);
        #[cfg(all(target_arch = "x86_64", target_os = "windows", not(target_env = "gnu")))]
        assert_eq!(default_ffi_abi(), 1);
        #[cfg(all(target_arch = "aarch64", not(target_os = "windows")))]
        assert_eq!(default_ffi_abi(), 1);
        #[cfg(all(target_arch = "aarch64", target_os = "windows"))]
        assert_eq!(default_ffi_abi(), 2);
    }
}

mod failure {
    use super::*;

    #[test]
    fn library_name_with_directory_does_not_add_platform_variants() {
        assert_eq!(
            library_candidates("vendor/sqlite3"),
            vec!["vendor/sqlite3".to_owned()]
        );
    }
}
