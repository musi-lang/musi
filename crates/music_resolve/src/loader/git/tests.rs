use std::path::PathBuf;

use super::*;

#[test]
fn parse_basic_specifier() {
    let spec = GitSpecifier::parse("github.com/musi-lang/std").unwrap();
    assert_eq!(spec.domain, "github.com");
    assert_eq!(spec.owner, "musi-lang");
    assert_eq!(spec.repo, "std");
    assert_eq!(spec.version, None);
}

#[test]
fn parse_specifier_with_version() {
    let spec = GitSpecifier::parse("github.com/musi-lang/std@v0.1.0").unwrap();
    assert_eq!(spec.domain, "github.com");
    assert_eq!(spec.owner, "musi-lang");
    assert_eq!(spec.repo, "std");
    assert_eq!(spec.version, Some("v0.1.0".to_owned()));
}

#[test]
fn parse_specifier_with_branch() {
    let spec = GitSpecifier::parse("gitlab.com/user/pkg@main").unwrap();
    assert_eq!(spec.domain, "gitlab.com");
    assert_eq!(spec.owner, "user");
    assert_eq!(spec.repo, "pkg");
    assert_eq!(spec.version, Some("main".to_owned()));
}

#[test]
fn parse_empty_returns_none() {
    assert!(GitSpecifier::parse("").is_none());
}

#[test]
fn parse_missing_domain_dot_returns_none() {
    // "github" has no dot -- not a valid domain
    assert!(GitSpecifier::parse("github/musi-lang/std").is_none());
}

#[test]
fn parse_too_few_segments_returns_none() {
    assert!(GitSpecifier::parse("github.com/musi-lang").is_none());
}

#[test]
fn parse_too_many_segments_returns_none() {
    assert!(GitSpecifier::parse("github.com/a/b/c").is_none());
}

#[test]
fn parse_empty_owner_returns_none() {
    assert!(GitSpecifier::parse("github.com//std").is_none());
}

#[test]
fn parse_empty_repo_returns_none() {
    assert!(GitSpecifier::parse("github.com/musi-lang/").is_none());
}

#[test]
fn clone_url_format() {
    let spec = GitSpecifier::parse("github.com/musi-lang/std").unwrap();
    assert_eq!(spec.clone_url(), "https://github.com/musi-lang/std");
}

#[test]
fn cache_path_without_version() {
    let spec = GitSpecifier::parse("github.com/musi-lang/std").unwrap();
    let cache = spec.cache_path(&PathBuf::from("/cache"));
    assert_eq!(
        cache,
        PathBuf::from("/cache/github.com/musi-lang/std/latest")
    );
}

#[test]
fn cache_path_with_version() {
    let spec = GitSpecifier::parse("github.com/musi-lang/std@v0.1.0").unwrap();
    let cache = spec.cache_path(&PathBuf::from("/cache"));
    assert_eq!(
        cache,
        PathBuf::from("/cache/github.com/musi-lang/std/v0.1.0")
    );
}

#[test]
fn find_entry_point_with_musi_json() {
    let dir = tempfile::tempdir().unwrap();
    fs::write(dir.path().join("musi.json"), r#"{ "main": "src/lib.ms" }"#).unwrap();
    fs::create_dir_all(dir.path().join("src")).unwrap();
    fs::write(dir.path().join("src/lib.ms"), "// lib").unwrap();

    let entry = find_entry_point(dir.path()).unwrap();
    assert_eq!(entry, dir.path().join("src/lib.ms"));
}

#[test]
fn find_entry_point_with_main_no_extension() {
    let dir = tempfile::tempdir().unwrap();
    fs::write(dir.path().join("musi.json"), r#"{ "main": "lib" }"#).unwrap();
    fs::write(dir.path().join("lib.ms"), "// lib").unwrap();

    let entry = find_entry_point(dir.path()).unwrap();
    assert_eq!(entry, dir.path().join("lib.ms"));
}

#[test]
fn find_entry_point_fallback_index_ms() {
    let dir = tempfile::tempdir().unwrap();
    fs::write(dir.path().join("index.ms"), "// index").unwrap();

    let entry = find_entry_point(dir.path()).unwrap();
    assert_eq!(entry, dir.path().join("index.ms"));
}

#[test]
fn find_entry_point_none_when_empty() {
    let dir = tempfile::tempdir().unwrap();
    assert!(find_entry_point(dir.path()).is_none());
}
