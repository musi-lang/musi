use crate::error::ResolveError;
use crate::specifier::{GitSource, ImportScheme, parse_git_source, parse_specifier};

#[test]
fn test_parse_specifier_musi_core() {
    let spec = parse_specifier("musi:core").unwrap();
    assert_eq!(spec.scheme, ImportScheme::Musi);
    assert_eq!(&*spec.raw, "musi:core");
    assert_eq!(&*spec.module_path, "core");
}

#[test]
fn test_parse_specifier_musi_nested_path() {
    let spec = parse_specifier("musi:io/print").unwrap();
    assert_eq!(spec.scheme, ImportScheme::Musi);
    assert_eq!(&*spec.module_path, "io/print");
}

#[test]
fn test_parse_specifier_git_with_tag() {
    let spec = parse_specifier("git:github.com/user/repo@v1.0").unwrap();
    assert_eq!(spec.scheme, ImportScheme::Git);
    assert_eq!(&*spec.module_path, "github.com/user/repo@v1.0");
}

#[test]
fn test_parse_specifier_git_without_tag() {
    let spec = parse_specifier("git:github.com/user/repo").unwrap();
    assert_eq!(spec.scheme, ImportScheme::Git);
    assert_eq!(&*spec.module_path, "github.com/user/repo");
}

#[test]
fn test_parse_specifier_msr_returns_error() {
    let err = parse_specifier("msr:my-package").unwrap_err();
    assert!(matches!(err, ResolveError::RegistryNotSupported { .. }));
}

#[test]
fn test_parse_specifier_relative_dot_slash() {
    let spec = parse_specifier("./foo").unwrap();
    assert_eq!(spec.scheme, ImportScheme::Relative);
    assert_eq!(&*spec.module_path, "./foo");
}

#[test]
fn test_parse_specifier_relative_dot_dot_slash() {
    let spec = parse_specifier("../bar/baz").unwrap();
    assert_eq!(spec.scheme, ImportScheme::Relative);
    assert_eq!(&*spec.module_path, "../bar/baz");
}

#[test]
fn test_parse_specifier_bare() {
    let spec = parse_specifier("my-lib").unwrap();
    assert_eq!(spec.scheme, ImportScheme::Bare);
    assert_eq!(&*spec.module_path, "my-lib");
}

#[test]
fn test_parse_specifier_empty_returns_error() {
    let err = parse_specifier("").unwrap_err();
    assert!(matches!(err, ResolveError::EmptyPath));
}

#[test]
fn test_parse_specifier_musi_no_path_returns_error() {
    let err = parse_specifier("musi:").unwrap_err();
    assert!(matches!(err, ResolveError::ModuleNotFound { .. }));
}

#[test]
fn test_parse_specifier_git_no_path_returns_error() {
    let err = parse_specifier("git:").unwrap_err();
    assert!(matches!(err, ResolveError::InvalidGitSpecifier { .. }));
}

#[test]
fn test_parse_git_source_with_tag() {
    let src = parse_git_source("github.com/user/repo@v1.0").unwrap();
    assert_eq!(
        src,
        GitSource {
            url: Box::from("github.com/user/repo"),
            tag: Some(Box::from("v1.0")),
        }
    );
}

#[test]
fn test_parse_git_source_without_tag() {
    let src = parse_git_source("github.com/user/repo").unwrap();
    assert_eq!(
        src,
        GitSource {
            url: Box::from("github.com/user/repo"),
            tag: None,
        }
    );
}

#[test]
fn test_parse_git_source_empty_returns_error() {
    let err = parse_git_source("").unwrap_err();
    assert!(matches!(err, ResolveError::InvalidGitSpecifier { .. }));
}

#[test]
fn test_parse_git_source_trailing_at_returns_error() {
    let err = parse_git_source("github.com/user/repo@").unwrap_err();
    assert!(matches!(err, ResolveError::InvalidGitSpecifier { .. }));
}

#[test]
fn test_parse_git_source_leading_at_returns_error() {
    let err = parse_git_source("@v1.0").unwrap_err();
    assert!(matches!(err, ResolveError::InvalidGitSpecifier { .. }));
}

#[test]
fn test_parse_git_source_multiple_at_uses_last() {
    let src = parse_git_source("github.com/user/repo@feat/branch@v2").unwrap();
    assert_eq!(&*src.url, "github.com/user/repo@feat/branch");
    assert_eq!(src.tag.as_deref(), Some("v2"));
}
