use super::*;
use crate::SourcePosition;
use std::{
    io::Write,
    path::{Path, PathBuf},
};
use tempfile::NamedTempFile;

#[test]
fn test_new_empty() {
    let manager = SourceManager::new();
    assert!(manager.is_empty());
    assert_eq!(manager.len(), 0);
}

#[test]
fn test_default() {
    let manager = SourceManager::default();
    assert!(manager.is_empty());
    assert_eq!(manager.len(), 0);
}

#[test]
fn test_add_single_file() {
    let mut manager = SourceManager::new();
    let mut temp = NamedTempFile::with_suffix(".ms").unwrap();
    writeln!(temp, "content").unwrap();

    let id = manager.add_file(temp.path().to_path_buf()).unwrap();
    assert_eq!(id, 0);
    assert!(!manager.is_empty());
    assert_eq!(manager.len(), 1);
}

#[test]
fn test_add_multiple_files() {
    let mut manager = SourceManager::new();

    let mut temp1 = NamedTempFile::with_suffix(".ms").unwrap();
    writeln!(temp1, "content1").unwrap();
    let id1 = manager.add_file(temp1.path().to_path_buf()).unwrap();

    let mut temp2 = NamedTempFile::with_suffix(".ms").unwrap();
    writeln!(temp2, "content2").unwrap();
    let id2 = manager.add_file(temp2.path().to_path_buf()).unwrap();

    assert_eq!(id1, 0);
    assert_eq!(id2, 1);
    assert_eq!(manager.len(), 2);
}

#[test]
fn test_get_file() {
    let mut manager = SourceManager::new();
    let mut temp = NamedTempFile::with_suffix(".ms").unwrap();
    let path = temp.path().to_path_buf();
    writeln!(temp, "content").unwrap();

    let id = manager.add_file(path.clone()).unwrap();
    let file = manager.get(id);

    assert_eq!(file.path, path);
    assert!(file.contents.contains("content"));
}

#[test]
#[should_panic(expected = "index out of bounds")]
#[allow(clippy::let_underscore_must_use, reason = "test should panic")]
fn test_get_invalid_id() {
    let manager = SourceManager::new();
    let _ = manager.get(0);
}

#[test]
fn test_locate() {
    let mut manager = SourceManager::new();
    let mut temp = NamedTempFile::with_suffix(".ms").unwrap();
    writeln!(temp, "line1\nline2").unwrap();

    let id = manager.add_file(temp.path().to_path_buf()).unwrap();
    let span = SourceSpan::new(SourcePosition::new(0), SourcePosition::new(5));

    let (path, line, col) = manager.locate(span, id);
    assert!(
        Path::new(&path)
            .extension()
            .is_some_and(|ext| ext.eq_ignore_ascii_case("ms"))
    );
    assert_eq!(line, 1);
    assert_eq!(col, 1);
}

#[test]
fn test_locate_second_line() {
    let mut manager = SourceManager::new();
    let mut temp = NamedTempFile::with_suffix(".ms").unwrap();
    writeln!(temp, "line1\nline2").unwrap();

    let id = manager.add_file(temp.path().to_path_buf()).unwrap();
    let span = SourceSpan::new(SourcePosition::new(6), SourcePosition::new(10));

    let (_, line, col) = manager.locate(span, id);
    assert_eq!(line, 2);
    assert_eq!(col, 1);
}

#[test]
fn test_add_file_invalid_extension() {
    let mut manager = SourceManager::new();
    let result = manager.add_file(PathBuf::from("test.txt"));

    assert!(result.is_err());
    assert!(manager.is_empty());
}
