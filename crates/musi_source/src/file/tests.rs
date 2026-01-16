use super::*;
use std::{
    io::{self, Write},
    path::PathBuf,
};
use tempfile::NamedTempFile;

#[test]
fn test_file_invalid_extension() {
    let result = SourceFile::from_path(PathBuf::from("test.txt"));
    assert!(result.is_err());

    let err = result.unwrap_err();
    assert_eq!(err.kind(), io::ErrorKind::InvalidInput);
}

#[test]
fn test_file_nonexistent() {
    let result = SourceFile::from_path(PathBuf::from("nonexistent.ms"));
    assert!(result.is_err());

    let err = result.unwrap_err();
    assert_eq!(err.kind(), io::ErrorKind::NotFound);
}

#[test]
fn test_compute_line_starts_empty() {
    let starts = SourceFile::compute_line_starts("");
    assert_eq!(starts, vec![0]);
}

#[test]
fn test_compute_line_starts_single_line() {
    let starts = SourceFile::compute_line_starts("hello");
    assert_eq!(starts, vec![0]);
}

#[test]
fn test_compute_line_starts_multiple_lines() {
    let content = "line1\nline2\nline3";
    let starts = SourceFile::compute_line_starts(content);
    assert_eq!(starts, vec![0, 6, 12]);
}

#[test]
fn test_compute_line_starts_trailing_newline() {
    let content = "line1\nline2\n";
    let starts = SourceFile::compute_line_starts(content);
    assert_eq!(starts, vec![0, 6, 12]);
}

#[test]
fn test_line_index_exact_match() {
    let content = "line1\nline2\nline3";
    let starts = SourceFile::compute_line_starts(content);
    let file = SourceFile {
        path: PathBuf::from("test.ms"),
        contents: content.to_owned(),
        line_starts: starts,
    };

    assert_eq!(file.line_index(SourcePosition::new(0)), 0);
    assert_eq!(file.line_index(SourcePosition::new(6)), 1);
    assert_eq!(file.line_index(SourcePosition::new(12)), 2);
}

#[test]
fn test_line_index_between_lines() {
    let content = "line1\nline2\nline3";
    let starts = SourceFile::compute_line_starts(content);
    let file = SourceFile {
        path: PathBuf::from("test.ms"),
        contents: content.to_owned(),
        line_starts: starts,
    };

    assert_eq!(file.line_index(SourcePosition::new(3)), 0);
    assert_eq!(file.line_index(SourcePosition::new(8)), 1);
    assert_eq!(file.line_index(SourcePosition::new(15)), 2);
}

#[test]
fn test_line_column_first_line() {
    let content = "line1\nline2\nline3";
    let starts = SourceFile::compute_line_starts(content);
    let file = SourceFile {
        path: PathBuf::from("test.ms"),
        contents: content.to_owned(),
        line_starts: starts,
    };

    assert_eq!(file.line_column(SourcePosition::new(0)), (1, 1));
    assert_eq!(file.line_column(SourcePosition::new(1)), (1, 2));
    assert_eq!(file.line_column(SourcePosition::new(5)), (1, 6));
}

#[test]
fn test_line_column_second_line() {
    let content = "line1\nline2\nline3";
    let starts = SourceFile::compute_line_starts(content);
    let file = SourceFile {
        path: PathBuf::from("test.ms"),
        contents: content.to_owned(),
        line_starts: starts,
    };

    assert_eq!(file.line_column(SourcePosition::new(6)), (2, 1));
    assert_eq!(file.line_column(SourcePosition::new(7)), (2, 2));
    assert_eq!(file.line_column(SourcePosition::new(11)), (2, 6));
}

#[test]
fn test_slice_entire_file() {
    let content = "hello world";
    let starts = SourceFile::compute_line_starts(content);
    let file = SourceFile {
        path: PathBuf::from("test.ms"),
        contents: content.to_owned(),
        line_starts: starts,
    };

    let span = SourceSpan::new(
        SourcePosition::new(0),
        SourcePosition::new(u32::try_from(content.len()).unwrap()),
    );
    assert_eq!(file.slice(span), content);
}

#[test]
fn test_slice_partial() {
    let content = "hello world";
    let starts = SourceFile::compute_line_starts(content);
    let file = SourceFile {
        path: PathBuf::from("test.ms"),
        contents: content.to_owned(),
        line_starts: starts,
    };

    let span = SourceSpan::new(SourcePosition::new(0), SourcePosition::new(5));
    assert_eq!(file.slice(span), "hello");
}

#[test]
fn test_slice_unicode() {
    let content = "hello 世界";
    let starts = SourceFile::compute_line_starts(content);
    let file = SourceFile {
        path: PathBuf::from("test.ms"),
        contents: content.to_owned(),
        line_starts: starts,
    };

    let span = SourceSpan::new(
        SourcePosition::new(6),
        SourcePosition::new(u32::try_from(content.len()).unwrap()),
    );
    assert_eq!(file.slice(span), "世界");
}

#[test]
fn test_slice_empty() {
    let content = "hello world";
    let starts = SourceFile::compute_line_starts(content);
    let file = SourceFile {
        path: PathBuf::from("test.ms"),
        contents: content.to_owned(),
        line_starts: starts,
    };

    let span = SourceSpan::new(SourcePosition::new(5), SourcePosition::new(5));
    assert_eq!(file.slice(span), "");
}

#[test]
fn test_from_path_valid() {
    let mut temp = NamedTempFile::with_suffix(".ms").unwrap();
    writeln!(temp, "test content").unwrap();

    let result = SourceFile::from_path(temp.path().to_path_buf());
    assert!(result.is_ok());

    let file = result.unwrap();
    assert_eq!(file.path, temp.path());
    assert!(file.contents.contains("test content"));
}
