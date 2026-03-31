use std::path::Path;

use super::{Source, SourceId, SourceMap};

#[test]
fn test_single_line_line_col() {
    let mut map = SourceMap::default();
    let id = map.add("test.ms", "hello").expect("add succeeds");
    let src = map.get(id).expect("source exists");
    assert_eq!(src.line_col(0), (1, 1));
    assert_eq!(src.line_col(4), (1, 5));
}

#[test]
fn test_multi_line_line_col() {
    let mut map = SourceMap::default();
    let id = map.add("test.ms", "ab\ncd\nef").expect("add succeeds");
    let src = map.get(id).expect("source exists");
    assert_eq!(src.line_col(0), (1, 1));
    assert_eq!(src.line_col(1), (1, 2));
    assert_eq!(src.line_col(2), (1, 3));
    assert_eq!(src.line_col(3), (2, 1));
    assert_eq!(src.line_col(6), (3, 1));
}

#[test]
fn test_line_col_at_newline_boundary() {
    let mut map = SourceMap::default();
    let id = map.add("test.ms", "a\nb").expect("add succeeds");
    let src = map.get(id).expect("source exists");
    assert_eq!(src.line_col(1), (1, 2));
    assert_eq!(src.line_col(2), (2, 1));
}

#[test]
fn test_line_text_returns_correct_lines() {
    let mut map = SourceMap::default();
    let id = map
        .add("test.ms", "first\nsecond\nthird")
        .expect("add succeeds");
    let src = map.get(id).expect("source exists");
    assert_eq!(src.line_text(1), Some("first"));
    assert_eq!(src.line_text(2), Some("second"));
    assert_eq!(src.line_text(3), Some("third"));
}

#[test]
fn test_line_text_out_of_range() {
    let mut map = SourceMap::default();
    let id = map.add("test.ms", "hello").expect("add succeeds");
    let src = map.get(id).expect("source exists");
    assert_eq!(src.line_text(0), None);
    assert_eq!(src.line_text(2), None);
}

#[test]
fn test_line_count_trailing_newline() {
    let mut map = SourceMap::default();
    let id = map.add("test.ms", "a\n").expect("add succeeds");
    let src = map.get(id).expect("source exists");
    assert_eq!(src.line_count(), 2);
}

#[test]
fn test_source_map_round_trip() {
    let mut map = SourceMap::default();
    let id1 = map.add("a.ms", "aaa").expect("add succeeds");
    let id2 = map.add("b.ms", "bbb").expect("add succeeds");

    let s1 = map.get(id1).expect("source exists");
    assert_eq!(s1.path(), Path::new("a.ms"));
    assert_eq!(s1.text(), "aaa");
    assert_eq!(s1.id(), id1);

    let s2 = map.get(id2).expect("source exists");
    assert_eq!(s2.path(), Path::new("b.ms"));
    assert_eq!(s2.text(), "bbb");
    assert_eq!(s2.id(), id2);
}

#[test]
fn test_empty_source() {
    let mut map = SourceMap::default();
    let id = map.add("empty.ms", "").expect("add succeeds");
    let src = map.get(id).expect("source exists");
    assert_eq!(src.line_count(), 1);
    assert_eq!(src.line_text(1), Some(""));
    assert_eq!(src.text(), "");
}

#[test]
fn test_source_map_iter() {
    let mut map = SourceMap::default();
    let _id1 = map.add("a.ms", "aaa").expect("add succeeds");
    let _id2 = map.add("b.ms", "bbb").expect("add succeeds");
    let paths: Vec<&Path> = map.iter().map(Source::path).collect();
    assert_eq!(paths, vec![Path::new("a.ms"), Path::new("b.ms")]);
}

#[test]
fn test_source_id_raw_and_display() {
    let id = SourceId(5);
    assert_eq!(id.raw(), 5);
    assert_eq!(format!("{id}"), "5");
}

#[test]
fn test_source_span_covers_full_text() {
    let mut map = SourceMap::default();
    let id = map.add("test.ms", "hello").expect("add succeeds");
    let src = map.get(id).expect("source exists");
    let span = src.span();
    assert_eq!(span.start, 0);
    assert_eq!(span.end, 5);
}

#[test]
fn test_source_map_get_invalid_id_returns_none() {
    let map = SourceMap::default();
    assert!(map.get(SourceId(0)).is_none());
    assert!(map.get(SourceId(99)).is_none());
}

#[test]
fn test_line_col_at_eof_and_past_eof() {
    let mut map = SourceMap::default();
    let id = map.add("test.ms", "hello").expect("add succeeds");
    let src = map.get(id).expect("source exists");
    assert_eq!(src.line_col(5), (1, 6));
    assert_eq!(src.line_col(100), (1, 6));
}

#[test]
fn test_line_text_strips_cr() {
    let mut map = SourceMap::default();
    let id = map
        .add("test.ms", "first\r\nsecond\r\nthird")
        .expect("add succeeds");
    let src = map.get(id).expect("source exists");
    assert_eq!(src.line_text(1), Some("first"));
    assert_eq!(src.line_text(2), Some("second"));
    assert_eq!(src.line_text(3), Some("third"));
}
