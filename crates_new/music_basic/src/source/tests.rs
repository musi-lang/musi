use std::path::Path;

use super::{Source, SourceId, SourceMap};

#[test]
fn single_line_line_col() {
    let mut map = SourceMap::default();
    let id = map.add("test.ms", "hello");
    let src = map.get(id).expect("source exists");
    assert_eq!(src.line_col(0), (1, 1));
    assert_eq!(src.line_col(4), (1, 5));
}

#[test]
fn multi_line_line_col() {
    let mut map = SourceMap::default();
    let id = map.add("test.ms", "ab\ncd\nef");
    let src = map.get(id).expect("source exists");
    assert_eq!(src.line_col(0), (1, 1));
    assert_eq!(src.line_col(1), (1, 2));
    assert_eq!(src.line_col(2), (1, 3));
    assert_eq!(src.line_col(3), (2, 1));
    assert_eq!(src.line_col(6), (3, 1));
}

#[test]
fn line_col_at_newline_boundary() {
    let mut map = SourceMap::default();
    let id = map.add("test.ms", "a\nb");
    let src = map.get(id).expect("source exists");
    assert_eq!(src.line_col(1), (1, 2));
    assert_eq!(src.line_col(2), (2, 1));
}

#[test]
fn line_text_returns_correct_lines() {
    let mut map = SourceMap::default();
    let id = map.add("test.ms", "first\nsecond\nthird");
    let src = map.get(id).expect("source exists");
    assert_eq!(src.line_text(1), Some("first"));
    assert_eq!(src.line_text(2), Some("second"));
    assert_eq!(src.line_text(3), Some("third"));
}

#[test]
fn line_text_out_of_range() {
    let mut map = SourceMap::default();
    let id = map.add("test.ms", "hello");
    let src = map.get(id).expect("source exists");
    assert_eq!(src.line_text(0), None);
    assert_eq!(src.line_text(2), None);
}

#[test]
fn line_count_single_line() {
    let mut map = SourceMap::default();
    let id = map.add("test.ms", "hello");
    let src = map.get(id).expect("source exists");
    assert_eq!(src.line_count(), 1);
}

#[test]
fn line_count_multi_line() {
    let mut map = SourceMap::default();
    let id = map.add("test.ms", "a\nb\nc");
    let src = map.get(id).expect("source exists");
    assert_eq!(src.line_count(), 3);
}

#[test]
fn line_count_trailing_newline() {
    let mut map = SourceMap::default();
    let id = map.add("test.ms", "a\n");
    let src = map.get(id).expect("source exists");
    assert_eq!(src.line_count(), 2);
}

#[test]
fn source_map_round_trip() {
    let mut map = SourceMap::default();
    let id1 = map.add("a.ms", "aaa");
    let id2 = map.add("b.ms", "bbb");

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
fn empty_source() {
    let mut map = SourceMap::default();
    let id = map.add("empty.ms", "");
    let src = map.get(id).expect("source exists");
    assert_eq!(src.line_count(), 1);
    assert_eq!(src.line_text(1), Some(""));
    assert_eq!(src.text(), "");
}

#[test]
fn source_map_iter() {
    let mut map = SourceMap::default();
    let _id1 = map.add("a.ms", "aaa");
    let _id2 = map.add("b.ms", "bbb");
    let paths: Vec<&Path> = map.iter().map(Source::path).collect();
    assert_eq!(paths, vec![Path::new("a.ms"), Path::new("b.ms")]);
}

#[test]
fn source_id_raw() {
    let id = SourceId(5);
    assert_eq!(id.raw(), 5);
}

#[test]
fn source_span_covers_full_text() {
    let mut map = SourceMap::default();
    let id = map.add("test.ms", "hello");
    let src = map.get(id).expect("source exists");
    let span = src.span();
    assert_eq!(span.start, 0);
    assert_eq!(span.end, 5);
}

#[test]
fn source_map_get_invalid_id_returns_none() {
    let map = SourceMap::default();
    assert!(map.get(SourceId(0)).is_none());
    assert!(map.get(SourceId(99)).is_none());
}

#[test]
fn line_col_at_eof() {
    let mut map = SourceMap::default();
    let id = map.add("test.ms", "hello");
    let src = map.get(id).expect("source exists");
    let (line, col) = src.line_col(5);
    assert_eq!(line, 1);
    assert_eq!(col, 6);
}

#[test]
fn line_col_past_eof() {
    let mut map = SourceMap::default();
    let id = map.add("test.ms", "hello");
    let src = map.get(id).expect("source exists");
    let (line, col) = src.line_col(100);
    assert_eq!(line, 1);
    assert_eq!(col, 6);
}

#[test]
fn line_text_strips_cr() {
    let mut map = SourceMap::default();
    let id = map.add("test.ms", "first\r\nsecond\r\nthird");
    let src = map.get(id).expect("source exists");
    assert_eq!(src.line_text(1), Some("first"));
    assert_eq!(src.line_text(2), Some("second"));
    assert_eq!(src.line_text(3), Some("third"));
}

#[test]
fn source_id_display() {
    let id = SourceId(5);
    assert_eq!(format!("{id}"), "5");
}
