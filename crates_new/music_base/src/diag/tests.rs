use crate::{SourceId, SourceMap, Span};

use super::*;

fn test_make_source_map() -> (SourceMap, SourceId) {
    let mut map = SourceMap::default();
    let id = map
        .add("src/main.ms", "let x := 42 + y\nlet z := 10")
        .expect("add succeeds");
    (map, id)
}

fn test_emit_to_string(diag: &Diag, sources: &SourceMap, use_color: bool) -> String {
    let mut buf = Vec::new();
    emit(&mut buf, diag, sources, use_color).expect("emit succeeds");
    String::from_utf8(buf).expect("valid utf8")
}

#[test]
fn test_builder_chain() {
    let (_, source_id) = test_make_source_map();
    let diag = Diag::error("expected ';' after expression")
        .with_code(DiagCode::new(2001))
        .with_label(Span::new(16, 17), source_id, "here")
        .with_note("add semicolon");

    assert_eq!(diag.level, DiagLevel::Error);
    assert_eq!(diag.code, Some(DiagCode::new(2001)));
    assert_eq!(diag.message, "expected ';' after expression");
    assert_eq!(diag.labels.len(), 1);
    assert_eq!(diag.labels[0].message, "here");
    assert_eq!(diag.notes.len(), 1);
    assert_eq!(diag.notes[0], "add semicolon");
}

#[test]
fn test_emit_colorless_matches_expected_format() {
    let (sources, source_id) = test_make_source_map();
    let diag = Diag::error("expected ';' after expression")
        .with_code(DiagCode::new(2001))
        .with_label(Span::new(15, 16), source_id, "");

    let output = test_emit_to_string(&diag, &sources, false);

    let expected = concat!(
        "src/main.ms:1:16: error[ms2001]: expected ';' after expression\n",
        "  |\n",
        "1 | let x := 42 + y\n",
        "  |                ^\n",
    );
    assert_eq!(output, expected);
}

#[test]
fn test_emit_colored_includes_ansi_codes() {
    let (sources, source_id) = test_make_source_map();
    let diag = Diag::error("bad thing").with_label(Span::new(0, 3), source_id, "");

    let output = test_emit_to_string(&diag, &sources, true);

    assert!(output.contains(DiagColor::Red.ansi_code()));
    assert!(output.contains(DiagColor::Reset.ansi_code()));
}

#[test]
fn test_no_labels_just_message() {
    let sources = SourceMap::default();
    let diag = Diag::error("something went wrong").with_code(DiagCode::new(2999));

    let output = test_emit_to_string(&diag, &sources, false);

    assert_eq!(output, "error[ms2999]: something went wrong\n");
}

#[test]
fn test_fatal_produces_correct_label() {
    let sources = SourceMap::default();
    let diag = Diag::fatal("internal compiler failure");

    let output = test_emit_to_string(&diag, &sources, false);

    assert!(output.starts_with("fatal error: "));
}

#[test]
fn test_warning_produces_correct_label() {
    let sources = SourceMap::default();
    let diag = Diag::warning("unused variable");

    let output = test_emit_to_string(&diag, &sources, false);

    assert!(output.starts_with("warning: "));
}

#[test]
fn test_note_level_produces_correct_label() {
    let sources = SourceMap::default();
    let diag = Diag::note("defined here");

    let output = test_emit_to_string(&diag, &sources, false);

    assert!(output.starts_with("note: "));
}

#[test]
fn test_label_message_appears_after_caret() {
    let (sources, source_id) = test_make_source_map();
    let diag = Diag::error("expected type 'Int', found type 'String'").with_label(
        Span::new(13, 14),
        source_id,
        "expected Int",
    );

    let output = test_emit_to_string(&diag, &sources, false);

    assert!(output.contains("^ expected Int"));
}

#[test]
fn test_multi_label_diagnostic() {
    let (sources, source_id) = test_make_source_map();
    let diag = Diag::error("expected type 'Int', found type 'String'")
        .with_label(Span::new(0, 3), source_id, "first")
        .with_label(Span::new(17, 20), source_id, "second");

    let output = test_emit_to_string(&diag, &sources, false);

    assert_eq!(output.match_indices("src/main.ms").count(), 2);
}

#[test]
fn test_note_sub_diagnostic_appears() {
    let (sources, source_id) = test_make_source_map();
    let diag = Diag::error("missing semicolon")
        .with_label(Span::new(0, 3), source_id, "")
        .with_note("try adding ';' here");

    let output = test_emit_to_string(&diag, &sources, false);

    assert!(output.contains("note: try adding ';' here"));
}

#[test]
fn test_hint_is_appended_to_headline() {
    let (sources, source_id) = test_make_source_map();
    let diag = Diag::error("undefined binding 'writein'")
        .with_code(DiagCode::new(3001))
        .with_hint("did you mean 'writeln'?")
        .with_label(Span::new(0, 7), source_id, "");

    let output = test_emit_to_string(&diag, &sources, false);
    assert!(output.contains("error[ms3001]: undefined binding 'writein'; did you mean 'writeln'?"));
}

#[test]
fn test_diag_level_label_values() {
    assert_eq!(DiagLevel::Fatal.label(), "fatal error");
    assert_eq!(DiagLevel::Error.label(), "error");
    assert_eq!(DiagLevel::Warning.label(), "warning");
    assert_eq!(DiagLevel::Note.label(), "note");
}

#[test]
fn test_diag_level_color_values() {
    assert_eq!(DiagLevel::Fatal.color(), DiagColor::Purple);
    assert_eq!(DiagLevel::Error.color(), DiagColor::Red);
    assert_eq!(DiagLevel::Warning.color(), DiagColor::Yellow);
    assert_eq!(DiagLevel::Note.color(), DiagColor::Cyan);
}

#[test]
fn test_multi_char_span_produces_multiple_carets() {
    let (sources, source_id) = test_make_source_map();
    let diag = Diag::error("bad identifier").with_label(Span::new(0, 3), source_id, "");

    let output = test_emit_to_string(&diag, &sources, false);

    assert!(output.contains("^^^"));
}

#[test]
fn test_zero_length_span_produces_single_caret() {
    let (sources, source_id) = test_make_source_map();
    let diag = Diag::error("missing something").with_label(Span::new(0, 0), source_id, "");

    let output = test_emit_to_string(&diag, &sources, false);

    assert!(output.contains('^'));
}
