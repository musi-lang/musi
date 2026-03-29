use crate::{SourceId, SourceMap, Span};

use super::*;

fn make_source_map() -> (SourceMap, SourceId) {
    let mut map = SourceMap::default();
    let id = map.add("src/main.ms", "let x := 42 + y\nlet z := 10");
    (map, id)
}

fn emit_to_string(diag: &Diag, sources: &SourceMap, use_color: bool) -> String {
    let mut buf = vec![];
    emit(&mut buf, diag, sources, use_color).expect("emit succeeds");
    String::from_utf8(buf).expect("valid utf8")
}

#[test]
fn builder_chain() {
    let (_, source_id) = make_source_map();
    let diag = Diag::error("expected ';' after expression")
        .with_code(DiagCode::new(2001))
        .with_label(Span::new(16, 17), source_id, "here")
        .with_note("add a semicolon");

    assert_eq!(diag.level, DiagLevel::Error);
    assert_eq!(diag.code, Some(DiagCode::new(2001)));
    assert_eq!(diag.message, "expected ';' after expression");
    assert_eq!(diag.labels.len(), 1);
    assert_eq!(diag.labels[0].message, "here");
    assert_eq!(diag.notes.len(), 1);
    assert_eq!(diag.notes[0], "add a semicolon");
}

#[test]
fn emit_colorless_matches_expected_format() {
    let (sources, source_id) = make_source_map();
    let diag = Diag::error("expected ';' after expression")
        .with_code(DiagCode::new(2001))
        .with_label(Span::new(15, 16), source_id, "");

    let output = emit_to_string(&diag, &sources, false);

    assert!(output.contains("src/main.ms:1:16: error[ms2001]: expected ';' after expression"));
    assert!(output.contains("1 | let x := 42 + y"));
    assert!(output.contains('^'));
}

#[test]
fn emit_colored_includes_ansi_codes() {
    let (sources, source_id) = make_source_map();
    let diag = Diag::error("bad thing").with_label(Span::new(0, 3), source_id, "");

    let output = emit_to_string(&diag, &sources, true);

    assert!(output.contains(DiagColor::Red.ansi_code()));
    assert!(output.contains(DiagColor::Reset.ansi_code()));
}

#[test]
fn no_labels_just_message() {
    let sources = SourceMap::default();
    let diag = Diag::error("something went wrong").with_code(DiagCode::new(2999));

    let output = emit_to_string(&diag, &sources, false);

    assert_eq!(output, "error[ms2999]: something went wrong\n");
}

#[test]
fn fatal_produces_correct_label() {
    let sources = SourceMap::default();
    let diag = Diag::fatal("internal compiler failure");

    let output = emit_to_string(&diag, &sources, false);

    assert!(output.starts_with("fatal error: "));
}

#[test]
fn warning_produces_correct_label() {
    let sources = SourceMap::default();
    let diag = Diag::warning("unused variable");

    let output = emit_to_string(&diag, &sources, false);

    assert!(output.starts_with("warning: "));
}

#[test]
fn note_level_produces_correct_label() {
    let sources = SourceMap::default();
    let diag = Diag::note("defined here");

    let output = emit_to_string(&diag, &sources, false);

    assert!(output.starts_with("note: "));
}

#[test]
fn label_message_appears_after_caret() {
    let (sources, source_id) = make_source_map();
    let diag =
        Diag::error("type mismatch").with_label(Span::new(13, 14), source_id, "expected Int");

    let output = emit_to_string(&diag, &sources, false);

    assert!(output.contains("^ expected Int"));
}

#[test]
fn multi_label_diagnostic() {
    let (sources, source_id) = make_source_map();
    let diag = Diag::error("type mismatch")
        .with_label(Span::new(0, 3), source_id, "first")
        .with_label(Span::new(17, 20), source_id, "second");

    let output = emit_to_string(&diag, &sources, false);

    assert_eq!(output.match_indices("src/main.ms").count(), 2);
}

#[test]
fn note_sub_diagnostic_appears() {
    let (sources, source_id) = make_source_map();
    let diag = Diag::error("missing semicolon")
        .with_label(Span::new(0, 3), source_id, "")
        .with_note("try adding ';' here");

    let output = emit_to_string(&diag, &sources, false);

    assert!(output.contains("note: try adding ';' here"));
}

#[test]
fn hint_is_appended_to_headline() {
    let (sources, source_id) = make_source_map();
    let diag = Diag::error("undefined binding 'writein'")
        .with_code(DiagCode::new(3001))
        .with_hint("did you mean 'writeln'?")
        .with_label(Span::new(0, 7), source_id, "");

    let output = emit_to_string(&diag, &sources, false);
    assert!(output.contains("error[ms3001]: undefined binding 'writein'; did you mean 'writeln'?"));
}

#[test]
fn diag_level_label_values() {
    assert_eq!(DiagLevel::Fatal.label(), "fatal error");
    assert_eq!(DiagLevel::Error.label(), "error");
    assert_eq!(DiagLevel::Warning.label(), "warning");
    assert_eq!(DiagLevel::Note.label(), "note");
}

#[test]
fn diag_level_color_values() {
    assert_eq!(DiagLevel::Fatal.color(), DiagColor::Purple);
    assert_eq!(DiagLevel::Error.color(), DiagColor::Red);
    assert_eq!(DiagLevel::Warning.color(), DiagColor::Yellow);
    assert_eq!(DiagLevel::Note.color(), DiagColor::Cyan);
}

#[test]
fn multi_char_span_produces_multiple_carets() {
    let (sources, source_id) = make_source_map();
    let diag = Diag::error("bad identifier").with_label(Span::new(0, 3), source_id, "");

    let output = emit_to_string(&diag, &sources, false);

    assert!(output.contains("^^^"));
}

#[test]
fn zero_length_span_produces_single_caret() {
    let (sources, source_id) = make_source_map();
    let diag = Diag::error("insertion point").with_label(Span::new(4, 4), source_id, "");

    let output = emit_to_string(&diag, &sources, false);

    assert!(output.contains('^'));
    let caret_line = output
        .lines()
        .find(|line| line.contains('^'))
        .expect("caret line");
    let caret_count = caret_line.chars().filter(|&ch| ch == '^').count();
    assert_eq!(caret_count, 1);
}
