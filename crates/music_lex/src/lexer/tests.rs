use music_basic::Span;

use crate::token::TokenKinds;

use super::*;

fn test_lex(source: &str) -> LexedSource<'_> {
    Lexer::new(source).lex()
}

fn test_token_kinds(source: &str) -> TokenKinds {
    test_lex(source)
        .tokens()
        .iter()
        .map(|token| token.kind.clone())
        .collect()
}

#[test]
fn test_empty_source_emits_only_eof() {
    assert_eq!(test_token_kinds(""), vec![TokenKind::Eof]);
}

#[test]
fn test_reduced_keywords_are_lexed() {
    assert_eq!(
        test_token_kinds("let quote resume"),
        vec![
            TokenKind::KwLet,
            TokenKind::KwQuote,
            TokenKind::KwResume,
            TokenKind::Eof,
        ]
    );
}

#[test]
fn test_reduced_keyword_table_is_current() {
    assert_eq!(keyword_from_text("let"), Some(TokenKind::KwLet));
    assert_eq!(keyword_from_text("quote"), Some(TokenKind::KwQuote));
    assert_eq!(keyword_from_text("resume"), Some(TokenKind::KwResume));
    assert_eq!(keyword_from_text("return"), None);
    assert_eq!(keyword_from_text("via"), None);
}

#[test]
fn test_compound_table_is_current() {
    assert_eq!(compound_token(b":="), Some((2, TokenKind::ColonEq)));
    assert_eq!(
        compound_token(b":?>"),
        Some((3, TokenKind::ColonQuestionGt))
    );
    assert_eq!(compound_token(b"?.x"), Some((2, TokenKind::QuestionDot)));
    assert_eq!(
        compound_token(b"#[x]"),
        Some((2, TokenKind::SpliceLBracket))
    );
    assert_eq!(compound_token(b"::"), None);
    assert_eq!(compound_token(b".."), None);
    assert_eq!(compound_token(b"..<"), None);
    assert_eq!(compound_token(b"??"), None);
}

#[test]
fn test_removed_keywords_fall_back_to_identifiers() {
    assert_eq!(
        test_token_kinds("return via"),
        vec![TokenKind::Ident, TokenKind::Ident, TokenKind::Eof,]
    );
}

#[test]
fn test_current_compounds_are_lexed() {
    assert_eq!(
        test_token_kinds(":= :? :?> ... .{ .[ ?. !. |> #( #["),
        vec![
            TokenKind::ColonEq,
            TokenKind::ColonQuestion,
            TokenKind::ColonQuestionGt,
            TokenKind::DotDotDot,
            TokenKind::DotLBrace,
            TokenKind::DotLBracket,
            TokenKind::QuestionDot,
            TokenKind::BangDot,
            TokenKind::PipeGt,
            TokenKind::SpliceLParen,
            TokenKind::SpliceLBracket,
            TokenKind::Eof,
        ]
    );
}

#[test]
fn test_splice_prefix_and_operator_chars_lex_as_single_tokens() {
    assert_eq!(
        test_token_kinds("#foo & ^ ~ ++"),
        vec![
            TokenKind::Hash,
            TokenKind::Ident,
            TokenKind::Amp,
            TokenKind::Caret,
            TokenKind::Tilde,
            TokenKind::SymOp,
            TokenKind::Eof,
        ]
    );
}

#[test]
fn test_removed_compounds_do_not_lex_as_single_tokens() {
    let lexed = test_lex(":: .. ..< ??");
    let kinds = lexed
        .tokens()
        .iter()
        .map(|token| token.kind.clone())
        .collect::<Vec<_>>();

    assert_eq!(
        kinds,
        vec![
            TokenKind::Colon,
            TokenKind::Colon,
            TokenKind::Dot,
            TokenKind::Dot,
            TokenKind::Dot,
            TokenKind::Dot,
            TokenKind::Lt,
            TokenKind::Question,
            TokenKind::Question,
            TokenKind::Eof,
        ]
    );
}

#[test]
fn test_identifiers_are_span_based() {
    let lexed = test_lex("alpha beta");
    assert_eq!(lexed.tokens()[0].kind, TokenKind::Ident);
    assert_eq!(lexed.token_text(0), "alpha");
    assert_eq!(lexed.tokens()[1].kind, TokenKind::Ident);
    assert_eq!(lexed.token_text(1), "beta");
}

#[test]
fn test_escaped_identifiers_are_lexed() {
    let lexed = test_lex("`match`");
    assert_eq!(lexed.tokens()[0].kind, TokenKind::EscapedIdent);
    assert_eq!(lexed.token_text(0), "`match`");
}

#[test]
fn test_literals_are_classified_without_payloads() {
    assert_eq!(
        test_token_kinds("42 0xFF 0b10 3.25 \"ok\" 'x'"),
        vec![
            TokenKind::IntLit,
            TokenKind::IntLit,
            TokenKind::IntLit,
            TokenKind::FloatLit,
            TokenKind::StringLit,
            TokenKind::RuneLit,
            TokenKind::Eof,
        ]
    );
}

#[test]
fn test_exponent_without_decimal_point_is_float() {
    let lexed = test_lex("1e10");
    assert_eq!(lexed.tokens()[0].kind, TokenKind::FloatLit);
    assert_eq!(lexed.tokens()[1].kind, TokenKind::Eof);
}

#[test]
fn test_invalid_number_prefix_reports_error() {
    let lexed = test_lex("0x");
    assert_eq!(lexed.errors().len(), 1);
    assert!(matches!(
        lexed.errors()[0].kind,
        LexErrorKind::ExpectedDigitsAfterBasePrefix
    ));
}

#[test]
fn test_invalid_exponent_reports_error() {
    let lexed = test_lex("1.0e+");
    assert_eq!(lexed.errors().len(), 1);
    assert!(matches!(
        lexed.errors()[0].kind,
        LexErrorKind::ExpectedDigitsAfterExponent
    ));
}

#[test]
fn test_leading_dot_float_is_float_lit() {
    let lexed = test_lex(".5");
    assert_eq!(lexed.tokens()[0].kind, TokenKind::FloatLit);
}

#[test]
fn test_digit_separators_are_strict() {
    let lexed = test_lex("1__0 1_ 0x_FF 1e_10 1.0e+_2");
    let invalid = lexed
        .errors()
        .iter()
        .filter(|error| matches!(error.kind, LexErrorKind::InvalidDigitSeparator))
        .count();

    assert!(invalid >= 5);
}

#[test]
fn test_structured_fstring_uses_span_parts() {
    let lexed = test_lex("f\"hi {name} !\"");
    let TokenKind::FStringLit(parts) = &lexed.tokens()[0].kind else {
        panic!("expected f-string token");
    };

    assert_eq!(parts.len(), 3);
    assert_eq!(parts[0].kind, FStringPartKind::Literal);
    assert_eq!(parts[1].kind, FStringPartKind::Interpolation);
    assert_eq!(parts[2].kind, FStringPartKind::Literal);
    assert_eq!(lexed.text(parts[0].span), "hi ");
    assert_eq!(lexed.text(parts[1].span), "name");
    assert_eq!(lexed.text(parts[2].span), " !");
}

#[test]
fn test_fstring_interpolation_handles_nested_braces() {
    let lexed = test_lex("f\"{case x of (| y => { value := y })}\"");
    let TokenKind::FStringLit(parts) = &lexed.tokens()[0].kind else {
        panic!("expected f-string token");
    };

    assert_eq!(parts.len(), 1);
    assert_eq!(parts[0].kind, FStringPartKind::Interpolation);
    assert_eq!(
        lexed.text(parts[0].span),
        "case x of (| y => { value := y })"
    );
}

#[test]
fn test_comments_and_whitespace_attach_as_trivia() {
    let lexed = test_lex("  // note\nlet");
    let token = &lexed.tokens()[0];

    assert_eq!(token.kind, TokenKind::KwLet);
    assert!(matches!(
        token.leading_trivia[0].kind,
        TriviaKind::Whitespace
    ));
    assert!(matches!(
        token.leading_trivia[1].kind,
        TriviaKind::LineComment { .. }
    ));
    assert!(matches!(token.leading_trivia[2].kind, TriviaKind::Newline));
}

#[test]
fn test_trailing_comment_stays_with_token() {
    let lexed = test_lex("let // trailing\nmut");
    let first = &lexed.tokens()[0];
    let second = &lexed.tokens()[1];

    assert_eq!(first.kind, TokenKind::KwLet);
    assert!(matches!(
        first.trailing_trivia[1].kind,
        TriviaKind::LineComment { .. }
    ));
    assert_eq!(second.kind, TokenKind::KwMut);
}

#[test]
fn test_unterminated_block_comment_is_reported_and_eof_still_emits() {
    let lexed = test_lex("let /* never closes");
    assert_eq!(lexed.tokens()[0].kind, TokenKind::KwLet);
    assert_eq!(
        lexed.tokens().last().map(|token| &token.kind),
        Some(&TokenKind::Eof)
    );
    assert!(matches!(
        lexed.errors()[0].kind,
        LexErrorKind::UnterminatedBlockComment
    ));
}

#[test]
fn test_string_escape_validation_reports_errors() {
    let lexed = test_lex("\"bad\\z\"");
    assert!(matches!(
        lexed.errors()[0].kind,
        LexErrorKind::InvalidEscape('z')
    ));
}

#[test]
fn test_rune_validation_reports_multi_char_errors() {
    let lexed = test_lex("'ab'");
    assert!(matches!(
        lexed.errors()[0].kind,
        LexErrorKind::MultiCharRune
    ));
}

#[test]
fn test_batch_and_streaming_see_the_same_tokens() {
    let source = "let value := f\"{name}\";";
    let batch = test_lex(source);
    let streaming = Lexer::new(source)
        .filter_map(Result::ok)
        .map(|token| token.kind)
        .collect::<Vec<_>>();

    assert_eq!(
        batch
            .tokens()
            .iter()
            .map(|token| token.kind.clone())
            .collect::<Vec<_>>(),
        streaming
    );
}

#[test]
fn test_token_text_uses_spans_from_batch_result() {
    let lexed = test_lex("let value");
    assert_eq!(lexed.token_text(0), "let");
    assert_eq!(lexed.token_text(1), "value");
    assert_eq!(lexed.tokens()[0].span, Span::new(0, 3));
}
