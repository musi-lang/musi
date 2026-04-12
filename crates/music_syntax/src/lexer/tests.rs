use crate::{LexErrorKind, LexedSource, Lexer, TokenKind, TriviaKind};

fn lex(input: &str) -> LexedSource {
    Lexer::new(input).lex()
}

fn assert_token_kinds(input: &str, expected: &[TokenKind]) {
    let lexed = lex(input);
    let kinds: Vec<TokenKind> = lexed.tokens().iter().map(|t| t.kind).collect();
    assert_eq!(kinds, expected);
}

fn lex_with_token_kinds(input: &str, expected: &[TokenKind]) -> LexedSource {
    let lexed = lex(input);
    let kinds: Vec<TokenKind> = lexed.tokens().iter().map(|t| t.kind).collect();
    assert_eq!(kinds, expected);
    lexed
}

fn assert_no_errors(input: &str, expected: &[TokenKind]) {
    assert!(lex_with_token_kinds(input, expected).errors().is_empty());
}

#[test]
fn lex_keywords_idents_and_literals() {
    let lexed = lex_with_token_kinds(
        "let x := 1\n",
        [
            TokenKind::KwLet,
            TokenKind::Ident,
            TokenKind::ColonEq,
            TokenKind::Int,
            TokenKind::Eof,
        ]
        .as_slice(),
    );

    assert!(lexed.token_trivia(0).is_empty());
    assert_eq!(lexed.token_trivia(1).len(), 1);
    assert_eq!(lexed.token_trivia(1)[0].kind, TriviaKind::Whitespace);
    assert_eq!(lexed.token_trivia(4).len(), 1);
    assert_eq!(lexed.token_trivia(4)[0].kind, TriviaKind::Newline);

    let rec_kw = lex("rec");
    assert_eq!(rec_kw.tokens()[0].kind, TokenKind::KwRec);
    assert_eq!(rec_kw.tokens()[1].kind, TokenKind::Eof);
}

#[test]
fn lex_line_doc_comment_trivia() {
    let lexed = lex("/// hi\nlet");
    assert_eq!(lexed.tokens()[0].kind, TokenKind::KwLet);
    assert_eq!(
        lexed.token_trivia(0)[0].kind,
        TriviaKind::LineComment { doc: true }
    );
}

#[test]
fn lex_compound_tokens_and_symbolic_ops() {
    assert_token_kinds(
        "a:?>b a ++ b",
        [
            TokenKind::Ident,
            TokenKind::ColonQuestionGt,
            TokenKind::Ident,
            TokenKind::Ident,
            TokenKind::SymbolicOp,
            TokenKind::Ident,
            TokenKind::Eof,
        ]
        .as_slice(),
    );
}

#[test]
fn underscore_is_a_token() {
    let lexed = lex("_");
    assert_eq!(lexed.tokens()[0].kind, TokenKind::Underscore);
    assert_eq!(lexed.tokens()[1].kind, TokenKind::Eof);
}

#[test]
fn type_names_lex_as_identifiers() {
    assert_token_kinds(
        "Type Type0 Type123 TypeX",
        [
            TokenKind::Ident,
            TokenKind::Ident,
            TokenKind::Ident,
            TokenKind::Ident,
            TokenKind::Eof,
        ]
        .as_slice(),
    );
}

#[test]
fn lex_op_ident() {
    let lexed = lex("(+)");
    assert_eq!(lexed.tokens()[0].kind, TokenKind::OpIdent);
    assert_eq!(lexed.tokens()[1].kind, TokenKind::Eof);
}

#[test]
fn lex_template_literal_no_substitutions() {
    assert_no_errors("`hi`", &[TokenKind::TemplateNoSubst, TokenKind::Eof]);
}

#[test]
fn lex_template_literal_with_substitution() {
    assert_no_errors(
        "`hi ${x} ok`",
        [
            TokenKind::TemplateHead,
            TokenKind::Ident,
            TokenKind::TemplateTail,
            TokenKind::Eof,
        ]
        .as_slice(),
    );
}

#[test]
fn lex_template_literal_does_not_end_interpolation_on_inner_rbrace() {
    assert_no_errors(
        "`a ${{x := 1}} b`",
        [
            TokenKind::TemplateHead,
            TokenKind::LBrace,
            TokenKind::Ident,
            TokenKind::ColonEq,
            TokenKind::Int,
            TokenKind::RBrace,
            TokenKind::TemplateTail,
            TokenKind::Eof,
        ]
        .as_slice(),
    );
}

#[test]
fn lex_template_literal_allows_escaped_dollar() {
    assert_no_errors("`\\${x}`", &[TokenKind::TemplateNoSubst, TokenKind::Eof]);
}

#[test]
fn dot_start_float_is_float() {
    assert_token_kinds(".5", &[TokenKind::Float, TokenKind::Eof]);
}

#[test]
fn lex_reserved_compound_tokens() {
    let cases = [
        (
            ":?> := = :? ... .{ .[ -> ~> => /= <= >= <: |>",
            vec![
                TokenKind::ColonQuestionGt,
                TokenKind::ColonEq,
                TokenKind::Eq,
                TokenKind::ColonQuestion,
                TokenKind::DotDotDot,
                TokenKind::DotLBrace,
                TokenKind::DotLBracket,
                TokenKind::MinusGt,
                TokenKind::TildeGt,
                TokenKind::EqGt,
                TokenKind::SlashEq,
                TokenKind::LtEq,
                TokenKind::GtEq,
                TokenKind::LtColon,
                TokenKind::PipeGt,
                TokenKind::Eof,
            ],
        ),
        (
            "(->) (:=) (=>) (|>)",
            vec![
                TokenKind::LParen,
                TokenKind::MinusGt,
                TokenKind::RParen,
                TokenKind::LParen,
                TokenKind::ColonEq,
                TokenKind::RParen,
                TokenKind::LParen,
                TokenKind::EqGt,
                TokenKind::RParen,
                TokenKind::LParen,
                TokenKind::PipeGt,
                TokenKind::RParen,
                TokenKind::Eof,
            ],
        ),
    ];

    for (input, expected) in cases {
        assert_token_kinds(input, &expected);
    }
}

#[test]
fn lt_minus_is_a_user_symbolic_op() {
    assert_token_kinds("<-", &[TokenKind::SymbolicOp, TokenKind::Eof]);
}

#[test]
fn c_operators_are_not_part_of_symbolic_op_alphabet() {
    let lexed = lex("& && ^ ^^ ~ ~~ (&) (^ ) (~)");
    assert!(!lexed.errors().is_empty());
}

#[test]
fn invalid_char_includes_character() {
    let lexed = lex("€");
    assert_eq!(lexed.errors().len(), 1);
    assert_eq!(
        lexed.errors()[0].kind,
        LexErrorKind::InvalidChar { ch: '€' }
    );
}

#[test]
fn question_and_bang_are_only_valid_in_compounds() {
    let q = lex("?");
    assert!(
        q.errors()
            .iter()
            .any(|e| e.kind == LexErrorKind::InvalidChar { ch: '?' })
    );

    let b = lex("!");
    assert!(
        b.errors()
            .iter()
            .any(|e| e.kind == LexErrorKind::InvalidChar { ch: '!' })
    );

    let compounds = lex("a?.b a!.b");
    assert!(!compounds.errors().is_empty());
}

#[test]
fn base_prefix_requires_digits() {
    let lexed = Lexer::new("0x").lex();
    assert!(
        lexed
            .errors()
            .iter()
            .any(|e| e.kind == LexErrorKind::MissingDigitsAfterBasePrefix { base: 16 })
    );
}

#[test]
fn invalid_digit_for_base_is_reported() {
    let lexed = Lexer::new("0b2").lex();
    assert!(
        lexed
            .errors()
            .iter()
            .any(|e| e.kind == LexErrorKind::InvalidDigitForBase { base: 2, ch: '2' })
    );
    assert!(
        !lexed
            .errors()
            .iter()
            .any(|e| { e.kind == LexErrorKind::MissingDigitsAfterBasePrefix { base: 2 } })
    );
}

#[test]
fn invalid_numeric_separator_is_reported() {
    let lexed = Lexer::new("1_").lex();
    assert!(
        lexed
            .errors()
            .iter()
            .any(|e| e.kind == LexErrorKind::MissingDigitAfterUnderscoreInNumberLiteral)
    );
}

#[test]
fn missing_exponent_digits_is_reported() {
    let lexed = Lexer::new("1e+").lex();
    assert!(
        lexed
            .errors()
            .iter()
            .any(|e| e.kind == LexErrorKind::MissingExponentDigits)
    );
}

#[test]
fn rune_errors_are_specific() {
    let empty = Lexer::new("''").lex();
    assert!(
        empty
            .errors()
            .iter()
            .any(|e| e.kind == LexErrorKind::EmptyRuneLiteral)
    );

    let too_long = Lexer::new("'ab'").lex();
    assert!(
        too_long
            .errors()
            .iter()
            .any(|e| e.kind == LexErrorKind::RuneLiteralTooLong)
    );
}

#[test]
fn unterminated_block_comment_reports_error() {
    let lexed = Lexer::new("/*").lex();
    assert_eq!(
        lexed.trivia()[0].kind,
        TriviaKind::BlockComment { doc: false }
    );
    assert!(
        lexed
            .errors()
            .iter()
            .any(|e| e.kind == LexErrorKind::UnterminatedBlockComment)
    );
}

#[test]
fn escape_errors_are_specific() {
    let missing = Lexer::new(r#""\"#).lex();
    assert!(
        missing
            .errors()
            .iter()
            .any(|e| e.kind == LexErrorKind::MissingEscapeCode)
    );

    let unexpected = Lexer::new(r#""\q""#).lex();
    assert!(
        unexpected
            .errors()
            .iter()
            .any(|e| e.kind == LexErrorKind::UnexpectedEscape { ch: 'q' })
    );

    let x_missing = Lexer::new(r#""\x""#).lex();
    assert!(
        x_missing
            .errors()
            .iter()
            .any(|e| e.kind == LexErrorKind::MissingHexDigitsInByteEscape)
    );

    let x_invalid = Lexer::new(r#""\xG0""#).lex();
    assert!(
        x_invalid
            .errors()
            .iter()
            .any(|e| e.kind == LexErrorKind::InvalidHexDigitInByteEscape { ch: 'G' })
    );

    let u_missing = Lexer::new(r#""\u12""#).lex();
    assert!(
        u_missing
            .errors()
            .iter()
            .any(|e| e.kind == LexErrorKind::MissingHexDigitsInUnicodeEscape)
    );

    let u_invalid = Lexer::new(r#""\u12G4""#).lex();
    assert!(
        u_invalid
            .errors()
            .iter()
            .any(|e| e.kind == LexErrorKind::InvalidHexDigitInUnicodeEscape { ch: 'G' })
    );

    let u_len_5 = Lexer::new(r#""\u12345""#).lex();
    assert!(
        u_len_5
            .errors()
            .iter()
            .any(|e| { e.kind == LexErrorKind::ExpectedFourOrSixHexDigitsInUnicodeEscape })
    );

    let u_non_scalar_4 = Lexer::new(r#""\uD800""#).lex();
    assert!(
        u_non_scalar_4
            .errors()
            .iter()
            .any(|e| matches!(e.kind, LexErrorKind::InvalidUnicodeScalar { .. }))
    );

    let u_non_scalar_6 = Lexer::new(r#""\u00D800""#).lex();
    assert!(
        u_non_scalar_6
            .errors()
            .iter()
            .any(|e| matches!(e.kind, LexErrorKind::InvalidUnicodeScalar { .. }))
    );

    let u_too_large = Lexer::new(r#""\u110000""#).lex();
    assert!(
        u_too_large
            .errors()
            .iter()
            .any(|e| matches!(e.kind, LexErrorKind::InvalidUnicodeScalar { .. }))
    );
}

#[test]
fn unterminated_string_is_reported() {
    let lexed = Lexer::new("\"abc").lex();
    assert!(
        lexed
            .errors()
            .iter()
            .any(|e| e.kind == LexErrorKind::UnterminatedStringLiteral)
    );
}

#[test]
fn unterminated_rune_is_reported() {
    let lexed = Lexer::new("'a").lex();
    assert!(
        lexed
            .errors()
            .iter()
            .any(|e| e.kind == LexErrorKind::UnterminatedRuneLiteral)
    );
}

#[test]
fn unterminated_template_literal_is_reported() {
    let lexed = Lexer::new("`abc").lex();
    assert!(
        lexed
            .errors()
            .iter()
            .any(|e| e.kind == LexErrorKind::UnterminatedTemplateLiteral)
    );
}

#[test]
fn unexpected_underscore_in_number_literal_is_reported() {
    let lexed = Lexer::new("0x_FF").lex();
    assert!(
        lexed
            .errors()
            .iter()
            .any(|e| e.kind == LexErrorKind::UnexpectedUnderscoreInNumberLiteral)
    );
}
