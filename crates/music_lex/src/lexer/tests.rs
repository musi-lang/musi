use music_found::Span;

use crate::Lexer;
use crate::errors::LexErrorKind;
use crate::token::{FStrPart, TokenKind, TriviaKind};

fn lex_kinds(src: &str) -> Vec<TokenKind> {
    let (tokens, errors) = Lexer::new(src).lex();
    assert!(errors.is_empty(), "unexpected errors: {errors:?}");
    tokens.into_iter().map(|t| t.kind).collect()
}

fn lex_first_kind(src: &str) -> TokenKind {
    let kinds = lex_kinds(src);
    assert!(kinds.len() >= 2, "expected at least token + Eof");
    kinds.into_iter().next().unwrap()
}

#[test]
fn empty_source_produces_eof() {
    let kinds = lex_kinds("");
    assert_eq!(kinds, vec![TokenKind::Eof]);
}

#[test]
fn delimiters() {
    let kinds = lex_kinds("( ) [ ] { }");
    assert_eq!(
        kinds,
        vec![
            TokenKind::LParen,
            TokenKind::RParen,
            TokenKind::LBracket,
            TokenKind::RBracket,
            TokenKind::LBrace,
            TokenKind::RBrace,
            TokenKind::Eof,
        ]
    );
}

#[test]
fn punctuation() {
    let kinds = lex_kinds("; , . : | ! ? < > = + - * / % @ #");
    assert_eq!(
        kinds,
        vec![
            TokenKind::Semi,
            TokenKind::Comma,
            TokenKind::Dot,
            TokenKind::Colon,
            TokenKind::Pipe,
            TokenKind::Bang,
            TokenKind::Question,
            TokenKind::Lt,
            TokenKind::Gt,
            TokenKind::Eq,
            TokenKind::Plus,
            TokenKind::Minus,
            TokenKind::Star,
            TokenKind::Slash,
            TokenKind::Percent,
            TokenKind::At,
            TokenKind::Hash,
            TokenKind::Eof,
        ]
    );
}

#[test]
fn compound_tokens() {
    let cases: Vec<(&str, TokenKind)> = vec![
        (":=", TokenKind::ColonEq),
        ("::", TokenKind::ColonColon),
        (":?", TokenKind::ColonQuestion),
        (":?>", TokenKind::ColonQuestionGt),
        ("..", TokenKind::DotDot),
        ("..<", TokenKind::DotDotLt),
        ("...", TokenKind::DotDotDot),
        (".[", TokenKind::DotLBracket),
        (".{", TokenKind::DotLBrace),
        ("<-", TokenKind::LtMinus),
        ("<=", TokenKind::LtEq),
        ("<:", TokenKind::LtColon),
        ("->", TokenKind::MinusGt),
        ("~>", TokenKind::TildeGt),
        ("=>", TokenKind::EqGt),
        ("/=", TokenKind::SlashEq),
        (">=", TokenKind::GtEq),
        ("??", TokenKind::QuestionQuestion),
        ("?.", TokenKind::QuestionDot),
        ("!.", TokenKind::BangDot),
        ("|>", TokenKind::PipeGt),
        ("#(", TokenKind::HashLParen),
        ("#[", TokenKind::HashLBracket),
    ];
    for (src, expected) in cases {
        assert_eq!(lex_first_kind(src), expected, "compound {src:?}");
    }
}

#[test]
fn maximal_munch_colon_question_gt() {
    let kinds = lex_kinds(":?>");
    assert_eq!(kinds, vec![TokenKind::ColonQuestionGt, TokenKind::Eof]);
}

#[test]
fn maximal_munch_dot_dot_dot() {
    let kinds = lex_kinds("...");
    assert_eq!(kinds, vec![TokenKind::DotDotDot, TokenKind::Eof]);
}

#[test]
fn maximal_munch_colon_eq() {
    let kinds = lex_kinds(":=");
    assert_eq!(kinds, vec![TokenKind::ColonEq, TokenKind::Eof]);
}

#[test]
fn keywords() {
    assert_eq!(lex_first_kind("and"), TokenKind::KwAnd);
    assert_eq!(lex_first_kind("let"), TokenKind::KwLet);
    assert_eq!(lex_first_kind("return"), TokenKind::KwReturn);
    assert_eq!(lex_first_kind("match"), TokenKind::KwMatch);
}

#[test]
fn identifier_not_keyword() {
    assert_eq!(lex_first_kind("android"), TokenKind::Ident);
    assert_eq!(lex_first_kind("letter"), TokenKind::Ident);
    assert_eq!(lex_first_kind("_foo"), TokenKind::Ident);
}

#[test]
fn identifier_text_via_span() {
    let source = "android";
    let (tokens, errors) = Lexer::new(source).lex();
    assert!(errors.is_empty());
    assert_eq!(tokens[0].kind, TokenKind::Ident);
    let start = usize::try_from(tokens[0].span.start).unwrap();
    let end = usize::try_from(tokens[0].span.end).unwrap();
    assert_eq!(source.get(start..end).unwrap(), "android");
}

#[test]
fn decimal_int() {
    assert_eq!(lex_first_kind("42"), TokenKind::Int(42));
    assert_eq!(lex_first_kind("0"), TokenKind::Int(0));
}

#[test]
fn hex_int() {
    assert_eq!(lex_first_kind("0xFF"), TokenKind::Int(255));
}

#[test]
fn octal_int() {
    assert_eq!(lex_first_kind("0o77"), TokenKind::Int(63));
}

#[test]
fn binary_int() {
    assert_eq!(lex_first_kind("0b1010"), TokenKind::Int(10));
}

#[test]
fn int_with_underscores() {
    assert_eq!(lex_first_kind("1_000_000"), TokenKind::Int(1_000_000));
    assert_eq!(lex_first_kind("0xFF_FF"), TokenKind::Int(0xFFFF));
}

#[test]
fn float_literal() {
    assert_eq!(lex_first_kind("3.25"), TokenKind::Float(3.25));
    assert_eq!(lex_first_kind("0.5"), TokenKind::Float(0.5));
}

#[test]
fn float_scientific() {
    assert_eq!(lex_first_kind("1e10"), TokenKind::Float(1e10));
    assert_eq!(lex_first_kind("2.5E-3"), TokenKind::Float(2.5e-3));
    assert_eq!(lex_first_kind("1E+2"), TokenKind::Float(1e2));
}

#[test]
fn invalid_number_prefix() {
    let (_, errors) = Lexer::new("0x").lex();
    assert_eq!(errors.len(), 1);
    assert!(
        errors[0]
            .to_string()
            .contains("expected digits after base prefix"),
        "got: {}",
        errors[0]
    );
}

#[test]
fn simple_string() {
    assert_eq!(
        lex_first_kind("\"hello\""),
        TokenKind::Str(String::from("hello"))
    );
}

#[test]
fn string_escapes() {
    assert_eq!(
        lex_first_kind("\"a\\nb\""),
        TokenKind::Str(String::from("a\nb"))
    );
    assert_eq!(
        lex_first_kind("\"a\\tb\""),
        TokenKind::Str(String::from("a\tb"))
    );
    assert_eq!(
        lex_first_kind("\"a\\\\b\""),
        TokenKind::Str(String::from("a\\b"))
    );
    assert_eq!(
        lex_first_kind("\"a\\\"b\""),
        TokenKind::Str(String::from("a\"b"))
    );
}

#[test]
fn unterminated_string() {
    let (_, errors) = Lexer::new("\"hello").lex();
    assert_eq!(errors.len(), 1);
    assert!(errors[0].to_string().contains("unterminated string"));
}

#[test]
fn fstring_literal_only() {
    let kind = lex_first_kind("f\"hello\"");
    assert_eq!(
        kind,
        TokenKind::FStr(vec![FStrPart::Lit(String::from("hello"))])
    );
}

#[test]
fn fstring_with_expr() {
    let source = "f\"hello {name}\"";
    let kind = lex_first_kind(source);
    match kind {
        TokenKind::FStr(parts) => {
            assert_eq!(parts.len(), 2);
            assert_eq!(parts[0], FStrPart::Lit(String::from("hello ")));
            match &parts[1] {
                FStrPart::Expr(tokens) => {
                    assert_eq!(tokens.len(), 1);
                    assert_eq!(tokens[0].kind, TokenKind::Ident);
                    let start = usize::try_from(tokens[0].span.start).unwrap();
                    let end = usize::try_from(tokens[0].span.end).unwrap();
                    assert_eq!(source.get(start..end).unwrap(), "name");
                }
                other @ FStrPart::Lit(_) => panic!("expected Expr, got {other:?}"),
            }
        }
        other => panic!("expected FStr, got {other:?}"),
    }
}

#[test]
fn rune_simple() {
    assert_eq!(lex_first_kind("'a'"), TokenKind::Rune('a'));
}

#[test]
fn rune_escape() {
    assert_eq!(lex_first_kind("'\\n'"), TokenKind::Rune('\n'));
    assert_eq!(lex_first_kind("'\\0'"), TokenKind::Rune('\0'));
}

#[test]
fn rune_empty_error() {
    let (_, errors) = Lexer::new("''").lex();
    assert_eq!(errors.len(), 1);
    assert!(errors[0].to_string().contains("empty rune"));
}

#[test]
fn rune_multi_char_error() {
    let (_, errors) = Lexer::new("'ab'").lex();
    assert_eq!(errors.len(), 1);
    assert!(errors[0].to_string().contains("more than one character"));
}

#[test]
fn escaped_ident() {
    let source = "`hello world`";
    let (tokens, errors) = Lexer::new(source).lex();
    assert!(errors.is_empty());
    assert_eq!(tokens[0].kind, TokenKind::EscapedIdent);
    let start = usize::try_from(tokens[0].span.start).unwrap();
    let end = usize::try_from(tokens[0].span.end).unwrap();
    assert_eq!(source.get(start..end).unwrap(), "`hello world`");
}

#[test]
fn unterminated_escaped_ident() {
    let (_, errors) = Lexer::new("`hello").lex();
    assert_eq!(errors.len(), 1);
    assert!(
        errors[0]
            .to_string()
            .contains("unterminated escaped identifier")
    );
}

#[test]
fn line_comment_trivia() {
    let (tokens, errors) = Lexer::new("// comment\n42").lex();
    assert!(errors.is_empty());
    assert_eq!(tokens.len(), 2); // Int + Eof
    assert_eq!(tokens[0].kind, TokenKind::Int(42));
    assert!(
        tokens[0]
            .leading_trivia
            .iter()
            .any(|t| t.kind == TriviaKind::LineComment { doc: false })
    );
}

#[test]
fn doc_line_comment_trivia() {
    let (tokens, errors) = Lexer::new("/// doc\n42").lex();
    assert!(errors.is_empty());
    assert!(
        tokens[0]
            .leading_trivia
            .iter()
            .any(|t| t.kind == TriviaKind::LineComment { doc: true })
    );
}

#[test]
fn block_comment_trivia() {
    let (tokens, errors) = Lexer::new("/* block */42").lex();
    assert!(errors.is_empty());
    assert_eq!(tokens[0].kind, TokenKind::Int(42));
    assert!(
        tokens[0]
            .leading_trivia
            .iter()
            .any(|t| t.kind == TriviaKind::BlockComment { doc: false })
    );
}

#[test]
fn doc_block_comment_trivia() {
    let (tokens, errors) = Lexer::new("/** doc */42").lex();
    assert!(errors.is_empty());
    assert!(
        tokens[0]
            .leading_trivia
            .iter()
            .any(|t| t.kind == TriviaKind::BlockComment { doc: true })
    );
}

#[test]
fn unterminated_block_comment() {
    let (_, errors) = Lexer::new("/* oops").lex();
    assert_eq!(errors.len(), 1);
    assert!(errors[0].to_string().contains("unterminated block comment"));
}

#[test]
fn leading_whitespace_trivia() {
    let (tokens, errors) = Lexer::new("  42").lex();
    assert!(errors.is_empty());
    assert_eq!(tokens[0].kind, TokenKind::Int(42));
    assert!(
        tokens[0]
            .leading_trivia
            .iter()
            .any(|t| t.kind == TriviaKind::Whitespace)
    );
}

#[test]
fn trailing_comment_trivia() {
    let (tokens, errors) = Lexer::new("42 // trailing").lex();
    assert!(errors.is_empty());
    assert_eq!(tokens[0].kind, TokenKind::Int(42));
    assert!(
        tokens[0]
            .trailing_trivia
            .iter()
            .any(|t| matches!(t.kind, TriviaKind::LineComment { doc: false }))
    );
}

#[test]
fn error_recovery_continues_lexing() {
    let (tokens, errors) = Lexer::new("$ 42").lex();
    assert_eq!(errors.len(), 1);
    let kinds: Vec<_> = tokens.iter().map(|t| &t.kind).collect();
    assert!(kinds.contains(&&TokenKind::Int(42)));
    assert!(kinds.contains(&&TokenKind::Eof));
}

#[test]
fn full_program_snippet() {
    let src = "let x := 42;\nlet y := x + 1";
    let kinds = lex_kinds(src);
    assert_eq!(
        kinds,
        vec![
            TokenKind::KwLet,
            TokenKind::Ident,
            TokenKind::ColonEq,
            TokenKind::Int(42),
            TokenKind::Semi,
            TokenKind::KwLet,
            TokenKind::Ident,
            TokenKind::ColonEq,
            TokenKind::Ident,
            TokenKind::Plus,
            TokenKind::Int(1),
            TokenKind::Eof,
        ]
    );
}

#[test]
fn underscore_only_after_base_prefix() {
    for input in &["0x_", "0b_", "0o_"] {
        let (_, errors) = Lexer::new(input).lex();
        assert_eq!(
            errors.len(),
            1,
            "{input:?} should produce exactly one error"
        );
        assert!(
            matches!(errors[0].kind, LexErrorKind::InvalidNumberPrefix),
            "{input:?} should produce InvalidNumberPrefix, got: {:?}",
            errors[0]
        );
    }
}

#[test]
fn incomplete_exponent() {
    for input in &["1e", "1e+", "1e-"] {
        let (_, errors) = Lexer::new(input).lex();
        assert_eq!(
            errors.len(),
            1,
            "{input:?} should produce exactly one error"
        );
        assert!(
            matches!(errors[0].kind, LexErrorKind::InvalidNumberPrefix),
            "{input:?} should produce InvalidNumberPrefix, got: {:?}",
            errors[0]
        );
    }
}

#[test]
fn int_then_dot() {
    for (input, int_val) in &[("0.", 0_i64), ("1.", 1_i64)] {
        let kinds = lex_kinds(input);
        assert_eq!(
            kinds,
            vec![TokenKind::Int(*int_val), TokenKind::Dot, TokenKind::Eof],
            "{input:?} should produce [Int, Dot, Eof]"
        );
    }
}

#[test]
fn nested_block_comments() {
    let (tokens, errors) = Lexer::new("/* /* inner */ outer */").lex();
    assert!(
        errors.is_empty(),
        "nested block comment should not error: {errors:?}"
    );
    assert_eq!(tokens.len(), 1, "should only have Eof token");
    assert_eq!(tokens[0].kind, TokenKind::Eof);
}

#[test]
fn fstring_nested_braces() {
    let kind = lex_first_kind("f\"{ {1, 2} }\"");
    match kind {
        TokenKind::FStr(parts) => {
            assert_eq!(parts.len(), 1);
            match &parts[0] {
                FStrPart::Expr(tokens) => {
                    let inner_kinds: Vec<_> = tokens.iter().map(|t| &t.kind).collect();
                    assert!(
                        inner_kinds.contains(&&TokenKind::LBrace),
                        "should contain inner LBrace"
                    );
                    assert!(
                        inner_kinds.contains(&&TokenKind::RBrace),
                        "should contain inner RBrace"
                    );
                }
                other @ FStrPart::Lit(_) => panic!("expected Expr, got {other:?}"),
            }
        }
        other => panic!("expected FStr, got {other:?}"),
    }
}

#[test]
fn fstring_with_escapes() {
    let source = "f\"hello\\n{x}\"";
    let kind = lex_first_kind(source);
    match kind {
        TokenKind::FStr(parts) => {
            assert_eq!(parts.len(), 2);
            assert_eq!(parts[0], FStrPart::Lit(String::from("hello\n")));
            match &parts[1] {
                FStrPart::Expr(tokens) => {
                    assert_eq!(tokens.len(), 1);
                    assert_eq!(tokens[0].kind, TokenKind::Ident);
                    let start = usize::try_from(tokens[0].span.start).unwrap();
                    let end = usize::try_from(tokens[0].span.end).unwrap();
                    assert_eq!(source.get(start..end).unwrap(), "x");
                }
                other @ FStrPart::Lit(_) => panic!("expected Expr, got {other:?}"),
            }
        }
        other => panic!("expected FStr, got {other:?}"),
    }
}

#[test]
fn hex_escape_in_string() {
    assert_eq!(
        lex_first_kind("\"\\x41\""),
        TokenKind::Str(String::from("A"))
    );
}

#[test]
fn unicode_escape_in_string() {
    assert_eq!(
        lex_first_kind("\"\\u{1F600}\""),
        TokenKind::Str(String::from("\u{1F600}"))
    );
}

#[test]
fn number_overflow() {
    let (_, errors) = Lexer::new("99999999999999999999").lex();
    assert_eq!(errors.len(), 1);
    assert!(
        matches!(errors[0].kind, LexErrorKind::NumberOverflow),
        "expected NumberOverflow, got: {:?}",
        errors[0]
    );
}

#[test]
fn token_spans_correct() {
    let (tokens, errors) = Lexer::new("let x := 42;").lex();
    assert!(errors.is_empty());
    // "let" at 0..3
    assert_eq!(tokens[0].kind, TokenKind::KwLet);
    assert_eq!(tokens[0].span, Span::new(0, 3));
    // "x" at 4..5
    assert_eq!(tokens[1].kind, TokenKind::Ident);
    assert_eq!(tokens[1].span, Span::new(4, 5));
    // ":=" at 6..8
    assert_eq!(tokens[2].kind, TokenKind::ColonEq);
    assert_eq!(tokens[2].span, Span::new(6, 8));
    // "42" at 9..11
    assert_eq!(tokens[3].kind, TokenKind::Int(42));
    assert_eq!(tokens[3].span, Span::new(9, 11));
    // ";" at 11..12
    assert_eq!(tokens[4].kind, TokenKind::Semi);
    assert_eq!(tokens[4].span, Span::new(11, 12));
}

#[test]
fn multiple_errors() {
    let (_, errors) = Lexer::new("$ ~ $").lex();
    assert!(
        errors.len() >= 3,
        "expected at least 3 errors for 3 bad chars, got {}",
        errors.len()
    );
    for err in &errors {
        assert!(
            matches!(err.kind, LexErrorKind::UnexpectedChar(_)),
            "expected UnexpectedChar, got: {err:?}"
        );
    }
}

#[test]
fn whitespace_trivia_has_correct_span() {
    let (tokens, errors) = Lexer::new("  42").lex();
    assert!(errors.is_empty());
    let ws = &tokens[0].leading_trivia[0];
    assert_eq!(ws.kind, TriviaKind::Whitespace);
    assert_eq!(ws.span, Span::new(0, 2));
}

#[test]
fn newline_trivia_has_correct_span() {
    let (tokens, errors) = Lexer::new("\n42").lex();
    assert!(errors.is_empty());
    let nl = &tokens[0].leading_trivia[0];
    assert_eq!(nl.kind, TriviaKind::Newline);
    assert_eq!(nl.span, Span::new(0, 1));
}

#[test]
fn line_comment_trivia_has_correct_span() {
    let (tokens, errors) = Lexer::new("// comment\n42").lex();
    assert!(errors.is_empty());
    let comment = tokens[0]
        .leading_trivia
        .iter()
        .find(|t| matches!(t.kind, TriviaKind::LineComment { .. }))
        .unwrap();
    assert_eq!(comment.span, Span::new(0, 10));
}

#[test]
fn block_comment_trivia_has_correct_span() {
    let (tokens, errors) = Lexer::new("/* block */42").lex();
    assert!(errors.is_empty());
    let comment = tokens[0]
        .leading_trivia
        .iter()
        .find(|t| matches!(t.kind, TriviaKind::BlockComment { .. }))
        .unwrap();
    assert_eq!(comment.span, Span::new(0, 11));
}

#[test]
fn invalid_hex_escape_digits() {
    let (_, errors) = Lexer::new(r#""\xGG""#).lex();
    assert!(!errors.is_empty());
    assert!(matches!(
        errors[0].kind,
        LexErrorKind::InvalidHexEscape { .. }
    ));
}

#[test]
fn unicode_escape_no_braces() {
    let (_, errors) = Lexer::new(r#""\u0041""#).lex();
    assert!(!errors.is_empty());
    assert!(matches!(errors[0].kind, LexErrorKind::InvalidUnicodeEscape));
}

#[test]
fn unicode_escape_empty() {
    let (_, errors) = Lexer::new(r#""\u{}""#).lex();
    assert!(!errors.is_empty());
    assert!(matches!(errors[0].kind, LexErrorKind::InvalidUnicodeEscape));
}

#[test]
fn unicode_escape_too_many_digits() {
    let (_, errors) = Lexer::new(r#""\u{1234567}""#).lex();
    assert!(!errors.is_empty());
    assert!(matches!(errors[0].kind, LexErrorKind::InvalidUnicodeEscape));
}

#[test]
fn unicode_escape_surrogate() {
    let (_, errors) = Lexer::new(r#""\u{D800}""#).lex();
    assert!(!errors.is_empty());
    assert!(matches!(errors[0].kind, LexErrorKind::InvalidUnicodeEscape));
}

#[test]
fn backslash_at_eof_in_string() {
    let src = "\"\\";
    let (_, errors) = Lexer::new(src).lex();
    assert!(!errors.is_empty());
}

#[test]
fn backslash_rune() {
    let kinds = lex_kinds("'\\\\'");
    assert_eq!(kinds[0], TokenKind::Rune('\\'));
}

#[test]
fn unterminated_fstring() {
    let (_, errors) = Lexer::new("f\"hello").lex();
    assert!(!errors.is_empty());
    assert!(matches!(errors[0].kind, LexErrorKind::UnterminatedFString));
}

#[test]
fn unterminated_fstring_expr() {
    let (_, errors) = Lexer::new("f\"hello {x").lex();
    assert!(!errors.is_empty());
    assert!(matches!(
        errors[0].kind,
        LexErrorKind::UnterminatedFStringExpr
    ));
}

#[test]
fn unterminated_rune_no_close() {
    let (_, errors) = Lexer::new("'a").lex();
    assert!(!errors.is_empty());
    assert!(matches!(errors[0].kind, LexErrorKind::UnterminatedRune));
}

#[test]
fn float_with_underscores() {
    let kinds = lex_kinds("1_000.5");
    assert_eq!(kinds[0], TokenKind::Float(1000.5));
}

#[test]
fn keyword_case_sensitivity() {
    let kinds = lex_kinds("And LET Match");
    assert_eq!(kinds[0], TokenKind::Ident);
    assert_eq!(kinds[1], TokenKind::Ident);
    assert_eq!(kinds[2], TokenKind::Ident);
}

#[test]
fn tilde_unexpected_char() {
    let (_, errors) = Lexer::new("~").lex();
    assert!(!errors.is_empty());
    assert!(matches!(errors[0].kind, LexErrorKind::UnexpectedChar('~')));
}
