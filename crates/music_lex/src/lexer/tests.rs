use music_found::Span;

use crate::Lexer;
use crate::errors::LexError;
use crate::token::{FStrPart, TokenKind, Trivia};

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

// --- Empty source ---

#[test]
fn empty_source_produces_eof() {
    let kinds = lex_kinds("");
    assert_eq!(kinds, vec![TokenKind::Eof]);
}

// --- Delimiters ---

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

// --- Punctuation ---

#[test]
fn punctuation() {
    let kinds = lex_kinds("; , . : | ! ? < > = + - * / % @ $");
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
            TokenKind::Dollar,
            TokenKind::Eof,
        ]
    );
}

// --- Compound tokens ---

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
        ("$(", TokenKind::DollarLParen),
        ("$[", TokenKind::DollarLBracket),
    ];
    for (src, expected) in cases {
        assert_eq!(lex_first_kind(src), expected, "compound {src:?}");
    }
}

// --- Maximal munch ---

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

// --- Keywords vs identifiers ---

#[test]
fn keywords() {
    assert_eq!(lex_first_kind("and"), TokenKind::KwAnd);
    assert_eq!(lex_first_kind("let"), TokenKind::KwLet);
    assert_eq!(lex_first_kind("return"), TokenKind::KwReturn);
    assert_eq!(lex_first_kind("match"), TokenKind::KwMatch);
}

#[test]
fn identifier_not_keyword() {
    assert_eq!(
        lex_first_kind("android"),
        TokenKind::Ident(String::from("android"))
    );
    assert_eq!(
        lex_first_kind("letter"),
        TokenKind::Ident(String::from("letter"))
    );
    assert_eq!(
        lex_first_kind("_foo"),
        TokenKind::Ident(String::from("_foo"))
    );
}

// --- Number literals ---

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

// --- String literals ---

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

// --- F-string ---

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
    let kind = lex_first_kind("f\"hello {name}\"");
    match kind {
        TokenKind::FStr(parts) => {
            assert_eq!(parts.len(), 2);
            assert_eq!(parts[0], FStrPart::Lit(String::from("hello ")));
            match &parts[1] {
                FStrPart::Expr(tokens) => {
                    assert_eq!(tokens.len(), 1);
                    assert_eq!(tokens[0].kind, TokenKind::Ident(String::from("name")));
                }
                other @ FStrPart::Lit(_) => panic!("expected Expr, got {other:?}"),
            }
        }
        other => panic!("expected FStr, got {other:?}"),
    }
}

// --- Rune ---

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

// --- Escaped identifier ---

#[test]
fn escaped_ident() {
    assert_eq!(
        lex_first_kind("`hello world`"),
        TokenKind::EscapedIdent(String::from("hello world"))
    );
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

// --- Comments as trivia ---

#[test]
fn line_comment_trivia() {
    let (tokens, errors) = Lexer::new("// comment\n42").lex();
    assert!(errors.is_empty());
    assert_eq!(tokens.len(), 2); // Int + Eof
    assert_eq!(tokens[0].kind, TokenKind::Int(42));
    assert!(tokens[0].leading_trivia.contains(&Trivia::LineComment {
        text: String::from(" comment"),
        doc: false,
    }));
}

#[test]
fn doc_line_comment_trivia() {
    let (tokens, errors) = Lexer::new("/// doc\n42").lex();
    assert!(errors.is_empty());
    assert!(tokens[0].leading_trivia.contains(&Trivia::LineComment {
        text: String::from(" doc"),
        doc: true,
    }));
}

#[test]
fn block_comment_trivia() {
    let (tokens, errors) = Lexer::new("/* block */42").lex();
    assert!(errors.is_empty());
    assert_eq!(tokens[0].kind, TokenKind::Int(42));
    assert!(tokens[0].leading_trivia.contains(&Trivia::BlockComment {
        text: String::from(" block "),
        doc: false,
    }));
}

#[test]
fn doc_block_comment_trivia() {
    let (tokens, errors) = Lexer::new("/** doc */42").lex();
    assert!(errors.is_empty());
    assert!(tokens[0].leading_trivia.contains(&Trivia::BlockComment {
        text: String::from(" doc "),
        doc: true,
    }));
}

#[test]
fn unterminated_block_comment() {
    let (_, errors) = Lexer::new("/* oops").lex();
    assert_eq!(errors.len(), 1);
    assert!(errors[0].to_string().contains("unterminated block comment"));
}

// --- Trivia attachment ---

#[test]
fn leading_whitespace_trivia() {
    let (tokens, errors) = Lexer::new("  42").lex();
    assert!(errors.is_empty());
    assert_eq!(tokens[0].kind, TokenKind::Int(42));
    assert!(
        tokens[0]
            .leading_trivia
            .contains(&Trivia::Whitespace(String::from("  ")))
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
            .any(|t| matches!(t, Trivia::LineComment { doc: false, .. }))
    );
}

// --- Error recovery ---

#[test]
fn error_recovery_continues_lexing() {
    let (tokens, errors) = Lexer::new("# 42").lex();
    assert_eq!(errors.len(), 1);
    let kinds: Vec<_> = tokens.iter().map(|t| &t.kind).collect();
    assert!(kinds.contains(&&TokenKind::Int(42)));
    assert!(kinds.contains(&&TokenKind::Eof));
}

// --- Full program snippet ---

#[test]
fn full_program_snippet() {
    let src = "let x := 42;\nlet y := x + 1";
    let kinds = lex_kinds(src);
    assert_eq!(
        kinds,
        vec![
            TokenKind::KwLet,
            TokenKind::Ident(String::from("x")),
            TokenKind::ColonEq,
            TokenKind::Int(42),
            TokenKind::Semi,
            TokenKind::KwLet,
            TokenKind::Ident(String::from("y")),
            TokenKind::ColonEq,
            TokenKind::Ident(String::from("x")),
            TokenKind::Plus,
            TokenKind::Int(1),
            TokenKind::Eof,
        ]
    );
}

// --- Bug fix: underscore-only after base prefix ---

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
            matches!(errors[0], LexError::InvalidNumberPrefix { .. }),
            "{input:?} should produce InvalidNumberPrefix, got: {:?}",
            errors[0]
        );
    }
}

// --- Bug fix: incomplete exponent ---

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
            matches!(errors[0], LexError::InvalidNumberPrefix { .. }),
            "{input:?} should produce InvalidNumberPrefix, got: {:?}",
            errors[0]
        );
    }
}

// --- Int followed by dot (not float) ---

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

// --- Nested block comments ---

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

// --- F-string with nested braces ---

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

// --- F-string with escapes ---

#[test]
fn fstring_with_escapes() {
    let kind = lex_first_kind("f\"hello\\n{x}\"");
    match kind {
        TokenKind::FStr(parts) => {
            assert_eq!(parts.len(), 2);
            assert_eq!(parts[0], FStrPart::Lit(String::from("hello\n")));
            match &parts[1] {
                FStrPart::Expr(tokens) => {
                    assert_eq!(tokens.len(), 1);
                    assert_eq!(tokens[0].kind, TokenKind::Ident(String::from("x")));
                }
                other @ FStrPart::Lit(_) => panic!("expected Expr, got {other:?}"),
            }
        }
        other => panic!("expected FStr, got {other:?}"),
    }
}

// --- Hex escape in string ---

#[test]
fn hex_escape_in_string() {
    assert_eq!(
        lex_first_kind("\"\\x41\""),
        TokenKind::Str(String::from("A"))
    );
}

// --- Unicode escape in string ---

#[test]
fn unicode_escape_in_string() {
    assert_eq!(
        lex_first_kind("\"\\u{1F600}\""),
        TokenKind::Str(String::from("\u{1F600}"))
    );
}

// --- Number overflow ---

#[test]
fn number_overflow() {
    let (_, errors) = Lexer::new("99999999999999999999").lex();
    assert_eq!(errors.len(), 1);
    assert!(
        matches!(errors[0], LexError::NumberOverflow { .. }),
        "expected NumberOverflow, got: {:?}",
        errors[0]
    );
}

// --- Token spans correct ---

#[test]
fn token_spans_correct() {
    let (tokens, errors) = Lexer::new("let x := 42;").lex();
    assert!(errors.is_empty());
    // "let" at 0..3
    assert_eq!(tokens[0].kind, TokenKind::KwLet);
    assert_eq!(tokens[0].span, Span::new(0, 3));
    // "x" at 4..5
    assert_eq!(tokens[1].kind, TokenKind::Ident(String::from("x")));
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

// --- Multiple errors ---

#[test]
fn multiple_errors() {
    let (_, errors) = Lexer::new("# ~ #").lex();
    assert!(
        errors.len() >= 3,
        "expected at least 3 errors for 3 bad chars, got {}",
        errors.len()
    );
    for err in &errors {
        assert!(
            matches!(err, LexError::UnexpectedChar { .. }),
            "expected UnexpectedChar, got: {err:?}"
        );
    }
}
