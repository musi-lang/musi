use crate::token::TokenKind;
use musi_basic::interner::Interner;
use musi_basic::source::SourceFile;

struct TestContext {
    interner: Interner,
    source: SourceFile,
}

impl TestContext {
    fn new(input: &str) -> Self {
        Self {
            interner: Interner::new(),
            source: SourceFile::new("test.ms".into(), input.into(), 0),
        }
    }
}

fn check(input: &str, expected: impl FnOnce(&mut Interner) -> Vec<TokenKind>) {
    let mut ctx = TestContext::new(input);
    let mut actual = vec![];
    let (tokens, _) = crate::lexer::tokenize(&ctx.source, &mut ctx.interner);
    for tok in tokens {
        if tok.kind == TokenKind::EOF {
            break;
        }
        actual.push(tok.kind);
    }

    let expected_tokens = expected(&mut ctx.interner);
    assert_eq!(
        actual, expected_tokens,
        "Token mismatch for input: {input:?}"
    );
}

#[test]
fn test_numbers() {
    check("123 456_789 0xFF 0o77 0b1011", |i| {
        vec![
            TokenKind::LitInt(i.intern("123")),
            TokenKind::LitInt(i.intern("456789")),
            TokenKind::LitInt(i.intern("0xFF")),
            TokenKind::LitInt(i.intern("0o77")),
            TokenKind::LitInt(i.intern("0b1011")),
        ]
    });

    check("3.14 1e10 1.2e-5", |i| {
        vec![
            TokenKind::LitReal(i.intern("3.14")),
            TokenKind::LitReal(i.intern("1e10")),
            TokenKind::LitReal(i.intern("1.2e-5")),
        ]
    });
}

#[test]
fn test_idents_keywords() {
    check("val var if else match record sum fn", |_| {
        vec![
            TokenKind::KwVal,
            TokenKind::KwVar,
            TokenKind::KwIf,
            TokenKind::KwElse,
            TokenKind::KwMatch,
            TokenKind::KwRecord,
            TokenKind::KwSum,
            TokenKind::KwFn,
        ]
    });

    check("my_var _unused `escaped ident` `if`", |i| {
        vec![
            TokenKind::Ident(i.intern("my_var")),
            TokenKind::Ident(i.intern("_unused")),
            TokenKind::Ident(i.intern("escaped ident")),
            TokenKind::Ident(i.intern("if")),
        ]
    });
}

#[test]
fn test_strings_runes() {
    check(r#""hello" "with \"quotes\"" 'a' "\n""#, |i| {
        vec![
            TokenKind::LitString(i.intern("hello")),
            TokenKind::LitString(i.intern("with \"quotes\"")),
            TokenKind::LitRune('a'),
            TokenKind::LitString(i.intern("\n")),
        ]
    });
}

#[test]
fn test_templates() {
    check(r#"$"hello {x} middle {y} tail" $"no subst""#, |i| {
        vec![
            TokenKind::TemplateHead(i.intern("hello ")),
            TokenKind::Ident(i.intern("x")),
            TokenKind::TemplateMiddle(i.intern(" middle ")),
            TokenKind::Ident(i.intern("y")),
            TokenKind::TemplateTail(i.intern(" tail")),
            TokenKind::LitTemplateNoSubst(i.intern("no subst")),
        ]
    });
}

#[test]
fn test_symbols() {
    check("+ - * / % **", |_| {
        vec![
            TokenKind::Plus,
            TokenKind::Minus,
            TokenKind::Star,
            TokenKind::Slash,
            TokenKind::Percent,
            TokenKind::StarStar,
        ]
    });

    check("= /= < <= > >= and or not", |_| {
        vec![
            TokenKind::Eq,
            TokenKind::SlashEq,
            TokenKind::Lt,
            TokenKind::LtEq,
            TokenKind::Gt,
            TokenKind::GtEq,
            TokenKind::KwAnd,
            TokenKind::KwOr,
            TokenKind::KwNot,
        ]
    });

    check(".. ..< |> ?? := => -> <- :: .^ ?", |_| {
        vec![
            TokenKind::DotDot,
            TokenKind::DotDotLt,
            TokenKind::BarGt,
            TokenKind::QuestionQuestion,
            TokenKind::ColonEq,
            TokenKind::EqGt,
            TokenKind::MinusGt,
            TokenKind::LtMinus,
            TokenKind::ColonColon,
            TokenKind::DotCaret,
            TokenKind::Question,
        ]
    });
}

#[test]
fn test_comments() {
    check(
        "val x := 1; // line comment\nval y := 2; /* block\ncomment */ val z := 3;",
        |i| {
            vec![
                TokenKind::KwVal,
                TokenKind::Ident(i.intern("x")),
                TokenKind::ColonEq,
                TokenKind::LitInt(i.intern("1")),
                TokenKind::Semicolon,
                TokenKind::KwVal,
                TokenKind::Ident(i.intern("y")),
                TokenKind::ColonEq,
                TokenKind::LitInt(i.intern("2")),
                TokenKind::Semicolon,
                TokenKind::KwVal,
                TokenKind::Ident(i.intern("z")),
                TokenKind::ColonEq,
                TokenKind::LitInt(i.intern("3")),
                TokenKind::Semicolon,
            ]
        },
    );

    check("/* nested /* block */ comment */ val x := 1;", |i| {
        vec![
            TokenKind::KwVal,
            TokenKind::Ident(i.intern("x")),
            TokenKind::ColonEq,
            TokenKind::LitInt(i.intern("1")),
            TokenKind::Semicolon,
        ]
    });
}

#[test]
fn test_error_reporting() {
    let cases = [
        "\"unclosed string literal",
        "$'unclosed template {x}",
        "'unclosed rune",
        "`unclosed escaped ident",
        "/* unclosed block comment",
        r#""bad escape \z""#,
        r#"'bad hex \xGG'"#,
        r#""bad uni \u{!!!!}""#,
        "''",
        "'abc'",
        "0x",
        "1.0_",
        "1__2",
        "1e",
    ];
    for input in cases {
        let mut ctx = TestContext::new(input);
        let (_, diagnostics) = crate::lexer::tokenize(&ctx.source, &mut ctx.interner);
        assert!(
            !diagnostics.diagnostics.is_empty(),
            "expected error for input: {input:?}"
        );
    }
}

#[test]
fn test_rejection_of_unicode() {
    let mut ctx = TestContext::new("你好 世界 π_value 🦀_emojis");
    let (_, diagnostics) = crate::lexer::tokenize(&ctx.source, &mut ctx.interner);
    assert!(!diagnostics.diagnostics.is_empty());
}

#[test]
fn test_nested_templates() {
    check(r#"$"outer { $"inner {x} tail" } end""#, |i| {
        vec![
            TokenKind::TemplateHead(i.intern("outer ")),
            TokenKind::TemplateHead(i.intern("inner ")),
            TokenKind::Ident(i.intern("x")),
            TokenKind::TemplateTail(i.intern(" tail")),
            TokenKind::TemplateTail(i.intern(" end")),
        ]
    });
}

#[test]
fn test_underscores() {
    check("_ _unused __test", |i| {
        vec![
            TokenKind::Underscore,
            TokenKind::Ident(i.intern("_unused")),
            TokenKind::Ident(i.intern("__test")),
        ]
    });
}

#[test]
fn test_spans() {
    let mut ctx = TestContext::new("val x := 123;");
    let x_id = ctx.interner.intern("x");
    let num_id = ctx.interner.intern("123");

    let tokens_with_spans = vec![
        (TokenKind::KwVal, 0, 3),
        (TokenKind::Ident(x_id), 4, 5),
        (TokenKind::ColonEq, 6, 8),
        (TokenKind::LitInt(num_id), 9, 12),
        (TokenKind::Semicolon, 12, 13),
    ];

    let (tokens, _) = crate::lexer::tokenize(&ctx.source, &mut ctx.interner);
    for (i, (expected_tok, expected_lo, expected_hi)) in tokens_with_spans.iter().enumerate() {
        let tok = &tokens[i];
        assert_eq!(tok.kind, *expected_tok);
        assert_eq!(tok.span.lo, *expected_lo);
        assert_eq!(tok.span.hi, *expected_hi);
    }
}
