use crate::lexer::tokenize;
use crate::test_utils::{TestCtx, check};
use crate::token::TokenKind;
use musi_basic::span::Span;
use musi_basic::types::Ident;

#[test]
fn test_numbers() {
    check("123 456_789 0xFF 0o77 0b1011", |_| {
        vec![
            TokenKind::LitInt(123),
            TokenKind::LitInt(456_789),
            TokenKind::LitInt(255),
            TokenKind::LitInt(63),
            TokenKind::LitInt(11),
        ]
    });

    check("1.23 4.56", |_| {
        vec![TokenKind::LitReal(1.23), TokenKind::LitReal(4.56)]
    });
}

#[test]
fn test_idents_keywords() {
    check("val var if else match record choice fn", |_| {
        vec![
            TokenKind::KwVal,
            TokenKind::KwVar,
            TokenKind::KwIf,
            TokenKind::KwElse,
            TokenKind::KwMatch,
            TokenKind::KwRecord,
            TokenKind::KwChoice,
            TokenKind::KwFn,
        ]
    });

    check("my_var _unused `escaped ident` `if`", |i| {
        let span = Span::default();
        let mut id = |s| Ident::new(i.intern(s), span);
        vec![
            TokenKind::Ident(id("my_var")),
            TokenKind::Ident(id("_unused")),
            TokenKind::Ident(id("escaped ident")),
            TokenKind::Ident(id("if")),
        ]
    });
}

#[test]
fn test_strings_runes() {
    check(r#""hello" "with \"quotes\"" 'a' "\n""#, |i| {
        let span = Span::default();
        let mut id = |s| Ident::new(i.intern(s), span);
        vec![
            TokenKind::LitString(id("hello")),
            TokenKind::LitString(id("with \"quotes\"")),
            TokenKind::LitRune('a'),
            TokenKind::LitString(id("\n")),
        ]
    });
}

#[test]
fn test_templates() {
    check(r#"$"hello {x} middle {y} tail" $"no subst""#, |i| {
        let span = Span::default();
        let mut id = |s| Ident::new(i.intern(s), span);
        vec![
            TokenKind::TemplateHead(id("hello ")),
            TokenKind::Ident(id("x")),
            TokenKind::TemplateMiddle(id(" middle ")),
            TokenKind::Ident(id("y")),
            TokenKind::TemplateTail(id(" tail")),
            TokenKind::LitTemplateNoSubst(id("no subst")),
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
            let span = Span::default();
            let mut id = |s| Ident::new(i.intern(s), span);
            vec![
                TokenKind::KwVal,
                TokenKind::Ident(id("x")),
                TokenKind::ColonEq,
                TokenKind::LitInt(1),
                TokenKind::Semicolon,
                TokenKind::KwVal,
                TokenKind::Ident(id("y")),
                TokenKind::ColonEq,
                TokenKind::LitInt(2),
                TokenKind::Semicolon,
                TokenKind::KwVal,
                TokenKind::Ident(id("z")),
                TokenKind::ColonEq,
                TokenKind::LitInt(3),
                TokenKind::Semicolon,
            ]
        },
    );

    check("/* nested /* block */ comment */ val x := 1;", |i| {
        let span = Span::default();
        let mut id = |s| Ident::new(i.intern(s), span);
        vec![
            TokenKind::KwVal,
            TokenKind::Ident(id("x")),
            TokenKind::ColonEq,
            TokenKind::LitInt(1),
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
        r"'bad hex \xGG'",
        r#""bad uni \u{!!!!}""#,
        "''",
        "'abc'",
        "0x",
        "1.0_",
        "1__2",
        "1e",
    ];
    for input in cases {
        let mut ctx = TestCtx::new(input);
        let (_, diagnostics) = tokenize(&ctx.source, &mut ctx.interner);
        assert!(
            !diagnostics.diagnostics.is_empty(),
            "expected error for input: {input:?}"
        );
    }
}

#[test]
fn test_rejection_of_unicode() {
    let mut ctx = TestCtx::new("你好 世界 π_value 🦀_emojis");
    let (_, diagnostics) = tokenize(&ctx.source, &mut ctx.interner);
    assert!(!diagnostics.diagnostics.is_empty());
}

#[test]
fn test_nested_templates() {
    check(r#"$"outer { $"inner {x} tail" } end""#, |i| {
        let span = Span::default();
        let mut id = |s| Ident::new(i.intern(s), span);
        vec![
            TokenKind::TemplateHead(id("outer ")),
            TokenKind::TemplateHead(id("inner ")),
            TokenKind::Ident(id("x")),
            TokenKind::TemplateTail(id(" tail")),
            TokenKind::TemplateTail(id(" end")),
        ]
    });
}

#[test]
fn test_underscores() {
    check("_ _unused __test", |i| {
        let span = Span::default();
        let mut id = |s| Ident::new(i.intern(s), span);
        vec![
            TokenKind::Underscore,
            TokenKind::Ident(id("_unused")),
            TokenKind::Ident(id("__test")),
        ]
    });
}

#[test]
fn test_spans() {
    let mut ctx = TestCtx::new("val x := 123;");
    let x_id = Ident::new(ctx.interner.intern("x"), Span::new(4, 5));

    let tokens_with_spans = [
        (TokenKind::KwVal, 0, 3),
        (TokenKind::Ident(x_id), 4, 5),
        (TokenKind::ColonEq, 6, 8),
        (TokenKind::LitInt(123), 9, 12),
        (TokenKind::Semicolon, 12, 13),
    ];

    let (tokens, _) = tokenize(&ctx.source, &mut ctx.interner);
    for (i, (expected_tok, expected_lo, expected_hi)) in tokens_with_spans.iter().enumerate() {
        let tok = &tokens[i];
        assert_eq!(tok.kind, *expected_tok);
        assert_eq!(tok.span.lo, *expected_lo);
        assert_eq!(tok.span.hi, *expected_hi);
    }
}
