use crate::basic::interner::Interner;
use crate::basic::source::SourceFile;
use crate::lex::lexer::Lexer;
use crate::lex::token::Token;

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

    fn lexer(&mut self) -> Lexer<'_> {
        Lexer::new(&self.source, &mut self.interner)
    }
}

fn check(input: &str, expected: impl FnOnce(&mut Interner) -> Vec<Token>) {
    let mut ctx = TestContext::new(input);
    let mut actual = vec![];
    {
        let mut lexer = ctx.lexer();
        loop {
            let (tok, _) = lexer.next_token();
            if tok == Token::EOF {
                break;
            }
            actual.push(tok);
        }
    }

    let expected_tokens = expected(&mut ctx.interner);
    assert_eq!(
        actual, expected_tokens,
        "Token mismatch for input: {:?}",
        input
    );
}

#[test]
fn test_numbers() {
    check("123 456_789 0xFF 0o77 0b1011", |i| {
        vec![
            Token::LitInt(i.intern("123")),
            Token::LitInt(i.intern("456789")),
            Token::LitInt(i.intern("0xFF")),
            Token::LitInt(i.intern("0o77")),
            Token::LitInt(i.intern("0b1011")),
        ]
    });

    check("3.14 1e10 1.2e-5", |i| {
        vec![
            Token::LitReal(i.intern("3.14")),
            Token::LitReal(i.intern("1e10")),
            Token::LitReal(i.intern("1.2e-5")),
        ]
    });
}

#[test]
fn test_idents_keywords() {
    check("val var if else match record sum fn", |_| {
        vec![
            Token::KwVal,
            Token::KwVar,
            Token::KwIf,
            Token::KwElse,
            Token::KwMatch,
            Token::KwRecord,
            Token::KwSum,
            Token::KwFn,
        ]
    });

    check("my_var _unused `escaped ident` `if`", |i| {
        vec![
            Token::Ident(i.intern("my_var")),
            Token::Ident(i.intern("_unused")),
            Token::Ident(i.intern("escaped ident")),
            Token::Ident(i.intern("if")),
        ]
    });
}

#[test]
fn test_strings_runes() {
    check(r#""hello" "with \"quotes\"" 'a' "\n""#, |i| {
        vec![
            Token::LitString(i.intern("hello")),
            Token::LitString(i.intern("with \"quotes\"")),
            Token::LitRune('a'),
            Token::LitRune('\n'),
        ]
    });
}

#[test]
fn test_templates() {
    check(r#"$"hello {x} middle {y} tail" $"no subst""#, |i| {
        vec![
            Token::TemplateHead(i.intern("hello ")),
            Token::Ident(i.intern("x")),
            Token::TemplateMiddle(i.intern(" middle ")),
            Token::Ident(i.intern("y")),
            Token::TemplateTail(i.intern(" tail")),
            Token::LitTemplateNoSubst(i.intern("no subst")),
        ]
    });
}

#[test]
fn test_symbols() {
    check("+ - * / % mod **", |_| {
        vec![
            Token::Plus,
            Token::Minus,
            Token::Star,
            Token::Slash,
            Token::Percent,
            Token::KwMod,
            Token::StarStar,
        ]
    });

    check("= /= < <= > >= and or not", |_| {
        vec![
            Token::Eq,
            Token::SlashEq,
            Token::Lt,
            Token::LtEq,
            Token::Gt,
            Token::GtEq,
            Token::KwAnd,
            Token::KwOr,
            Token::KwNot,
        ]
    });

    check(".. ..< |> ?? := => -> <- :: .^ ?", |_| {
        vec![
            Token::DotDot,
            Token::DotDotLt,
            Token::BarGt,
            Token::QuestionQuestion,
            Token::ColonEq,
            Token::EqGt,
            Token::MinusGt,
            Token::LtMinus,
            Token::ColonColon,
            Token::DotCaret,
            Token::Question,
        ]
    });
}

#[test]
fn test_comments() {
    check(
        "val x := 1; // line comment\nval y := 2; /* block\ncomment */ val z := 3;",
        |i| {
            vec![
                Token::KwVal,
                Token::Ident(i.intern("x")),
                Token::ColonEq,
                Token::LitInt(i.intern("1")),
                Token::Semicolon,
                Token::KwVal,
                Token::Ident(i.intern("y")),
                Token::ColonEq,
                Token::LitInt(i.intern("2")),
                Token::Semicolon,
                Token::KwVal,
                Token::Ident(i.intern("z")),
                Token::ColonEq,
                Token::LitInt(i.intern("3")),
                Token::Semicolon,
            ]
        },
    );

    check("/* nested /* block */ comment */ val x := 1;", |i| {
        vec![
            Token::KwVal,
            Token::Ident(i.intern("x")),
            Token::ColonEq,
            Token::LitInt(i.intern("1")),
            Token::Semicolon,
        ]
    });
}

#[test]
fn test_error_reporting() {
    let mut ctx = TestContext::new("\"unclosed string");
    let mut lexer = ctx.lexer();
    let (tok, _) = lexer.next_token();
    assert!(matches!(tok, Token::Invalid(_)));
    assert!(!lexer.errors().is_empty());
}

#[test]
fn test_spans() {
    let mut ctx = TestContext::new("val x := 123;");
    let x_id = ctx.interner.intern("x");
    let num_id = ctx.interner.intern("123");

    let tokens_with_spans = vec![
        (Token::KwVal, 0, 3),
        (Token::Ident(x_id), 4, 5),
        (Token::ColonEq, 6, 8),
        (Token::LitInt(num_id), 9, 12),
        (Token::Semicolon, 12, 13),
    ];

    let mut lexer = ctx.lexer();
    for (expected_tok, expected_lo, expected_hi) in tokens_with_spans {
        let (tok, span) = lexer.next_token();
        assert_eq!(tok, expected_tok);
        assert_eq!(span.lo, expected_lo);
        assert_eq!(span.hi, expected_hi);
    }
}
