use super::*;
use musi_shared::{DiagnosticBag, FileId, Interner};

fn lex(source: &str) -> (Vec<Token>, DiagnosticBag) {
    let mut interner = Interner::new();
    let mut diags = DiagnosticBag::new();
    let file_id = FileId(0);
    let tokens: Vec<Token> = Lexer::new(source, file_id, &mut interner, &mut diags).collect();
    (tokens, diags)
}

fn lex_kinds(source: &str) -> Vec<TokenKind> {
    let (tokens, _) = lex(source);
    tokens.iter().map(|t| t.kind).collect()
}

fn lex_with_interner(source: &str) -> (Vec<Token>, Interner, DiagnosticBag) {
    let mut interner = Interner::new();
    let mut diags = DiagnosticBag::new();
    let file_id = FileId(0);
    let tokens: Vec<Token> = Lexer::new(source, file_id, &mut interner, &mut diags).collect();
    (tokens, interner, diags)
}

#[test]
fn lex_hello_world() {
    let kinds = lex_kinds(r#"writeln("Hello, world!");"#);
    assert_eq!(
        kinds,
        vec![
            TokenKind::Ident,
            TokenKind::LParen,
            TokenKind::StringLit,
            TokenKind::RParen,
            TokenKind::Semi,
            TokenKind::Eof,
        ]
    );
}

#[test]
fn all_keywords() {
    let keywords = [
        ("fn", TokenKind::Fn),
        ("const", TokenKind::Const),
        ("var", TokenKind::Var),
        ("if", TokenKind::If),
        ("then", TokenKind::Then),
        ("elif", TokenKind::Elif),
        ("else", TokenKind::Else),
        ("match", TokenKind::Match),
        ("case", TokenKind::Case),
        ("while", TokenKind::While),
        ("loop", TokenKind::Loop),
        ("for", TokenKind::For),
        ("in", TokenKind::In),
        ("break", TokenKind::Break),
        ("cycle", TokenKind::Cycle),
        ("return", TokenKind::Return),
        ("defer", TokenKind::Defer),
        ("import", TokenKind::Import),
        ("from", TokenKind::From),
        ("export", TokenKind::Export),
        ("native", TokenKind::Native),
        ("opaque", TokenKind::Opaque),
        ("record", TokenKind::Record),
        ("choice", TokenKind::Choice),
        ("and", TokenKind::And),
        ("or", TokenKind::Or),
        ("xor", TokenKind::Xor),
        ("not", TokenKind::Not),
        ("as", TokenKind::As),
        ("with", TokenKind::With),
        ("label", TokenKind::Label),
        ("shl", TokenKind::Shl),
        ("shr", TokenKind::Shr),
    ];
    for (text, expected) in keywords {
        let kinds = lex_kinds(text);
        assert_eq!(kinds[0], expected, "keyword: {text}");
    }
}

#[test]
fn hex_literal() {
    let (tokens, interner, _) = lex_with_interner("0xFF");
    assert_eq!(tokens[0].kind, TokenKind::IntLit);
    assert_eq!(
        interner.resolve(tokens[0].symbol.expect("has symbol")),
        "0xFF"
    );
}

#[test]
fn octal_literal() {
    let (tokens, interner, _) = lex_with_interner("0o77");
    assert_eq!(tokens[0].kind, TokenKind::IntLit);
    assert_eq!(
        interner.resolve(tokens[0].symbol.expect("has symbol")),
        "0o77"
    );
}

#[test]
fn binary_literal() {
    let (tokens, interner, _) = lex_with_interner("0b1010");
    assert_eq!(tokens[0].kind, TokenKind::IntLit);
    assert_eq!(
        interner.resolve(tokens[0].symbol.expect("has symbol")),
        "0b1010"
    );
}

#[test]
fn decimal_literal() {
    let (tokens, interner, _) = lex_with_interner("42");
    assert_eq!(tokens[0].kind, TokenKind::IntLit);
    assert_eq!(
        interner.resolve(tokens[0].symbol.expect("has symbol")),
        "42"
    );
}

#[test]
fn float_literal() {
    let (tokens, interner, _) = lex_with_interner("3.14");
    assert_eq!(tokens[0].kind, TokenKind::FloatLit);
    assert_eq!(
        interner.resolve(tokens[0].symbol.expect("has symbol")),
        "3.14"
    );
}

#[test]
fn numeric_with_underscores() {
    let (tokens, interner, _) = lex_with_interner("1_000_000");
    assert_eq!(tokens[0].kind, TokenKind::IntLit);
    assert_eq!(
        interner.resolve(tokens[0].symbol.expect("has symbol")),
        "1_000_000"
    );
}

#[test]
fn compound_dot_lbracket() {
    let kinds = lex_kinds(".[");
    assert_eq!(kinds[0], TokenKind::DotLBracket);
}

#[test]
fn compound_dot_lbrace() {
    let kinds = lex_kinds(".{");
    assert_eq!(kinds[0], TokenKind::DotLBrace);
}

#[test]
fn compound_lt_dot_dot() {
    let kinds = lex_kinds("<..");
    assert_eq!(kinds[0], TokenKind::LtDotDot);
}

#[test]
fn compound_dot_dot() {
    let kinds = lex_kinds("..");
    assert_eq!(kinds[0], TokenKind::DotDot);
}

#[test]
fn compound_dot_dot_lt() {
    let kinds = lex_kinds("..<");
    assert_eq!(kinds[0], TokenKind::DotDotLt);
}

#[test]
fn ty_ident_single() {
    let (tokens, interner, _) = lex_with_interner("'a");
    assert_eq!(tokens[0].kind, TokenKind::TyIdent);
    assert_eq!(
        interner.resolve(tokens[0].symbol.expect("has symbol")),
        "'a"
    );
}

#[test]
fn ty_ident_multi() {
    let (tokens, interner, _) = lex_with_interner("'key");
    assert_eq!(tokens[0].kind, TokenKind::TyIdent);
    assert_eq!(
        interner.resolve(tokens[0].symbol.expect("has symbol")),
        "'key"
    );
}

#[test]
fn char_literal() {
    let (tokens, interner, _) = lex_with_interner("'x'");
    assert_eq!(tokens[0].kind, TokenKind::CharLit);
    assert_eq!(
        interner.resolve(tokens[0].symbol.expect("has symbol")),
        "'x'"
    );
}

#[test]
fn char_literal_escape() {
    let (tokens, interner, _) = lex_with_interner(r"'\n'");
    assert_eq!(tokens[0].kind, TokenKind::CharLit);
    assert_eq!(
        interner.resolve(tokens[0].symbol.expect("has symbol")),
        r"'\n'"
    );
}

#[test]
fn escaped_ident() {
    let (tokens, interner, _) = lex_with_interner("`some-ident`");
    assert_eq!(tokens[0].kind, TokenKind::Ident);
    assert_eq!(
        interner.resolve(tokens[0].symbol.expect("has symbol")),
        "some-ident"
    );
}

#[test]
fn eq_token() {
    let kinds = lex_kinds("=");
    assert_eq!(kinds[0], TokenKind::Eq);
}

#[test]
fn slash_eq_token() {
    let kinds = lex_kinds("/=");
    assert_eq!(kinds[0], TokenKind::SlashEq);
}

#[test]
fn colon_colon_token() {
    let kinds = lex_kinds("::");
    assert_eq!(kinds[0], TokenKind::ColonColon);
}

#[test]
fn unterminated_string() {
    let (tokens, diags) = lex("\"hello");
    assert_eq!(tokens[0].kind, TokenKind::Error);
    assert!(diags.has_errors());
}

#[test]
fn unknown_character_produces_error_and_continues() {
    let (tokens, diags) = lex("\x01 foo");
    assert_eq!(tokens[0].kind, TokenKind::Error);
    assert_eq!(tokens[1].kind, TokenKind::Ident);
    assert_eq!(tokens[2].kind, TokenKind::Eof);
    assert!(diags.has_errors());
}

#[test]
fn doc_comment() {
    let (tokens, interner, _) = lex_with_interner("/// hello docs\nfoo");
    assert_eq!(tokens[0].kind, TokenKind::DocComment);
    assert_eq!(
        interner.resolve(tokens[0].symbol.expect("has symbol")),
        " hello docs"
    );
    assert_eq!(tokens[1].kind, TokenKind::Ident);
}

#[test]
fn line_comment_skipped() {
    let kinds = lex_kinds("// this is a comment\nfoo");
    assert_eq!(kinds[0], TokenKind::Ident); // foo
    assert_eq!(kinds[1], TokenKind::Eof);
}

#[test]
fn arrow_tokens() {
    let kinds = lex_kinds("-> =>");
    assert_eq!(kinds[0], TokenKind::MinusGt);
    assert_eq!(kinds[1], TokenKind::EqGt);
}

#[test]
fn colon_eq_token() {
    let kinds = lex_kinds(":=");
    assert_eq!(kinds[0], TokenKind::ColonEq);
}

#[test]
fn lt_minus_token() {
    let kinds = lex_kinds("<-");
    assert_eq!(kinds[0], TokenKind::LtMinus);
}

#[test]
fn lt_eq_and_gt_eq() {
    let kinds = lex_kinds("<= >=");
    assert_eq!(kinds[0], TokenKind::LtEq);
    assert_eq!(kinds[1], TokenKind::GtEq);
}

#[test]
fn single_char_tokens() {
    let kinds = lex_kinds("( ) { } [ ] , ; + * % | & ^ ~ ! @ #");
    assert_eq!(
        kinds,
        vec![
            TokenKind::LParen,
            TokenKind::RParen,
            TokenKind::LBrace,
            TokenKind::RBrace,
            TokenKind::LBracket,
            TokenKind::RBracket,
            TokenKind::Comma,
            TokenKind::Semi,
            TokenKind::Plus,
            TokenKind::Star,
            TokenKind::Percent,
            TokenKind::Pipe,
            TokenKind::Amp,
            TokenKind::Caret,
            TokenKind::Tilde,
            TokenKind::Bang,
            TokenKind::At,
            TokenKind::Hash,
            TokenKind::Eof,
        ]
    );
}

#[test]
fn underscore_is_its_own_token() {
    let kinds = lex_kinds("_");
    assert_eq!(kinds[0], TokenKind::Underscore);
}

#[test]
fn underscore_prefix_ident() {
    let (tokens, interner, _) = lex_with_interner("_foo");
    assert_eq!(tokens[0].kind, TokenKind::Ident);
    assert_eq!(
        interner.resolve(tokens[0].symbol.expect("has symbol")),
        "_foo"
    );
}

#[test]
fn empty_source_yields_eof() {
    let kinds = lex_kinds("");
    assert_eq!(kinds, vec![TokenKind::Eof]);
}

#[test]
fn whitespace_only_yields_eof() {
    let kinds = lex_kinds("   \t\n\r  ");
    assert_eq!(kinds, vec![TokenKind::Eof]);
}

#[test]
fn eof_emitted_only_once() {
    let mut interner = Interner::new();
    let mut diags = DiagnosticBag::new();
    let file_id = FileId(0);
    let mut lexer = Lexer::new("", file_id, &mut interner, &mut diags);
    let first = lexer.next();
    assert!(first.is_some());
    assert_eq!(first.expect("first token").kind, TokenKind::Eof);
    let second = lexer.next();
    assert!(second.is_none());
}

#[test]
fn span_positions_correct() {
    let (tokens, _, _) = lex_with_interner("foo bar");
    // "foo" at 0..3
    assert_eq!(tokens[0].span.start, 0);
    assert_eq!(tokens[0].span.length, 3);
    // "bar" at 4..7
    assert_eq!(tokens[1].span.start, 4);
    assert_eq!(tokens[1].span.length, 3);
}

#[test]
fn string_with_escapes() {
    let (tokens, interner, diags) = lex_with_interner(r#""hello\nworld""#);
    assert!(!diags.has_errors());
    assert_eq!(tokens[0].kind, TokenKind::StringLit);
    assert_eq!(
        interner.resolve(tokens[0].symbol.expect("has symbol")),
        r#""hello\nworld""#
    );
}

#[test]
fn unterminated_escaped_ident() {
    let (tokens, diags) = lex("`unterminated");
    assert_eq!(tokens[0].kind, TokenKind::Error);
    assert!(diags.has_errors());
}

#[test]
fn dot_alone() {
    let kinds = lex_kinds(".");
    assert_eq!(kinds[0], TokenKind::Dot);
}

#[test]
fn slash_alone() {
    let kinds = lex_kinds("/");
    assert_eq!(kinds[0], TokenKind::Slash);
}

#[test]
fn colon_alone() {
    let kinds = lex_kinds(":");
    assert_eq!(kinds[0], TokenKind::Colon);
}

#[test]
fn minus_alone() {
    let kinds = lex_kinds("-");
    assert_eq!(kinds[0], TokenKind::Minus);
}

#[test]
fn eq_alone() {
    let kinds = lex_kinds("=");
    assert_eq!(kinds[0], TokenKind::Eq);
}

#[test]
fn gt_alone() {
    let kinds = lex_kinds(">");
    assert_eq!(kinds[0], TokenKind::Gt);
}

#[test]
fn lt_alone() {
    let kinds = lex_kinds("<");
    assert_eq!(kinds[0], TokenKind::Lt);
}

#[test]
fn hex_with_underscores() {
    let (tokens, interner, _) = lex_with_interner("0xFF_FF");
    assert_eq!(tokens[0].kind, TokenKind::IntLit);
    assert_eq!(
        interner.resolve(tokens[0].symbol.expect("has symbol")),
        "0xFF_FF"
    );
}

#[test]
fn multiple_doc_comments() {
    let kinds = lex_kinds("/// first\n/// second\n");
    assert_eq!(kinds[0], TokenKind::DocComment);
    assert_eq!(kinds[1], TokenKind::DocComment);
    assert_eq!(kinds[2], TokenKind::Eof);
}

#[test]
fn size_hint_upper_bounds_token_count() {
    let src = "fn foo(x: Int): Int (x)";
    let mut interner = Interner::new();
    let mut diags = DiagnosticBag::new();
    let file_id = FileId(0);
    let lexer = Lexer::new(src, file_id, &mut interner, &mut diags);
    let (_, upper) = lexer.size_hint();
    let tokens: Vec<_> = Lexer::new(src, file_id, &mut interner, &mut diags).collect();
    assert!(upper.is_some());
    assert!(tokens.len() <= upper.expect("has upper bound"));
}
