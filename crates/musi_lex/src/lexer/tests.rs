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

// === Keywords ===

#[test]
fn test_all_keywords_recognized() {
    let keywords = [
        ("and", TokenKind::KwAnd),
        ("await", TokenKind::KwAwait),
        ("class", TokenKind::KwClass),
        ("defer", TokenKind::KwDefer),
        ("exists", TokenKind::KwExists),
        ("export", TokenKind::KwExport),
        ("forall", TokenKind::KwForall),
        ("given", TokenKind::KwGiven),
        ("if", TokenKind::KwIf),
        ("import", TokenKind::KwImport),
        ("in", TokenKind::KwIn),
        ("inout", TokenKind::KwInout),
        ("law", TokenKind::KwLaw),
        ("let", TokenKind::KwLet),
        ("match", TokenKind::KwMatch),
        ("not", TokenKind::KwNot),
        ("of", TokenKind::KwOf),
        ("or", TokenKind::KwOr),
        ("over", TokenKind::KwOver),
        ("ref", TokenKind::KwRef),
        ("return", TokenKind::KwReturn),
        ("spawn", TokenKind::KwSpawn),
        ("try", TokenKind::KwTry),
        ("var", TokenKind::KwVar),
        ("where", TokenKind::KwWhere),
        ("xor", TokenKind::KwXor),
    ];
    for (text, expected) in keywords {
        let kinds = lex_kinds(text);
        assert_eq!(kinds[0], expected, "keyword: {text}");
    }
}

// === Multi-char punctuation ===

#[test]
fn test_dot_disambiguation() {
    assert_eq!(lex_kinds(".")[0], TokenKind::Dot);
    assert_eq!(lex_kinds("..")[0], TokenKind::DotDot);
    assert_eq!(lex_kinds("...")[0], TokenKind::DotDotDot);
    assert_eq!(lex_kinds("..<")[0], TokenKind::DotDotLt);
    assert_eq!(lex_kinds(".[")[0], TokenKind::DotLBracket);
    assert_eq!(lex_kinds(".{")[0], TokenKind::DotLBrace);
}

#[test]
fn test_arrow_tokens() {
    let kinds = lex_kinds("-> =>");
    assert_eq!(kinds[0], TokenKind::DashGt);
    assert_eq!(kinds[1], TokenKind::EqGt);
}

#[test]
fn test_tilde_gt() {
    assert_eq!(lex_kinds("~>")[0], TokenKind::TildeGt);
}

#[test]
fn test_bare_tilde_is_error() {
    let (tokens, diags) = lex("~");
    assert_eq!(tokens[0].kind, TokenKind::Error);
    assert!(diags.has_errors());
}

#[test]
fn test_pipe_gt() {
    assert_eq!(lex_kinds("|>")[0], TokenKind::PipeGt);
}

#[test]
fn test_pipe_alone() {
    assert_eq!(lex_kinds("|")[0], TokenKind::Pipe);
}

#[test]
fn test_hash_lbracket() {
    assert_eq!(lex_kinds("#[")[0], TokenKind::HashLBracket);
}

#[test]
fn test_bare_hash_is_error() {
    let (tokens, diags) = lex("# ");
    assert_eq!(tokens[0].kind, TokenKind::Error);
    assert!(diags.has_errors());
}

#[test]
fn test_lt_dash() {
    assert_eq!(lex_kinds("<-")[0], TokenKind::LtDash);
}

#[test]
fn test_lt_colon() {
    assert_eq!(lex_kinds("<:")[0], TokenKind::LtColon);
}

#[test]
fn test_lt_lt() {
    assert_eq!(lex_kinds("<<")[0], TokenKind::LtLt);
}

#[test]
fn test_gt_gt() {
    assert_eq!(lex_kinds(">>")[0], TokenKind::GtGt);
}

#[test]
fn test_colon_gt() {
    assert_eq!(lex_kinds(":>")[0], TokenKind::ColonGt);
}

#[test]
fn test_colon_eq() {
    assert_eq!(lex_kinds(":=")[0], TokenKind::ColonEq);
}

#[test]
fn test_colon_colon() {
    assert_eq!(lex_kinds("::")[0], TokenKind::ColonColon);
}

#[test]
fn test_slash_eq() {
    assert_eq!(lex_kinds("/=")[0], TokenKind::SlashEq);
}

#[test]
fn test_lt_eq_and_gt_eq() {
    let kinds = lex_kinds("<= >=");
    assert_eq!(kinds[0], TokenKind::LtEq);
    assert_eq!(kinds[1], TokenKind::GtEq);
}

#[test]
fn test_question_tokens() {
    assert_eq!(lex_kinds("?")[0], TokenKind::Question);
    assert_eq!(lex_kinds("?.")[0], TokenKind::QuestionDot);
    assert_eq!(lex_kinds("??")[0], TokenKind::QuestionQuestion);
}

#[test]
fn test_question_disambiguation() {
    assert_eq!(
        lex_kinds("x?.y ?? z"),
        vec![
            TokenKind::Ident,
            TokenKind::QuestionDot,
            TokenKind::Ident,
            TokenKind::QuestionQuestion,
            TokenKind::Ident,
            TokenKind::Eof,
        ]
    );
}

// === Single-char tokens ===

#[test]
fn test_single_char_tokens() {
    let kinds = lex_kinds("( ) { } [ ] , ; + * %");
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
            TokenKind::Eof,
        ]
    );
}

#[test]
fn test_standalone_tokens() {
    assert_eq!(lex_kinds(".")[0], TokenKind::Dot);
    assert_eq!(lex_kinds("/")[0], TokenKind::Slash);
    assert_eq!(lex_kinds(":")[0], TokenKind::Colon);
    assert_eq!(lex_kinds("-")[0], TokenKind::Minus);
    assert_eq!(lex_kinds("=")[0], TokenKind::Eq);
    assert_eq!(lex_kinds(">")[0], TokenKind::Gt);
    assert_eq!(lex_kinds("<")[0], TokenKind::Lt);
}

// === Identifiers ===

#[test]
fn test_underscore_is_its_own_token() {
    assert_eq!(lex_kinds("_")[0], TokenKind::Underscore);
}

#[test]
fn test_underscore_prefix_ident() {
    let (tokens, interner, _) = lex_with_interner("_foo");
    assert_eq!(tokens[0].kind, TokenKind::Ident);
    assert_eq!(
        interner.resolve(tokens[0].symbol.expect("has symbol")),
        "_foo"
    );
}

#[test]
fn test_escaped_ident() {
    let (tokens, interner, _) = lex_with_interner("`some-ident`");
    assert_eq!(tokens[0].kind, TokenKind::Ident);
    assert_eq!(
        interner.resolve(tokens[0].symbol.expect("has symbol")),
        "some-ident"
    );
}

#[test]
fn test_unterminated_escaped_ident() {
    let (tokens, diags) = lex("`unterminated");
    assert_eq!(tokens[0].kind, TokenKind::Error);
    assert!(diags.has_errors());
}

// === Tick disambiguation ===

#[test]
fn test_rune_literal() {
    let (tokens, interner, _) = lex_with_interner("'x'");
    assert_eq!(tokens[0].kind, TokenKind::RuneLit);
    assert_eq!(
        interner.resolve(tokens[0].symbol.expect("has symbol")),
        "'x'"
    );
}

#[test]
fn test_rune_literal_escape() {
    let (tokens, interner, _) = lex_with_interner(r"'\n'");
    assert_eq!(tokens[0].kind, TokenKind::RuneLit);
    assert_eq!(
        interner.resolve(tokens[0].symbol.expect("has symbol")),
        r"'\n'"
    );
}

#[test]
fn test_ty_ident_single_letter() {
    let (tokens, interner, _) = lex_with_interner("'a");
    assert_eq!(tokens[0].kind, TokenKind::TyIdent);
    assert_eq!(
        interner.resolve(tokens[0].symbol.expect("has symbol")),
        "'a"
    );
}

#[test]
fn test_ty_ident_multi_letter() {
    let (tokens, interner, _) = lex_with_interner("'key");
    assert_eq!(tokens[0].kind, TokenKind::TyIdent);
    assert_eq!(
        interner.resolve(tokens[0].symbol.expect("has symbol")),
        "'key"
    );
}

// === Number literals ===

#[test]
fn test_decimal_literal() {
    let (tokens, interner, _) = lex_with_interner("42");
    assert_eq!(tokens[0].kind, TokenKind::IntLit);
    assert_eq!(
        interner.resolve(tokens[0].symbol.expect("has symbol")),
        "42"
    );
}

#[test]
fn test_hex_literal() {
    let (tokens, interner, _) = lex_with_interner("0xFF");
    assert_eq!(tokens[0].kind, TokenKind::IntLit);
    assert_eq!(
        interner.resolve(tokens[0].symbol.expect("has symbol")),
        "0xFF"
    );
}

#[test]
fn test_octal_literal() {
    let (tokens, interner, _) = lex_with_interner("0o77");
    assert_eq!(tokens[0].kind, TokenKind::IntLit);
    assert_eq!(
        interner.resolve(tokens[0].symbol.expect("has symbol")),
        "0o77"
    );
}

#[test]
fn test_binary_literal() {
    let (tokens, interner, _) = lex_with_interner("0b1010");
    assert_eq!(tokens[0].kind, TokenKind::IntLit);
    assert_eq!(
        interner.resolve(tokens[0].symbol.expect("has symbol")),
        "0b1010"
    );
}

#[test]
fn test_float_literal() {
    let (tokens, interner, _) = lex_with_interner("3.14");
    assert_eq!(tokens[0].kind, TokenKind::FloatLit);
    assert_eq!(
        interner.resolve(tokens[0].symbol.expect("has symbol")),
        "3.14"
    );
}

#[test]
fn test_float_with_exponent() {
    let (tokens, interner, _) = lex_with_interner("1.5e10");
    assert_eq!(tokens[0].kind, TokenKind::FloatLit);
    assert_eq!(
        interner.resolve(tokens[0].symbol.expect("has symbol")),
        "1.5e10"
    );
}

#[test]
fn test_float_with_negative_exponent() {
    let (tokens, interner, _) = lex_with_interner("2.0E-3");
    assert_eq!(tokens[0].kind, TokenKind::FloatLit);
    assert_eq!(
        interner.resolve(tokens[0].symbol.expect("has symbol")),
        "2.0E-3"
    );
}

#[test]
fn test_int_with_exponent_is_float() {
    let (tokens, interner, _) = lex_with_interner("1e10");
    assert_eq!(tokens[0].kind, TokenKind::FloatLit);
    assert_eq!(
        interner.resolve(tokens[0].symbol.expect("has symbol")),
        "1e10"
    );
}

#[test]
fn test_numeric_with_underscores() {
    let (tokens, interner, _) = lex_with_interner("1_000_000");
    assert_eq!(tokens[0].kind, TokenKind::IntLit);
    assert_eq!(
        interner.resolve(tokens[0].symbol.expect("has symbol")),
        "1_000_000"
    );
}

#[test]
fn test_hex_with_underscores() {
    let (tokens, interner, _) = lex_with_interner("0xFF_FF");
    assert_eq!(tokens[0].kind, TokenKind::IntLit);
    assert_eq!(
        interner.resolve(tokens[0].symbol.expect("has symbol")),
        "0xFF_FF"
    );
}

// === String literals ===

#[test]
fn test_string_with_escapes() {
    let (tokens, interner, diags) = lex_with_interner(r#""hello\nworld""#);
    assert!(!diags.has_errors());
    assert_eq!(tokens[0].kind, TokenKind::StringLit);
    assert_eq!(
        interner.resolve(tokens[0].symbol.expect("has symbol")),
        r#""hello\nworld""#
    );
}

#[test]
fn test_unterminated_string() {
    let (tokens, diags) = lex("\"hello");
    assert_eq!(tokens[0].kind, TokenKind::Error);
    assert!(diags.has_errors());
}

// === F-string tests ===

#[test]
fn test_fstring_no_interpolation() {
    let kinds = lex_kinds(r#"f"hello""#);
    // f"hello" with no braces: FStringHead with full content
    assert_eq!(kinds[0], TokenKind::FStringHead);
    assert_eq!(kinds[1], TokenKind::Eof);
}

#[test]
fn test_fstring_single_interpolation() {
    let kinds = lex_kinds(r#"f"hi {name}""#);
    assert_eq!(kinds[0], TokenKind::FStringHead); // f"hi {
    assert_eq!(kinds[1], TokenKind::Ident); // name
    assert_eq!(kinds[2], TokenKind::FStringTail); // }"
    assert_eq!(kinds[3], TokenKind::Eof);
}

#[test]
fn test_fstring_multiple_interpolations() {
    let kinds = lex_kinds(r#"f"{a} and {b}""#);
    assert_eq!(kinds[0], TokenKind::FStringHead); // f"{
    assert_eq!(kinds[1], TokenKind::Ident); // a
    assert_eq!(kinds[2], TokenKind::FStringMiddle); // } and {
    assert_eq!(kinds[3], TokenKind::Ident); // b
    assert_eq!(kinds[4], TokenKind::FStringTail); // }"
    assert_eq!(kinds[5], TokenKind::Eof);
}

#[test]
fn test_fstring_nested_braces() {
    // f"{ {1, 2} }" — the inner { } should be tracked by brace_depth
    let kinds = lex_kinds(r#"f"{ {1, 2} }""#);
    assert_eq!(kinds[0], TokenKind::FStringHead); // f"{
    assert_eq!(kinds[1], TokenKind::LBrace); // {
    assert_eq!(kinds[2], TokenKind::IntLit); // 1
    assert_eq!(kinds[3], TokenKind::Comma); // ,
    assert_eq!(kinds[4], TokenKind::IntLit); // 2
    assert_eq!(kinds[5], TokenKind::RBrace); // }
    assert_eq!(kinds[6], TokenKind::FStringTail); // }"
    assert_eq!(kinds[7], TokenKind::Eof);
}

// === Comments as trivia ===

#[test]
fn test_line_comment_skipped() {
    let kinds = lex_kinds("// this is a comment\nfoo");
    assert_eq!(kinds[0], TokenKind::Ident); // foo
    assert_eq!(kinds[1], TokenKind::Eof);
}

#[test]
fn test_doc_comment_becomes_trivia() {
    let (tokens, _, _) = lex_with_interner("/// hello docs\nfoo");
    assert_eq!(tokens[0].kind, TokenKind::Ident); // foo
    assert_eq!(tokens[1].kind, TokenKind::Eof);
    assert!(
        tokens[0].leading_trivia.len > 0,
        "doc comment should be in leading trivia"
    );
}

#[test]
fn test_block_comment_skipped() {
    let kinds = lex_kinds("/* comment */ foo");
    assert_eq!(kinds[0], TokenKind::Ident); // foo
    assert_eq!(kinds[1], TokenKind::Eof);
}

#[test]
fn test_block_doc_comment_becomes_trivia() {
    let (tokens, _, _) = lex_with_interner("/** doc comment */ foo");
    assert_eq!(tokens[0].kind, TokenKind::Ident);
    assert!(tokens[0].leading_trivia.len > 0);
}

#[test]
fn test_unclosed_block_comment_produces_error() {
    let (_, diags) = lex("/* unclosed");
    assert!(diags.has_errors());
}

// === Error recovery ===

#[test]
fn test_unknown_character_produces_error_and_continues() {
    let (tokens, diags) = lex("\x01 foo");
    assert_eq!(tokens[0].kind, TokenKind::Error);
    assert_eq!(tokens[1].kind, TokenKind::Ident);
    assert_eq!(tokens[2].kind, TokenKind::Eof);
    assert!(diags.has_errors());
}

// === Edge cases ===

#[test]
fn test_empty_source_yields_eof() {
    let kinds = lex_kinds("");
    assert_eq!(kinds, vec![TokenKind::Eof]);
}

#[test]
fn test_whitespace_only_yields_eof() {
    let kinds = lex_kinds("   \t\n\r  ");
    assert_eq!(kinds, vec![TokenKind::Eof]);
}

#[test]
fn test_eof_emitted_only_once() {
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
fn test_span_positions_correct() {
    let (tokens, _, _) = lex_with_interner("foo bar");
    // "foo" at 0..3
    assert_eq!(tokens[0].span.start, 0);
    assert_eq!(tokens[0].span.length, 3);
    // "bar" at 4..7
    assert_eq!(tokens[1].span.start, 4);
    assert_eq!(tokens[1].span.length, 3);
}

#[test]
fn test_multiple_doc_comments_become_trivia() {
    let kinds = lex_kinds("/// first\n/// second\n");
    assert_eq!(kinds[0], TokenKind::Eof);
}

#[test]
fn test_size_hint_upper_bounds_token_count() {
    let src = "let foo(x: Int): Int (x)";
    let mut interner = Interner::new();
    let mut diags = DiagnosticBag::new();
    let file_id = FileId(0);
    let lexer = Lexer::new(src, file_id, &mut interner, &mut diags);
    let (_, upper) = lexer.size_hint();
    let tokens: Vec<_> = Lexer::new(src, file_id, &mut interner, &mut diags).collect();
    assert!(upper.is_some());
    assert!(tokens.len() <= upper.expect("has upper bound"));
}

#[test]
fn test_lex_hello_world() {
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
