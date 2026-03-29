use music_basic::Span;

use super::*;

#[test]
fn token_can_carry_structured_fstring_parts() {
    let token = Token {
        kind: TokenKind::FStringLit(
            SmallVec::from_buf([
                FStringPart {
                    kind: FStringPartKind::Literal,
                    span: Span::new(2, 5),
                },
                FStringPart {
                    kind: FStringPartKind::Interpolation,
                    span: Span::new(6, 11),
                },
                FStringPart {
                    kind: FStringPartKind::Literal,
                    span: Span::new(12, 15),
                },
                FStringPart {
                    kind: FStringPartKind::Literal,
                    span: Span::new(0, 0),
                },
            ])
            .into_iter()
            .take(3)
            .collect(),
        ),
        span: Span::new(0, 16),
        leading_trivia: SmallVec::new(),
        trailing_trivia: SmallVec::new(),
    };

    let TokenKind::FStringLit(parts) = token.kind else {
        panic!("expected f-string token");
    };

    assert_eq!(parts.len(), 3);
    assert_eq!(parts[1].kind, FStringPartKind::Interpolation);
}

#[test]
fn token_display_uses_source_spelling_and_classes() {
    assert_eq!(display_token_kind(&TokenKind::LBrace).to_string(), "'{'");
    assert_eq!(display_token_kind(&TokenKind::KwLet).to_string(), "'let'");
    assert_eq!(display_token_kind(&TokenKind::Hash).to_string(), "'#'");
    assert_eq!(
        display_token_kind(&TokenKind::StringLit).to_string(),
        "string literal"
    );
    assert_eq!(
        display_token_kind(&TokenKind::Eof).to_string(),
        "end of file"
    );
}
