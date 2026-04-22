use music_base::SourceMap;
use music_base::diag::DiagContext;

use super::*;

fn source_id(text: &str) -> SourceId {
    let mut map = SourceMap::default();
    map.add("test.ms", text).expect("source add succeeds")
}

mod success {
    use super::*;

    #[test]
    fn parse_diag_uses_end_of_input() {
        let text = "let x := 1";
        let diag = ParseError::new(
            ParseErrorKind::ExpectedToken {
                expected: TokenKind::Semicolon,
                found: TokenKind::Eof,
            },
            Span::new(10, 10),
        )
        .to_diag(source_id(text), text);

        let context = DiagContext::new()
            .with("expected", TokenKind::Semicolon)
            .with("found", TokenKind::Eof);
        assert_eq!(
            diag.message(),
            SyntaxDiagKind::ExpectedToken.message_with(&context)
        );
        assert!(!diag.labels()[0].message().is_empty());
    }
}

mod failure {
    use super::*;

    #[test]
    fn lex_diag_points_at_invalid_character() {
        let text = "€";
        let diag = LexError::new(LexErrorKind::InvalidChar { ch: '€' }, Span::new(0, 3))
            .to_diag(source_id(text), text);

        let context = DiagContext::new().with("ch", '€');
        assert_eq!(
            diag.message(),
            SyntaxDiagKind::InvalidChar.message_with(&context)
        );
        assert!(diag.labels()[0].message().contains('€'));
    }

    #[test]
    fn parse_diag_uses_fixit_hint_only_for_grouping_error() {
        let text = "a < b < c";
        let diag = ParseError::new(ParseErrorKind::NonAssociativeChain, Span::new(6, 7))
            .to_diag(source_id(text), text);

        assert_eq!(
            diag.message(),
            SyntaxDiagKind::NonAssociativeChain.message()
        );
        assert_eq!(diag.hint(), SyntaxDiagKind::NonAssociativeChain.hint());
    }

    #[test]
    fn parse_diag_reports_reserved_keyword_identifier() {
        let text = "let some := 1;";
        let diag = ParseError::new(
            ParseErrorKind::ReservedKeywordIdentifier {
                keyword: TokenKind::KwSome,
            },
            Span::new(4, 8),
        )
        .to_diag(source_id(text), text);

        let context = DiagContext::new().with("keyword", TokenKind::KwSome);
        assert_eq!(
            diag.message(),
            SyntaxDiagKind::ReservedKeywordIdentifier.message_with(&context)
        );
        assert!(
            diag.labels()[0]
                .message()
                .contains(TokenKind::KwSome.to_string().as_str())
        );
        assert_eq!(
            diag.hint(),
            SyntaxDiagKind::ReservedKeywordIdentifier.hint()
        );
    }
}
