use music_base::SourceMap;

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

        assert_eq!(diag.message(), "expected `;`, found end of file");
        assert_eq!(diag.labels()[0].message(), "found end of input");
    }
}

mod failure {
    use super::*;

    #[test]
    fn lex_diag_points_at_invalid_character() {
        let text = "€";
        let diag = LexError::new(LexErrorKind::InvalidChar { ch: '€' }, Span::new(0, 3))
            .to_diag(source_id(text), text);

        assert_eq!(diag.message(), "invalid character `€`");
        assert_eq!(diag.labels()[0].message(), "found `€`");
    }

    #[test]
    fn parse_diag_uses_fixit_hint_only_for_grouping_error() {
        let text = "a < b < c";
        let diag = ParseError::new(ParseErrorKind::NonAssociativeChain, Span::new(6, 7))
            .to_diag(source_id(text), text);

        assert_eq!(diag.message(), "comparison chain requires grouping");
        assert_eq!(diag.hint(), Some("parenthesize comparison"));
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

        assert_eq!(
            diag.message(),
            "reserved keyword `some` cannot name identifier"
        );
        assert_eq!(
            diag.labels()[0].message(),
            "`some` found where identifier required"
        );
        assert_eq!(diag.hint(), Some("choose non-keyword identifier"));
    }
}
