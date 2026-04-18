use super::*;

impl Parser<'_> {
    pub(crate) fn peek(&self) -> &Token {
        self.tokens
            .get(self.pos)
            .unwrap_or_else(|| self.tokens.last().expect("lexer emits EOF token"))
    }

    pub(crate) fn nth(&self, offset: usize) -> &Token {
        self.tokens
            .get(self.pos + offset)
            .unwrap_or_else(|| self.tokens.last().expect("lexer emits EOF token"))
    }

    pub(crate) fn peek_kind(&self) -> TokenKind {
        self.peek().kind
    }

    pub(crate) fn nth_kind(&self, offset: usize) -> TokenKind {
        self.nth(offset).kind
    }

    pub(crate) fn at(&self, expected: TokenKind) -> bool {
        same_kind(self.peek_kind(), expected)
    }

    pub(crate) fn at_any(&self, expected: &[TokenKind]) -> bool {
        expected.iter().copied().any(|kind| self.at(kind))
    }

    pub(crate) fn advance(&mut self) -> &Token {
        let index = self.pos;
        if !self.at(TokenKind::Eof) {
            self.pos += 1;
        }
        self.tokens
            .get(index)
            .unwrap_or_else(|| self.tokens.last().expect("lexer emits EOF token"))
    }

    pub(crate) fn advance_element(&mut self) -> SyntaxElementId {
        let index = self.pos;
        let _ = self.advance();
        let raw = u32::try_from(index).expect("token index fits in u32");
        SyntaxElementId::Token(SyntaxTokenId::from_raw(raw))
    }

    pub(crate) fn eat(&mut self, expected: TokenKind) -> Option<SyntaxElementId> {
        self.at(expected).then(|| self.advance_element())
    }

    pub(crate) fn expect_token(&mut self, expected: TokenKind) -> SyntaxElementParseResult {
        if self.at(expected) {
            return Ok(self.advance_element());
        }
        Err(self.expected_token(expected))
    }

    pub(crate) fn expected_token(&self, expected: TokenKind) -> ParseError {
        ParseError::new(
            ParseErrorKind::ExpectedToken {
                expected,
                found: self.found_token(),
            },
            self.span(),
        )
    }

    pub(crate) fn expect_ident_element(&mut self) -> SyntaxElementParseResult {
        match self.peek_kind() {
            TokenKind::Ident | TokenKind::KwAny | TokenKind::KwSome => Ok(self.advance_element()),
            _ => Err(ParseError::new(
                ParseErrorKind::ExpectedIdentifier {
                    found: self.found_token(),
                },
                self.span(),
            )),
        }
    }

    pub(crate) fn expect_name_element(&mut self) -> SyntaxElementParseResult {
        match self.peek_kind() {
            TokenKind::Ident | TokenKind::OpIdent | TokenKind::KwAny | TokenKind::KwSome => {
                Ok(self.advance_element())
            }
            _ => Err(ParseError::new(
                ParseErrorKind::ExpectedIdentifier {
                    found: self.found_token(),
                },
                self.span(),
            )),
        }
    }

    pub(crate) fn span(&self) -> Span {
        self.peek().span
    }

    pub(crate) fn found_token(&self) -> TokenKind {
        self.peek_kind()
    }

    pub(crate) fn error(&mut self, error: ParseError) {
        self.errors.push(error);
    }
}
