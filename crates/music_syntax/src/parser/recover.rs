use super::*;

impl Parser<'_> {
    pub(super) fn parse_error_stmt(&mut self) -> SyntaxNodeId {
        let mut children = Vec::new();
        while !self.at_any(&[
            TokenKind::Semicolon,
            TokenKind::RParen,
            TokenKind::RBracket,
            TokenKind::RBrace,
            TokenKind::Pipe,
            TokenKind::Eof,
        ]) {
            children.push(self.advance_element());
        }
        if self.at(TokenKind::Semicolon) {
            children.push(self.advance_element());
        }
        self.builder.push_error_node(children)
    }

    pub(super) fn synchronize_stmt(&mut self) {
        while !self.at_any(&[
            TokenKind::Semicolon,
            TokenKind::RParen,
            TokenKind::RBracket,
            TokenKind::RBrace,
            TokenKind::Pipe,
            TokenKind::Eof,
        ]) {
            let _ = self.advance();
        }
        if self.at(TokenKind::Semicolon) {
            let _ = self.advance();
        }
    }

    pub(super) fn expected_expression(&self) -> ParseError {
        ParseError::new(
            ParseErrorKind::ExpectedExpression {
                found: self.found_token(),
            },
            self.span(),
        )
    }

    pub(super) fn expected_pattern(&self) -> ParseError {
        ParseError::new(
            ParseErrorKind::ExpectedPattern {
                found: self.found_token(),
            },
            self.span(),
        )
    }

    pub(super) fn expected_member(&self) -> ParseError {
        ParseError::new(
            ParseErrorKind::ExpectedMember {
                found: self.found_token(),
            },
            self.span(),
        )
    }

    pub(super) fn expected_splice_target(&self) -> ParseError {
        ParseError::new(
            ParseErrorKind::ExpectedSpliceTarget {
                found: self.found_token(),
            },
            self.span(),
        )
    }

    pub(super) fn expected_operator_member_name(&self) -> ParseError {
        ParseError::new(
            ParseErrorKind::ExpectedOperatorMemberName {
                found: self.found_token(),
            },
            self.span(),
        )
    }

    pub(super) fn expected_field_target(&self) -> ParseError {
        ParseError::new(
            ParseErrorKind::ExpectedFieldTarget {
                found: self.found_token(),
            },
            self.span(),
        )
    }

    pub(super) fn expected_constraint_operator(&self) -> ParseError {
        ParseError::new(
            ParseErrorKind::ExpectedConstraintOperator {
                found: self.found_token(),
            },
            self.span(),
        )
    }

    pub(super) fn expected_attr_value(&self) -> ParseError {
        ParseError::new(
            ParseErrorKind::ExpectedAttrValue {
                found: self.found_token(),
            },
            self.span(),
        )
    }
}
