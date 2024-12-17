use std::{
    cmp::{self},
    sync::Arc,
};

use crate::core::{diagnostics::Diagnostic, source::SourceFile, span::Span, MusiResult};

use super::{
    cursor::Cursor,
    token::{Token, TokenKind},
};

const MAX_INDENT_DEPTH: usize = 32;

const RESERVED_KEYWORDS: &[(&[u8], TokenKind)] = &[
    (b"and", TokenKind::And),
    (b"as", TokenKind::As),
    (b"break", TokenKind::Break),
    (b"continue", TokenKind::Continue),
    (b"deref", TokenKind::Deref),
    (b"do", TokenKind::Do),
    (b"else", TokenKind::Else),
    (b"false", TokenKind::False),
    (b"for", TokenKind::For),
    (b"forall", TokenKind::Forall),
    (b"from", TokenKind::From),
    (b"if", TokenKind::If),
    (b"in", TokenKind::In),
    (b"is", TokenKind::Is),
    (b"let", TokenKind::Let),
    (b"match", TokenKind::Match),
    (b"not", TokenKind::Not),
    (b"of", TokenKind::Of),
    (b"or", TokenKind::Or),
    (b"ref", TokenKind::Ref),
    (b"return", TokenKind::Return),
    (b"then", TokenKind::Then),
    (b"true", TokenKind::True),
    (b"type", TokenKind::Type),
    (b"until", TokenKind::Until),
    (b"var", TokenKind::Var),
    (b"when", TokenKind::When),
    (b"where", TokenKind::Where),
    (b"while", TokenKind::While),
    (b"with", TokenKind::With),
    (b"yield", TokenKind::Yield),
];

pub struct Lexer {
    cursor: Cursor,
    indent_stack: Vec<u32>,
    indent_width: Option<u32>,
    at_line_start: bool,
    pending_dedents: u32,
}

impl Lexer {
    #[inline]
    #[must_use]
    pub fn new(source: Arc<SourceFile>) -> Self {
        let mut indent_stack = Vec::with_capacity(MAX_INDENT_DEPTH);
        indent_stack.push(0);

        Self {
            cursor: Cursor {
                source,
                position: 0,
                line: 0,
            },
            indent_stack,
            indent_width: None,
            at_line_start: true,
            pending_dedents: 0,
        }
    }

    // #[expect(clippy::print_stdout, reason = "debugging")]
    // #[expect(clippy::use_debug, reason = "debugging")]
    #[inline]
    pub fn lex(&mut self) -> MusiResult<Vec<Token>> {
        let mut tokens = Vec::with_capacity(self.cursor.remaining() >> 3); // divided by 8

        loop {
            match self.next_token() {
                Ok(token) => match token.kind {
                    TokenKind::Whitespace => continue,
                    TokenKind::Eof => {
                        tokens.push(token);
                        break;
                    }
                    _ => tokens.push(token),
                },
                Err(diagnostic) => return Err(diagnostic),
            }
        }

        Ok(tokens)
    }

    fn next_token(&mut self) -> MusiResult<Token> {
        let start = self.cursor.position;

        if self.pending_dedents > 0 {
            self.pending_dedents = self.pending_dedents.saturating_sub(1);

            return Ok(Token::new(
                TokenKind::Dedent,
                &[],
                Span {
                    start,
                    end: self.cursor.position,
                },
            ));
        }

        if self.cursor.is_at_end() {
            if self.indent_stack.len() > 1 {
                let dedents = self.indent_stack.len().saturating_sub(1);

                self.indent_stack.truncate(1);

                if dedents > 0 {
                    self.pending_dedents = u32::try_from(dedents.saturating_sub(1)).unwrap_or(0);

                    return Ok(Token::new(TokenKind::Dedent, &[], Span::default()));
                }
            }

            return Ok(Token::new(TokenKind::Eof, &[], Span::default()));
        }

        if let Some(whitespace_token) = self.lex_whitespace() {
            return Ok(whitespace_token);
        }
        if let Some(indent_token) = self.lex_indentation()? {
            return Ok(indent_token);
        }

        match self.cursor.peek() {
            Some(current) => match current {
                b'\r' | b'\n' => Ok(self.lex_newline()),

                byte if byte.is_ascii_alphabetic() || byte == b'_' => {
                    Ok(self.lex_identifier_or_keyword())
                }
                byte if byte.is_ascii_digit() => Ok(self.lex_numberic_literal()?),

                b'"' => self.lex_string_literal(),

                b'(' => Ok(self.make_token(TokenKind::LeftParen, 1)),
                b')' => Ok(self.make_token(TokenKind::RightParen, 1)),
                b'{' => Ok(self.make_token(TokenKind::LeftBrace, 1)),
                b'}' => Ok(self.make_token(TokenKind::RightBrace, 1)),
                b'[' => Ok(self.make_token(TokenKind::LeftBracket, 1)),
                b']' => Ok(self.make_token(TokenKind::RightBracket, 1)),
                b',' => Ok(self.make_token(TokenKind::Comma, 1)),
                b':' => self.lex_multi_byte_token(&[
                    (TokenKind::ColonEquals, b":="),
                    (TokenKind::Colon, b":"),
                ]),
                b'.' => Ok(self.make_token(TokenKind::Dot, 1)),

                b'+' => Ok(self.make_token(TokenKind::Plus, 1)),
                b'-' => self.lex_multi_byte_token(&[
                    (TokenKind::MinusGreater, b"->"),
                    (TokenKind::Minus, b"-"),
                ]),
                b'*' => Ok(self.make_token(TokenKind::Star, 1)),
                b'/' => self.lex_multi_byte_token(&[
                    (TokenKind::SlashEquals, b"/="),
                    (TokenKind::Slash, b"/"),
                ]),
                b'^' => Ok(self.make_token(TokenKind::Caret, 1)),
                b'|' => self.lex_multi_byte_token(&[
                    (TokenKind::PipeGreater, b"|>"),
                    (TokenKind::Pipe, b"|"),
                ]),
                b'<' => self.lex_multi_byte_token(&[
                    (TokenKind::LessEqualsGreater, b"<=>"),
                    (TokenKind::LessEquals, b"<="),
                    (TokenKind::Less, b"<"),
                ]),
                b'>' => self.lex_multi_byte_token(&[
                    (TokenKind::GreaterEquals, b">="),
                    (TokenKind::Greater, b">"),
                ]),
                b'=' => Ok(self.make_token(TokenKind::Equals, 1)),
                _ => Ok(self.make_token(TokenKind::Unknown, 1)),
            },
            None => Ok(Token::new(
                TokenKind::Eof,
                &[],
                Span {
                    start: self.cursor.position,
                    end: self.cursor.position,
                },
            )),
        }
    }

    fn lex_whitespace(&mut self) -> Option<Token> {
        if !self.at_line_start {
            let cursor_start = self.cursor.position;
            let mut length = 0_i32;

            while let Some(current) = self.cursor.peek() {
                match current {
                    b' ' => {
                        length = length.saturating_add(1);
                        self.cursor.advance();
                    }
                    _ => break,
                }
            }

            if length > 0_i32 {
                return Some(Token::new(
                    TokenKind::Whitespace,
                    &[],
                    Span {
                        start: cursor_start,
                        end: self.cursor.position,
                    },
                ));
            }
        }

        None
    }

    fn lex_indentation(&mut self) -> MusiResult<Option<Token>> {
        if !self.at_line_start {
            return Ok(None);
        }

        let start = self.cursor.position;
        let mut spaces = 0_u32;

        while let Some(current) = self.cursor.peek() {
            match current {
                b' ' => spaces = spaces.saturating_add(1),
                b'\t' => {
                    return Err(self.error("inconsistent use of tabs and spaces in indentation"))
                }
                b'\r' | b'\n' => {
                    self.cursor.advance();
                    return Ok(None);
                }
                _ => break,
            }
            self.cursor.advance();
        }

        if spaces > 0 && self.indent_width.is_none() {
            self.indent_width = Some(spaces);
        }

        if let Some(width) = self.indent_width {
            if spaces > 0
                && !spaces
                    .checked_rem(width)
                    .map_or(false, |remainder| remainder == 0)
            {
                return Err(self.error("inconsistent indentation"));
            }
        }

        let current_indent = *self.indent_stack.last().unwrap_or(&0);

        match spaces.cmp(&current_indent) {
            cmp::Ordering::Greater => {
                self.indent_stack.push(spaces);
                self.at_line_start = false;

                Ok(Some(Token::new(
                    TokenKind::Indent,
                    format!("{spaces}").as_bytes(),
                    Span {
                        start,
                        end: self.cursor.position,
                    },
                )))
            }
            cmp::Ordering::Less => {
                let mut dedent_count = 0_u32;
                while let Some(&level) = self.indent_stack.last() {
                    if level <= spaces {
                        break;
                    }

                    self.indent_stack.pop();
                    dedent_count = dedent_count.saturating_add(1);
                }

                if self.indent_stack.last().copied().unwrap_or(0) != spaces {
                    return Err(self.error("unindent does not match any outer indentation level"));
                }

                self.pending_dedents = dedent_count.saturating_sub(1);
                self.at_line_start = false;

                Ok(Some(Token::new(
                    TokenKind::Dedent,
                    &[],
                    Span {
                        start,
                        end: self.cursor.position,
                    },
                )))
            }

            cmp::Ordering::Equal => {
                self.at_line_start = false;
                Ok(None)
            }
        }
    }

    fn lex_newline(&mut self) -> Token {
        self.at_line_start = true;
        self.cursor.advance();

        if self.cursor.peek() == Some(b'\n') {
            self.cursor.advance();
        }

        Token::new(
            TokenKind::Newline,
            b"\n",
            Span {
                start: self.cursor.position.saturating_sub(1),
                end: self.cursor.position,
            },
        )
    }

    fn lex_identifier_or_keyword(&mut self) -> Token {
        let start = self.cursor.position;

        while let Some(current) = self.cursor.peek() {
            if !current.is_ascii_alphanumeric() && current != b'_' {
                break;
            }
            self.cursor.advance();
        }

        let lexeme = self.cursor.slice_from(start);
        let kind = RESERVED_KEYWORDS
            .iter()
            .find(|&&(keyword, _)| keyword == lexeme)
            .map_or(TokenKind::Identifer, |&(_, kind)| kind);

        Token::new(
            kind,
            lexeme,
            Span {
                start,
                end: self.cursor.position,
            },
        )
    }

    fn lex_numberic_literal(&mut self) -> MusiResult<Token> {
        let start = self.cursor.position;

        if self.cursor.peek() == Some(b'0')
            && self.cursor.peek_next().map_or(false, |byte| {
                matches!(byte, b'x' | b'X' | b'b' | b'B' | b'o' | b'O')
            })
        {
            self.lex_radix_number()?;

            return Ok(Token::new(
                TokenKind::NumbericLiteral,
                self.cursor.slice_from(start),
                Span {
                    start,
                    end: self.cursor.position,
                },
            ));
        }

        self.lex_decimal_digits();

        if self.cursor.peek() == Some(b'.')
            && self
                .cursor
                .peek_next()
                .map_or(false, |byte| byte.is_ascii_digit())
        {
            self.cursor.advance();
            self.lex_decimal_digits();
        }

        if let Some(b'e' | b'E') = self.cursor.peek() {
            self.lex_exponent();
        }

        Ok(Token::new(
            TokenKind::NumbericLiteral,
            self.cursor.slice_from(start),
            Span {
                start,
                end: self.cursor.position,
            },
        ))
    }

    fn lex_string_literal(&mut self) -> MusiResult<Token> {
        let start = self.cursor.position;

        self.cursor.advance(); // skip opening quote

        while let Some(current) = self.cursor.peek() {
            match current {
                b'"' => {
                    self.cursor.advance(); // skip closing quote

                    return Ok(Token::new(
                        TokenKind::StringLiteral,
                        self.cursor.slice_from(start),
                        Span {
                            start,
                            end: self.cursor.position,
                        },
                    ));
                }
                b'\n' | b'\r' => {
                    self.cursor.advance();
                    return Err(self.error("unclosed string literal"));
                }
                b'\\' => {
                    if let Err(diagnostic) = self.lex_escape_sequence() {
                        self.cursor.advance();
                        return Err(diagnostic);
                    }
                }
                0..=0x1F => {
                    self.cursor.advance();
                    return Err(self.error("control character not allowed in string literal"));
                }
                _ => {
                    self.cursor.advance();
                }
            }
        }

        self.cursor.position = self.cursor.source.content.len();
        Err(self.error("unclosed string literal"))
    }

    #[inline]
    fn lex_radix_number(&mut self) -> MusiResult<()> {
        match self.cursor.peek_next() {
            Some(b'x' | b'X') => {
                self.cursor.advance_by(2);

                let mut has_valid_hexadecimal = false;

                while let Some(current) = self.cursor.peek() {
                    if current == b'_'
                        && !self
                            .cursor
                            .peek_next()
                            .map_or(false, |byte| byte.is_ascii_hexdigit())
                    {
                        return Err(self.error("expected hexadecimal digit after '_'"));
                    }
                    if !current.is_ascii_hexdigit() && current != b'_' {
                        break;
                    }

                    has_valid_hexadecimal = has_valid_hexadecimal || current != b'_';
                    self.cursor.advance();
                }

                if !has_valid_hexadecimal {
                    return Err(self.error("expected hexadecimal digit"));
                }
            }
            Some(b'b' | b'B') => {
                self.cursor.advance_by(2);

                let mut has_valid_binary = false;

                while let Some(current) = self.cursor.peek() {
                    if current == b'_'
                        && !self
                            .cursor
                            .peek_next()
                            .map_or(false, |next| [b'0', b'1'].contains(&next))
                    {
                        return Err(self.error("expected binary digit after '_'"));
                    }
                    if ![b'0', b'1', b'_'].contains(&current) {
                        break;
                    }

                    has_valid_binary = has_valid_binary || current != b'_';
                    self.cursor.advance();
                }

                if !has_valid_binary {
                    return Err(self.error("expected binary digit after binary prefix"));
                }
            }
            Some(b'o' | b'O') => {
                self.cursor.advance_by(2);

                let mut has_valid_octal = false;

                while let Some(current) = self.cursor.peek() {
                    if current == b'_'
                        && !self
                            .cursor
                            .peek_next()
                            .map_or(false, |byte| matches!(byte, b'0'..=b'7'))
                    {
                        return Err(self.error("expected octal digit after '_'"));
                    }
                    if !matches!(current, b'0'..=b'7' | b'_') {
                        break;
                    }

                    has_valid_octal = has_valid_octal || current != b'_';
                    self.cursor.advance();
                }

                if !has_valid_octal {
                    return Err(self.error("expected octal digit after octal prefix"));
                }
            }
            _ => {}
        }
        Ok(())
    }

    #[inline]
    fn lex_decimal_digits(&mut self) {
        while let Some(current) = self.cursor.peek() {
            if !current.is_ascii_digit() && current != b'_' {
                break;
            }
            if current == b'_'
                && !self
                    .cursor
                    .peek_next()
                    .map_or(false, |byte| byte.is_ascii_digit())
            {
                break;
            }
            self.cursor.advance();
        }
    }

    fn lex_exponent(&mut self) {
        self.cursor.advance();

        if let Some(b'+' | b'-') = self.cursor.peek() {
            self.cursor.advance();
        }

        while let Some(current) = self.cursor.peek() {
            if !current.is_ascii_digit() && current != b'_' {
                break;
            }
            if current == b'_'
                && !self
                    .cursor
                    .peek_next()
                    .map_or(false, |byte| byte.is_ascii_digit())
            {
                break;
            }
            self.cursor.advance();
        }
    }

    fn lex_escape_sequence(&mut self) -> MusiResult<()> {
        match self.cursor.peek_next() {
            Some(b'n' | b'r' | b't' | b'\\' | b'"' | b'\'') => {
                self.cursor.advance_by(2);
                Ok(())
            }
            Some(b'u' | b'U' | b'x') => {
                self.cursor.advance_by(2);
                self.lex_uncode_escape::<4>()
            }
            Some(invalid) => Err(self.error(format!(
                "unexpected escape sequence '{:#?}'",
                char::from(invalid)
            ))),
            None => Err(self.error("unclosed escape sequence")),
        }
    }

    fn lex_uncode_escape<const N: usize>(&mut self) -> MusiResult<()> {
        let mut unicode_value = 0;

        for _ in 0..N {
            match self.cursor.peek() {
                None => {
                    return Err(self.error("unclosed unicode escape"));
                }
                Some(current) if !current.is_ascii_hexdigit() => {
                    return Err(self.error(format!("unexpected hexadecimal digit '{current:#X}'")));
                }
                Some(current) => {
                    let hexadecimal_digit = match current {
                        b'0'..=b'9' => current.wrapping_sub(b'0'),
                        b'a'..=b'f' | b'A'..=b'F' => current.wrapping_sub(b'A').wrapping_add(10),
                        unexpected => {
                            return Err(self
                                .error(format!("unexpected hexadecimal digit '{unexpected:#X}'")))
                        }
                    };

                    unicode_value = (unicode_value << 4_i32) | u32::from(hexadecimal_digit);
                    self.cursor.advance();
                }
            }
        }

        if (0xD800..=0xDFFF).contains(&unicode_value) {
            return Err(self.error("surrogate pairs not allowed in unicode escape"));
        }

        if unicode_value > 0x0010_FFFF {
            return Err(self.error("unicode escape out of range"));
        }

        Ok(())
    }

    #[inline]
    fn make_token(&mut self, kind: TokenKind, length: usize) -> Token {
        let start = self.cursor.position;
        self.cursor.advance_by(length);

        Token::new(
            kind,
            self.cursor.slice_from(start),
            Span {
                start,
                end: self.cursor.position,
            },
        )
    }

    #[inline]
    fn lex_multi_byte_token(&mut self, patterns: &[(TokenKind, &[u8])]) -> MusiResult<Token> {
        let matched = patterns.iter().find(|&&(_, pattern)| match pattern.len() {
            2 => {
                if pattern.len() < 2 {
                    return false;
                }

                matches!((pattern.first(), pattern.get(1)), (Some(&first), Some(&second)) if self.cursor.matches_pair(first, second))
            }
            3 => {
                if pattern.len() < 3 {
                    return false;
                }

                matches!((pattern.first(), pattern.get(1), pattern.get(2)), (Some(&first), Some(&second), Some(&third)) if self.cursor.matches_triplet(first, second, third))
            }
            _ => false,
        });
        match matched {
            Some(&(kind, pattern)) => Ok(self.make_token(kind, pattern.len())),
            None => Ok(self.make_token(
                patterns
                    .first()
                    .ok_or_else(|| self.error("patterns cannot be empty"))?
                    .0,
                1,
            )),
        }
    }

    #[inline]
    fn error(&self, message: impl Into<String>) -> Diagnostic {
        Diagnostic::error(
            message,
            Span {
                start: self.cursor.position,
                end: self.cursor.position,
            },
        )
        .with_source(&self.cursor.source)
    }
}
