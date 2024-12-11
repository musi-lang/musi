use std::sync::Arc;

use crate::core::{self, diagnostics::Diagnostic, source::Source, span::Span, MusiResult};

use super::{
    cursor::Cursor,
    token::{Kind, LiteralKind, Token},
};

const MAX_INDENT_LEVELS: usize = 32;

const KEYWORDS: &[(&[u8], Kind)] = &[
    (b"and", Kind::And),
    (b"as", Kind::As),
    (b"break", Kind::Break),
    (b"case", Kind::Case),
    (b"continue", Kind::Continue),
    (b"deref", Kind::Deref),
    (b"do", Kind::Do),
    (b"else", Kind::Else),
    (b"false", Kind::False),
    (b"for", Kind::For),
    (b"forall", Kind::Forall),
    (b"from", Kind::From),
    (b"if", Kind::If),
    (b"in", Kind::In),
    (b"is", Kind::Is),
    (b"let", Kind::Let),
    (b"not", Kind::Not),
    (b"of", Kind::Of),
    (b"or", Kind::Or),
    (b"ref", Kind::Ref),
    (b"return", Kind::Return),
    (b"then", Kind::Then),
    (b"true", Kind::True),
    (b"type", Kind::Type),
    (b"where", Kind::Where),
    (b"while", Kind::While),
];

const UNICODE_MAX_DIGITS: u32 = 0x0010_FFFF;
const SURROGATE_START: u32 = 0xD800;
const SURROGATE_END: u32 = 0xDFFF;

const HEX_LOWER_START: u8 = b'a';
const HEX_LOWER_END: u8 = b'f';
const HEX_UPPER_START: u8 = b'A';
const HEX_UPPER_END: u8 = b'F';

pub struct Lexer {
    cursor: Cursor,
    indent_stack: [u32; MAX_INDENT_LEVELS],
    indent_level: usize,
}

impl Lexer {
    #[must_use]
    pub const fn new(source: Arc<Source>) -> Self {
        Self {
            cursor: Cursor::new(source),
            indent_stack: [0; MAX_INDENT_LEVELS],
            indent_level: 0,
        }
    }

    /// Analyses source code and produces a bunch of tokens.
    ///
    /// # Errors
    ///
    /// Returns `MusiError` if:
    /// - invalid characters encountered
    /// - string or character literals not properly terminated
    /// - invalid numeric literals found
    /// - maximum indentation level exceeded
    pub fn lex(&mut self) -> MusiResult<Vec<Token>> {
        let mut tokens = Vec::with_capacity(self.cursor.source.content.len() >> 3); // divide by 8

        loop {
            let token = self.next_token()?;
            if token.kind == Kind::Eof {
                while self.indent_level > 0 {
                    tokens.push(Token::new(
                        Kind::Dedent,
                        &[],
                        Span {
                            start: self.cursor.position,
                            end: self.cursor.position,
                        },
                    ));
                    self.indent_level -= 1;
                }

                tokens.push(token);
                break;
            } else if token.kind != Kind::Unknown {
                tokens.push(token);
            }
        }

        Ok(tokens)
    }
    fn next_token(&mut self) -> MusiResult<Token> {
        let start_position = self.cursor.position;

        let whitespace = self.skip_whitespace()?;
        if whitespace.kind != Kind::Unknown {
            return Ok(whitespace);
        }

        match self.cursor.peek() {
            Some(current) => {
                match current {
                    core::CHAR_CR | core::CHAR_LF => Ok(self.lex_newline(start_position)),

                    b if is_identifier_start(b) => Ok(self.lex_identifier_or_keyword()),
                    b if b.is_ascii_digit() => Ok(self.lex_number_literal()),

                    core::CHAR_QUOTE => self.lex_string_literal(),
                    core::CHAR_APOSTROPHE => self.lex_character_literal(),

                    b'(' => Ok(self.make_token(Kind::LeftParen, 1)),
                    b')' => Ok(self.make_token(Kind::RightParen, 1)),
                    b'{' => Ok(self.make_token(Kind::LeftBrace, 1)),
                    b'}' => Ok(self.make_token(Kind::RightBrace, 1)),
                    b'[' => Ok(self.make_token(Kind::LeftBracket, 1)),
                    b']' => Ok(self.make_token(Kind::RightBracket, 1)),
                    b',' => Ok(self.make_token(Kind::Comma, 1)),
                    b':' => Ok(self
                        .match_compound_token(&[(Kind::ColonEquals, b":="), (Kind::Colon, b":")])),
                    core::CHAR_DOT => Ok(self.match_compound_token(&[
                        (Kind::DotDotLess, b"..<"),
                        (Kind::DotDot, b".."),
                        (Kind::Dot, b"."),
                    ])),

                    b'+' => Ok(self.make_token(Kind::Plus, 1)),
                    b'-' => Ok(self
                        .match_compound_token(&[(Kind::MinusGreater, b"->"), (Kind::Minus, b"-")])),
                    b'*' => Ok(self.make_token(Kind::Star, 1)),
                    b'/' => Ok(self
                        .match_compound_token(&[(Kind::SlashEquals, b"/="), (Kind::Slash, b"/")])),
                    b'^' => Ok(self.make_token(Kind::Caret, 1)),
                    b'|' => Ok(self
                        .match_compound_token(&[(Kind::PipeGreater, b"|>"), (Kind::Pipe, b"|")])),
                    b'<' => Ok(self.match_compound_token(&[
                        (Kind::LessEqualsGreater, b"<=>"),
                        (Kind::LessEquals, b"<="),
                        (Kind::Less, b"<"),
                    ])),
                    b'>' => Ok(self.match_compound_token(&[
                        (Kind::GreaterEquals, b">="),
                        (Kind::Greater, b">"),
                    ])),
                    b'=' => Ok(self.make_token(Kind::Equals, 1)),

                    _ => Ok(self.make_token(Kind::Unknown, 1)),
                }
            }
            None => Ok(Token::new(
                Kind::Eof,
                &[],
                Span {
                    start: start_position,
                    end: self.cursor.position,
                },
            )),
        }
    }

    fn skip_whitespace(&mut self) -> MusiResult<Token> {
        if self.cursor.position == self.cursor.source.line_starts[self.cursor.current_line] {
            let mut spaces = 0;
            let start_position = self.cursor.position;

            while let Some(current) = self.cursor.peek() {
                match current {
                    b if b.is_ascii_whitespace() => {
                        spaces += 1;
                        self.cursor.advance();
                    }
                    _ => {
                        if current != core::CHAR_CR && current != core::CHAR_LF {
                            let current_indent = self.indent_stack[self.indent_level];

                            match spaces.cmp(&current_indent) {
                                std::cmp::Ordering::Greater => {
                                    self.indent_level += 1;
                                    self.indent_stack[self.indent_level] = spaces;

                                    return Ok(Token::new(
                                        Kind::Indent,
                                        &[u8::try_from(spaces).expect("usize->u8")],
                                        Span {
                                            start: start_position,
                                            end: self.cursor.position,
                                        },
                                    ));
                                }
                                std::cmp::Ordering::Less => {
                                    while self.indent_level > 0
                                        && spaces < self.indent_stack[self.indent_level]
                                    {
                                        self.indent_level -= 1;
                                    }

                                    if spaces < self.indent_stack[0] {
                                        return Err(self.error(
                                            "unindent does not match any outer indentation level",
                                        ));
                                    }
                                    if spaces != self.indent_stack[self.indent_level] {
                                        return Err(self.error("inconsistent indentation"));
                                    }

                                    return Ok(Token::new(
                                        Kind::Dedent,
                                        &[],
                                        Span {
                                            start: start_position,
                                            end: self.cursor.position,
                                        },
                                    ));
                                }
                                std::cmp::Ordering::Equal => {}
                            };
                        }
                        break;
                    }
                }
            }
        }

        Ok(Token::new(
            Kind::Unknown,
            &[],
            Span {
                start: self.cursor.position,
                end: self.cursor.position,
            },
        ))
    }

    fn lex_newline(&mut self, start: usize) -> Token {
        let lexeme = if self.cursor.peek() == Some(core::CHAR_CR) {
            self.cursor.advance();
            if self.cursor.peek() == Some(core::CHAR_LF) {
                self.cursor.advance();
                vec![core::CHAR_CR, core::CHAR_LF]
            } else {
                vec![core::CHAR_CR]
            }
        } else {
            self.cursor.advance();
            vec![core::CHAR_LF]
        };

        Token::new(
            Kind::Newline,
            &lexeme,
            Span {
                start,
                end: self.cursor.position,
            },
        )
    }

    fn lex_identifier_or_keyword(&mut self) -> Token {
        let start_position = self.cursor.position;

        while let Some(current) = self.cursor.peek() {
            if !is_identifier_continue(current) {
                break;
            }
            self.cursor.advance();
        }

        let lexeme = self.cursor.slice_from(start_position);
        let kind = KEYWORDS
            .iter()
            .find(|(keyword, _)| *keyword == lexeme)
            .map_or(Kind::Identifier, |(_, kind)| *kind);

        Token::new(
            kind,
            lexeme,
            Span {
                start: start_position,
                end: self.cursor.position,
            },
        )
    }

    fn lex_number_literal(&mut self) -> Token {
        let start_position = self.cursor.position;

        if self.cursor.peek() == Some(core::CHAR_ZERO)
            && self.cursor.peek_next().map_or(false, |b| {
                matches!(b, b'x' | b'X' | b'b' | b'B' | b'o' | b'O')
            })
        {
            self.lex_number_base();

            return Token::new(
                Kind::Literal(LiteralKind::Number),
                self.cursor.slice_from(start_position),
                Span {
                    start: start_position,
                    end: self.cursor.position,
                },
            );
        }

        self.lex_decimal_digits();

        if self.cursor.peek() == Some(core::CHAR_DOT)
            && self
                .cursor
                .peek_next()
                .map_or(false, |b| b.is_ascii_digit())
        {
            self.cursor.advance();
            self.lex_decimal_digits();
        }

        if let Some(b'e' | b'E') = self.cursor.peek() {
            self.lex_scientific_notation();
        }

        Token::new(
            Kind::Literal(LiteralKind::Number),
            self.cursor.slice_from(start_position),
            Span {
                start: start_position,
                end: self.cursor.position,
            },
        )
    }

    fn lex_string_literal(&mut self) -> MusiResult<Token> {
        let start_position = self.cursor.position;

        self.cursor.advance(); // skip opening quote

        while let Some(current) = self.cursor.peek() {
            match current {
                core::CHAR_QUOTE => {
                    self.cursor.advance(); // skip closing quote

                    return Ok(Token::new(
                        Kind::Literal(LiteralKind::String),
                        self.cursor.slice_from(start_position),
                        Span {
                            start: start_position,
                            end: self.cursor.position,
                        },
                    ));
                }
                core::CHAR_BACKSLASH => {
                    self.lex_escape_sequence()?;
                    continue;
                }
                0..=0x1F => {
                    return Err(self.error(
                        if current == core::CHAR_CR || current == core::CHAR_LF {
                            "unclosed string literal"
                        } else {
                            "control character not allowed in string literal"
                        },
                    ));
                }
                _ => {
                    self.cursor.advance();
                    continue;
                }
            }
        }

        Err(self.error("unclosed string literal"))
    }

    fn lex_character_literal(&mut self) -> MusiResult<Token> {
        let start_position = self.cursor.position;
        let mut chars = 0;

        self.cursor.advance(); // skip opening apostrophe

        while let Some(current) = self.cursor.peek() {
            match current {
                core::CHAR_APOSTROPHE => {
                    if chars == 0 {
                        return Err(self.error("empty character literal"));
                    } else if chars > 1 {
                        return Err(self.error("character literal may only contain one character"));
                    }

                    self.cursor.advance(); // skip closing apostrophe

                    return Ok(Token::new(
                        Kind::Literal(LiteralKind::Character),
                        self.cursor.slice_from(start_position),
                        Span {
                            start: start_position,
                            end: self.cursor.position,
                        },
                    ));
                }
                core::CHAR_BACKSLASH => {
                    self.lex_escape_sequence()?;
                    chars += 1;
                    continue;
                }
                core::CHAR_LF => {
                    return Err(self.error("unclosed character literal"));
                }
                _ => {
                    self.cursor.advance();
                    chars += 1;
                    continue;
                }
            }
        }

        Err(self.error("unclosed character literal"))
    }

    #[inline]
    fn lex_number_base(&mut self) {
        match self.cursor.peek_next() {
            Some(b'x' | b'X') => {
                self.cursor.advance_by(2);
                while let Some(current) = self.cursor.peek() {
                    if !current.is_ascii_hexdigit() && current != core::CHAR_UNDERSCORE {
                        break;
                    }
                    self.cursor.advance();
                }
            }
            Some(b'b' | b'B') => {
                self.cursor.advance_by(2);
                while let Some(current) = self.cursor.peek() {
                    if ![core::CHAR_ZERO, b'1', core::CHAR_UNDERSCORE].contains(&current) {
                        break;
                    }
                    self.cursor.advance();
                }
            }
            Some(b'o' | b'O') => {
                self.cursor.advance_by(2);
                while let Some(current) = self.cursor.peek() {
                    if !matches!(current, core::CHAR_ZERO..=b'7' | core::CHAR_UNDERSCORE) {
                        break;
                    }
                    self.cursor.advance();
                }
            }
            _ => {}
        }
    }

    #[inline]
    fn lex_decimal_digits(&mut self) {
        while let Some(current) = self.cursor.peek() {
            if !current.is_ascii_digit() && current != core::CHAR_UNDERSCORE {
                break;
            }
            if current == core::CHAR_UNDERSCORE
                && !self
                    .cursor
                    .peek_next()
                    .map_or(false, |b| b.is_ascii_digit())
            {
                break;
            }
            self.cursor.advance();
        }
    }

    fn lex_scientific_notation(&mut self) {
        self.cursor.advance();

        if let Some(b'+' | b'-') = self.cursor.peek() {
            self.cursor.advance();
        }

        while let Some(current) = self.cursor.peek() {
            if !current.is_ascii_digit() && current != core::CHAR_UNDERSCORE {
                break;
            }
            if current == core::CHAR_UNDERSCORE
                && !self
                    .cursor
                    .peek_next()
                    .map_or(false, |b| b.is_ascii_digit())
            {
                break;
            }
            self.cursor.advance();
        }
    }

    fn lex_escape_sequence(&mut self) -> MusiResult<()> {
        match self.cursor.peek_next() {
            Some(
                b'n'
                | b'r'
                | b't'
                | core::CHAR_BACKSLASH
                | core::CHAR_QUOTE
                | core::CHAR_APOSTROPHE,
            ) => {
                self.cursor.advance_by(2);
                Ok(())
            }
            Some(b'u' | b'U' | b'x') => {
                self.cursor.advance_by(2);
                self.lex_unicode_escape_sequence::<4>()
            }
            Some(invalid) => {
                Err(self.error(format!("invalid escape sequence '\\{}'", invalid as char)))
            }
            None => Err(self.error("unclosed escape sequence")),
        }
    }

    fn lex_unicode_escape_sequence<const N: usize>(&mut self) -> MusiResult<()> {
        let mut value: u32 = 0;

        for _ in 0..N {
            match self.cursor.peek() {
                None => {
                    return Err(self.error("unclosed unicode escape sequence"));
                }
                Some(current) if !current.is_ascii_hexdigit() => {
                    return Err(self.error(format!("invalid hexadecimal digit '0x{current:X}'")));
                }
                Some(current) => {
                    let digit = match current {
                        core::CHAR_ZERO..=b'9' => current - core::CHAR_ZERO,
                        HEX_LOWER_START..=HEX_LOWER_END => current - HEX_LOWER_START + 10,
                        HEX_UPPER_START..=HEX_UPPER_END => current - HEX_UPPER_START + 10,
                        _ => unreachable!(),
                    };
                    value = (value << 4) | u32::from(digit);
                    self.cursor.advance();
                }
            }
        }

        if (SURROGATE_START..=SURROGATE_END).contains(&value) {
            return Err(self.error("surrogate pairs not allowed in unicode escape sequences"));
        }

        if value > UNICODE_MAX_DIGITS {
            return Err(self.error("unicode escape sequence out of range"));
        }

        Ok(())
    }

    #[inline]
    fn make_token(&mut self, kind: Kind, length: usize) -> Token {
        let start_position = self.cursor.position;
        self.cursor.advance_by(length);

        Token::new(
            kind,
            self.cursor.slice_from(start_position),
            Span {
                start: start_position,
                end: self.cursor.position,
            },
        )
    }

    #[inline]
    fn match_compound_token(&mut self, patterns: &[(Kind, &[u8])]) -> Token {
        let matched = patterns.iter().find(|(_, pattern)| match pattern.len() {
            2 => self.cursor.match_2byte(pattern[0], pattern[1]),
            3 => self.cursor.match_3byte(pattern[0], pattern[1], pattern[2]),
            _ => false,
        });

        match matched {
            Some((kind, pattern)) => self.make_token(*kind, pattern.len()),
            None => self.make_token(patterns[0].0, 1),
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

#[inline]
const fn is_identifier_start(input: u8) -> bool {
    input.is_ascii_alphabetic() || input == core::CHAR_UNDERSCORE
}

#[inline]
const fn is_identifier_continue(input: u8) -> bool {
    input.is_ascii_alphanumeric() || input == core::CHAR_UNDERSCORE
}
