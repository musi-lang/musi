use cursor::Cursor;

use crate::common::{
    errors::{ErrorDiagnostic, MusiResult},
    location::Location,
    span::Span,
    token::{Kind, LiteralKind, Token},
};

mod cursor;

const MAX_INDENT_LEVELS: usize = 32;

const KEYWORDS: &[(&[u8], Kind)] = &[
    (b"and", Kind::And),
    (b"as", Kind::As),
    (b"cases", Kind::Cases),
    (b"const", Kind::Const),
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
    (b"otherwise", Kind::Otherwise),
    (b"then", Kind::Then),
    (b"to", Kind::To),
    (b"true", Kind::True),
    (b"type", Kind::Type),
    (b"until", Kind::Until),
    (b"var", Kind::Var),
    (b"where", Kind::Where),
    (b"while", Kind::While),
    (b"xor", Kind::Xor),
];

const UNICODE_MAX_DIGITS: u32 = 0x0010_FFFF;
const SURROGATE_START: u32 = 0xD800;
const SURROGATE_END: u32 = 0xDFFF;

const CHAR_QUOTE: u8 = b'"';
const CHAR_APOSTROPHE: u8 = b'\'';
const CHAR_BACKSLASH: u8 = b'\\';
const CHAR_UNDERSCORE: u8 = b'_';
const CHAR_DOT: u8 = b'.';
const CHAR_CR: u8 = b'\r';
const CHAR_LF: u8 = b'\n';
const CHAR_ZERO: u8 = b'0';

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
    pub fn new(input: &[u8]) -> Self {
        Self {
            cursor: Cursor::new(input),
            indent_stack: [0; MAX_INDENT_LEVELS],
            indent_level: 0,
        }
    }

    pub fn lex(&mut self) -> Vec<Token> {
        let mut tokens = Vec::with_capacity(self.cursor.source.len() >> 3); // divide by 8

        while let Ok(token) = self.next_token() {
            if token.kind == Kind::Eof {
                while self.indent_level > 0 {
                    tokens.push(Token::new(
                        Kind::Dedent,
                        &[],
                        Span {
                            start: token.span.start,
                            end: token.span.end,
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

        tokens
    }

    fn next_token(&mut self) -> MusiResult<Token> {
        let start_location = self.cursor.location;

        let whitespace = self.skip_whitespace()?;
        if whitespace.kind != Kind::Unknown {
            return Ok(whitespace);
        }

        match self.cursor.peek() {
            Some(current) => {
                match current {
                    CHAR_CR | CHAR_LF => Ok(self.lex_newline(start_location)),

                    b if is_identifier_start(b) => Ok(self.lex_identifier_or_keyword()),
                    b if b.is_ascii_digit() => Ok(self.lex_number_literal()),

                    CHAR_QUOTE => self.lex_string_literal(),
                    CHAR_APOSTROPHE => self.lex_character_literal(),

                    b'*' => Ok(self.make_token(Kind::Star, 1)),
                    b'/' => Ok(self
                        .match_compound_token(&[(Kind::SlashEquals, b"/="), (Kind::Slash, b"/")])),
                    b'+' => Ok(self.make_token(Kind::Plus, 1)),
                    b'-' => Ok(self
                        .match_compound_token(&[(Kind::MinusGreater, b"->"), (Kind::Minus, b"-")])),
                    b'<' => Ok(self.match_compound_token(&[
                        (Kind::LessEqualsGreater, b"<=>"),
                        (Kind::LessLess, b"<<"),
                        (Kind::LessEquals, b"<="),
                        (Kind::LessMinus, b"<-"),
                        (Kind::Less, b"<"),
                    ])),
                    b'>' => Ok(self.match_compound_token(&[
                        (Kind::GreaterGreater, b">>"),
                        (Kind::GreaterEquals, b">="),
                        (Kind::Greater, b">"),
                    ])),
                    b'!' => Ok(self.make_token(Kind::Bang, 1)),
                    b'&' => Ok(self.make_token(Kind::Ampersand, 1)),
                    b'^' => Ok(self.make_token(Kind::Caret, 1)),
                    b'|' => Ok(self.match_compound_token(&[
                        (Kind::PipePipe, b"||"),
                        (Kind::PipeGreater, b"|>"),
                        (Kind::Pipe, b"|"),
                    ])),
                    b'~' => Ok(self
                        .match_compound_token(&[(Kind::TildeEquals, b"~="), (Kind::Tilde, b"~")])),
                    b'=' => Ok(self.match_compound_token(&[
                        (Kind::EqualsGreater, b"=>"),
                        (Kind::Equals, b"="),
                    ])),
                    b':' => Ok(self
                        .match_compound_token(&[(Kind::ColonEquals, b":="), (Kind::Colon, b":")])),

                    b'(' => Ok(self.make_token(Kind::LeftParen, 1)),
                    b')' => Ok(self.make_token(Kind::RightParen, 1)),
                    b'{' => Ok(self.make_token(Kind::LeftBrace, 1)),
                    b'}' => Ok(self.make_token(Kind::RightBrace, 1)),
                    b'[' => Ok(self.make_token(Kind::LeftBracket, 1)),
                    b']' => Ok(self.make_token(Kind::RightBracket, 1)),
                    b',' => Ok(self.make_token(Kind::Comma, 1)),
                    CHAR_DOT => {
                        Ok(self.match_compound_token(&[(Kind::DotDot, b".."), (Kind::Dot, b".")]))
                    }
                    b';' => Ok(self.make_token(Kind::Semicolon, 1)),
                    b'?' => Ok(self.make_token(Kind::Question, 1)),
                    b'@' => Ok(self.make_token(Kind::At, 1)),

                    _ => Ok(self.make_token(Kind::Unknown, 1)),
                }
            }
            None => Ok(Token::new(
                Kind::Eof,
                &[],
                Span {
                    start: start_location,
                    end: self.cursor.location,
                },
            )),
        }
    }

    fn skip_whitespace(&mut self) -> MusiResult<Token> {
        if self.cursor.location.column == 1 {
            let mut spaces = 0;
            let start_location = self.cursor.location;

            while let Some(current) = self.cursor.peek() {
                match current {
                    b if b.is_ascii_whitespace() => {
                        spaces += 1;
                        self.cursor.advance();
                    }
                    _ => {
                        if current != CHAR_CR && current != CHAR_LF {
                            let current_indent = self.indent_stack[self.indent_level];

                            log::debug!(
                                "{}:{}:{}: spaces={}, current_indent={}",
                                self.cursor.location.line,
                                self.cursor.location.column,
                                self.cursor.location.offset,
                                spaces,
                                current_indent
                            );

                            match spaces.cmp(&current_indent) {
                                std::cmp::Ordering::Greater => {
                                    self.indent_level += 1;
                                    self.indent_stack[self.indent_level] = spaces;

                                    log::debug!(
                                        "Indent => indent_level={}->{}, spaces={}",
                                        self.indent_level - 1,
                                        self.indent_level,
                                        spaces
                                    );

                                    return Ok(Token::new(
                                        Kind::Indent,
                                        &[u8::try_from(spaces).expect("usize->u8")],
                                        Span {
                                            start: start_location,
                                            end: self.cursor.location,
                                        },
                                    ));
                                }
                                std::cmp::Ordering::Less => {
                                    while self.indent_level > 0
                                        && spaces < self.indent_stack[self.indent_level]
                                    {
                                        log::debug!(
                                            "Dedent => indent_level={}->{}",
                                            self.indent_level,
                                            self.indent_level - 1
                                        );
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
                                            start: start_location,
                                            end: self.cursor.location,
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
                start: self.cursor.location,
                end: self.cursor.location,
            },
        ))
    }

    fn lex_newline(&mut self, start_location: Location) -> Token {
        let lexeme = if self.cursor.peek() == Some(CHAR_CR) {
            self.cursor.advance();
            if self.cursor.peek() == Some(CHAR_LF) {
                self.cursor.advance();
                vec![CHAR_CR, CHAR_LF]
            } else {
                vec![CHAR_CR]
            }
        } else {
            self.cursor.advance();
            vec![CHAR_LF]
        };

        self.cursor.location.line += 1;
        self.cursor.location.column = 1;

        Token::new(
            Kind::Newline,
            &lexeme,
            Span {
                start: start_location,
                end: self.cursor.location,
            },
        )
    }

    fn lex_identifier_or_keyword(&mut self) -> Token {
        let start_location = self.cursor.location;
        let start_position = self.cursor.position;

        while let Some(current) = self.cursor.peek() {
            if !is_identifier_continue(current) {
                break;
            }
            self.cursor.advance();
        }

        let lexeme = &self.cursor.source[start_position..self.cursor.position];
        let kind = KEYWORDS
            .iter()
            .find(|(keyword, _)| *keyword == lexeme)
            .map_or(Kind::Identifier, |(_, kind)| *kind);

        Token::new(
            kind,
            lexeme,
            Span {
                start: start_location,
                end: self.cursor.location,
            },
        )
    }

    fn lex_number_literal(&mut self) -> Token {
        let start_location = self.cursor.location;
        let start_position = self.cursor.position;

        if self.cursor.peek() == Some(CHAR_ZERO)
            && self.cursor.peek_next().map_or(false, |b| {
                matches!(b, b'x' | b'X' | b'b' | b'B' | b'o' | b'O')
            })
        {
            self.lex_number_base();

            Token::new(
                Kind::Literal(LiteralKind::Number),
                &self.cursor.source[start_position..self.cursor.position],
                Span {
                    start: start_location,
                    end: self.cursor.location,
                },
            );
        }

        self.lex_decimal_digits();

        if self.cursor.peek() == Some(CHAR_DOT)
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
            &self.cursor.source[start_position..self.cursor.position],
            Span {
                start: start_location,
                end: self.cursor.location,
            },
        )
    }

    fn lex_string_literal(&mut self) -> MusiResult<Token> {
        let start_location = self.cursor.location;
        let start_position = self.cursor.position;

        self.cursor.advance(); // skip opening quote

        while let Some(current) = self.cursor.peek() {
            match current {
                CHAR_QUOTE => {
                    self.cursor.advance(); // skip closing quote

                    return Ok(Token::new(
                        Kind::Literal(LiteralKind::String),
                        &self.cursor.source[start_position..self.cursor.position],
                        Span {
                            start: start_location,
                            end: self.cursor.location,
                        },
                    ));
                }
                CHAR_BACKSLASH => {
                    self.lex_escape_sequence()?;
                    continue;
                }
                CHAR_CR => {
                    return Err(self.error("invalid line ending in string literal"));
                }
                0..=0x1F => {
                    return Err(self.error("invalid control character in string literal"));
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
        let start_location = self.cursor.location;
        let mut chars = 0;

        self.cursor.advance(); // skip opening apostrophe

        while let Some(current) = self.cursor.peek() {
            match current {
                CHAR_APOSTROPHE => {
                    if chars == 0 {
                        return Err(self.error("empty character literal"));
                    } else if chars > 1 {
                        return Err(self.error("too many characters in character literal"));
                    }

                    self.cursor.advance(); // skip closing apostrophe

                    return Ok(Token::new(
                        Kind::Literal(LiteralKind::Character),
                        &self.cursor.source[start_position..self.cursor.position],
                        Span {
                            start: start_location,
                            end: self.cursor.location,
                        },
                    ));
                }
                CHAR_BACKSLASH => {
                    self.lex_escape_sequence()?;
                    chars += 1;
                    continue;
                }
                CHAR_LF => {
                    return Err(self.error("invalid line ending in character literal"));
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
                    if !current.is_ascii_hexdigit() && current != CHAR_UNDERSCORE {
                        break;
                    }
                    self.cursor.advance();
                }
            }
            Some(b'b' | b'B') => {
                self.cursor.advance_by(2);
                while let Some(current) = self.cursor.peek() {
                    if ![CHAR_ZERO, b'1', CHAR_UNDERSCORE].contains(&current) {
                        break;
                    }
                    self.cursor.advance();
                }
            }
            Some(b'o' | b'O') => {
                self.cursor.advance_by(2);
                while let Some(current) = self.cursor.peek() {
                    if !matches!(current, CHAR_ZERO..=b'7' | CHAR_UNDERSCORE) {
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
            if !current.is_ascii_digit() && current != CHAR_UNDERSCORE {
                break;
            }
            if current == CHAR_UNDERSCORE
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
            if !current.is_ascii_digit() && current != CHAR_UNDERSCORE {
                break;
            }
            if current == CHAR_UNDERSCORE
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
            Some(b'n' | b'r' | b't' | CHAR_BACKSLASH | CHAR_QUOTE | CHAR_APOSTROPHE) => {
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
                        CHAR_ZERO..=b'9' => current - CHAR_ZERO,
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
        let start_location = self.cursor.location;
        let start_position = self.cursor.position;
        self.cursor.advance_by(length);

        Token::new(
            kind,
            &self.cursor.source[start_position..self.cursor.position],
            Span {
                start: start_location,
                end: self.cursor.location,
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
    fn error(&self, message: impl Into<String>) -> ErrorDiagnostic {
        ErrorDiagnostic::error(
            message,
            Span {
                start: self.cursor.location,
                end: self.cursor.location,
            },
        )
    }
}

#[inline]
const fn is_identifier_start(input: u8) -> bool {
    input.is_ascii_alphabetic() || input == CHAR_UNDERSCORE
}

#[inline]
const fn is_identifier_continue(input: u8) -> bool {
    input.is_ascii_alphanumeric() || input == CHAR_UNDERSCORE
}
