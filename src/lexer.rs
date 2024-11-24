use crate::{
    errors::{LexicalError, MusiError, MusiResult},
    token::{LiteralKind, Token, TokenKind},
    utils::{Location, Span},
};

pub struct Lexer {
    source: Vec<u8>,
    position: usize,
    location: Location,
    indent_stack: [u32; 32],
    indent_level: usize,
}

impl Lexer {
    pub const fn new(source: Vec<u8>) -> Self {
        Self {
            source,
            position: 0,
            location: Location::new(),
            indent_stack: [0; 32],
            indent_level: 0,
        }
    }

    pub fn lex(&mut self) -> MusiResult<Vec<Token>> {
        let mut tokens = vec![];

        loop {
            let whitespace = self.skip_whitespace()?;
            if whitespace.kind != TokenKind::Unknown {
                tokens.push(whitespace);
            }

            let token = self.lex_token()?;

            if token.kind == TokenKind::Eof {
                while self.indent_level > 0 {
                    tokens.push(Token::new(
                        TokenKind::Dedent,
                        vec![],
                        Span {
                            start: token.span.start,
                            end: token.span.end,
                        },
                    ));
                    self.indent_level -= 1;
                }
                tokens.push(token);
                break;
            } else if token.kind != TokenKind::Unknown {
                tokens.push(token);
            }
        }

        Ok(tokens)
    }

    fn lex_token(&mut self) -> MusiResult<Token> {
        let start_location = self.location;

        match self.peek() {
            Some(current) => match current {
                b'\r' => {
                    self.advance();
                    if self.peek() == Some(b'\n') {
                        self.advance();
                    }
                    self.location.line += 1;
                    self.location.column = 1;

                    Ok(Token::new(
                        TokenKind::Newline,
                        vec![b'\r', b'\n'],
                        Span {
                            start: start_location,
                            end: self.location,
                        },
                    ))
                }
                b'\n' => {
                    self.advance();
                    self.location.line += 1;
                    self.location.column = 1;

                    Ok(Token::new(
                        TokenKind::Newline,
                        vec![b'\n'],
                        Span {
                            start: start_location,
                            end: self.location,
                        },
                    ))
                }

                b if is_identifier_start(b) => self.lex_identifier_or_keyword(),
                b if b.is_ascii_digit() => self.lex_number_literal(),
                b'"' => self.lex_string_literal(),
                b'\'' => self.lex_character_literal(),

                b'(' => self.make_token(TokenKind::LeftParen, 1),
                b')' => self.make_token(TokenKind::RightParen, 1),
                b'{' => self.make_token(TokenKind::LeftBrace, 1),
                b'}' => self.make_token(TokenKind::RightBrace, 1),
                b'[' => self.make_token(TokenKind::LeftBracket, 1),
                b']' => self.make_token(TokenKind::RightBracket, 1),
                b',' => self.make_token(TokenKind::Comma, 1),
                b'.' => self.make_token(TokenKind::Dot, 1),
                b':' => match self.peek_next() {
                    Some(b'=') => self.make_token(TokenKind::ColonEquals, 2),
                    _ => self.make_token(TokenKind::Colon, 1),
                },
                b'+' => match self.peek_next() {
                    Some(b'+') => self.make_token(TokenKind::PlusPlus, 2),
                    Some(b'=') => self.make_token(TokenKind::PlusEquals, 2),
                    _ => self.make_token(TokenKind::Plus, 1),
                },
                b'-' => match self.peek_next() {
                    Some(b'>') => self.make_token(TokenKind::MinusGreater, 2),
                    Some(b'=') => self.make_token(TokenKind::MinusEquals, 2),
                    _ => self.make_token(TokenKind::Minus, 1),
                },
                b'*' => match self.peek_next() {
                    Some(b'*') => match self.source.get(self.position + 2) {
                        Some(b'=') => self.make_token(TokenKind::StarStarEquals, 3),
                        _ => self.make_token(TokenKind::StarStar, 2),
                    },
                    Some(b'=') => self.make_token(TokenKind::StarEquals, 2),
                    _ => self.make_token(TokenKind::Star, 1),
                },
                b'/' => match self.peek_next() {
                    Some(b'/') => match self.source.get(self.position + 2) {
                        Some(b'=') => self.make_token(TokenKind::SlashSlashEquals, 3),
                        _ => self.make_token(TokenKind::SlashSlash, 2),
                    },
                    Some(b'=') => self.make_token(TokenKind::SlashEquals, 2),
                    _ => self.make_token(TokenKind::Slash, 1),
                },
                b'%' => match self.peek_next() {
                    Some(b'=') => self.make_token(TokenKind::PercentEquals, 2),
                    _ => self.make_token(TokenKind::Percent, 1),
                },
                b'&' => match self.peek_next() {
                    Some(b'=') => self.make_token(TokenKind::AmpersandEquals, 2),
                    _ => self.make_token(TokenKind::Ampersand, 1),
                },
                b'|' => match self.peek_next() {
                    Some(b'=') => self.make_token(TokenKind::PipeEquals, 2),
                    _ => self.make_token(TokenKind::Pipe, 1),
                },
                b'^' => match self.peek_next() {
                    Some(b'=') => self.make_token(TokenKind::CaretEquals, 2),
                    _ => self.make_token(TokenKind::Caret, 1),
                },
                b'~' => self.make_token(TokenKind::Tilde, 1),
                b'<' => match self.peek_next() {
                    Some(b'<') => match self.source.get(self.position + 2) {
                        Some(b'=') => self.make_token(TokenKind::LessLessEquals, 3),
                        _ => self.make_token(TokenKind::LessLess, 2),
                    },
                    Some(b'=') => match self.source.get(self.position + 2) {
                        Some(b'>') => self.make_token(TokenKind::LessEqualsGreater, 3),
                        _ => self.make_token(TokenKind::LessEquals, 2),
                    },
                    Some(b'>') => self.make_token(TokenKind::LessGreater, 2),
                    _ => self.make_token(TokenKind::Less, 1),
                },
                b'>' => match self.peek_next() {
                    Some(b'>') => match self.source.get(self.position + 2) {
                        Some(b'=') => self.make_token(TokenKind::GreaterGreaterEquals, 3),
                        _ => self.make_token(TokenKind::GreaterGreater, 2),
                    },
                    Some(b'=') => self.make_token(TokenKind::GreaterEquals, 2),
                    _ => self.make_token(TokenKind::Greater, 1),
                },
                b'=' => match self.peek_next() {
                    Some(b'=') => self.make_token(TokenKind::EqualsEquals, 2),
                    Some(b'>') => self.make_token(TokenKind::EqualsGreater, 2),
                    _ => self.make_token(TokenKind::Equals, 1),
                },

                _ => self.make_token(TokenKind::Unknown, 1),
            },
            None => Ok(Token::new(
                TokenKind::Eof,
                vec![],
                Span {
                    start: start_location,
                    end: self.location,
                },
            )),
        }
    }

    fn lex_identifier_or_keyword(&mut self) -> MusiResult<Token> {
        let start_location = self.location;
        let start_position = self.position;

        while let Some(current) = self.peek() {
            if !is_identifier_continue(current) {
                break;
            }
            self.advance();
        }

        let lexeme = &self.source[start_position..self.position];
        let kind = match lexeme {
            b"and" => TokenKind::And,
            b"as" => TokenKind::As,
            b"async" => TokenKind::Async,
            b"await" => TokenKind::Await,
            b"break" => TokenKind::Break,
            b"const" => TokenKind::Const,
            b"continue" => TokenKind::Continue,
            b"def" => TokenKind::Def,
            b"do" => TokenKind::Do,
            b"else" => TokenKind::Else,
            b"false" => TokenKind::False,
            b"for" => TokenKind::For,
            b"foreign" => TokenKind::Foreign,
            b"from" => TokenKind::From,
            b"if" => TokenKind::If,
            b"import" => TokenKind::Import,
            b"in" => TokenKind::In,
            b"inline" => TokenKind::Inline,
            b"is" => TokenKind::Is,
            b"let" => TokenKind::Let,
            b"match" => TokenKind::Match,
            b"not" => TokenKind::Not,
            b"or" => TokenKind::Or,
            b"repeat" => TokenKind::Repeat,
            b"return" => TokenKind::Return,
            b"then" => TokenKind::Then,
            b"to" => TokenKind::To,
            b"true" => TokenKind::True,
            b"type" => TokenKind::Type,
            b"unsafe" => TokenKind::Unsafe,
            b"until" => TokenKind::Until,
            b"var" => TokenKind::Var,
            b"when" => TokenKind::When,
            b"where" => TokenKind::Where,
            b"while" => TokenKind::While,
            b"with" => TokenKind::With,
            b"yield" => TokenKind::Yield,
            _ => TokenKind::Identifier,
        };

        Ok(Token::new(
            kind,
            Vec::from(lexeme),
            Span {
                start: start_location,
                end: self.location,
            },
        ))
    }

    fn lex_number_literal(&mut self) -> MusiResult<Token> {
        let start_location = self.location;
        let start_position = self.position;

        while let Some(current) = self.peek() {
            if !current.is_ascii_digit() && current != b'_' {
                break;
            }
            if current == b'_' && !self.peek_next().map_or(false, |b| b.is_ascii_digit()) {
                break;
            }
            self.advance();
        }

        if self.peek() == Some(b'.') && self.peek_next().map_or(false, |b| b.is_ascii_digit()) {
            self.advance();
            while let Some(current) = self.peek() {
                if is_identifier_continue(current) {
                    break;
                }
                if current == b'_' && !self.peek_next().map_or(false, |b| b.is_ascii_digit()) {
                    break;
                }
                self.advance();
            }
        }

        Ok(Token::new(
            TokenKind::Literal(LiteralKind::Number),
            Vec::from(&self.source[start_position..self.position]),
            Span {
                start: start_location,
                end: self.location,
            },
        ))
    }

    fn lex_string_literal(&mut self) -> MusiResult<Token> {
        let start_location = self.location;
        let start_position = self.position;

        if self.match_bytes(b"\"\"\"") {
            while let Some(current) = self.peek() {
                if current == b'"' && self.match_bytes(b"\"\"\"") {
                    break;
                }
                if current == b'\n' {
                    self.location.line += 1;
                    self.location.column = 1;
                }
                self.advance();
            }

            return Ok(Token::new(
                TokenKind::Literal(LiteralKind::String),
                Vec::from(&self.source[start_position..self.position]),
                Span {
                    start: start_location,
                    end: self.location,
                },
            ));
        }

        self.advance(); // opening '"'

        while let Some(current) = self.peek() {
            match current {
                b'"' => {
                    self.advance(); // closing '"'
                    break;
                }
                b'\\' => self.lex_escape_sequence()?,
                b'\n' => {
                    return Err(MusiError::Lexical(LexicalError {
                        message: "unclosed string literal",
                    }));
                }
                _ => {
                    self.advance();
                }
            }
        }

        Ok(Token::new(
            TokenKind::Literal(LiteralKind::String),
            Vec::from(&self.source[start_position..self.position]),
            Span {
                start: start_location,
                end: self.location,
            },
        ))
    }

    fn lex_character_literal(&mut self) -> MusiResult<Token> {
        let start_position = self.position;
        let start_location = self.location;
        let mut chars = 0;

        self.advance(); // opening '\''

        while let Some(current) = self.peek() {
            match current {
                b'\'' => {
                    if chars == 0 {
                        return Err(MusiError::Lexical(LexicalError {
                            message: "empty character literal",
                        }));
                    } else if chars > 1 {
                        return Err(MusiError::Lexical(LexicalError {
                            message: "too many codepoints in character literal",
                        }));
                    }
                    self.advance(); // closing '\''
                    break;
                }
                b'\\' => {
                    self.lex_escape_sequence()?;
                    chars += 1;
                }
                b'\n' => {
                    return Err(MusiError::Lexical(LexicalError {
                        message: "unclosed character literal",
                    }));
                }
                _ => {
                    self.advance();
                    chars += 1;
                }
            }
        }

        Ok(Token::new(
            TokenKind::Literal(LiteralKind::Character),
            Vec::from(&self.source[start_position..self.position]),
            Span {
                start: start_location,
                end: self.location,
            },
        ))
    }

    fn lex_escape_sequence(&mut self) -> MusiResult<()> {
        match self.peek_next() {
            Some(b'n') | Some(b'r') | Some(b't') | Some(b'\\') | Some(b'"') | Some(b'\'') => {
                self.advance_by(2);
                Ok(())
            }

            Some(b'u') | Some(b'U') => {
                self.advance_by(2);
                self.lex_unicode_escape_sequence(4)
            }

            Some(b'x') => {
                self.advance_by(2);
                self.lex_unicode_escape_sequence(2)
            }

            Some(invalid) => Err(MusiError::Lexical(LexicalError {
                message: Box::leak(
                    format!("invalid escape sequence: \\{}", invalid as char).into_boxed_str(),
                ),
            })),
            None => Err(MusiError::Lexical(LexicalError {
                message: "unclosed escape sequence",
            })),
        }
    }

    fn lex_unicode_escape_sequence(&mut self, digits: usize) -> MusiResult<()> {
        let start_position = self.position;
        let mut value: u32 = 0;

        for _ in 0..digits {
            match self.peek() {
                None => {
                    return Err(MusiError::Lexical(LexicalError {
                        message: "unclosed unicode escape sequence",
                    }));
                }
                Some(current) if !current.is_ascii_hexdigit() => {
                    return Err(MusiError::Lexical(LexicalError {
                        message: Box::leak(
                            format!("invalid hexadecimal digit: 0x{current:X}").into_boxed_str(),
                        ),
                    }));
                }
                Some(current) => {
                    value = (value << 4)
                        | (match current {
                            b'0'..=b'9' => current - b'0',
                            b'a'..=b'f' => current - b'a' + 10,
                            b'A'..=b'F' => current - b'A' + 10,
                            _ => {
                                return Err(MusiError::Lexical(LexicalError {
                                    message: Box::leak(
                                        format!(
                                            "invalid unicode escape: \\u{}",
                                            self.source[start_position..self.position]
                                                .escape_ascii()
                                        )
                                        .into_boxed_str(),
                                    ),
                                }))
                            }
                        } as u32);
                    self.advance();
                }
            }
        }

        if (0xD800..=0xDFFF).contains(&value) {
            return Err(MusiError::Lexical(LexicalError {
                message: "surrogate range not allowed in unicode escape sequence",
            }));
        }

        if value > 0x10FFFF {
            return Err(MusiError::Lexical(LexicalError {
                message: "unicode escape sequence out of range",
            }));
        }

        Ok(())
    }
}

impl Lexer {
    #[inline]
    fn peek(&self) -> Option<u8> {
        self.source.get(self.position).copied()
    }

    #[inline]
    fn peek_next(&self) -> Option<u8> {
        self.source.get(self.position + 1).copied()
    }

    #[inline]
    fn advance(&mut self) -> Option<u8> {
        let current = self.peek()?;
        self.position += 1;
        self.location.column += 1;
        self.location.offset += 1;
        Some(current)
    }

    #[inline]
    fn advance_by(&mut self, offset: usize) -> Option<u8> {
        let mut previous = None;
        for _ in 0..offset {
            previous = self.advance();
        }
        previous
    }

    #[inline]
    fn match_bytes(&mut self, bytes: &[u8]) -> bool {
        let mut start_position = self.position;
        for &expected in bytes {
            match self.source.get(start_position) {
                Some(&current) if current == expected => start_position += 1,
                _ => return false,
            }
        }

        start_position > self.position
    }

    fn skip_whitespace(&mut self) -> MusiResult<Token> {
        if self.location.column == 1 {
            let mut spaces = 0;
            let start_location = self.location;

            while let Some(current) = self.peek() {
                match current {
                    b' ' => {
                        spaces += 1;
                        self.advance();
                    }
                    _ => {
                        if current != b'\r' && current != b'\n' {
                            let current_indent = self.indent_stack[self.indent_level];
                            if spaces.cmp(&{ current_indent }) == std::cmp::Ordering::Greater {
                                self.indent_level += 1;
                                self.indent_stack[self.indent_level] = spaces;

                                return Ok(Token::new(
                                    TokenKind::Indent,
                                    vec![b' '; spaces as usize],
                                    Span {
                                        start: start_location,
                                        end: self.location,
                                    },
                                ));
                            }
                        }
                        break;
                    }
                }
            }

            log::debug!(
                "{}:{}:{}: spaces={}, indent_level={}",
                self.location.line,
                self.location.column,
                self.location.offset,
                spaces,
                self.indent_level
            );
        }

        Ok(Token::new(
            TokenKind::Unknown,
            vec![],
            Span {
                start: self.location,
                end: self.location,
            },
        ))
    }

    fn make_token(&mut self, kind: TokenKind, length: usize) -> MusiResult<Token> {
        let start_location = self.location;
        let start_position = self.position;
        self.advance_by(length);

        Ok(Token::new(
            kind,
            Vec::from(&self.source[start_position..self.position]),
            Span {
                start: start_location,
                end: self.location,
            },
        ))
    }
}

#[inline]
const fn is_identifier_start(byte: u8) -> bool {
    byte.is_ascii_alphabetic() || byte == b'_'
}

#[inline]
const fn is_identifier_continue(input: u8) -> bool {
    input.is_ascii_alphanumeric() || input == b'_'
}
