use crate::{
    cursor::Cursor,
    errors::{LexicalError, MusiError, MusiResult},
    location::Location,
    span::Span,
    token::{LiteralKind, Token, TokenKind},
};

const KEYWORDS: &[(&[u8], TokenKind)] = &[
    (b"and", TokenKind::And),
    (b"as", TokenKind::As),
    (b"at", TokenKind::At),
    (b"async", TokenKind::Async),
    (b"await", TokenKind::Await),
    (b"break", TokenKind::Break),
    (b"const", TokenKind::Const),
    (b"continue", TokenKind::Continue),
    (b"def", TokenKind::Def),
    (b"deref", TokenKind::Deref),
    (b"do", TokenKind::Do),
    (b"else", TokenKind::Else),
    (b"false", TokenKind::False),
    (b"for", TokenKind::For),
    (b"foreign", TokenKind::Foreign),
    (b"from", TokenKind::From),
    (b"if", TokenKind::If),
    (b"import", TokenKind::Import),
    (b"in", TokenKind::In),
    (b"inline", TokenKind::Inline),
    (b"is", TokenKind::Is),
    (b"let", TokenKind::Let),
    (b"match", TokenKind::Match),
    (b"not", TokenKind::Not),
    (b"of", TokenKind::Of),
    (b"or", TokenKind::Or),
    (b"ref", TokenKind::Ref),
    (b"repeat", TokenKind::Repeat),
    (b"return", TokenKind::Return),
    (b"then", TokenKind::Then),
    (b"to", TokenKind::To),
    (b"true", TokenKind::True),
    (b"type", TokenKind::Type),
    (b"unsafe", TokenKind::Unsafe),
    (b"until", TokenKind::Until),
    (b"var", TokenKind::Var),
    (b"when", TokenKind::When),
    (b"where", TokenKind::Where),
    (b"while", TokenKind::While),
    (b"with", TokenKind::With),
    (b"yield", TokenKind::Yield),
];

const MAX_INDENT_LEVELS: usize = 32;
const UNICODE_ESCAPE_HEX_DIGITS: usize = 4;

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

    pub fn lex(&mut self) -> MusiResult<Vec<Token>> {
        let mut tokens = vec![];

        loop {
            let whitespace = self.skip_whitespace()?;
            if whitespace.kind != TokenKind::Unknown {
                tokens.push(whitespace);
            }

            let token = self.next_token()?;

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

    fn next_token(&mut self) -> MusiResult<Token> {
        let start_location = self.cursor.location;

        match self.cursor.peek() {
            Some(current) => match current {
                b'\r' => {
                    self.cursor.advance();
                    if self.cursor.peek() == Some(b'\n') {
                        self.cursor.advance();
                    }
                    self.cursor.location.line += 1;
                    self.cursor.location.column = 1;

                    Ok(Token::new(
                        TokenKind::Newline,
                        vec![b'\r', b'\n'],
                        Span {
                            start: start_location,
                            end: self.cursor.location,
                        },
                    ))
                }
                b'\n' => {
                    self.cursor.advance();
                    self.cursor.location.line += 1;
                    self.cursor.location.column = 1;

                    Ok(Token::new(
                        TokenKind::Newline,
                        vec![b'\n'],
                        Span {
                            start: start_location,
                            end: self.cursor.location,
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
                b':' => match self.cursor.peek_next() {
                    Some(b'=') => self.make_token(TokenKind::ColonEquals, 2),
                    _ => self.make_token(TokenKind::Colon, 1),
                },
                b'+' => match self.cursor.peek_next() {
                    Some(b'+') => self.make_token(TokenKind::PlusPlus, 2),
                    Some(b'=') => self.make_token(TokenKind::PlusEquals, 2),
                    _ => self.make_token(TokenKind::Plus, 1),
                },
                b'-' => match self.cursor.peek_next() {
                    Some(b'>') => self.make_token(TokenKind::MinusGreater, 2),
                    Some(b'=') => self.make_token(TokenKind::MinusEquals, 2),
                    _ => self.make_token(TokenKind::Minus, 1),
                },
                b'*' => match self.cursor.peek_next() {
                    Some(b'*') => match self.cursor.source.get(self.cursor.position + 2) {
                        Some(b'=') => self.make_token(TokenKind::StarStarEquals, 3),
                        _ => self.make_token(TokenKind::StarStar, 2),
                    },
                    Some(b'=') => self.make_token(TokenKind::StarEquals, 2),
                    _ => self.make_token(TokenKind::Star, 1),
                },
                b'/' => match self.cursor.peek_next() {
                    Some(b'/') => match self.cursor.source.get(self.cursor.position + 2) {
                        Some(b'=') => self.make_token(TokenKind::SlashSlashEquals, 3),
                        _ => self.make_token(TokenKind::SlashSlash, 2),
                    },
                    Some(b'=') => self.make_token(TokenKind::SlashEquals, 2),
                    _ => self.make_token(TokenKind::Slash, 1),
                },
                b'%' => match self.cursor.peek_next() {
                    Some(b'=') => self.make_token(TokenKind::PercentEquals, 2),
                    _ => self.make_token(TokenKind::Percent, 1),
                },
                b'&' => match self.cursor.peek_next() {
                    Some(b'=') => self.make_token(TokenKind::AmpersandEquals, 2),
                    _ => self.make_token(TokenKind::Ampersand, 1),
                },
                b'|' => match self.cursor.peek_next() {
                    Some(b'=') => self.make_token(TokenKind::PipeEquals, 2),
                    _ => self.make_token(TokenKind::Pipe, 1),
                },
                b'^' => match self.cursor.peek_next() {
                    Some(b'=') => self.make_token(TokenKind::CaretEquals, 2),
                    _ => self.make_token(TokenKind::Caret, 1),
                },
                b'~' => match self.cursor.peek_next() {
                    Some(b'=') => self.make_token(TokenKind::TildeEquals, 2),
                    _ => self.make_token(TokenKind::Tilde, 1),
                },
                b'<' => match self.cursor.peek_next() {
                    Some(b'<') => match self.cursor.source.get(self.cursor.position + 2) {
                        Some(b'=') => self.make_token(TokenKind::LessLessEquals, 3),
                        _ => self.make_token(TokenKind::LessLess, 2),
                    },
                    Some(b'=') => match self.cursor.source.get(self.cursor.position + 2) {
                        Some(b'>') => self.make_token(TokenKind::LessEqualsGreater, 3),
                        _ => self.make_token(TokenKind::LessEquals, 2),
                    },
                    Some(b'>') => self.make_token(TokenKind::LessGreater, 2),
                    _ => self.make_token(TokenKind::Less, 1),
                },
                b'>' => match self.cursor.peek_next() {
                    Some(b'>') => match self.cursor.source.get(self.cursor.position + 2) {
                        Some(b'=') => self.make_token(TokenKind::GreaterGreaterEquals, 3),
                        _ => self.make_token(TokenKind::GreaterGreater, 2),
                    },
                    Some(b'=') => self.make_token(TokenKind::GreaterEquals, 2),
                    _ => self.make_token(TokenKind::Greater, 1),
                },
                b'=' => match self.cursor.peek_next() {
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
                    end: self.cursor.location,
                },
            )),
        }
    }

    fn make_token(&mut self, kind: TokenKind, length: usize) -> MusiResult<Token> {
        let start_location = self.cursor.location;
        let start_position = self.cursor.position;
        self.cursor.advance_by(length);

        Ok(Token::new(
            kind,
            Vec::from(&self.cursor.source[start_position..self.cursor.position]),
            Span {
                start: start_location,
                end: self.cursor.location,
            },
        ))
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
                        if current != b'\r' && current != b'\n' {
                            let current_indent = self.indent_stack[self.indent_level];

                            match spaces.cmp(&{ current_indent }) {
                                std::cmp::Ordering::Greater => {
                                    self.indent_level += 1;
                                    self.indent_stack[self.indent_level] = spaces;

                                    return Ok(Token::new(
                                        TokenKind::Indent,
                                        vec![b' '; spaces as usize],
                                        Span {
                                            start: start_location,
                                            end: self.cursor.location,
                                        },
                                    ));
                                }
                                std::cmp::Ordering::Less => {
                                    self.lex_error("inconsistent indentation")?;
                                }
                                std::cmp::Ordering::Equal => {}
                            }
                        }
                        break;
                    }
                }
            }

            log::debug!(
                "{}:{}:{}: spaces={}, indent_level={}",
                self.cursor.location.line,
                self.cursor.location.column,
                self.cursor.location.offset,
                spaces,
                self.indent_level
            );
        }

        Ok(Token::new(
            TokenKind::Unknown,
            vec![],
            Span {
                start: self.cursor.location,
                end: self.cursor.location,
            },
        ))
    }

    fn lex_identifier_or_keyword(&mut self) -> MusiResult<Token> {
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
            .map(|(_, kind)| *kind)
            .unwrap_or(TokenKind::Identifier);

        Ok(Token::new(
            kind,
            Vec::from(lexeme),
            Span {
                start: start_location,
                end: self.cursor.location,
            },
        ))
    }

    fn lex_number_literal(&mut self) -> MusiResult<Token> {
        let start_location = self.cursor.location;
        let start_position = self.cursor.position;

        if self.cursor.peek() == Some(b'0')
            && self.cursor.peek_next().map_or(false, |b| {
                matches!(b, b'x' | b'X' | b'b' | b'B' | b'o' | b'O')
            })
        {
            self.lex_number_base()?;

            return Ok(Token::new(
                TokenKind::Literal(LiteralKind::Number),
                Vec::from(&self.cursor.source[start_position..self.cursor.position]),
                Span {
                    start: start_location,
                    end: self.cursor.location,
                },
            ));
        }

        self.lex_decimal_digits();

        if self.cursor.peek() == Some(b'.')
            && self
                .cursor
                .peek_next()
                .map_or(false, |b| b.is_ascii_digit())
        {
            self.cursor.advance();
            self.lex_decimal_digits();
        }

        if let Some(b'e') | Some(b'E') = self.cursor.peek() {
            self.lex_scientific_notation()?;
        }

        Ok(Token::new(
            TokenKind::Literal(LiteralKind::Number),
            Vec::from(&self.cursor.source[start_position..self.cursor.position]),
            Span {
                start: start_location,
                end: self.cursor.location,
            },
        ))
    }

    fn lex_string_literal(&mut self) -> MusiResult<Token> {
        let start_location = self.cursor.location;
        let start_position = self.cursor.position;

        let byte_string = self.cursor.peek() == Some(b'b');
        if byte_string {
            self.cursor.advance();
        }

        if self.cursor.peek() == Some(b'r') {
            return self.lex_raw_string(start_location, start_position);
        }

        if self.cursor.match_sequence(b"\"\"\"") {
            return self.lex_multiline_string(start_location, start_position);
        }

        self.cursor.advance(); // opening '"'

        while let Some(current) = self.cursor.peek() {
            match current {
                b'"' => {
                    self.cursor.advance(); // closing '"'
                    break;
                }
                b'\\' => self.lex_escape_sequence()?,
                b'\n' => {
                    self.lex_error("unclosed string literal")?;
                }
                _ if byte_string && !current.is_ascii() => {
                    self.lex_error("non-ASCII character in byte string literal")?;
                }
                _ => {
                    self.cursor.advance();
                }
            }
        }

        Ok(Token::new(
            TokenKind::Literal(if byte_string {
                LiteralKind::ByteString
            } else {
                LiteralKind::String
            }),
            Vec::from(&self.cursor.source[start_position..self.cursor.position]),
            Span {
                start: start_location,
                end: self.cursor.location,
            },
        ))
    }

    fn lex_character_literal(&mut self) -> MusiResult<Token> {
        let start_position = self.cursor.position;
        let start_location = self.cursor.location;
        let mut chars = 0;

        let byte_char = self.cursor.peek() == Some(b'b');
        if byte_char {
            self.cursor.advance();
        }

        self.cursor.advance(); // opening '\''

        while let Some(current) = self.cursor.peek() {
            match current {
                b'\'' => {
                    if chars == 0 {
                        self.lex_error("empty character literal")?;
                    } else if chars > 1 {
                        self.lex_error("too many codepoints in character literal")?;
                    }
                    self.cursor.advance(); // closing '\''
                    break;
                }
                b'\\' => {
                    self.lex_escape_sequence()?;
                    chars += 1;
                }
                b'\n' => {
                    self.lex_error("unclosed character literal")?;
                }
                _ if byte_char && !current.is_ascii() => {
                    self.lex_error("non-ASCII character in byte character literal")?;
                }
                _ => {
                    self.cursor.advance();
                    chars += 1;
                }
            }
        }

        Ok(Token::new(
            TokenKind::Literal(if byte_char {
                LiteralKind::ByteCharacter
            } else {
                LiteralKind::Character
            }),
            Vec::from(&self.cursor.source[start_position..self.cursor.position]),
            Span {
                start: start_location,
                end: self.cursor.location,
            },
        ))
    }

    fn lex_number_base(&mut self) -> MusiResult<()> {
        match self.cursor.peek_next() {
            Some(b'x') | Some(b'X') => {
                self.cursor.advance_by(2);
                while let Some(current) = self.cursor.peek() {
                    if !current.is_ascii_hexdigit() && current != b'_' {
                        break;
                    }
                    self.cursor.advance();
                }
            }
            Some(b'b') | Some(b'B') => {
                self.cursor.advance_by(2);
                while let Some(current) = self.cursor.peek() {
                    if current != b'0' && current != b'1' && current != b'_' {
                        break;
                    }
                    self.cursor.advance();
                }
            }
            Some(b'o') | Some(b'O') => {
                self.cursor.advance_by(2);
                while let Some(current) = self.cursor.peek() {
                    if !matches!(current, b'0'..=b'7' | b'_') {
                        break;
                    }
                    self.cursor.advance();
                }
            }
            _ => {}
        }
        Ok(())
    }

    fn lex_decimal_digits(&mut self) {
        while let Some(current) = self.cursor.peek() {
            if !current.is_ascii_digit() && current != b'_' {
                break;
            }
            if current == b'_'
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

    fn lex_scientific_notation(&mut self) -> MusiResult<()> {
        self.cursor.advance();

        if let Some(b'+') | Some(b'-') = self.cursor.peek() {
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
                    .map_or(false, |b| b.is_ascii_digit())
            {
                break;
            }
            self.cursor.advance();
        }
        Ok(())
    }

    fn lex_escape_sequence(&mut self) -> MusiResult<()> {
        match self.cursor.peek_next() {
            Some(b'n') | Some(b'r') | Some(b't') | Some(b'\\') | Some(b'"') | Some(b'\'') => {
                self.cursor.advance_by(2);
                Ok(())
            }

            Some(b'u') | Some(b'U') => {
                self.cursor.advance_by(2);
                self.lex_unicode_escape_sequence::<UNICODE_ESCAPE_HEX_DIGITS>()
            }

            Some(b'x') => {
                self.cursor.advance_by(2);
                self.lex_unicode_escape_sequence::<UNICODE_ESCAPE_HEX_DIGITS>()
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

    fn lex_unicode_escape_sequence<const N: usize>(&mut self) -> MusiResult<()> {
        let start_position = self.cursor.position;
        let mut value: u32 = 0;

        for _ in 0..N {
            match self.cursor.peek() {
                None => {
                    self.lex_error("unclosed unicode escape sequence")?;
                }
                Some(current) if !current.is_ascii_hexdigit() => {
                    self.lex_error(&format!("invalid hexadecimal digit: 0x{current:X}"))?;
                }
                Some(current) => {
                    if current.is_ascii_hexdigit() {
                        let digit = match current {
                            b'0'..=b'9' => current - b'0',
                            b'a'..=b'f' => current - b'a' + 10,
                            b'A'..=b'F' => current - b'A' + 10,
                            _ => unreachable!(),
                        };
                        value = (value << 4) | (digit as u32);
                    } else {
                        self.lex_error(&format!(
                            "invalid unicode escape: \\u{}",
                            self.cursor.source[start_position..self.cursor.position].escape_ascii()
                        ))?;
                    }
                    self.cursor.advance();
                }
            }
        }

        if (0xD800..=0xDFFF).contains(&value) {
            self.lex_error("surrogate range not allowed in unicode escape sequence")?;
        }

        if value > 0x10FFFF {
            self.lex_error("unicode escape sequence out of range")?;
        }

        Ok(())
    }

    fn lex_raw_string(
        &mut self,
        start_location: Location,
        start_position: usize,
    ) -> MusiResult<Token> {
        self.cursor.advance_by(2);
        while let Some(current) = self.cursor.peek() {
            if current == b'"' {
                self.cursor.advance();
                break;
            }
            self.cursor.advance();
        }

        Ok(Token::new(
            TokenKind::Literal(LiteralKind::String),
            Vec::from(&self.cursor.source[start_position..self.cursor.position]),
            Span {
                start: start_location,
                end: self.cursor.location,
            },
        ))
    }

    fn lex_multiline_string(
        &mut self,
        start_location: Location,
        start_position: usize,
    ) -> MusiResult<Token> {
        let byte_string = self.cursor.source[start_position] == b'b';
        while let Some(current) = self.cursor.peek() {
            if current == b'"' && self.cursor.match_sequence(b"\"\"\"") {
                break;
            }
            if byte_string && !current.is_ascii() {
                self.lex_error("non-ASCII character in byte string literal")?;
            }
            if current == b'\n' {
                self.cursor.location.line += 1;
                self.cursor.location.column = 1;
            }
            self.cursor.advance();
        }

        Ok(Token::new(
            TokenKind::Literal(if byte_string {
                LiteralKind::ByteString
            } else {
                LiteralKind::String
            }),
            Vec::from(&self.cursor.source[start_position..self.cursor.position]),
            Span {
                start: start_location,
                end: self.cursor.location,
            },
        ))
    }

    fn lex_error(&self, message: &str) -> MusiResult<()> {
        Err(MusiError::Lexical(LexicalError {
            message: Box::leak(message.into()),
        }))
    }
}

#[inline(always)]
const fn is_identifier_start(input: u8) -> bool {
    input.is_ascii_alphabetic() || input == b'_'
}

#[inline(always)]
const fn is_identifier_continue(input: u8) -> bool {
    input.is_ascii_alphanumeric() || input == b'_'
}
