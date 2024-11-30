use crate::{
    cursor::Cursor,
    errors::{LexicalError, MusiError, MusiResult},
    location::Location,
    span::Span,
    token::{Kind, LiteralKind, Token},
};

const KEYWORDS: &[(&[u8], Kind)] = &[
    (b"and", Kind::And),
    (b"as", Kind::As),
    (b"async", Kind::Async),
    (b"await", Kind::Await),
    (b"break", Kind::Break),
    (b"const", Kind::Const),
    (b"continue", Kind::Continue),
    (b"deref", Kind::Deref),
    (b"do", Kind::Do),
    (b"downto", Kind::Downto),
    (b"else", Kind::Else),
    (b"exists", Kind::Exists),
    (b"false", Kind::False),
    (b"for", Kind::For),
    (b"foreign", Kind::Foreign),
    (b"from", Kind::From),
    (b"if", Kind::If),
    (b"in", Kind::In),
    (b"include", Kind::Include),
    (b"inherit", Kind::Inherit),
    (b"inline", Kind::Inline),
    (b"is", Kind::Is),
    (b"let", Kind::Let),
    (b"match", Kind::Match),
    (b"not", Kind::Not),
    (b"of", Kind::Of),
    (b"or", Kind::Or),
    (b"ref", Kind::Ref),
    (b"repeat", Kind::Repeat),
    (b"return", Kind::Return),
    (b"then", Kind::Then),
    (b"to", Kind::To),
    (b"true", Kind::True),
    (b"type", Kind::Type),
    (b"unsafe", Kind::Unsafe),
    (b"until", Kind::Until),
    (b"var", Kind::Var),
    (b"when", Kind::When),
    (b"where", Kind::Where),
    (b"while", Kind::While),
    (b"with", Kind::With),
    (b"xor", Kind::Xor),
    (b"yield", Kind::Yield),
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
            if whitespace.kind != Kind::Unknown {
                tokens.push(whitespace);
            }

            let token = self.next_token()?;

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

        Ok(tokens)
    }

    fn next_token(&mut self) -> MusiResult<Token> {
        let start_location = self.cursor.location;

        match self.cursor.peek() {
            Some(current) => {
                match current {
                    b'\r' | b'\n' => Ok(self.lex_newline(start_location)),
                    b if is_identifier_start(b) => Ok(self.lex_identifier_or_keyword()),
                    b if b.is_ascii_digit() => Ok(self.lex_number_literal()),
                    b'"' => self.lex_string_literal(),
                    b'\'' => self.lex_character_literal(),

                    b'(' => Ok(self.make_token(Kind::LeftParen, 1)),
                    b')' => Ok(self.make_token(Kind::RightParen, 1)),
                    b'{' => Ok(self.make_token(Kind::LeftBrace, 1)),
                    b'}' => Ok(self.make_token(Kind::RightBrace, 1)),
                    b'[' => Ok(self.make_token(Kind::LeftBracket, 1)),
                    b']' => Ok(self.make_token(Kind::RightBracket, 1)),
                    b',' => Ok(self.make_token(Kind::Comma, 1)),
                    b'.' => Ok(self.make_token(Kind::Dot, 1)),
                    b':' => Ok(self
                        .make_multibyte_token(&[(Kind::Colon, b":"), (Kind::ColonEquals, b":=")])),
                    b';' => Ok(self.make_token(Kind::Semicolon, 1)),
                    b'?' => Ok(self.make_token(Kind::Question, 1)),
                    b'@' => Ok(self.make_token(Kind::At, 1)),

                    b'+' => Ok(self.make_token(Kind::Plus, 1)),
                    b'-' => Ok(self
                        .make_multibyte_token(&[(Kind::Minus, b"-"), (Kind::MinusGreater, b"->")])),
                    b'*' => Ok(self.make_multibyte_token(&[(Kind::Star, b"*")])),
                    b'/' => Ok(self
                        .make_multibyte_token(&[(Kind::Slash, b"/"), (Kind::SlashEquals, b"/=")])),
                    b'&' => Ok(self.make_multibyte_token(&[(Kind::Ampersand, b"&")])),
                    b'|' => Ok(self.make_multibyte_token(&[
                        (Kind::Pipe, b"|"),
                        (Kind::PipeGreater, b"|>"),
                        (Kind::PipeMinusGreater, b"|->"),
                    ])),
                    b'^' => Ok(self.make_token(Kind::Caret, 1)),
                    b'~' => Ok(self
                        .make_multibyte_token(&[(Kind::Tilde, b"~"), (Kind::TildeEquals, b"~=")])),
                    b'<' => Ok(self.make_multibyte_token(&[
                        (Kind::Less, b"<"),
                        (Kind::LessEquals, b"<="),
                        (Kind::LessEqualsGreater, b"<=>"),
                        (Kind::LessLess, b"<<"),
                    ])),
                    b'>' => Ok(self.make_multibyte_token(&[
                        (Kind::Greater, b">"),
                        (Kind::GreaterEquals, b">="),
                        (Kind::GreaterGreater, b">>"),
                    ])),
                    b'=' => Ok(self.make_multibyte_token(&[
                        (Kind::Equals, b"="),
                        (Kind::EqualsGreater, b"=>"),
                    ])),

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

    fn make_multibyte_token(&mut self, options: &[(Kind, &[u8])]) -> Token {
        for (token_kind, pattern) in options {
            if pattern.len() == 2 && self.cursor.match_2byte(pattern[0], pattern[1]) {
                return self.make_token(*token_kind, 2);
            } else if pattern.len() == 3
                && self.cursor.match_3byte(pattern[0], pattern[1], pattern[2])
            {
                return self.make_token(*token_kind, 3);
            }
        }

        self.make_token(options[0].0, 1)
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
                                        lex_error(
                                            "unindent does not match any outer indentation level",
                                        )?;
                                    }
                                    if spaces != self.indent_stack[self.indent_level] {
                                        lex_error("inconsistent indentation")?;
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
        let lexeme = if self.cursor.peek() == Some(b'\r') {
            self.cursor.advance();
            if self.cursor.peek() == Some(b'\n') {
                self.cursor.advance();
                vec![b'\r', b'\n']
            } else {
                vec![b'\r']
            }
        } else {
            self.cursor.advance();
            vec![b'\n']
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

        if self.cursor.peek() == Some(b'0')
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

        if self.cursor.peek() == Some(b'.')
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

        let byte_string = self.cursor.peek() == Some(b'b');
        if byte_string {
            self.cursor.advance();
        }

        if self.cursor.peek() == Some(b'r') {
            return Ok(self.lex_raw_string(start_location, start_position));
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
                    // multi-line by default
                    self.cursor.advance();
                }
                _ if byte_string && !current.is_ascii() => {
                    lex_error("non-ASCII character in byte string literal")?;
                }
                _ => {
                    self.cursor.advance();
                }
            }
        }

        Ok(Token::new(
            Kind::Literal(if byte_string {
                LiteralKind::ByteString
            } else {
                LiteralKind::String
            }),
            &self.cursor.source[start_position..self.cursor.position],
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
                        lex_error("empty character literal")?;
                    } else if chars > 1 {
                        lex_error("too many codepoints in character literal")?;
                    }
                    self.cursor.advance(); // closing '\''
                    break;
                }
                b'\\' => {
                    self.lex_escape_sequence()?;
                    chars += 1;
                }
                b'\n' => {
                    lex_error("unclosed character literal")?;
                }
                _ if byte_char && !current.is_ascii() => {
                    lex_error("non-ASCII character in byte character literal")?;
                }
                _ => {
                    self.cursor.advance();
                    chars += 1;
                }
            }
        }

        Ok(Token::new(
            Kind::Literal(if byte_char {
                LiteralKind::ByteCharacter
            } else {
                LiteralKind::Character
            }),
            &self.cursor.source[start_position..self.cursor.position],
            Span {
                start: start_location,
                end: self.cursor.location,
            },
        ))
    }

    fn lex_number_base(&mut self) {
        match self.cursor.peek_next() {
            Some(b'x' | b'X') => {
                self.cursor.advance_by(2);
                while let Some(current) = self.cursor.peek() {
                    if !current.is_ascii_hexdigit() && current != b'_' {
                        break;
                    }
                    self.cursor.advance();
                }
            }
            Some(b'b' | b'B') => {
                self.cursor.advance_by(2);
                while let Some(current) = self.cursor.peek() {
                    if current != b'0' && current != b'1' && current != b'_' {
                        break;
                    }
                    self.cursor.advance();
                }
            }
            Some(b'o' | b'O') => {
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

    fn lex_scientific_notation(&mut self) {
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
                    .map_or(false, |b| b.is_ascii_digit())
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
                    lex_error("unclosed unicode escape sequence")?;
                }
                Some(current) if !current.is_ascii_hexdigit() => {
                    lex_error(&format!("invalid hexadecimal digit: 0x{current:X}"))?;
                }
                Some(current) => {
                    if current.is_ascii_hexdigit() {
                        let digit = match current {
                            b'0'..=b'9' => current - b'0',
                            b'a'..=b'f' => current - b'a' + 10,
                            b'A'..=b'F' => current - b'A' + 10,
                            _ => unreachable!(),
                        };
                        value = (value << 4) | u32::from(digit);
                    } else {
                        lex_error(&format!(
                            "invalid unicode escape: \\u{}",
                            self.cursor.source[start_position..self.cursor.position].escape_ascii()
                        ))?;
                    }
                    self.cursor.advance();
                }
            }
        }

        if (0xD800..=0xDFFF).contains(&value) {
            lex_error("surrogate range not allowed in unicode escape sequence")?;
        }

        if value > 0x0010_FFFF {
            lex_error("unicode escape sequence out of range")?;
        }

        Ok(())
    }

    fn lex_raw_string(&mut self, start_location: Location, start_position: usize) -> Token {
        self.cursor.advance_by(2);
        while let Some(current) = self.cursor.peek() {
            if current == b'"' {
                self.cursor.advance();
                break;
            }
            self.cursor.advance();
        }

        Token::new(
            Kind::Literal(LiteralKind::String),
            &self.cursor.source[start_position..self.cursor.position],
            Span {
                start: start_location,
                end: self.cursor.location,
            },
        )
    }
}

#[inline]
const fn is_identifier_start(input: u8) -> bool {
    input.is_ascii_alphabetic() || input == b'_'
}

#[inline]
const fn is_identifier_continue(input: u8) -> bool {
    input.is_ascii_alphanumeric() || input == b'_'
}

#[inline]
fn lex_error(message: &str) -> MusiResult<()> {
    Err(MusiError::Lexical(LexicalError {
        message: Box::leak(message.into()),
    }))
}
