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
                b'\r' | b'\n' => Ok(self.lex_newline(start_location)),
                b if is_identifier_start(b) => Ok(self.lex_identifier_or_keyword()),
                b if b.is_ascii_digit() => Ok(self.lex_number_literal()),
                b'"' => self.lex_string_literal(),
                b'\'' => self.lex_character_literal(),

                b'(' => Ok(self.make_token(TokenKind::LeftParen, 1)),
                b')' => Ok(self.make_token(TokenKind::RightParen, 1)),
                b'{' => Ok(self.make_token(TokenKind::LeftBrace, 1)),
                b'}' => Ok(self.make_token(TokenKind::RightBrace, 1)),
                b'[' => Ok(self.make_token(TokenKind::LeftBracket, 1)),
                b']' => Ok(self.make_token(TokenKind::RightBracket, 1)),
                b',' => Ok(self.make_token(TokenKind::Comma, 1)),
                b'.' => Ok(self.make_token(TokenKind::Dot, 1)),
                b':' => Ok(self.make_multibyte_token(&[
                    (TokenKind::Colon, b":"),
                    (TokenKind::ColonEquals, b":="),
                ])),

                b'+' => Ok(self.make_multibyte_token(&[
                    (TokenKind::Plus, b"+"),
                    (TokenKind::PlusPlus, b"++"),
                    (TokenKind::PlusEquals, b"+="),
                ])),
                b'-' => Ok(self.make_multibyte_token(&[
                    (TokenKind::Minus, b"-"),
                    (TokenKind::MinusGreater, b"->"),
                    (TokenKind::MinusEquals, b"-="),
                ])),
                b'*' => Ok(self.make_multibyte_token(&[
                    (TokenKind::Star, b"*"),
                    (TokenKind::StarStar, b"**"),
                    (TokenKind::StarStarEquals, b"**="),
                    (TokenKind::StarEquals, b"*="),
                ])),
                b'/' => Ok(self.make_multibyte_token(&[
                    (TokenKind::Slash, b"/"),
                    (TokenKind::SlashSlash, b"//"),
                    (TokenKind::SlashSlashEquals, b"//="),
                    (TokenKind::SlashEquals, b"/="),
                ])),
                b'%' => Ok(self.make_multibyte_token(&[
                    (TokenKind::Percent, b"%"),
                    (TokenKind::PercentEquals, b"%="),
                ])),
                b'&' => Ok(self.make_multibyte_token(&[
                    (TokenKind::Ampersand, b"&"),
                    (TokenKind::AmpersandEquals, b"&="),
                ])),
                b'|' => Ok(self.make_multibyte_token(&[
                    (TokenKind::Pipe, b"|"),
                    (TokenKind::PipeEquals, b"|="),
                ])),
                b'^' => Ok(self.make_multibyte_token(&[
                    (TokenKind::Caret, b"^"),
                    (TokenKind::CaretEquals, b"^="),
                ])),
                b'~' => Ok(self.make_multibyte_token(&[
                    (TokenKind::Tilde, b"~"),
                    (TokenKind::TildeEquals, b"~="),
                ])),
                b'<' => Ok(self.make_multibyte_token(&[
                    (TokenKind::Less, b"<"),
                    (TokenKind::LessLess, b"<<"),
                    (TokenKind::LessLessEquals, b"<<="),
                    (TokenKind::LessEquals, b"<="),
                    (TokenKind::LessEqualsGreater, b"<=>"),
                    (TokenKind::LessGreater, b"<>"),
                ])),
                b'>' => Ok(self.make_multibyte_token(&[
                    (TokenKind::Greater, b">"),
                    (TokenKind::GreaterEquals, b">="),
                    (TokenKind::GreaterGreater, b">>"),
                    (TokenKind::GreaterGreaterEquals, b">>="),
                ])),
                b'=' => Ok(self.make_multibyte_token(&[
                    (TokenKind::Equals, b"="),
                    (TokenKind::EqualsEquals, b"=="),
                    (TokenKind::EqualsGreater, b"=>"),
                ])),

                _ => Ok(self.make_token(TokenKind::Unknown, 1)),
            },
            None => Ok(Token::new(
                TokenKind::Eof,
                &[],
                Span {
                    start: start_location,
                    end: self.cursor.location,
                },
            )),
        }
    }

    #[inline]
    fn make_token(&mut self, kind: TokenKind, length: usize) -> Token {
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

    fn make_multibyte_token(&mut self, options: &[(TokenKind, &[u8])]) -> Token {
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

                            match spaces.cmp(&{ current_indent }) {
                                std::cmp::Ordering::Greater => {
                                    self.indent_level += 1;
                                    self.indent_stack[self.indent_level] = spaces;

                                    return Ok(Token::new(
                                        TokenKind::Indent,
                                        &vec![b' '; spaces as usize],
                                        Span {
                                            start: start_location,
                                            end: self.cursor.location,
                                        },
                                    ));
                                }
                                std::cmp::Ordering::Less => {
                                    lex_error("inconsistent indentation")?;
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
            TokenKind::Newline,
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
            .map_or(TokenKind::Identifier, |(_, kind)| *kind);

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
                TokenKind::Literal(LiteralKind::Number),
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
            TokenKind::Literal(LiteralKind::Number),
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
                    lex_error("unclosed string literal")?;
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
            TokenKind::Literal(if byte_string {
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
            TokenKind::Literal(if byte_char {
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
            TokenKind::Literal(LiteralKind::String),
            &self.cursor.source[start_position..self.cursor.position],
            Span {
                start: start_location,
                end: self.cursor.location,
            },
        )
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
                lex_error("non-ASCII character in byte string literal")?;
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
            &self.cursor.source[start_position..self.cursor.position],
            Span {
                start: start_location,
                end: self.cursor.location,
            },
        ))
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
