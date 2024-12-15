use std::sync::Arc;

use crate::core::{diagnostics::Diagnostic, source::Source, span::Span, MusiResult};

use super::{
    cursor::Cursor,
    token::{Kind, Token},
};

const MAX_INDENT_LEVELS: usize = 32;

const KEYWORDS: &[(&[u8], Kind)] = &[
    (b"and", Kind::And),
    (b"as", Kind::As),
    (b"break", Kind::Break),
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
    (b"match", Kind::Match),
    (b"not", Kind::Not),
    (b"of", Kind::Of),
    (b"or", Kind::Or),
    (b"ref", Kind::Ref),
    (b"return", Kind::Return),
    (b"then", Kind::Then),
    (b"true", Kind::True),
    (b"type", Kind::Type),
    (b"until", Kind::Until),
    (b"var", Kind::Var),
    (b"when", Kind::When),
    (b"where", Kind::Where),
    (b"while", Kind::While),
    (b"with", Kind::With),
    (b"yield", Kind::Yield),
];

pub struct Lexer {
    cursor: Cursor,
    indent_stack: [u32; MAX_INDENT_LEVELS],
    indent_level: usize,
    diagnostics: Vec<Diagnostic>,
}

impl Lexer {
    #[must_use]
    pub const fn new(source: Arc<Source>) -> Self {
        Self {
            cursor: Cursor::new(source),
            indent_stack: [0; MAX_INDENT_LEVELS],
            indent_level: 0,
            diagnostics: vec![],
        }
    }

    #[allow(clippy::missing_errors_doc)]
    pub fn lex(&mut self) -> MusiResult<Vec<Token>> {
        let mut tokens = Vec::with_capacity(self.cursor.source.content.len() >> 3); // divide by 8

        loop {
            let token = match self.next_token() {
                Ok(token) => token,
                Err(diagnostic) => {
                    self.diagnostics.push(diagnostic);
                    self.sync();
                    continue;
                }
            };
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
                    b'\r' | b'\n' => Ok(self.lex_newline(start_position)),

                    b if is_identifier_start(b) => Ok(self.lex_identifier_or_keyword()),
                    b if b.is_ascii_digit() => Ok(self.lex_number()?),

                    b'"' => self.lex_string(),
                    b'\'' => self.lex_character(),

                    b'(' => Ok(self.make_token(Kind::LeftParen, 1)),
                    b')' => Ok(self.make_token(Kind::RightParen, 1)),
                    b'{' => Ok(self.make_token(Kind::LeftBrace, 1)),
                    b'}' => Ok(self.make_token(Kind::RightBrace, 1)),
                    b'[' => Ok(self.make_token(Kind::LeftBracket, 1)),
                    b']' => Ok(self.make_token(Kind::RightBracket, 1)),
                    b',' => Ok(self.make_token(Kind::Comma, 1)),
                    b':' => Ok(self
                        .match_compound_token(&[(Kind::ColonEquals, b":="), (Kind::Colon, b":")])),
                    b'.' => Ok(self.make_token(Kind::Dot, 1)),

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
                        if current != b'\r' && current != b'\n' {
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
            .map_or(Kind::Identifer, |(_, kind)| *kind);

        Token::new(
            kind,
            lexeme,
            Span {
                start: start_position,
                end: self.cursor.position,
            },
        )
    }

    fn lex_number(&mut self) -> MusiResult<Token> {
        let start_position = self.cursor.position;

        if self.cursor.peek() == Some(b'0')
            && self.cursor.peek_next().map_or(false, |b| {
                matches!(b, b'x' | b'X' | b'b' | b'B' | b'o' | b'O')
            })
        {
            self.lex_number_base()?;

            return Ok(Token::new(
                Kind::Number,
                self.cursor.slice_from(start_position),
                Span {
                    start: start_position,
                    end: self.cursor.position,
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

        if let Some(b'e' | b'E') = self.cursor.peek() {
            self.lex_scientific_notation();
        }

        Ok(Token::new(
            Kind::Number,
            self.cursor.slice_from(start_position),
            Span {
                start: start_position,
                end: self.cursor.position,
            },
        ))
    }

    fn lex_string(&mut self) -> MusiResult<Token> {
        let start_position = self.cursor.position;

        self.cursor.advance(); // skip opening quote

        while let Some(current) = self.cursor.peek() {
            match current {
                b'"' => {
                    self.cursor.advance(); // skip closing quote

                    return Ok(Token::new(
                        Kind::String,
                        self.cursor.slice_from(start_position),
                        Span {
                            start: start_position,
                            end: self.cursor.position,
                        },
                    ));
                }
                b'\\' => {
                    self.lex_escape_sequence()?;
                    continue;
                }
                0..=0x1F => {
                    return Err(self.error(if current == b'\r' || current == b'\n' {
                        "unclosed string literal"
                    } else {
                        "control character not allowed in string literal"
                    }));
                }
                _ => {
                    self.cursor.advance();
                    continue;
                }
            }
        }

        Err(self.error("unclosed string literal"))
    }

    fn lex_character(&mut self) -> MusiResult<Token> {
        let start_position = self.cursor.position;
        let mut chars = 0;

        self.cursor.advance(); // skip opening apostrophe

        while let Some(current) = self.cursor.peek() {
            match current {
                b'\'' => {
                    if chars == 0 {
                        return Err(self.error("empty character literal"));
                    } else if chars > 1 {
                        return Err(self.error("character literal may only contain one character"));
                    }

                    self.cursor.advance(); // skip closing apostrophe

                    return Ok(Token::new(
                        Kind::Character,
                        self.cursor.slice_from(start_position),
                        Span {
                            start: start_position,
                            end: self.cursor.position,
                        },
                    ));
                }
                b'\\' => {
                    self.lex_escape_sequence()?;
                    chars += 1;
                    continue;
                }
                b'\n' => {
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
    fn lex_number_base(&mut self) -> MusiResult<()> {
        match self.cursor.peek_next() {
            Some(b'x' | b'X') => {
                self.cursor.advance_by(2);

                let mut has_digits = false;
                while let Some(current) = self.cursor.peek() {
                    if current == b'_'
                        && !self
                            .cursor
                            .peek_next()
                            .map_or(false, |b| b.is_ascii_hexdigit())
                    {
                        return Err(self.error("expected hexadecimal digit after '_'"));
                    }
                    if !current.is_ascii_hexdigit() && current != b'_' {
                        break;
                    }
                    has_digits = has_digits || current != b'_';
                    self.cursor.advance();
                }
                if !has_digits {
                    return Err(self.error("expected hexadecimal digit"));
                }
            }
            Some(b'b' | b'B') => {
                self.cursor.advance_by(2);
                let mut has_digits = false;
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
                    has_digits = has_digits || current != b'_';
                    self.cursor.advance();
                }
                if !has_digits {
                    return Err(self.error("expected binary digit after binary prefix"));
                }
            }
            Some(b'o' | b'O') => {
                self.cursor.advance_by(2);
                let mut has_digits = false;
                while let Some(current) = self.cursor.peek() {
                    if current == b'_'
                        && !self
                            .cursor
                            .peek_next()
                            .map_or(false, |b| matches!(b, b'0'..=b'7'))
                    {
                        return Err(self.error("expected octal digit after '_'"));
                    }
                    if !matches!(current, b'0'..=b'7' | b'_') {
                        break;
                    }
                    has_digits = has_digits || current != b'_';
                    self.cursor.advance();
                }
                if !has_digits {
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
                self.lex_unicode_escape_sequence::<4>()
            }
            Some(invalid) => Err(self.error(format!(
                "unexpected escape sequence '{:#?}'",
                invalid as char
            ))),
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
                    return Err(self.error(format!("unexpected hexadecimal digit '{current:#X}'")));
                }
                Some(current) => {
                    let digit = match current {
                        b'0'..=b'9' => current - b'0',
                        b'a'..=b'f' | b'A'..=b'F' => (current & 0xDF) - b'A' + 10,
                        unexpected => {
                            return Err(self
                                .error(format!("unexpected hexadecimal digit '{unexpected:#X}'")))
                        }
                    };
                    value = (value << 4) | u32::from(digit);
                    self.cursor.advance();
                }
            }
        }

        if (0xD800..=0xDFFF).contains(&value) {
            return Err(self.error("surrogate pairs not allowed in unicode escape sequence"));
        }

        if value > 0x0010_FFFF {
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

    fn sync(&mut self) {
        while let Some(current) = self.cursor.peek() {
            match current {
                b'\n' => {
                    self.cursor.advance();
                    self.indent_level = 0;
                    break;
                }
                b'}' | b')' => {
                    self.cursor.advance();
                    break;
                }
                _ => {
                    self.cursor.advance();
                }
            }
        }
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
