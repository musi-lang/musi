use std::{
    cmp::{self},
    sync::Arc,
};

use crate::core::{diagnostics::Diagnostic, source::Source, span::Span, MusiResult};

use super::{
    cursor::Cursor,
    token::{Token, TokenKind},
};

const MAX_INDENT_LEVELS: usize = 32;

const KEYWORDS: &[(&[u8], TokenKind)] = &[
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
    indent_stack: [u32; MAX_INDENT_LEVELS],
    indent_level: usize,
    diagnostics: Vec<Diagnostic>,
}

impl Lexer {
    #[must_use]
    #[inline]
    pub const fn new(source: Arc<Source>) -> Self {
        Self {
            cursor: Cursor {
                source,
                position: 0,
                current_line: 0,
            },
            indent_stack: [0; MAX_INDENT_LEVELS],
            indent_level: 0,
            diagnostics: vec![],
        }
    }

    #[inline]
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
            if token.kind == TokenKind::Eof {
                while self.indent_level > 0 {
                    tokens.push(Token::new(
                        TokenKind::Dedent,
                        &[],
                        Span {
                            start: self.cursor.position,
                            end: self.cursor.position,
                        },
                    ));
                    self.indent_level = self.indent_level.saturating_sub(1);
                }

                tokens.push(token);
                break;
            } else if token.kind != TokenKind::Unknown {
                tokens.push(token);
            } else {
                // skip unknown
            }
        }

        Ok(tokens)
    }

    fn next_token(&mut self) -> MusiResult<Token> {
        let start_position = self.cursor.position;

        let whitespace = self.skip_whitespace()?;
        if whitespace.kind != TokenKind::Unknown {
            return Ok(whitespace);
        }

        match self.cursor.peek() {
            Some(current) => match current {
                b'\r' | b'\n' => Ok(self.lex_newline(start_position)),

                byte if byte.is_ascii_alphabetic() || byte == b'_' => {
                    Ok(self.lex_identifier_or_keyword())
                }
                byte if byte.is_ascii_digit() => Ok(self.lex_number()?),

                b'"' => self.lex_string(),
                b'\'' => self.lex_character(),

                b'(' => Ok(self.make_token(TokenKind::LeftParen, 1)),
                b')' => Ok(self.make_token(TokenKind::RightParen, 1)),
                b'{' => Ok(self.make_token(TokenKind::LeftBrace, 1)),
                b'}' => Ok(self.make_token(TokenKind::RightBrace, 1)),
                b'[' => Ok(self.make_token(TokenKind::LeftBracket, 1)),
                b']' => Ok(self.make_token(TokenKind::RightBracket, 1)),
                b',' => Ok(self.make_token(TokenKind::Comma, 1)),
                b':' => self.match_compound_token(&[
                    (TokenKind::ColonEquals, b":="),
                    (TokenKind::Colon, b":"),
                ]),
                b'.' => Ok(self.make_token(TokenKind::Dot, 1)),

                b'+' => Ok(self.make_token(TokenKind::Plus, 1)),
                b'-' => self.match_compound_token(&[
                    (TokenKind::MinusGreater, b"->"),
                    (TokenKind::Minus, b"-"),
                ]),
                b'*' => Ok(self.make_token(TokenKind::Star, 1)),
                b'/' => self.match_compound_token(&[
                    (TokenKind::SlashEquals, b"/="),
                    (TokenKind::Slash, b"/"),
                ]),
                b'^' => Ok(self.make_token(TokenKind::Caret, 1)),
                b'|' => self.match_compound_token(&[
                    (TokenKind::PipeGreater, b"|>"),
                    (TokenKind::Pipe, b"|"),
                ]),
                b'<' => self.match_compound_token(&[
                    (TokenKind::LessEqualsGreater, b"<=>"),
                    (TokenKind::LessEquals, b"<="),
                    (TokenKind::Less, b"<"),
                ]),
                b'>' => self.match_compound_token(&[
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
                    start: start_position,
                    end: self.cursor.position,
                },
            )),
        }
    }

    fn skip_whitespace(&mut self) -> MusiResult<Token> {
        if self.cursor.position
            == *self
                .cursor
                .source
                .line_starts
                .get(self.cursor.current_line)
                .ok_or_else(|| self.error("line start index out of bounds"))?
        {
            let mut spaces = 0_u32;
            let start_position = self.cursor.position;

            while let Some(current) = self.cursor.peek() {
                match current {
                    byte if byte.is_ascii_whitespace() => {
                        spaces = spaces.saturating_add(1);
                        self.cursor.advance();
                    }
                    _ => {
                        if current != b'\r' && current != b'\n' {
                            let current_indent =
                                *self.indent_stack.get(self.indent_level).unwrap_or(&0);

                            match spaces.cmp(&current_indent) {
                                cmp::Ordering::Greater => {
                                    self.indent_level = self.indent_level.saturating_add(1);
                                    *self
                                        .indent_stack
                                        .get_mut(self.indent_level)
                                        .unwrap_or(&mut 0) = spaces;

                                    return Ok(Token::new(
                                        TokenKind::Indent,
                                        &[u8::try_from(spaces)
                                            .map_err(|_error| self.error("integer overflow"))?],
                                        Span {
                                            start: start_position,
                                            end: self.cursor.position,
                                        },
                                    ));
                                }
                                cmp::Ordering::Less => {
                                    while self.indent_level > 0
                                        && spaces
                                            < *self
                                                .indent_stack
                                                .get(self.indent_level)
                                                .unwrap_or(&0)
                                    {
                                        self.indent_level = self.indent_level.saturating_sub(1);
                                    }

                                    if spaces < self.indent_stack[0] {
                                        return Err(self.error(
                                            "unindent does not match any outer indentation level",
                                        ));
                                    }
                                    if spaces
                                        != *self.indent_stack.get(self.indent_level).unwrap_or(&0)
                                    {
                                        return Err(self.error("inconsistent indentation"));
                                    }

                                    return Ok(Token::new(
                                        TokenKind::Dedent,
                                        &[],
                                        Span {
                                            start: start_position,
                                            end: self.cursor.position,
                                        },
                                    ));
                                }
                                cmp::Ordering::Equal => {}
                            };
                        }
                        break;
                    }
                }
            }
        }

        Ok(Token::new(
            TokenKind::Unknown,
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
            TokenKind::Newline,
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
            if !current.is_ascii_alphanumeric() || current == b'_' {
                break;
            }
            self.cursor.advance();
        }

        let lexeme = self.cursor.slice_from(start_position);
        let kind = KEYWORDS
            .iter()
            .find(|&&(keyword, _)| keyword == lexeme)
            .map_or(TokenKind::Identifer, |&(_, kind)| kind);

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
            && self.cursor.peek_next().map_or(false, |byte| {
                matches!(byte, b'x' | b'X' | b'b' | b'B' | b'o' | b'O')
            })
        {
            self.lex_number_base()?;

            return Ok(Token::new(
                TokenKind::Number,
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
                .map_or(false, |byte| byte.is_ascii_digit())
        {
            self.cursor.advance();
            self.lex_decimal_digits();
        }

        if let Some(b'e' | b'E') = self.cursor.peek() {
            self.lex_scientific_notation();
        }

        Ok(Token::new(
            TokenKind::Number,
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
                        TokenKind::String,
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
        let mut chars = 0_i32;

        self.cursor.advance(); // skip opening apostrophe

        while let Some(current) = self.cursor.peek() {
            match current {
                b'\'' => {
                    match chars {
                        0_i32 => return Err(self.error("empty character literal")),
                        char if char > 1_i32 => {
                            return Err(
                                self.error("character literal may only contain one character")
                            )
                        }
                        _ => (),
                    }

                    self.cursor.advance(); // skip closing apostrophe

                    return Ok(Token::new(
                        TokenKind::Character,
                        self.cursor.slice_from(start_position),
                        Span {
                            start: start_position,
                            end: self.cursor.position,
                        },
                    ));
                }
                b'\\' => {
                    self.lex_escape_sequence()?;
                    chars = chars.saturating_add(1);
                    continue;
                }
                b'\n' => {
                    return Err(self.error("unclosed character literal"));
                }
                _ => {
                    self.cursor.advance();
                    chars = chars.saturating_add(1);
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
                            .map_or(false, |byte| byte.is_ascii_hexdigit())
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
                            .map_or(false, |byte| matches!(byte, b'0'..=b'7'))
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
                    .map_or(false, |byte| byte.is_ascii_digit())
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
                self.lex_unicode_escape_sequence::<4>()
            }
            Some(invalid) => Err(self.error(format!(
                "unexpected escape sequence '{:#?}'",
                char::from(invalid)
            ))),
            None => Err(self.error("unclosed escape sequence")),
        }
    }

    fn lex_unicode_escape_sequence<const N: usize>(&mut self) -> MusiResult<()> {
        let mut value = 0;

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
                        b'0'..=b'9' => current.wrapping_sub(b'0'),
                        b'a'..=b'f' | b'A'..=b'F' => current.wrapping_sub(b'A').wrapping_add(10),
                        unexpected => {
                            return Err(self
                                .error(format!("unexpected hexadecimal digit '{unexpected:#X}'")))
                        }
                    };
                    value = (value << 4_i32) | u32::from(digit);
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
    fn make_token(&mut self, kind: TokenKind, length: usize) -> Token {
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
    fn match_compound_token(&mut self, patterns: &[(TokenKind, &[u8])]) -> MusiResult<Token> {
        let matched = patterns.iter().find(|&&(_, pattern)| match pattern.len() {
            2 => {
                if pattern.len() < 2 {
                    return false;
                }

                matches!((pattern.first(), pattern.get(1)), (Some(&first), Some(&second)) if self.cursor.match_2byte(first, second))
            }
            3 => {
                if pattern.len() < 3 {
                    return false;
                }

                matches!((pattern.first(), pattern.get(1), pattern.get(2)), (Some(&first), Some(&second), Some(&third)) if self.cursor.match_3byte(first, second, third))
            }
            _ => false,
        });

        match matched {
            Some(&(kind, pattern)) => Ok(self.make_token(kind, pattern.len())),
            None => Ok(self.make_token(
                patterns
                    .first()
                    .ok_or_else(|| self.error("patterns must not be empty"))?
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
