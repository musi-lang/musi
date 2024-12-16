use std::{
    cmp::{self},
    sync::Arc,
};

use crate::core::{diagnostics::Diagnostic, source::NamedSource, span::Span, MusiResult};

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
    indent_widths: [u32; MAX_INDENT_DEPTH],
    indent_stack_depth: usize,
    diagnostics: Vec<Diagnostic>,
}

impl Lexer {
    #[inline]
    #[must_use]
    pub const fn new(source: Arc<NamedSource>) -> Self {
        Self {
            cursor: Cursor {
                source,
                offset: 0,
                line: 0,
            },
            indent_widths: [0; MAX_INDENT_DEPTH],
            indent_stack_depth: 0,
            diagnostics: vec![],
        }
    }

    #[inline]
    pub fn lex(&mut self) -> MusiResult<Vec<Token>> {
        let mut tokens = Vec::with_capacity(self.cursor.remaining_bytes() >> 3); // divided by 8

        while let Ok(token) = self.next_token() {
            match token.kind {
                TokenKind::Eof => {
                    while self.indent_stack_depth > 0 {
                        tokens.push(Token::new(
                            TokenKind::Dedent,
                            &[],
                            Span {
                                start: self.cursor.offset,
                                end: self.cursor.offset,
                            },
                        ));
                        self.indent_stack_depth = self.indent_stack_depth.saturating_sub(1);
                    }
                    tokens.push(token);
                    break;
                }
                TokenKind::Unknown => continue,
                _ => {
                    tokens.push(token);
                    continue;
                }
            }
        }
        if let Err(diagnostic) = self.next_token() {
            self.diagnostics.push(diagnostic);
            self.recover_from_error();
        }
        Ok(tokens)
    }

    fn next_token(&mut self) -> MusiResult<Token> {
        let start_offset = self.cursor.offset;

        let indent_token = self.lex_indentation()?;
        if indent_token.kind != TokenKind::Unknown {
            return Ok(indent_token);
        }

        if self.cursor.is_at_end() {
            return Ok(Token::new(
                TokenKind::Eof,
                &[],
                Span {
                    start: start_offset,
                    end: self.cursor.offset,
                },
            ));
        }

        let current_byte = self
            .cursor
            .current_byte()
            .ok_or_else(|| self.error("unexpected end of input"))?;
        match current_byte {
            b'\r' | b'\n' => Ok(self.lex_newline(start_offset)),

            byte if byte.is_ascii_alphabetic() || byte == b'_' => Ok(self.lex_identifier()),
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
            b':' => self
                .lex_compound_token(&[(TokenKind::ColonEquals, b":="), (TokenKind::Colon, b":")]),
            b'.' => Ok(self.make_token(TokenKind::Dot, 1)),

            b'+' => Ok(self.make_token(TokenKind::Plus, 1)),
            b'-' => self
                .lex_compound_token(&[(TokenKind::MinusGreater, b"->"), (TokenKind::Minus, b"-")]),
            b'*' => Ok(self.make_token(TokenKind::Star, 1)),
            b'/' => self
                .lex_compound_token(&[(TokenKind::SlashEquals, b"/="), (TokenKind::Slash, b"/")]),
            b'^' => Ok(self.make_token(TokenKind::Caret, 1)),
            b'|' => {
                self.lex_compound_token(&[(TokenKind::PipeGreater, b"|>"), (TokenKind::Pipe, b"|")])
            }
            b'<' => self.lex_compound_token(&[
                (TokenKind::LessEqualsGreater, b"<=>"),
                (TokenKind::LessEquals, b"<="),
                (TokenKind::Less, b"<"),
            ]),
            b'>' => self.lex_compound_token(&[
                (TokenKind::GreaterEquals, b">="),
                (TokenKind::Greater, b">"),
            ]),
            b'=' => Ok(self.make_token(TokenKind::Equals, 1)),
            _ => Ok(self.make_token(TokenKind::Unknown, 1)),
        }
    }
    fn lex_indentation(&mut self) -> MusiResult<Token> {
        if self.cursor.offset
            == *self
                .cursor
                .source
                .line_offsets
                .get(self.cursor.line)
                .ok_or_else(|| self.error("line start index out of bounds"))?
        {
            let mut indent_width = 0_u32;
            let start_offset = self.cursor.offset;

            while let Some(current_byte) = self.cursor.current_byte() {
                match current_byte {
                    byte if byte.is_ascii_whitespace() => {
                        indent_width = indent_width.saturating_add(1);
                        self.cursor.advance();
                    }
                    _ => {
                        if current_byte != b'\r' && current_byte != b'\n' {
                            let parent_indent_width = *self
                                .indent_widths
                                .get(self.indent_stack_depth)
                                .unwrap_or(&0);

                            match indent_width.cmp(&parent_indent_width) {
                                cmp::Ordering::Greater => {
                                    self.indent_stack_depth =
                                        self.indent_stack_depth.saturating_add(1);
                                    *self
                                        .indent_widths
                                        .get_mut(self.indent_stack_depth)
                                        .unwrap_or(&mut 0) = indent_width;

                                    return Ok(Token::new(
                                        TokenKind::Indent,
                                        &[u8::try_from(indent_width)
                                            .map_err(|_error| self.error("integer overflow"))?],
                                        Span {
                                            start: start_offset,
                                            end: self.cursor.offset,
                                        },
                                    ));
                                }
                                cmp::Ordering::Less => {
                                    while self.indent_stack_depth > 0
                                        && indent_width
                                            < *self
                                                .indent_widths
                                                .get(self.indent_stack_depth)
                                                .unwrap_or(&0)
                                    {
                                        self.indent_stack_depth =
                                            self.indent_stack_depth.saturating_sub(1);
                                    }

                                    if indent_width < self.indent_widths[0] {
                                        return Err(self.error(
                                            "unindent does not match any outer indentation level",
                                        ));
                                    }
                                    if indent_width
                                        != *self
                                            .indent_widths
                                            .get(self.indent_stack_depth)
                                            .unwrap_or(&0)
                                    {
                                        return Err(self.error("inconsistent indentation"));
                                    }

                                    return Ok(Token::new(
                                        TokenKind::Dedent,
                                        &[],
                                        Span {
                                            start: start_offset,
                                            end: self.cursor.offset,
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
                start: self.cursor.offset,
                end: self.cursor.offset,
            },
        ))
    }

    fn lex_newline(&mut self, start: usize) -> Token {
        let lexeme = if self.cursor.current_byte() == Some(b'\r') {
            self.cursor.advance();

            if self.cursor.current_byte() == Some(b'\n') {
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
                end: self.cursor.offset,
            },
        )
    }

    fn lex_identifier(&mut self) -> Token {
        let start_offset = self.cursor.offset;

        while let Some(current_byte) = self.cursor.current_byte() {
            if !current_byte.is_ascii_alphanumeric() && current_byte != b'_' {
                break;
            }
            self.cursor.advance();
        }

        let lexeme = self.cursor.byte_slice_from(start_offset);
        let kind = RESERVED_KEYWORDS
            .iter()
            .find(|&&(keyword, _)| keyword == lexeme)
            .map_or(TokenKind::Identifer, |&(_, kind)| kind);

        Token::new(
            kind,
            lexeme,
            Span {
                start: start_offset,
                end: self.cursor.offset,
            },
        )
    }

    fn lex_number(&mut self) -> MusiResult<Token> {
        let start_offset = self.cursor.offset;

        if self.cursor.current_byte() == Some(b'0')
            && self.cursor.next_byte().map_or(false, |byte| {
                matches!(byte, b'x' | b'X' | b'b' | b'B' | b'o' | b'O')
            })
        {
            self.lex_number_with_radix()?;

            return Ok(Token::new(
                TokenKind::Number,
                self.cursor.byte_slice_from(start_offset),
                Span {
                    start: start_offset,
                    end: self.cursor.offset,
                },
            ));
        }

        self.lex_number_digits();

        if self.cursor.current_byte() == Some(b'.')
            && self
                .cursor
                .next_byte()
                .map_or(false, |byte| byte.is_ascii_digit())
        {
            self.cursor.advance();
            self.lex_number_digits();
        }

        if let Some(b'e' | b'E') = self.cursor.current_byte() {
            self.lex_scientific_notation();
        }

        Ok(Token::new(
            TokenKind::Number,
            self.cursor.byte_slice_from(start_offset),
            Span {
                start: start_offset,
                end: self.cursor.offset,
            },
        ))
    }

    fn lex_string(&mut self) -> MusiResult<Token> {
        let start_offset = self.cursor.offset;

        self.cursor.advance(); // skip opening quote

        while let Some(current_byte) = self.cursor.current_byte() {
            match current_byte {
                b'"' => {
                    self.cursor.advance(); // skip closing quote

                    return Ok(Token::new(
                        TokenKind::String,
                        self.cursor.byte_slice_from(start_offset),
                        Span {
                            start: start_offset,
                            end: self.cursor.offset,
                        },
                    ));
                }
                b'\\' => {
                    self.lex_escape_sequence()?;
                    continue;
                }
                0..=0x1F => {
                    return Err(
                        self.error(if current_byte == b'\r' || current_byte == b'\n' {
                            "unclosed string literal"
                        } else {
                            "control character not allowed in string literal"
                        }),
                    );
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
        let start_offset = self.cursor.offset;
        let mut char_count = 0_i32;

        self.cursor.advance(); // skip opening apostrophe

        while let Some(current_byte) = self.cursor.current_byte() {
            match current_byte {
                b'\'' => {
                    match char_count {
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
                        self.cursor.byte_slice_from(start_offset),
                        Span {
                            start: start_offset,
                            end: self.cursor.offset,
                        },
                    ));
                }
                b'\\' => {
                    self.lex_escape_sequence()?;
                    char_count = char_count.saturating_add(1);
                    continue;
                }
                b'\n' => {
                    return Err(self.error("unclosed character literal"));
                }
                _ => {
                    self.cursor.advance();
                    char_count = char_count.saturating_add(1);
                    continue;
                }
            }
        }

        Err(self.error("unclosed character literal"))
    }

    #[inline]
    fn lex_number_with_radix(&mut self) -> MusiResult<()> {
        match self.cursor.next_byte() {
            Some(b'x' | b'X') => {
                self.cursor.advance_by(2);

                let mut has_valid_digits = false;

                while let Some(current_byte) = self.cursor.current_byte() {
                    if current_byte == b'_'
                        && !self
                            .cursor
                            .next_byte()
                            .map_or(false, |byte| byte.is_ascii_hexdigit())
                    {
                        return Err(self.error("expected hexadecimal digit after '_'"));
                    }
                    if !current_byte.is_ascii_hexdigit() && current_byte != b'_' {
                        break;
                    }

                    has_valid_digits = has_valid_digits || current_byte != b'_';
                    self.cursor.advance();
                }

                if !has_valid_digits {
                    return Err(self.error("expected hexadecimal digit"));
                }
            }
            Some(b'b' | b'B') => {
                self.cursor.advance_by(2);

                let mut has_digits = false;

                while let Some(current_byte) = self.cursor.current_byte() {
                    if current_byte == b'_'
                        && !self
                            .cursor
                            .next_byte()
                            .map_or(false, |next| [b'0', b'1'].contains(&next))
                    {
                        return Err(self.error("expected binary digit after '_'"));
                    }
                    if ![b'0', b'1', b'_'].contains(&current_byte) {
                        break;
                    }

                    has_digits = has_digits || current_byte != b'_';
                    self.cursor.advance();
                }

                if !has_digits {
                    return Err(self.error("expected binary digit after binary prefix"));
                }
            }
            Some(b'o' | b'O') => {
                self.cursor.advance_by(2);

                let mut has_digits = false;

                while let Some(current_byte) = self.cursor.current_byte() {
                    if current_byte == b'_'
                        && !self
                            .cursor
                            .next_byte()
                            .map_or(false, |byte| matches!(byte, b'0'..=b'7'))
                    {
                        return Err(self.error("expected octal digit after '_'"));
                    }
                    if !matches!(current_byte, b'0'..=b'7' | b'_') {
                        break;
                    }

                    has_digits = has_digits || current_byte != b'_';
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
    fn lex_number_digits(&mut self) {
        while let Some(current_byte) = self.cursor.current_byte() {
            if !current_byte.is_ascii_digit() && current_byte != b'_' {
                break;
            }
            if current_byte == b'_'
                && !self
                    .cursor
                    .next_byte()
                    .map_or(false, |byte| byte.is_ascii_digit())
            {
                break;
            }
            self.cursor.advance();
        }
    }

    fn lex_scientific_notation(&mut self) {
        self.cursor.advance();

        if let Some(b'+' | b'-') = self.cursor.current_byte() {
            self.cursor.advance();
        }

        while let Some(current_byte) = self.cursor.current_byte() {
            if !current_byte.is_ascii_digit() && current_byte != b'_' {
                break;
            }
            if current_byte == b'_'
                && !self
                    .cursor
                    .next_byte()
                    .map_or(false, |byte| byte.is_ascii_digit())
            {
                break;
            }
            self.cursor.advance();
        }
    }

    fn lex_escape_sequence(&mut self) -> MusiResult<()> {
        match self.cursor.next_byte() {
            Some(b'n' | b'r' | b't' | b'\\' | b'"' | b'\'') => {
                self.cursor.advance_by(2);
                Ok(())
            }
            Some(b'u' | b'U' | b'x') => {
                self.cursor.advance_by(2);
                self.lex_unicode_escape::<4>()
            }
            Some(invalid) => Err(self.error(format!(
                "unexpected escape sequence '{:#?}'",
                char::from(invalid)
            ))),
            None => Err(self.error("unclosed escape sequence")),
        }
    }

    fn lex_unicode_escape<const N: usize>(&mut self) -> MusiResult<()> {
        let mut unicode_value = 0;

        for _ in 0..N {
            match self.cursor.current_byte() {
                None => {
                    return Err(self.error("unclosed unicode escape"));
                }
                Some(current_byte) if !current_byte.is_ascii_hexdigit() => {
                    return Err(
                        self.error(format!("unexpected hexadecimal digit '{current_byte:#X}'"))
                    );
                }
                Some(current_byte) => {
                    let hexadecimal_digit = match current_byte {
                        b'0'..=b'9' => current_byte.wrapping_sub(b'0'),
                        b'a'..=b'f' | b'A'..=b'F' => {
                            current_byte.wrapping_sub(b'A').wrapping_add(10)
                        }
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
        let start_offset = self.cursor.offset;
        self.cursor.advance_by(length);

        Token::new(
            kind,
            self.cursor.byte_slice_from(start_offset),
            Span {
                start: start_offset,
                end: self.cursor.offset,
            },
        )
    }

    #[inline]
    fn lex_compound_token(&mut self, patterns: &[(TokenKind, &[u8])]) -> MusiResult<Token> {
        let matched = patterns.iter().find(|&&(_, pattern)| match pattern.len() {
            2 => {
                if pattern.len() < 2 {
                    return false;
                }

                matches!((pattern.first(), pattern.get(1)), (Some(&first), Some(&second)) if self.cursor.matches_byte_pair(first, second))
            }
            3 => {
                if pattern.len() < 3 {
                    return false;
                }

                matches!((pattern.first(), pattern.get(1), pattern.get(2)), (Some(&first), Some(&second), Some(&third)) if self.cursor.matches_byte_triplet(first, second, third))
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
                start: self.cursor.offset,
                end: self.cursor.offset,
            },
        )
        .with_source(&self.cursor.source)
    }

    fn recover_from_error(&mut self) {
        while let Some(current_byte) = self.cursor.current_byte() {
            match current_byte {
                b'\n' => {
                    self.cursor.advance();
                    self.indent_stack_depth = 0;
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
