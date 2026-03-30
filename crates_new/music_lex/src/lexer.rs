#![allow(clippy::arithmetic_side_effects)]

mod tables;

use music_basic::Span;
use smallvec::SmallVec;
use std::collections::VecDeque;

use crate::Cursor;
use crate::errors::{LexError, LexErrorKind, LexResult};
use crate::token::{
    FStringPart, FStringPartKind, FStringParts, Token, TokenKind, Trivia, TriviaKind, Trivias,
};
use tables::{compound_token, keyword_from_text, single_char_token};

const fn is_ident_start(ch: char) -> bool {
    ch.is_ascii_alphabetic() || ch == '_'
}

const fn is_symop_byte(byte: u8) -> bool {
    // Operator-runs are intentionally conservative: they must not swallow syntax
    // separators like `:` or `|`, and must not re-introduce removed compounds
    // like `::` or `??`.
    matches!(
        byte,
        b'+' | b'-' | b'*' | b'/' | b'%' | b'<' | b'>' | b'=' | b'&' | b'^' | b'~'
    )
}

const fn is_digit_for_base(byte: u8, base: u32) -> bool {
    match base {
        2 => matches!(byte, b'0' | b'1'),
        8 => matches!(byte, b'0'..=b'7'),
        16 => byte.is_ascii_hexdigit(),
        _ => byte.is_ascii_digit(),
    }
}

#[derive(Debug)]
pub struct LexedSource<'src> {
    source: &'src str,
    tokens: Vec<Token>,
    errors: Vec<LexError>,
}

impl<'src> LexedSource<'src> {
    #[must_use]
    pub const fn new(source: &'src str, tokens: Vec<Token>, errors: Vec<LexError>) -> Self {
        Self {
            source,
            tokens,
            errors,
        }
    }

    #[must_use]
    pub const fn source(&self) -> &'src str {
        self.source
    }

    #[must_use]
    pub fn tokens(&self) -> &[Token] {
        &self.tokens
    }

    #[must_use]
    pub fn errors(&self) -> &[LexError] {
        &self.errors
    }

    pub fn iter(&self) -> impl Iterator<Item = &Token> {
        self.tokens.iter()
    }

    #[must_use]
    ///
    /// # Panics
    ///
    /// Panics if `span` does not fit in `usize` or does not lie on valid
    /// UTF-8 boundaries within the stored source.
    pub fn text(&self, span: Span) -> &'src str {
        let start = usize::try_from(span.start).expect("span start fits in usize");
        let end = usize::try_from(span.end).expect("span end fits in usize");
        self.source
            .get(start..end)
            .expect("span boundaries stay on UTF-8 edges")
    }

    #[must_use]
    pub fn token_text(&self, index: usize) -> &'src str {
        self.text(self.tokens[index].span)
    }
}

pub struct Lexer<'src> {
    cursor: Cursor<'src>,
    source: &'src str,
    finished: bool,
    pending_errors: VecDeque<LexError>,
}

impl<'src> Lexer<'src> {
    #[must_use]
    pub const fn new(source: &'src str) -> Self {
        Self {
            cursor: Cursor::new(source),
            source,
            finished: false,
            pending_errors: VecDeque::new(),
        }
    }

    #[must_use]
    pub fn lex(mut self) -> LexedSource<'src> {
        let mut tokens = Vec::with_capacity(self.cursor.source_len() / 4);
        let mut errors = vec![];

        for result in self.by_ref() {
            match result {
                Ok(token) => tokens.push(token),
                Err(error) => errors.push(error),
            }
        }

        LexedSource::new(self.source, tokens, errors)
    }

    fn scan_token_kind(&mut self) -> LexResult<TokenKind> {
        if let Some((width, kind)) = compound_token(self.cursor.remaining_bytes()) {
            self.cursor.advance_by(width);
            return Ok(kind);
        }

        let start = self.cursor.pos();
        let ch = self
            .cursor
            .advance()
            .expect("scan_token_kind called before EOF");

        match ch {
            _ if is_ident_start(ch) => self.scan_ident_like(start),
            '0'..='9' => self.scan_number(start, ch),
            '.' if self.cursor.peek().is_some_and(|next| next.is_ascii_digit())
                && self.leading_dot_is_float(start) =>
            {
                self.scan_leading_dot_float(start)
            }
            '"' => self.scan_string(start),
            '\'' => self.scan_rune(start),
            '`' => self.scan_escaped_ident(start),
            _ if ch.is_ascii() => {
                let first = ch as u8;
                if is_symop_byte(first)
                    && self
                        .cursor
                        .peek()
                        .is_some_and(|next| next.is_ascii() && is_symop_byte(next as u8))
                {
                    let _ = self.cursor.eat_while_ascii(is_symop_byte);
                    return Ok(TokenKind::SymOp);
                }

                single_char_token(ch).ok_or_else(|| LexError {
                    kind: LexErrorKind::UnexpectedChar(ch),
                    span: self.cursor.span_from(start),
                })
            }
            _ => single_char_token(ch).ok_or_else(|| LexError {
                kind: LexErrorKind::UnexpectedChar(ch),
                span: self.cursor.span_from(start),
            }),
        }
    }

    fn leading_dot_is_float(&self, start: u32) -> bool {
        let mut i = usize::try_from(start).unwrap_or(0);
        let bytes = self.source.as_bytes();
        while i > 0 && bytes[i - 1].is_ascii_whitespace() {
            i -= 1;
        }
        if i == 0 {
            return true;
        }
        !Self::is_expr_end_byte(bytes[i - 1])
    }

    fn queue_error(&mut self, kind: LexErrorKind, span: Span) {
        self.pending_errors.push_back(LexError { kind, span });
    }

    fn is_expr_end_byte(byte: u8) -> bool {
        byte.is_ascii_alphanumeric()
            || byte == b'_'
            || matches!(byte, b')' | b']' | b'}' | b'"' | b'\'' | b'`')
    }

    fn scan_digits_with_separators(
        &mut self,
        is_digit: impl Fn(u8) -> bool,
        require_first_digit: bool,
    ) -> bool {
        let mut last_was_sep = false;
        let mut saw_digit = false;
        while let Some(byte) = self.cursor.peek_byte() {
            if !byte.is_ascii() {
                break;
            }
            if is_digit(byte) {
                saw_digit = true;
                last_was_sep = false;
                self.cursor.advance_by(1);
                continue;
            }
            if byte == b'_' {
                let underscore_pos = self.cursor.pos();
                self.cursor.advance_by(1);
                let next_is_digit = self
                    .cursor
                    .peek_byte()
                    .is_some_and(|next| next.is_ascii() && is_digit(next));
                if !saw_digit || last_was_sep || !next_is_digit {
                    self.queue_error(
                        LexErrorKind::InvalidDigitSeparator,
                        Span::new(underscore_pos, underscore_pos + 1),
                    );
                }
                last_was_sep = true;
                continue;
            }
            break;
        }
        saw_digit || !require_first_digit
    }

    fn scan_exponent_part(&mut self, exp_start: u32) {
        let _ = self.cursor.advance();
        let _ = self.cursor.eat('+') || self.cursor.eat('-');

        if !self.scan_digits_with_separators(|byte| byte.is_ascii_digit(), true) {
            self.queue_error(
                LexErrorKind::ExpectedDigitsAfterExponent,
                self.cursor.span_from(exp_start),
            );
        }
    }

    fn scan_ident_like(&mut self, start: u32) -> LexResult<TokenKind> {
        let _ = self
            .cursor
            .eat_while_ascii(|byte| byte.is_ascii_alphanumeric() || byte == b'_');
        let text = self.cursor.slice(start);
        if text == "f" && self.cursor.peek() == Some('"') {
            return self.scan_fstring(start);
        }
        Ok(keyword_from_text(text).unwrap_or(TokenKind::Ident))
    }

    fn scan_number(&mut self, start: u32, first: char) -> LexResult<TokenKind> {
        if first == '0' {
            if let Some(prefix @ ('x' | 'o' | 'b')) = self.cursor.peek() {
                let _ = self.cursor.advance();
                let base = match prefix {
                    'x' => 16,
                    'o' => 8,
                    _ => 2,
                };
                if !self.scan_digits_with_separators(|byte| is_digit_for_base(byte, base), true) {
                    self.queue_error(
                        LexErrorKind::ExpectedDigitsAfterBasePrefix,
                        self.cursor.span_from(start),
                    );
                }
                return Ok(TokenKind::IntLit);
            }
        }

        let _ = self.scan_digits_with_separators(|byte| byte.is_ascii_digit(), false);
        if self.cursor.peek() == Some('.')
            && self
                .cursor
                .peek_next()
                .is_some_and(|next| next.is_ascii_digit())
        {
            let _ = self.cursor.advance();
            let _ = self.scan_digits_with_separators(|byte| byte.is_ascii_digit(), true);
            if matches!(self.cursor.peek(), Some('e' | 'E')) {
                let exp_start = self.cursor.pos();
                self.scan_exponent_part(exp_start);
            }
            return Ok(TokenKind::FloatLit);
        }

        if matches!(self.cursor.peek(), Some('e' | 'E')) {
            let exp_start = self.cursor.pos();
            self.scan_exponent_part(exp_start);
            return Ok(TokenKind::FloatLit);
        }

        Ok(TokenKind::IntLit)
    }

    fn scan_leading_dot_float(&mut self, _start: u32) -> LexResult<TokenKind> {
        let _ = self.scan_digits_with_separators(|byte| byte.is_ascii_digit(), true);
        if matches!(self.cursor.peek(), Some('e' | 'E')) {
            let exp_start = self.cursor.pos();
            self.scan_exponent_part(exp_start);
        }
        Ok(TokenKind::FloatLit)
    }

    fn scan_string(&mut self, start: u32) -> LexResult<TokenKind> {
        self.skip_string_body(start)?;
        Ok(TokenKind::StringLit)
    }

    fn scan_fstring(&mut self, start: u32) -> LexResult<TokenKind> {
        let _ = self.cursor.advance();
        let mut parts = FStringParts::new();
        let mut literal_start = self.cursor.pos();

        loop {
            match self.cursor.peek() {
                None => {
                    return Err(LexError {
                        kind: LexErrorKind::UnterminatedFString,
                        span: self.cursor.span_from(start),
                    });
                }
                Some('"') => {
                    let literal_end = self.cursor.pos();
                    if literal_start < literal_end {
                        parts.push(FStringPart {
                            kind: FStringPartKind::Literal,
                            span: Span::new(literal_start, literal_end),
                        });
                    }
                    let _ = self.cursor.advance();
                    return Ok(TokenKind::FStringLit(parts));
                }
                Some('{') => {
                    let literal_end = self.cursor.pos();
                    if literal_start < literal_end {
                        parts.push(FStringPart {
                            kind: FStringPartKind::Literal,
                            span: Span::new(literal_start, literal_end),
                        });
                    }
                    let _ = self.cursor.advance();
                    let expr_start = self.cursor.pos();
                    let expr_span = self.scan_fstring_interpolation(start, expr_start)?;
                    parts.push(FStringPart {
                        kind: FStringPartKind::Interpolation,
                        span: expr_span,
                    });
                    literal_start = self.cursor.pos();
                }
                Some('\\') => {
                    let _ = self.cursor.advance();
                    let _ = self.scan_escape(start)?;
                }
                Some(_) => {
                    let _ = self.cursor.advance();
                }
            }
        }
    }

    fn scan_fstring_interpolation(
        &mut self,
        fstring_start: u32,
        expr_start: u32,
    ) -> LexResult<Span> {
        let mut depth = 1_u32;

        loop {
            match self.cursor.peek() {
                None => {
                    return Err(LexError {
                        kind: LexErrorKind::UnterminatedFStringInterpolation,
                        span: self.cursor.span_from(fstring_start),
                    });
                }
                Some('{') => {
                    let _ = self.cursor.advance();
                    depth += 1;
                }
                Some('}') => {
                    let expr_end = self.cursor.pos();
                    let _ = self.cursor.advance();
                    depth -= 1;
                    if depth == 0 {
                        return Ok(Span::new(expr_start, expr_end));
                    }
                }
                Some('"') => self.skip_string_body(fstring_start)?,
                Some('`') => self.skip_escaped_ident_body(fstring_start)?,
                Some('\'') => self.skip_rune_body(fstring_start)?,
                Some('f') if self.cursor.peek_next() == Some('"') => {
                    let nested_start = self.cursor.pos();
                    let _ = self.cursor.advance();
                    let _ = self.scan_fstring(nested_start)?;
                }
                Some('/') if self.cursor.peek_next() == Some('/') => self.skip_line_comment(),
                Some('/') if self.cursor.peek_next() == Some('*') => {
                    self.skip_block_comment(fstring_start)?;
                }
                Some(_) => {
                    let _ = self.cursor.advance();
                }
            }
        }
    }

    fn scan_rune(&mut self, start: u32) -> LexResult<TokenKind> {
        self.skip_rune_body(start)?;
        Ok(TokenKind::RuneLit)
    }

    fn scan_escaped_ident(&mut self, start: u32) -> LexResult<TokenKind> {
        self.skip_escaped_ident_body(start)?;
        Ok(TokenKind::EscapedIdent)
    }

    fn skip_string_body(&mut self, literal_start: u32) -> LexResult<()> {
        loop {
            match self.cursor.peek() {
                None => {
                    return Err(LexError {
                        kind: LexErrorKind::UnterminatedString,
                        span: self.cursor.span_from(literal_start),
                    });
                }
                Some('"') => {
                    let _ = self.cursor.advance();
                    return Ok(());
                }
                Some('\\') => {
                    let _ = self.cursor.advance();
                    let _ = self.scan_escape(literal_start)?;
                }
                Some(_) => {
                    let _ = self.cursor.advance();
                }
            }
        }
    }

    fn skip_rune_body(&mut self, literal_start: u32) -> LexResult<()> {
        let ch = match self.cursor.peek() {
            None | Some('\'') => {
                if self.cursor.peek() == Some('\'') {
                    let _ = self.cursor.advance();
                }
                return Err(LexError {
                    kind: LexErrorKind::EmptyRune,
                    span: self.cursor.span_from(literal_start),
                });
            }
            Some('\\') => {
                let _ = self.cursor.advance();
                let _ = self.scan_escape(literal_start)?;
                '\0'
            }
            Some(ch) => {
                let _ = self.cursor.advance();
                ch
            }
        };

        if ch != '\0' && self.cursor.peek() == Some('\'') {
            let _ = self.cursor.advance();
            return Ok(());
        }

        if ch == '\0' && self.cursor.peek() == Some('\'') {
            let _ = self.cursor.advance();
            return Ok(());
        }

        if self.cursor.is_eof() {
            return Err(LexError {
                kind: LexErrorKind::UnterminatedRune,
                span: self.cursor.span_from(literal_start),
            });
        }

        while let Some(next) = self.cursor.peek() {
            let _ = self.cursor.advance();
            if next == '\'' {
                return Err(LexError {
                    kind: LexErrorKind::MultiCharRune,
                    span: self.cursor.span_from(literal_start),
                });
            }
        }

        Err(LexError {
            kind: LexErrorKind::UnterminatedRune,
            span: self.cursor.span_from(literal_start),
        })
    }

    fn skip_escaped_ident_body(&mut self, literal_start: u32) -> LexResult<()> {
        loop {
            match self.cursor.peek() {
                None => {
                    return Err(LexError {
                        kind: LexErrorKind::UnterminatedEscapedIdent,
                        span: self.cursor.span_from(literal_start),
                    });
                }
                Some('`') => {
                    let _ = self.cursor.advance();
                    return Ok(());
                }
                Some(_) => {
                    let _ = self.cursor.advance();
                }
            }
        }
    }

    fn scan_escape(&mut self, literal_start: u32) -> LexResult<char> {
        let escape_start = self.cursor.pos().saturating_sub(1);
        match self.cursor.advance() {
            Some('n') => Ok('\n'),
            Some('t') => Ok('\t'),
            Some('r') => Ok('\r'),
            Some('\\') => Ok('\\'),
            Some('\'') => Ok('\''),
            Some('"') => Ok('"'),
            Some('0') => Ok('\0'),
            Some('x') => self.scan_hex_escape(2, literal_start),
            Some('u') => self.scan_unicode_escape(literal_start),
            Some(ch) => Err(LexError {
                kind: LexErrorKind::InvalidEscape(ch),
                span: Span::new(escape_start, self.cursor.pos()),
            }),
            None => Err(LexError {
                kind: LexErrorKind::UnterminatedString,
                span: self.cursor.span_from(literal_start),
            }),
        }
    }

    fn scan_hex_escape(&mut self, count: u8, literal_start: u32) -> LexResult<char> {
        let digits_start = self.cursor.pos();
        let mut value = 0_u32;

        for _ in 0..count {
            match self.cursor.peek() {
                Some(ch) if ch.is_ascii_hexdigit() => {
                    let _ = self.cursor.advance();
                    value = value * 16 + ch.to_digit(16).expect("hex digit already checked");
                }
                _ => {
                    return Err(LexError {
                        kind: LexErrorKind::ExpectedHexDigits { expected: count },
                        span: self.cursor.span_from(digits_start),
                    });
                }
            }
        }

        char::from_u32(value).ok_or_else(|| LexError {
            kind: LexErrorKind::InvalidUnicodeEscape,
            span: self.cursor.span_from(literal_start),
        })
    }

    fn scan_unicode_escape(&mut self, literal_start: u32) -> LexResult<char> {
        let start = self.cursor.pos();
        if !self.cursor.eat('{') {
            return Err(LexError {
                kind: LexErrorKind::InvalidUnicodeEscape,
                span: self.cursor.span_from(start),
            });
        }

        let mut value = 0_u32;
        let mut digits = 0_u8;
        loop {
            match self.cursor.peek() {
                Some('}') => {
                    let _ = self.cursor.advance();
                    break;
                }
                Some(ch) if ch.is_ascii_hexdigit() => {
                    let _ = self.cursor.advance();
                    value = value * 16 + ch.to_digit(16).expect("hex digit already checked");
                    digits += 1;
                    if digits > 6 {
                        return Err(LexError {
                            kind: LexErrorKind::InvalidUnicodeEscape,
                            span: self.cursor.span_from(start),
                        });
                    }
                }
                _ => {
                    return Err(LexError {
                        kind: LexErrorKind::InvalidUnicodeEscape,
                        span: self.cursor.span_from(start),
                    });
                }
            }
        }

        if digits == 0 {
            return Err(LexError {
                kind: LexErrorKind::InvalidUnicodeEscape,
                span: self.cursor.span_from(start),
            });
        }

        char::from_u32(value).ok_or_else(|| LexError {
            kind: LexErrorKind::InvalidUnicodeEscape,
            span: self.cursor.span_from(literal_start),
        })
    }

    fn collect_leading_trivia(&mut self) -> (Trivias, Option<LexError>) {
        let mut trivia = SmallVec::new();

        loop {
            match self.cursor.peek() {
                Some(' ' | '\t' | '\r') => trivia.push(self.scan_whitespace()),
                Some('\n') => {
                    let start = self.cursor.pos();
                    let _ = self.cursor.advance();
                    trivia.push(Trivia {
                        kind: TriviaKind::Newline,
                        span: Span::new(start, start + 1),
                    });
                }
                Some('/') if self.cursor.peek_next() == Some('/') => {
                    trivia.push(self.scan_line_comment());
                }
                Some('/') if self.cursor.peek_next() == Some('*') => {
                    match self.scan_block_comment(self.cursor.pos()) {
                        Ok(comment) => trivia.push(comment),
                        Err(error) => return (trivia, Some(error)),
                    }
                }
                _ => return (trivia, None),
            }
        }
    }

    fn collect_trailing_trivia(&mut self) -> (Trivias, Option<LexError>) {
        let mut trivia = SmallVec::new();

        loop {
            match self.cursor.peek() {
                Some(' ' | '\t' | '\r') => trivia.push(self.scan_whitespace()),
                Some('/') if self.cursor.peek_next() == Some('/') => {
                    trivia.push(self.scan_line_comment());
                    return (trivia, None);
                }
                Some('/') if self.cursor.peek_next() == Some('*') => {
                    match self.scan_block_comment(self.cursor.pos()) {
                        Ok(comment) => trivia.push(comment),
                        Err(error) => return (trivia, Some(error)),
                    }
                }
                _ => return (trivia, None),
            }
        }
    }

    fn scan_whitespace(&mut self) -> Trivia {
        let start = self.cursor.pos();
        let _ = self
            .cursor
            .eat_while_ascii(|byte| matches!(byte, b' ' | b'\t' | b'\r'));
        Trivia {
            kind: TriviaKind::Whitespace,
            span: self.cursor.span_from(start),
        }
    }

    fn scan_line_comment(&mut self) -> Trivia {
        let start = self.cursor.pos();
        let _ = self.cursor.advance();
        let _ = self.cursor.advance();
        let doc = self.cursor.peek() == Some('/');
        if doc {
            let _ = self.cursor.advance();
        }
        let _ = self.cursor.eat_while(|ch| ch != '\n');
        Trivia {
            kind: TriviaKind::LineComment { doc },
            span: self.cursor.span_from(start),
        }
    }

    fn skip_line_comment(&mut self) {
        let _ = self.cursor.advance();
        let _ = self.cursor.advance();
        let _ = self.cursor.eat_while(|ch| ch != '\n');
    }

    fn scan_block_comment(&mut self, start: u32) -> LexResult<Trivia> {
        let doc = self.cursor.remaining_bytes().get(2) == Some(&b'*');
        self.skip_block_comment(start)?;
        Ok(Trivia {
            kind: TriviaKind::BlockComment { doc },
            span: self.cursor.span_from(start),
        })
    }

    fn skip_block_comment(&mut self, start: u32) -> LexResult<()> {
        let _ = self.cursor.advance();
        let _ = self.cursor.advance();
        let mut depth = 1_u32;
        loop {
            match self.cursor.peek() {
                None => {
                    return Err(LexError {
                        kind: LexErrorKind::UnterminatedBlockComment,
                        span: self.cursor.span_from(start),
                    });
                }
                Some('/') if self.cursor.peek_next() == Some('*') => {
                    let _ = self.cursor.advance();
                    let _ = self.cursor.advance();
                    depth += 1;
                }
                Some('*') if self.cursor.peek_next() == Some('/') => {
                    let _ = self.cursor.advance();
                    let _ = self.cursor.advance();
                    depth -= 1;
                    if depth == 0 {
                        return Ok(());
                    }
                }
                Some(_) => {
                    let _ = self.cursor.advance();
                }
            }
        }
    }
}

impl Iterator for Lexer<'_> {
    type Item = LexResult<Token>;

    fn next(&mut self) -> Option<Self::Item> {
        if let Some(error) = self.pending_errors.pop_front() {
            return Some(Err(error));
        }

        if self.finished {
            return None;
        }

        let (leading_trivia, trivia_error) = self.collect_leading_trivia();
        if let Some(error) = trivia_error {
            return Some(Err(error));
        }

        if self.cursor.is_eof() {
            self.finished = true;
            let eof_span = self.cursor.span_from(self.cursor.pos());
            return Some(Ok(Token {
                kind: TokenKind::Eof,
                span: eof_span,
                leading_trivia,
                trailing_trivia: SmallVec::new(),
            }));
        }

        let start = self.cursor.pos();
        match self.scan_token_kind() {
            Ok(kind) => {
                let span = self.cursor.span_from(start);
                let (trailing_trivia, trailing_error) = self.collect_trailing_trivia();
                if let Some(error) = trailing_error {
                    self.pending_errors.push_back(error);
                }
                Some(Ok(Token {
                    kind,
                    span,
                    leading_trivia,
                    trailing_trivia,
                }))
            }
            Err(error) => Some(Err(error)),
        }
    }
}

#[cfg(test)]
#[allow(clippy::unwrap_used, clippy::panic)]
mod tests;
