use music_base::Span;

use crate::LexedSource;
use crate::cursor::Cursor;
use crate::errors::{LexError, LexErrorKind, LexErrorList};
use crate::token::{TOKEN_PATTERNS, Token, TokenKind};
use crate::trivia::{Trivia, TriviaKind, TriviaList, TriviaRange};

#[derive(Debug)]
pub struct Lexer<'src> {
    cursor: Cursor<'src>,
    template_stack: Vec<TemplateContext>,
}

#[derive(Debug, Clone, Copy)]
struct TemplateContext {
    brace_depth: u32,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum TemplateChunkEnd {
    Tail,
    NextExpr,
    Eof,
}

const fn is_sym_byte(b: u8) -> bool {
    matches!(b, b'+' | b'-' | b'*' | b'/' | b'%' | b'=' | b'<' | b'>')
}

const fn is_digit_for_base_byte(base: u32, b: u8) -> bool {
    match base {
        2 => matches!(b, b'0' | b'1'),
        8 => matches!(b, b'0'..=b'7'),
        10 => b.is_ascii_digit(),
        16 => b.is_ascii_hexdigit(),
        _ => false,
    }
}

impl<'src> Lexer<'src> {
    #[must_use]
    pub const fn new(text: &'src str) -> Self {
        Self {
            cursor: Cursor::new(text),
            template_stack: Vec::new(),
        }
    }

    #[must_use]
    pub fn lex(mut self) -> LexedSource {
        let mut out = LexedSource::new(
            self.cursor.text(),
            Vec::new(),
            Vec::new(),
            Vec::new(),
            Vec::new(),
        );
        let mut trivia_start = 0;

        while !self.cursor.is_eof() {
            if self.lex_trivia(&mut out.trivia, &mut out.errors) {
                continue;
            }

            let token_start = self.cursor.pos();
            let kind = self.lex_token_kind(&mut out.errors);
            let token_end = self.cursor.pos();

            let trivia_end = out.trivia.len();
            out.token_trivia
                .push(TriviaRange::new(trivia_start, trivia_end));
            trivia_start = trivia_end;

            out.tokens
                .push(Token::new(kind, Self::span(token_start, token_end)));
        }

        let eof_pos = self.cursor.pos();
        if !self.template_stack.is_empty() {
            let kind = LexErrorKind::UnterminatedTemplateLiteral;
            Self::push_error(&mut out.errors, kind, eof_pos, eof_pos);
        }
        let trivia_end = out.trivia.len();
        out.token_trivia
            .push(TriviaRange::new(trivia_start, trivia_end));
        out.tokens
            .push(Token::new(TokenKind::Eof, Self::span(eof_pos, eof_pos)));

        out
    }

    fn span(start: usize, end: usize) -> Span {
        let start = u32::try_from(start).unwrap_or(u32::MAX);
        let end = u32::try_from(end).unwrap_or(u32::MAX);
        Span::new(start, end)
    }

    fn push_error(errors: &mut LexErrorList, kind: LexErrorKind, start: usize, end: usize) {
        errors.push(LexError::new(kind, Self::span(start, end)));
    }

    fn lex_trivia(&mut self, trivia: &mut TriviaList, errors: &mut LexErrorList) -> bool {
        let start = self.cursor.pos();
        let Some(first) = self.cursor.peek_byte() else {
            return false;
        };

        let mut push = |kind: TriviaKind, end: usize| {
            trivia.push(Trivia::new(kind, Self::span(start, end)));
        };

        match first {
            b'\n' => {
                self.cursor.bump_bytes(1);
                push(TriviaKind::Newline, self.cursor.pos());
                return true;
            }
            b'/' if self.cursor.peek_byte_n(1) == Some(b'/') => {
                let doc = self.cursor.peek_byte_n(2) == Some(b'/');
                self.cursor.bump_bytes(if doc { 3 } else { 2 });
                let rest = self.cursor.slice();
                let nl = rest.find('\n');
                let cr = rest.find('\r');
                let end = match (nl, cr) {
                    (Some(a), Some(b)) => a.min(b),
                    (Some(a), None) => a,
                    (None, Some(b)) => b,
                    (None, None) => rest.len(),
                };
                self.cursor.bump_bytes(end);
                push(TriviaKind::LineComment { doc }, self.cursor.pos());
                return true;
            }
            b'/' if self.cursor.peek_byte_n(1) == Some(b'*') => {
                let doc = self.cursor.peek_byte_n(2) == Some(b'*');
                self.cursor.bump_bytes(if doc { 3 } else { 2 });
                let rest = self.cursor.slice();
                if let Some(i) = rest.find("*/") {
                    self.cursor.bump_bytes(i + 2);
                } else {
                    self.cursor.bump_bytes(rest.len());
                    let kind = LexErrorKind::UnterminatedBlockComment;
                    Self::push_error(errors, kind, start, self.cursor.pos());
                }
                push(TriviaKind::BlockComment { doc }, self.cursor.pos());
                return true;
            }
            b if b.is_ascii_whitespace() => {
                self.cursor.bump_bytes(1);
                self.cursor
                    .consume_while_byte(|b| b != b'\n' && b.is_ascii_whitespace());
                push(TriviaKind::Whitespace, self.cursor.pos());
                return true;
            }
            _ => {}
        }

        if matches!(self.cursor.peek_char(), Some(c) if c.is_whitespace()) {
            self.cursor.bump();
            self.cursor
                .consume_while(|c| c != '\n' && c.is_whitespace());
            push(TriviaKind::Whitespace, self.cursor.pos());
            return true;
        }

        false
    }

    fn lex_token_kind(&mut self, errors: &mut LexErrorList) -> TokenKind {
        if self
            .template_stack
            .last()
            .is_some_and(|ctx| ctx.brace_depth == 0)
            && self.cursor.peek_byte() == Some(b'}')
        {
            return self.lex_template_continuation(errors);
        }

        let Some(first) = self.cursor.peek_byte() else {
            return TokenKind::Eof;
        };
        let second = self.cursor.peek_byte_n(1);

        let mut kind = match (first, second) {
            (b'.', Some(b'0'..=b'9')) | (b'0'..=b'9', _) => return self.lex_number(errors),
            (b'(', _) => {
                if let Some(kind) = self.try_lex_op_ident() {
                    return kind;
                }
                TokenKind::Error
            }
            (b'A'..=b'Z' | b'a'..=b'z', _) => return self.lex_ident_or_keyword(),
            (b'`', _) => return self.lex_template_start(errors),
            (b'"', _) => return self.lex_string(errors),
            (b'\'', _) => return self.lex_rune(errors),
            _ => TokenKind::Error,
        };

        if kind != TokenKind::Error {
            self.update_template_brace_depth(kind);
            return kind;
        }

        if let Some(kind) = self.lex_fixed_token_kind() {
            self.update_template_brace_depth(kind);
            return kind;
        }

        if is_sym_byte(first) && matches!(second, Some(next) if is_sym_byte(next)) {
            self.cursor.consume_while_byte(is_sym_byte);
            kind = TokenKind::SymbolicOp;
            self.update_template_brace_depth(kind);
            return kind;
        }

        let start = self.cursor.pos();
        let Some(ch) = self.cursor.peek_char() else {
            return TokenKind::Eof;
        };
        self.cursor.bump();
        let kind = LexErrorKind::InvalidChar { ch };
        Self::push_error(errors, kind, start, self.cursor.pos());
        TokenKind::Error
    }

    fn update_template_brace_depth(&mut self, kind: TokenKind) {
        let Some(ctx) = self.template_stack.last_mut() else {
            return;
        };

        match kind {
            TokenKind::LBrace => {
                ctx.brace_depth = ctx.brace_depth.saturating_add(1);
            }
            TokenKind::RBrace => {
                if ctx.brace_depth > 0 {
                    ctx.brace_depth -= 1;
                }
            }
            _ => {}
        }
    }

    fn lex_fixed_token_kind(&mut self) -> Option<TokenKind> {
        let first = self.cursor.peek_byte()?;
        for (bytes, kind) in TOKEN_PATTERNS.iter().copied() {
            if bytes[0] != first {
                continue;
            }
            if bytes.len() == 1
                && is_sym_byte(first)
                && matches!(self.cursor.peek_byte_n(1), Some(next) if is_sym_byte(next))
            {
                return None;
            }
            if self.cursor.starts_with_bytes(bytes) {
                self.cursor.bump_bytes(bytes.len());
                return Some(kind);
            }
        }
        None
    }

    fn lex_ident_or_keyword(&mut self) -> TokenKind {
        let start = self.cursor.pos();
        self.cursor.bump_bytes(1);
        self.cursor
            .consume_while_byte(|b| b.is_ascii_alphanumeric() || b == b'_');
        let end = self.cursor.pos();
        let s = self.cursor.text().get(start..end).unwrap_or("");
        TokenKind::keyword_from_str(s).unwrap_or(TokenKind::Ident)
    }

    fn lex_number(&mut self, errors: &mut LexErrorList) -> TokenKind {
        if self.cursor.peek_byte() == Some(b'.') {
            self.cursor.bump_bytes(1);
            let _ = self.lex_digits_with_separators(errors, 10);
            self.lex_exp_part(errors);
            return TokenKind::Float;
        }

        if self.cursor.peek_byte() == Some(b'0') {
            let base = match self.cursor.peek_byte_n(1) {
                Some(b'x' | b'X') => 16,
                Some(b'o' | b'O') => 8,
                Some(b'b' | b'B') => 2,
                _ => 0,
            };
            if base != 0 {
                let prefix_start = self.cursor.pos();
                self.cursor.bump_bytes(2);
                let (saw_digits, saw_invalid_digit) = self.lex_digits_with_separators(errors, base);
                if !saw_digits && !saw_invalid_digit {
                    let kind = LexErrorKind::MissingDigitsAfterBasePrefix { base };
                    Self::push_error(errors, kind, prefix_start, self.cursor.pos());
                }
                return TokenKind::Int;
            }
        }

        let _ = self.lex_digits_with_separators(errors, 10);

        if self.cursor.peek_byte() == Some(b'.')
            && matches!(self.cursor.peek_byte_n(1), Some(b'0'..=b'9'))
        {
            self.cursor.bump_bytes(1);
            let _ = self.lex_digits_with_separators(errors, 10);
            self.lex_exp_part(errors);
            return TokenKind::Float;
        }

        if matches!(self.cursor.peek_char(), Some('e' | 'E')) {
            self.lex_exp_part(errors);
            return TokenKind::Float;
        }
        TokenKind::Int
    }

    fn lex_exp_part(&mut self, errors: &mut LexErrorList) {
        if !matches!(self.cursor.peek_char(), Some('e' | 'E')) {
            return;
        }
        let exp_start = self.cursor.pos();
        self.cursor.bump();
        if matches!(self.cursor.peek_char(), Some('+' | '-')) {
            self.cursor.bump();
        }
        let digit_start = self.cursor.pos();
        let (saw_digits, _saw_invalid_digit) = self.lex_digits_with_separators(errors, 10);
        if !saw_digits {
            let kind = LexErrorKind::MissingExponentDigits;
            Self::push_error(errors, kind, exp_start, digit_start);
        }
    }

    fn lex_digits_with_separators(&mut self, errors: &mut LexErrorList, base: u32) -> (bool, bool) {
        let mut saw_digit = false;
        let mut saw_invalid_digit = false;
        let mut prev_underscore = false;

        while let Some(b) = self.cursor.peek_byte() {
            match b {
                b if is_digit_for_base_byte(base, b) => {
                    saw_digit = true;
                    prev_underscore = false;
                    self.cursor.bump_bytes(1);
                }
                b'_' => {
                    let underscore_start = self.cursor.pos();
                    let underscore_ok = saw_digit && !prev_underscore;
                    self.cursor.bump_bytes(1);
                    prev_underscore = true;
                    if !matches!(self.cursor.peek_byte(), Some(next) if is_digit_for_base_byte(base, next))
                    {
                        let kind = LexErrorKind::MissingDigitAfterUnderscoreInNumberLiteral;
                        Self::push_error(errors, kind, underscore_start, self.cursor.pos());
                        break;
                    }
                    if !underscore_ok {
                        let kind = LexErrorKind::UnexpectedUnderscoreInNumberLiteral;
                        Self::push_error(errors, kind, underscore_start, self.cursor.pos());
                    }
                }
                b if base != 10 && b.is_ascii_alphanumeric() => {
                    let bad_start = self.cursor.pos();
                    self.cursor.bump_bytes(1);
                    let kind = LexErrorKind::InvalidDigitForBase {
                        base,
                        ch: char::from(b),
                    };
                    Self::push_error(errors, kind, bad_start, self.cursor.pos());
                    saw_invalid_digit = true;
                    prev_underscore = false;
                }
                _ => break,
            }
        }
        (saw_digit, saw_invalid_digit)
    }

    fn lex_escape_tail(
        &mut self,
        errors: &mut LexErrorList,
        escape_start: usize,
        closing_delim: u8,
    ) {
        let Some(code) = self.cursor.peek_byte() else {
            let kind = LexErrorKind::MissingEscapeCode;
            Self::push_error(errors, kind, escape_start, self.cursor.pos());
            return;
        };

        match code {
            b if b == closing_delim => self.cursor.bump_bytes(1),
            b'\\' | b'"' | b'\'' | b'`' | b'$' | b'n' | b'r' | b't' | b'0' => {
                self.cursor.bump_bytes(1);
            }
            b'x' => {
                self.cursor.bump_bytes(1);
                let _ = self.lex_hex_escape(
                    errors,
                    escape_start,
                    closing_delim,
                    2,
                    LexErrorKind::MissingHexDigitsInByteEscape,
                    |ch| LexErrorKind::InvalidHexDigitInByteEscape { ch },
                );
            }
            b'u' => {
                self.cursor.bump_bytes(1);
                let Some(mut value) = self.lex_hex_escape(
                    errors,
                    escape_start,
                    closing_delim,
                    4,
                    LexErrorKind::MissingHexDigitsInUnicodeEscape,
                    |ch| LexErrorKind::InvalidHexDigitInUnicodeEscape { ch },
                ) else {
                    return;
                };

                match (self.cursor.peek_byte(), self.cursor.peek_byte_n(1)) {
                    (Some(a), Some(b)) if a.is_ascii_hexdigit() && b.is_ascii_hexdigit() => {
                        for _ in 0..2 {
                            let b = self.cursor.peek_byte().unwrap();
                            value = (value << 4) | char::from(b).to_digit(16).unwrap_or(0);
                            self.cursor.bump_bytes(1);
                        }
                    }
                    (Some(a), _) if a.is_ascii_hexdigit() => {
                        self.cursor.bump_bytes(1);
                        let kind = LexErrorKind::ExpectedFourOrSixHexDigitsInUnicodeEscape;
                        Self::push_error(errors, kind, escape_start, self.cursor.pos());
                        return;
                    }
                    _ => {}
                }

                if char::from_u32(value).is_none() {
                    let kind = LexErrorKind::InvalidUnicodeScalar { value };
                    Self::push_error(errors, kind, escape_start, self.cursor.pos());
                }
            }
            _ => {
                let start = self.cursor.pos();
                let Some(ch) = self.cursor.peek_char() else {
                    Self::push_error(errors, LexErrorKind::MissingEscapeCode, escape_start, start);
                    return;
                };
                self.cursor.bump();
                let kind = LexErrorKind::UnexpectedEscape { ch };
                Self::push_error(errors, kind, escape_start, self.cursor.pos());
            }
        }
    }

    fn lex_hex_escape(
        &mut self,
        errors: &mut LexErrorList,
        escape_start: usize,
        closing_delim: u8,
        digits: usize,
        missing_kind: LexErrorKind,
        invalid_kind: impl FnOnce(char) -> LexErrorKind + Copy,
    ) -> Option<u32> {
        let mut value: u32 = 0;
        for _ in 0..digits {
            let Some(b) = self.cursor.peek_byte() else {
                Self::push_error(errors, missing_kind, escape_start, self.cursor.pos());
                return None;
            };
            if b == closing_delim {
                Self::push_error(errors, missing_kind, escape_start, self.cursor.pos());
                return None;
            }
            if !b.is_ascii_hexdigit() {
                let digit_start = self.cursor.pos();
                self.cursor.bump_bytes(1);
                let kind = invalid_kind(char::from(b));
                Self::push_error(errors, kind, digit_start, self.cursor.pos());
                return None;
            }
            value = (value << 4) | char::from(b).to_digit(16).unwrap_or(0);
            self.cursor.bump_bytes(1);
        }
        Some(value)
    }

    fn process_escape_sequence(&mut self, errors: &mut LexErrorList, delimiter: u8) {
        debug_assert_eq!(self.cursor.peek_byte(), Some(b'\\'));
        let escape_start = self.cursor.pos();
        self.cursor.bump_bytes(1);
        self.lex_escape_tail(errors, escape_start, delimiter);
    }

    fn lex_string(&mut self, errors: &mut LexErrorList) -> TokenKind {
        let start = self.cursor.pos();
        self.cursor.bump_bytes(1);
        loop {
            let rest = self.cursor.slice().as_bytes();
            let Some(i) = rest
                .iter()
                .position(|&b| matches!(b, b'"' | b'\\' | b'\n' | b'\r'))
            else {
                self.cursor.bump_bytes(rest.len());
                let err_kind = LexErrorKind::UnterminatedStringLiteral;
                Self::push_error(errors, err_kind, start, self.cursor.pos());
                return TokenKind::String;
            };
            self.cursor.bump_bytes(i);
            let b = self.cursor.peek_byte().unwrap();
            if matches!(b, b'\n' | b'\r') {
                let err_kind = LexErrorKind::UnterminatedStringLiteral;
                Self::push_error(errors, err_kind, start, self.cursor.pos());
                return TokenKind::String;
            }
            if b == b'"' {
                self.cursor.bump_bytes(1);
                return TokenKind::String;
            }
            debug_assert_eq!(b, b'\\');
            self.process_escape_sequence(errors, b'"');
        }
    }

    fn lex_rune(&mut self, errors: &mut LexErrorList) -> TokenKind {
        let start = self.cursor.pos();
        self.cursor.bump_bytes(1);
        if self.cursor.peek_char() == Some('\'') {
            self.cursor.bump_bytes(1);
            let kind = LexErrorKind::EmptyRuneLiteral;
            Self::push_error(errors, kind, start, self.cursor.pos());
            return TokenKind::Rune;
        }
        match self.cursor.peek_char() {
            Some('\\') => {
                self.process_escape_sequence(errors, b'\'');
            }
            Some('\'') | None => {}
            Some(_) => {
                self.cursor.bump();
            }
        }
        match self.cursor.peek_char() {
            Some('\'') => {
                self.cursor.bump_bytes(1);
                TokenKind::Rune
            }
            Some(_) => {
                let extra_start = self.cursor.pos();
                self.cursor.bump();
                let kind = LexErrorKind::RuneLiteralTooLong;
                Self::push_error(errors, kind, extra_start, self.cursor.pos());
                self.cursor
                    .consume_while(|c| c != '\'' && c != '\n' && c != '\r');
                if self.cursor.peek_char() == Some('\'') {
                    self.cursor.bump_bytes(1);
                } else {
                    let kind = LexErrorKind::UnterminatedRuneLiteral;
                    Self::push_error(errors, kind, start, self.cursor.pos());
                }
                TokenKind::Rune
            }
            None => {
                let kind = LexErrorKind::UnterminatedRuneLiteral;
                Self::push_error(errors, kind, start, self.cursor.pos());
                TokenKind::Rune
            }
        }
    }

    fn consume_template_chunk(&mut self, errors: &mut LexErrorList) -> TemplateChunkEnd {
        loop {
            let rest = self.cursor.slice().as_bytes();
            let Some(i) = rest
                .iter()
                .position(|&b| b == b'`' || b == b'\\' || b == b'$')
            else {
                self.cursor.bump_bytes(rest.len());
                return TemplateChunkEnd::Eof;
            };

            self.cursor.bump_bytes(i);
            let b = self.cursor.peek_byte().unwrap();
            match b {
                b'`' => {
                    self.cursor.bump_bytes(1);
                    return TemplateChunkEnd::Tail;
                }
                b'\\' => {
                    self.process_escape_sequence(errors, b'`');
                }
                b'$' => {
                    if self.cursor.peek_byte_n(1) == Some(b'{') {
                        self.cursor.bump_bytes(2);
                        return TemplateChunkEnd::NextExpr;
                    }
                    self.cursor.bump_bytes(1);
                }
                _ => {
                    debug_assert!(matches!(b, b'`' | b'\\' | b'$'));
                    self.cursor.bump_bytes(1);
                }
            }
        }
    }

    fn lex_template_start(&mut self, errors: &mut LexErrorList) -> TokenKind {
        debug_assert_eq!(self.cursor.peek_byte(), Some(b'`'));
        let start = self.cursor.pos();
        self.cursor.bump_bytes(1);
        match self.consume_template_chunk(errors) {
            TemplateChunkEnd::Tail => TokenKind::TemplateNoSubst,
            TemplateChunkEnd::NextExpr => {
                self.template_stack.push(TemplateContext { brace_depth: 0 });
                TokenKind::TemplateHead
            }
            TemplateChunkEnd::Eof => {
                let kind = LexErrorKind::UnterminatedTemplateLiteral;
                Self::push_error(errors, kind, start, self.cursor.pos());
                TokenKind::TemplateNoSubst
            }
        }
    }

    fn lex_template_continuation(&mut self, errors: &mut LexErrorList) -> TokenKind {
        debug_assert_eq!(self.cursor.peek_byte(), Some(b'}'));
        let start = self.cursor.pos();
        self.cursor.bump_bytes(1);

        let Some(ctx) = self.template_stack.last() else {
            let kind = LexErrorKind::InvalidChar { ch: '}' };
            Self::push_error(errors, kind, start, self.cursor.pos());
            return TokenKind::Error;
        };
        debug_assert_eq!(ctx.brace_depth, 0);

        let end = self.consume_template_chunk(errors);
        match end {
            TemplateChunkEnd::Tail => {
                let _ = self.template_stack.pop();
                TokenKind::TemplateTail
            }
            TemplateChunkEnd::NextExpr => {
                if let Some(ctx) = self.template_stack.last_mut() {
                    ctx.brace_depth = 0;
                }
                TokenKind::TemplateMiddle
            }
            TemplateChunkEnd::Eof => {
                let _ = self.template_stack.pop();
                let kind = LexErrorKind::UnterminatedTemplateLiteral;
                Self::push_error(errors, kind, start, self.cursor.pos());
                TokenKind::TemplateTail
            }
        }
    }

    fn try_lex_op_ident(&mut self) -> Option<TokenKind> {
        let checkpoint = self.cursor.checkpoint();
        self.cursor.bump_bytes(1);

        let op_start = self.cursor.pos();
        self.cursor.consume_while_byte(is_sym_byte);
        let op_end = self.cursor.pos();
        if op_end == op_start {
            self.cursor.reset(checkpoint);
            return None;
        }

        if self.cursor.peek_byte() != Some(b')') {
            self.cursor.reset(checkpoint);
            return None;
        }

        if op_end - op_start >= 2 {
            let text = self.cursor.text().as_bytes();
            let op_bytes = &text[op_start..op_end];
            let is_reserved = TOKEN_PATTERNS
                .iter()
                .any(|(bytes, _)| bytes.len() >= 2 && *bytes == op_bytes);
            if is_reserved {
                self.cursor.reset(checkpoint);
                return None;
            }
        }

        self.cursor.bump_bytes(1);
        Some(TokenKind::OpIdent)
    }
}

#[cfg(test)]
#[allow(clippy::unwrap_used, clippy::panic)]
mod tests;
