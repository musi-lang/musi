//! Hand-written, single-pass lexer for the Musi language.

use core::str;

use musi_shared::{DiagnosticBag, FileId, Interner, Span, Symbol};

use crate::token::{Token, TokenKind, keyword_from_str};

/// A single-pass lexer that converts a source byte slice into [`Token`]s.
///
/// Implements [`Iterator`] — each call to [`next`](Iterator::next) produces the
/// next token.  After [`TokenKind::Eof`] is emitted the iterator is exhausted.
pub struct Lexer<'src> {
    source: &'src [u8],
    pos: usize,
    file_id: FileId,
    interner: &'src mut Interner,
    diags: &'src mut DiagnosticBag,
    done: bool,
}

impl<'src> Lexer<'src> {
    /// Creates a new lexer over `source`.
    #[must_use]
    pub const fn new(
        source: &'src str,
        file_id: FileId,
        interner: &'src mut Interner,
        diags: &'src mut DiagnosticBag,
    ) -> Self {
        Self {
            source: source.as_bytes(),
            pos: 0,
            file_id,
            interner,
            diags,
            done: false,
        }
    }
}

impl Iterator for Lexer<'_> {
    type Item = Token;

    fn next(&mut self) -> Option<Token> {
        if self.done {
            return None;
        }
        let tok = self.next_token();
        if tok.kind == TokenKind::Eof {
            self.done = true;
        }
        Some(tok)
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        let remaining = self.source.len().saturating_sub(self.pos);
        // Upper bound: at most one token per source byte (single-char tokens), plus 1 for EOF.
        let upper = remaining + 1;
        (0, Some(upper))
    }
}

// ── Helpers ────────────────────────────────────────────────────────────────

impl Lexer<'_> {
    #[must_use]
    fn peek(&self) -> Option<u8> {
        self.source.get(self.pos).copied()
    }

    #[must_use]
    fn peek_at(&self, offset: usize) -> Option<u8> {
        self.source.get(self.pos + offset).copied()
    }

    fn advance(&mut self) -> u8 {
        let b = self.source[self.pos];
        self.pos += 1;
        b
    }

    #[must_use]
    fn current_span(&self, start: usize) -> Span {
        let s = u32::try_from(start).expect("source larger than 4 GiB");
        let len = u32::try_from(self.pos - start).expect("source larger than 4 GiB");
        Span::new(s, len)
    }

    fn intern_range(&mut self, start: usize, end: usize) -> Symbol {
        let slice = &self.source[start..end];
        let text = str::from_utf8(slice).unwrap_or("");
        self.interner.intern(text)
    }

    fn emit(&self, kind: TokenKind, start: usize) -> Token {
        Token::new(kind, self.current_span(start), None)
    }

    fn emit_sym(&self, kind: TokenKind, start: usize, sym: Symbol) -> Token {
        Token::new(kind, self.current_span(start), Some(sym))
    }

    fn error_token(&mut self, msg: &str, start: usize) -> Token {
        let span = self.current_span(start);
        let _diag = self.diags.error(msg, span, self.file_id);
        Token::new(TokenKind::Error, span, None)
    }

    /// Interns `start..self.pos` and emits a token with that symbol.
    fn emit_interned(&mut self, kind: TokenKind, start: usize) -> Token {
        let sym = self.intern_range(start, self.pos);
        self.emit_sym(kind, start, sym)
    }

    /// Advances past one already-known leading byte, then checks whether the
    /// next byte equals `next_byte`.  If so, advances again and emits
    /// `two_kind`; otherwise emits `one_kind`.
    fn lex_maybe_two_char(
        &mut self,
        start: usize,
        next_byte: u8,
        two_kind: TokenKind,
        one_kind: TokenKind,
    ) -> Token {
        let _ = self.advance();
        if self.peek() == Some(next_byte) {
            let _ = self.advance();
            self.emit(two_kind, start)
        } else {
            self.emit(one_kind, start)
        }
    }

    /// Advances while the next byte is an ASCII alphanumeric character or `_`.
    fn consume_ident_chars(&mut self) {
        while self.peek().is_some_and(|b| b.is_ascii_alphanumeric() || b == b'_') {
            let _ = self.advance();
        }
    }

    /// Advances while the next byte is not a newline (stops before `\n`).
    fn consume_until_newline(&mut self) {
        while self.peek().is_some_and(|b| b != b'\n') {
            let _ = self.advance();
        }
    }

    /// Advances through up to `max` ASCII hex digits.
    fn consume_hex_digits(&mut self, max: usize) {
        for _ in 0..max {
            if self.peek().is_some_and(|b| b.is_ascii_hexdigit()) {
                let _ = self.advance();
            } else {
                break;
            }
        }
    }
}

// ── Core dispatch ──────────────────────────────────────────────────────────

impl Lexer<'_> {
    fn next_token(&mut self) -> Token {
        self.skip_whitespace();

        let start = self.pos;

        let Some(b) = self.peek() else {
            return self.emit(TokenKind::Eof, start);
        };

        match b {
            b'a'..=b'z' | b'A'..=b'Z' | b'_' => self.lex_word(start),
            b'`' => self.lex_escaped_ident(start),
            b'\'' => self.lex_tick(start),
            b'0'..=b'9' => self.lex_number(start),
            b'"' => self.lex_string(start),
            b'/' => self.lex_slash(start),
            b'.' => self.lex_dot(start),
            b'<' => self.lex_lt(start),
            b':' => self.lex_colon(start),
            b'-' => self.lex_minus(start),
            b'=' => self.lex_eq(start),
            b'>' => self.lex_gt(start),
            _ => self.lex_single(start),
        }
    }

    fn skip_whitespace(&mut self) {
        while self.peek().is_some_and(|b| b.is_ascii_whitespace()) {
            let _ = self.advance();
        }
    }
}

// ── Lexing methods ─────────────────────────────────────────────────────────

impl Lexer<'_> {
    fn lex_word(&mut self, start: usize) -> Token {
        self.consume_ident_chars();
        let text = str::from_utf8(&self.source[start..self.pos]).unwrap_or("");
        // A bare `_` is the wildcard token, not an identifier.
        if text == "_" {
            return self.emit(TokenKind::Underscore, start);
        }
        if let Some(kw) = keyword_from_str(text) {
            self.emit(kw, start)
        } else {
            self.emit_interned(TokenKind::Ident, start)
        }
    }

    fn lex_escaped_ident(&mut self, start: usize) -> Token {
        // skip opening backtick
        let _open = self.advance();
        let content_start = self.pos;

        loop {
            match self.peek() {
                Some(b'`') => {
                    let content_end = self.pos;
                    let _close = self.advance();
                    let sym = self.intern_range(content_start, content_end);
                    return self.emit_sym(TokenKind::Ident, start, sym);
                }
                Some(_) => {
                    let _ch = self.advance();
                }
                None => {
                    return self.error_token("unterminated escaped identifier", start);
                }
            }
        }
    }

    fn lex_tick(&mut self, start: usize) -> Token {
        // skip the leading '
        let _tick = self.advance();

        // Try to determine if this is a char literal or a type ident.
        // Char literal: 'x' or '\n' (escape_char followed by ')
        // Type ident:   'a, 'key (letter followed by alphanumeric, no closing ')

        match self.peek() {
            Some(b'\\') => {
                // Must be a char literal with an escape sequence
                self.lex_char_lit_escape(start)
            }
            Some(c) if c.is_ascii_alphanumeric() || c == b' ' || is_symbol_char(c) => {
                // Could be 'x' (char lit) or 'a (type ident).
                // Check: if next char is a letter/digit and the char after that is '
                // then it's a char literal. Otherwise, if it starts with a letter,
                // it's a type ident.
                if self.peek_at(1) == Some(b'\'') {
                    // 'x' — single char literal
                    let _ch = self.advance();
                    let _close = self.advance();
                    self.emit_interned(TokenKind::CharLit, start)
                } else if c.is_ascii_alphabetic() {
                    // type ident: 'a, 'key, etc.
                    self.lex_ty_ident(start)
                } else {
                    // Single non-letter char not followed by ' — error
                    let _ch = self.advance();
                    self.error_token("expected closing quote for character literal", start)
                }
            }
            _ => {
                // bare ' with nothing useful after it
                self.error_token("unexpected character after quote", start)
            }
        }
    }

    fn lex_char_lit_escape(&mut self, start: usize) -> Token {
        // We've consumed the opening '. Current byte is '\'.
        let _backslash = self.advance(); // consume '\'

        // consume the escaped char
        match self.peek() {
            Some(_) => {
                let _esc = self.advance();
            }
            None => {
                return self.error_token("unterminated character literal", start);
            }
        }

        // expect closing '
        match self.peek() {
            Some(b'\'') => {
                let _close = self.advance();
                self.emit_interned(TokenKind::CharLit, start)
            }
            _ => self.error_token("expected closing quote for character literal", start),
        }
    }

    fn lex_ty_ident(&mut self, start: usize) -> Token {
        // We've consumed the opening '. The current byte is the first letter.
        // Consume letter followed by [A-Za-z0-9_]*
        self.consume_ident_chars();
        self.emit_interned(TokenKind::TyIdent, start)
    }

    fn lex_number(&mut self, start: usize) -> Token {
        let first = self.advance();

        if first == b'0' {
            match self.peek() {
                Some(b'x') => return self.lex_number_prefixed(start, is_hex_digit),
                Some(b'o') => return self.lex_number_prefixed(start, is_octal_digit),
                Some(b'b') => return self.lex_number_prefixed(start, is_binary_digit),
                _ => {}
            }
        }

        // Decimal: consume digits and underscores
        self.consume_digits_with_sep(is_decimal_digit);

        // Check for a fractional part: '.' followed by a digit
        if self.peek() == Some(b'.') && self.peek_at(1).is_some_and(|c| c.is_ascii_digit()) {
            let _dot = self.advance();
            // Must have at least one digit (already checked above)
            self.consume_digits_with_sep(is_decimal_digit);
            return self.emit_interned(TokenKind::FloatLit, start);
        }

        self.emit_interned(TokenKind::IntLit, start)
    }

    fn lex_number_prefixed(&mut self, start: usize, is_digit: fn(u8) -> bool) -> Token {
        // consume the prefix character ('x', 'o', or 'b')
        let _prefix = self.advance();
        self.consume_digits_with_sep(is_digit);
        self.emit_interned(TokenKind::IntLit, start)
    }

    fn consume_digits_with_sep(&mut self, is_digit: fn(u8) -> bool) {
        while let Some(b) = self.peek() {
            if is_digit(b) || b == b'_' {
                let _d = self.advance();
            } else {
                break;
            }
        }
    }

    fn lex_string(&mut self, start: usize) -> Token {
        // skip opening quote
        let _open = self.advance();

        loop {
            match self.peek() {
                Some(b'"') => {
                    let _close = self.advance();
                    return self.emit_interned(TokenKind::StringLit, start);
                }
                Some(b'\\') => {
                    let _backslash = self.advance();
                    match self.peek() {
                        Some(b'x') => {
                            let _x = self.advance();
                            // consume up to 2 hex digits
                            self.consume_hex_digits(2);
                        }
                        Some(b'u') => {
                            let _u = self.advance();
                            // expect '{' then up to 6 hex digits then '}'
                            if self.peek() == Some(b'{') {
                                let _lb = self.advance();
                                self.consume_hex_digits(6);
                                if self.peek() == Some(b'}') {
                                    let _rb = self.advance();
                                }
                            }
                        }
                        Some(b'\\' | b'"' | b'n' | b't' | b'r' | b'0') => {
                            let _esc = self.advance();
                        }
                        Some(_) => {
                            let err_start = self.pos - 1; // backslash pos
                            let _ch = self.advance();
                            let span = Span::new(
                                u32::try_from(err_start).expect("source larger than 4 GiB"),
                                2,
                            );
                            let _diag =
                                self.diags
                                    .error("unknown escape sequence", span, self.file_id);
                            // continue lexing the string
                        }
                        None => {
                            return self.error_token("unterminated string literal", start);
                        }
                    }
                }
                Some(_) => {
                    let _ch = self.advance();
                }
                None => {
                    return self.error_token("unterminated string literal", start);
                }
            }
        }
    }

    fn lex_slash(&mut self, start: usize) -> Token {
        let _slash = self.advance();

        match self.peek() {
            Some(b'/') => {
                // line comment — check for doc comment (///)
                let _slash2 = self.advance();

                if self.peek() == Some(b'/') {
                    // doc comment: ///
                    let _slash3 = self.advance();
                    let content_start = self.pos;

                    // consume to end of line
                    self.consume_until_newline();

                    let sym = self.intern_range(content_start, self.pos);
                    return self.emit_sym(TokenKind::DocComment, start, sym);
                }

                // regular line comment — skip to end of line
                self.consume_until_newline();

                // tail-recurse: skip the comment and return the next real token
                self.next_token()
            }
            Some(b'=') => {
                let _eq = self.advance();
                self.emit(TokenKind::SlashEq, start)
            }
            _ => self.emit(TokenKind::Slash, start),
        }
    }

    fn lex_dot(&mut self, start: usize) -> Token {
        let _dot = self.advance();

        match self.peek() {
            Some(b'[') => {
                let _lb = self.advance();
                self.emit(TokenKind::DotLBracket, start)
            }
            Some(b'{') => {
                let _lb = self.advance();
                self.emit(TokenKind::DotLBrace, start)
            }
            Some(b'.') => {
                let _d2 = self.advance();
                if self.peek() == Some(b'<') {
                    let _lt = self.advance();
                    self.emit(TokenKind::DotDotLt, start)
                } else {
                    self.emit(TokenKind::DotDot, start)
                }
            }
            _ => self.emit(TokenKind::Dot, start),
        }
    }

    fn lex_lt(&mut self, start: usize) -> Token {
        let _lt = self.advance();

        match self.peek() {
            Some(b'.') if self.peek_at(1) == Some(b'.') => {
                let _d1 = self.advance();
                let _d2 = self.advance();
                self.emit(TokenKind::LtDotDot, start)
            }
            Some(b'=') => {
                let _eq = self.advance();
                self.emit(TokenKind::LtEq, start)
            }
            Some(b'-') => {
                let _m = self.advance();
                self.emit(TokenKind::LtMinus, start)
            }
            _ => self.emit(TokenKind::Lt, start),
        }
    }

    fn lex_colon(&mut self, start: usize) -> Token {
        let _c = self.advance();

        match self.peek() {
            Some(b':') => {
                let _c2 = self.advance();
                self.emit(TokenKind::ColonColon, start)
            }
            Some(b'=') => {
                let _eq = self.advance();
                self.emit(TokenKind::ColonEq, start)
            }
            _ => self.emit(TokenKind::Colon, start),
        }
    }

    fn lex_minus(&mut self, start: usize) -> Token {
        self.lex_maybe_two_char(start, b'>', TokenKind::MinusGt, TokenKind::Minus)
    }

    fn lex_eq(&mut self, start: usize) -> Token {
        self.lex_maybe_two_char(start, b'>', TokenKind::EqGt, TokenKind::Eq)
    }

    fn lex_gt(&mut self, start: usize) -> Token {
        self.lex_maybe_two_char(start, b'=', TokenKind::GtEq, TokenKind::Gt)
    }

    fn lex_single(&mut self, start: usize) -> Token {
        let b = self.advance();
        match b {
            b'(' => self.emit(TokenKind::LParen, start),
            b')' => self.emit(TokenKind::RParen, start),
            b'{' => self.emit(TokenKind::LBrace, start),
            b'}' => self.emit(TokenKind::RBrace, start),
            b'[' => self.emit(TokenKind::LBracket, start),
            b']' => self.emit(TokenKind::RBracket, start),
            b',' => self.emit(TokenKind::Comma, start),
            b';' => self.emit(TokenKind::Semi, start),
            b'+' => self.emit(TokenKind::Plus, start),
            b'*' => self.emit(TokenKind::Star, start),
            b'%' => self.emit(TokenKind::Percent, start),
            b'|' => self.emit(TokenKind::Pipe, start),
            b'&' => self.emit(TokenKind::Amp, start),
            b'^' => self.emit(TokenKind::Caret, start),
            b'~' => self.emit(TokenKind::Tilde, start),
            b'!' => self.emit(TokenKind::Bang, start),
            b'@' => self.emit(TokenKind::At, start),
            b'#' => self.emit(TokenKind::Hash, start),
            _ => self.error_token("unknown character", start),
        }
    }
}

// ── Free helper functions ──────────────────────────────────────────────────

#[must_use]
const fn is_hex_digit(b: u8) -> bool {
    b.is_ascii_hexdigit()
}

#[must_use]
const fn is_octal_digit(b: u8) -> bool {
    matches!(b, b'0'..=b'7')
}

#[must_use]
const fn is_binary_digit(b: u8) -> bool {
    b == b'0' || b == b'1'
}

#[must_use]
const fn is_decimal_digit(b: u8) -> bool {
    b.is_ascii_digit()
}

#[must_use]
const fn is_symbol_char(b: u8) -> bool {
    matches!(
        b,
        b' ' | b'%'
            | b'!'
            | b'@'
            | b'('
            | b')'
            | b'*'
            | b'+'
            | b','
            | b'-'
            | b'.'
            | b'/'
            | b':'
            | b';'
            | b'<'
            | b'='
            | b'>'
            | b'['
            | b']'
            | b'`'
            | b'{'
            | b'|'
            | b'}'
            | b'\''
            | b'"'
            | b'#'
    )
}

#[cfg(test)]
mod tests;
