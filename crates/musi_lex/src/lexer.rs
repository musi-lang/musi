//! Hand-written, single-pass lexer for the Musi language.

use core::str;

use musi_shared::{DiagnosticBag, FileId, Interner, Span, Symbol};

use crate::token::{Token, TokenKind, keyword_from_str};
use crate::trivia::{Trivia, TriviaKind, TriviaRange};

#[derive(Debug, Clone, Copy, thiserror::Error)]
enum LexError {
    #[error("unexpected character `{0}`")]
    UnexpectedCharacter(char),
    #[error("unterminated string literal")]
    UnterminatedString,
    #[error("unterminated escaped identifier")]
    UnterminatedEscapedIdent,
    #[error("unterminated rune literal")]
    UnterminatedRuneLit,
    #[error("unterminated f-string literal")]
    UnterminatedFString,
    #[error("unclosed block comment")]
    UnclosedBlockComment,
}

pub struct Lexer<'src> {
    source: &'src [u8],
    pos: usize,
    file_id: FileId,
    interner: &'src mut Interner,
    diags: &'src mut DiagnosticBag,
    done: bool,
    pub(crate) trivia: Vec<Trivia>,
    /// Stack tracking brace depth for each nested f-string.
    /// Non-empty ⇒ inside an f-string; value = open-brace nesting depth.
    fstring_depths: Vec<u32>,
}

impl<'src> Lexer<'src> {
    pub(crate) const fn new(
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
            trivia: vec![],
            fstring_depths: vec![],
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
        (0, Some(self.source.len().saturating_sub(self.pos) + 1))
    }
}

// ============================================================================
// COMBINATORS & EMIT HELPERS
// ============================================================================
impl Lexer<'_> {
    fn peek(&self) -> Option<u8> {
        self.source.get(self.pos).copied()
    }
    fn peek_at(&self, n: usize) -> Option<u8> {
        self.source.get(self.pos + n).copied()
    }
    fn at(&self, b: u8) -> bool {
        self.peek() == Some(b)
    }

    fn advance(&mut self) -> u8 {
        let b = self.source[self.pos];
        self.pos += 1;
        b
    }

    const fn advance_by(&mut self, n: usize) {
        self.pos += n;
    }

    fn eat_while(&mut self, pred: fn(u8) -> bool) {
        while self.peek().is_some_and(pred) {
            let _ = self.advance();
        }
    }

    fn expect(&mut self, byte: u8) -> bool {
        if self.at(byte) {
            let _ = self.advance();
            true
        } else {
            false
        }
    }

    fn expect_if(&mut self, pred: fn(u8) -> bool) -> bool {
        if self.peek().is_some_and(pred) {
            let _ = self.advance();
            true
        } else {
            false
        }
    }

    fn span_from(&self, start: usize) -> Span {
        let s = u32::try_from(start).expect("source < 4 GiB");
        let len = u32::try_from(self.pos - start).expect("source < 4 GiB");
        Span::new(s, len)
    }

    fn intern_range(&mut self, start: usize, end: usize) -> Symbol {
        self.interner
            .intern(str::from_utf8(&self.source[start..end]).unwrap_or(""))
    }

    fn emit(&self, kind: TokenKind, start: usize) -> Token {
        Token::new(kind, self.span_from(start), None)
    }

    fn emit_sym(&self, kind: TokenKind, start: usize, sym: Symbol) -> Token {
        Token::new(kind, self.span_from(start), Some(sym))
    }

    fn emit_interned(&mut self, kind: TokenKind, start: usize) -> Token {
        let sym = self.intern_range(start, self.pos);
        self.emit_sym(kind, start, sym)
    }

    fn error_token(&mut self, error: LexError, start: usize) -> Token {
        let span = self.span_from(start);
        let _diag = self.diags.error(error.to_string(), span, self.file_id);
        Token::new(TokenKind::Error, span, None)
    }

    fn trivia_range_from(&self, start: u32) -> TriviaRange {
        let n = u16::try_from(self.trivia.len() - usize::try_from(start).expect("fits"))
            .expect("trivia < 65536");
        TriviaRange { start, len: n }
    }
}

// ============================================================================
// TRIVIA COLLECTORS
// ============================================================================
impl Lexer<'_> {
    fn at_line_comment(&self) -> bool {
        self.at(b'/') && self.peek_at(1) == Some(b'/')
    }
    fn at_block_comment(&self) -> bool {
        self.at(b'/') && self.peek_at(1) == Some(b'*')
    }

    fn collect_line_comment(&mut self) {
        let start = self.pos;
        self.advance_by(2);
        let doc_style = self.at(b'/');
        if doc_style {
            let _ = self.advance();
        }
        self.eat_while(|b| b != b'\n');
        self.trivia.push(Trivia {
            kind: TriviaKind::LineComment { doc_style },
            span: self.span_from(start),
        });
    }

    fn collect_block_comment(&mut self) {
        let start = self.pos;
        self.advance_by(2);
        let doc_style = self.at(b'*') && self.peek_at(1) != Some(b'/');
        if doc_style {
            let _ = self.advance();
        }
        loop {
            match self.peek() {
                Some(b'*') if self.peek_at(1) == Some(b'/') => {
                    self.advance_by(2);
                    break;
                }
                Some(_) => {
                    let _ = self.advance();
                }
                None => {
                    let span = self.span_from(start);
                    let _d = self.diags.error(
                        LexError::UnclosedBlockComment.to_string(),
                        span,
                        self.file_id,
                    );
                    break;
                }
            }
        }
        self.trivia.push(Trivia {
            kind: TriviaKind::BlockComment { doc_style },
            span: self.span_from(start),
        });
    }

    fn collect_whitespace(&mut self) {
        loop {
            let start = self.pos;
            match self.peek() {
                Some(b'\n') => {
                    let _ = self.advance();
                    self.trivia.push(Trivia {
                        kind: TriviaKind::Newline,
                        span: self.span_from(start),
                    });
                }
                Some(b) if b.is_ascii_whitespace() => {
                    self.eat_while(|b| b.is_ascii_whitespace() && b != b'\n');
                    self.trivia.push(Trivia {
                        kind: TriviaKind::Whitespace,
                        span: self.span_from(start),
                    });
                }
                _ => break,
            }
        }
    }

    fn collect_leading_trivia(&mut self) -> TriviaRange {
        let start = u32::try_from(self.trivia.len()).expect("trivia < 2^32");
        loop {
            self.collect_whitespace();
            if self.at_line_comment() {
                self.collect_line_comment();
            } else if self.at_block_comment() {
                self.collect_block_comment();
            } else {
                break;
            }
        }
        self.trivia_range_from(start)
    }

    fn collect_trailing_trivia(&mut self) -> TriviaRange {
        let start = u32::try_from(self.trivia.len()).expect("trivia < 2^32");
        loop {
            let ts = self.pos;
            match self.peek() {
                Some(b) if b.is_ascii_whitespace() && b != b'\n' => {
                    self.eat_while(|b| b.is_ascii_whitespace() && b != b'\n');
                    self.trivia.push(Trivia {
                        kind: TriviaKind::Whitespace,
                        span: self.span_from(ts),
                    });
                }
                Some(b'/') if self.at_line_comment() => {
                    self.collect_line_comment();
                    break;
                }
                Some(b'/') if self.at_block_comment() => {
                    self.collect_block_comment();
                    break;
                }
                _ => break,
            }
        }
        self.trivia_range_from(start)
    }
}

// ============================================================================
// MAIN DISPATCH
// ============================================================================
impl Lexer<'_> {
    fn next_token(&mut self) -> Token {
        if self.fstring_depths.last() == Some(&0) && self.at(b'}') {
            let _ = self.advance();
            return self.scan_fstring_text(
                self.pos - 1,
                TokenKind::FStringMiddle,
                TokenKind::FStringTail,
            );
        }
        let leading = self.collect_leading_trivia();
        let start = self.pos;
        let Some(b) = self.peek() else {
            let mut tok = self.emit(TokenKind::Eof, start);
            tok.leading_trivia = leading;
            return tok;
        };
        let mut tok = match b {
            b'a'..=b'z' | b'A'..=b'Z' | b'_' => self.lex_ident_or_keyword(start),
            b'`' => self.lex_escaped_ident(start),
            b'\'' => self.lex_rune_or_ty_var(start),
            b'0'..=b'9' => self.lex_number(start),
            b'"' => self.lex_string(start),
            b'{' => self.lex_lbrace(start),
            b'}' => self.lex_rbrace(start),
            _ => self.lex_punct(start),
        };
        tok.leading_trivia = leading;
        tok.trailing_trivia = self.collect_trailing_trivia();
        tok
    }
}

// ============================================================================
// TOKEN PRODUCERS
// ============================================================================
impl Lexer<'_> {
    fn lex_ident_or_keyword(&mut self, start: usize) -> Token {
        if self.at(b'f') && self.peek_at(1) == Some(b'"') {
            let _ = self.advance();
            return self.lex_fstring_head(start);
        }
        self.eat_while(is_ident_char);
        let text = str::from_utf8(&self.source[start..self.pos]).unwrap_or("");
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
        let _ = self.advance();
        let content_start = self.pos;
        loop {
            match self.peek() {
                Some(b'`') => {
                    let content_end = self.pos;
                    let _ = self.advance();
                    let sym = self.intern_range(content_start, content_end);
                    return self.emit_sym(TokenKind::Ident, start, sym);
                }
                Some(_) => {
                    let _ = self.advance();
                }
                None => return self.error_token(LexError::UnterminatedEscapedIdent, start),
            }
        }
    }

    /// Tick (`'`) starts either a rune literal (`'x'`) or type variable (`'a`).
    fn lex_rune_or_ty_var(&mut self, start: usize) -> Token {
        let _ = self.advance();
        match self.peek() {
            Some(b'\\') => {
                let _ = self.advance();
                if self.peek().is_none() {
                    return self.error_token(LexError::UnterminatedRuneLit, start);
                }
                let _ = self.advance();
                if self.expect(b'\'') {
                    self.emit_interned(TokenKind::RuneLit, start)
                } else {
                    self.error_token(LexError::UnterminatedRuneLit, start)
                }
            }
            None => self.error_token(LexError::UnterminatedRuneLit, start),
            Some(c) => {
                if self.peek_at(1) == Some(b'\'') {
                    self.advance_by(2);
                    self.emit_interned(TokenKind::RuneLit, start)
                } else if c.is_ascii_alphabetic() {
                    self.eat_while(is_ident_char);
                    self.emit_interned(TokenKind::TyIdent, start)
                } else {
                    let _ = self.advance();
                    self.error_token(LexError::UnterminatedRuneLit, start)
                }
            }
        }
    }

    fn lex_number(&mut self, start: usize) -> Token {
        let first = self.advance();
        if first == b'0'
            && let Some(prefix @ (b'x' | b'o' | b'b')) = self.peek()
        {
            let _ = self.advance();
            match prefix {
                b'x' => self.eat_digits(is_hex_digit),
                b'o' => self.eat_digits(is_octal_digit),
                _ => self.eat_digits(is_binary_digit),
            }
            return self.emit_interned(TokenKind::IntLit, start);
        }
        self.eat_digits(is_decimal_digit);
        if self.at(b'.') && self.peek_at(1).is_some_and(|c| c.is_ascii_digit()) {
            let _ = self.advance();
            self.eat_digits(is_decimal_digit);
            let _ = self.eat_exponent();
            return self.emit_interned(TokenKind::FloatLit, start);
        }
        if self.eat_exponent() {
            return self.emit_interned(TokenKind::FloatLit, start);
        }
        self.emit_interned(TokenKind::IntLit, start)
    }

    fn eat_digits(&mut self, pred: fn(u8) -> bool) {
        while self.peek().is_some_and(|b| pred(b) || b == b'_') {
            let _ = self.advance();
        }
    }

    fn eat_exponent(&mut self) -> bool {
        if !self.expect_if(is_exponent) {
            return false;
        }
        let _ = self.expect_if(is_sign);
        self.eat_digits(is_decimal_digit);
        true
    }

    fn lex_string(&mut self, start: usize) -> Token {
        let _ = self.advance();
        loop {
            match self.peek() {
                Some(b'"') => {
                    let _ = self.advance();
                    return self.emit_interned(TokenKind::StringLit, start);
                }
                Some(b'\\') => self.skip_escape(),
                Some(_) => {
                    let _ = self.advance();
                }
                None => return self.error_token(LexError::UnterminatedString, start),
            }
        }
    }

    /// Advance past a backslash and the character following it.
    /// Per spec, `str_char = "\\" , any` — all escapes are valid at the lexer level.
    fn skip_escape(&mut self) {
        let _ = self.advance();
        if self.peek().is_some() {
            let _ = self.advance();
        }
    }

    fn lex_punct(&mut self, start: usize) -> Token {
        let b0 = self.source[self.pos];
        let b1 = self.peek_at(1);
        let b2 = self.peek_at(2);
        let (kind, len) = match (b0, b1, b2) {
            (b'.', Some(b'.'), Some(b'.')) => (TokenKind::DotDotDot, 3),
            (b'.', Some(b'.'), Some(b'<')) => (TokenKind::DotDotLt, 3),
            (b'.', Some(b'.'), _) => (TokenKind::DotDot, 2),
            (b'.', Some(b'['), _) => (TokenKind::DotLBracket, 2),
            (b'.', Some(b'{'), _) => (TokenKind::DotLBrace, 2),
            (b':', Some(b':'), _) => (TokenKind::ColonColon, 2),
            (b':', Some(b'='), _) => (TokenKind::ColonEq, 2),
            (b':', Some(b'>'), _) => (TokenKind::ColonGt, 2),
            (b'<', Some(b'-'), _) => (TokenKind::LtDash, 2),
            (b'-', Some(b'>'), _) => (TokenKind::DashGt, 2),
            (b'~', Some(b'>'), _) => (TokenKind::TildeGt, 2),
            (b'<', Some(b':'), _) => (TokenKind::LtColon, 2),
            (b'?', Some(b'.'), _) => (TokenKind::QuestionDot, 2),
            (b'?', Some(b'?'), _) => (TokenKind::QuestionQuestion, 2),
            (b'<', Some(b'<'), _) => (TokenKind::LtLt, 2),
            (b'>', Some(b'>'), _) => (TokenKind::GtGt, 2),
            (b'/', Some(b'='), _) => (TokenKind::SlashEq, 2),
            (b'<', Some(b'='), _) => (TokenKind::LtEq, 2),
            (b'>', Some(b'='), _) => (TokenKind::GtEq, 2),
            (b'|', Some(b'>'), _) => (TokenKind::PipeGt, 2),
            (b'=', Some(b'>'), _) => (TokenKind::EqGt, 2),
            (b'#', Some(b'['), _) => (TokenKind::HashLBracket, 2),
            (b'.', _, _) => (TokenKind::Dot, 1),
            (b'(', _, _) => (TokenKind::LParen, 1),
            (b')', _, _) => (TokenKind::RParen, 1),
            (b'[', _, _) => (TokenKind::LBracket, 1),
            (b']', _, _) => (TokenKind::RBracket, 1),
            (b'+', _, _) => (TokenKind::Plus, 1),
            (b'-', _, _) => (TokenKind::Minus, 1),
            (b'*', _, _) => (TokenKind::Star, 1),
            (b'/', _, _) => (TokenKind::Slash, 1),
            (b'%', _, _) => (TokenKind::Percent, 1),
            (b'=', _, _) => (TokenKind::Eq, 1),
            (b'<', _, _) => (TokenKind::Lt, 1),
            (b'>', _, _) => (TokenKind::Gt, 1),
            (b',', _, _) => (TokenKind::Comma, 1),
            (b':', _, _) => (TokenKind::Colon, 1),
            (b';', _, _) => (TokenKind::Semi, 1),
            (b'|', _, _) => (TokenKind::Pipe, 1),
            (b'?', _, _) => (TokenKind::Question, 1),
            _ => {
                let b = self.advance();
                return self.error_token(LexError::UnexpectedCharacter(char::from(b)), start);
            }
        };
        self.advance_by(len);
        self.emit(kind, start)
    }

    fn lex_lbrace(&mut self, start: usize) -> Token {
        let _ = self.advance();
        if let Some(depth) = self.fstring_depths.last_mut() {
            *depth += 1;
        }
        self.emit(TokenKind::LBrace, start)
    }

    fn lex_rbrace(&mut self, start: usize) -> Token {
        match self.fstring_depths.last_mut() {
            Some(depth) if *depth > 0 => {
                *depth -= 1;
                let _ = self.advance();
                self.emit(TokenKind::RBrace, start)
            }
            Some(_) => {
                let _ = self.advance();
                self.scan_fstring_text(start, TokenKind::FStringMiddle, TokenKind::FStringTail)
            }
            None => {
                let _ = self.advance();
                self.emit(TokenKind::RBrace, start)
            }
        }
    }
}

// ============================================================================
// F-STRING SUPPORT
// ============================================================================
impl Lexer<'_> {
    fn lex_fstring_head(&mut self, start: usize) -> Token {
        let _ = self.advance();
        self.fstring_depths.push(0);
        self.scan_fstring_text(start, TokenKind::FStringHead, TokenKind::FStringHead)
    }

    /// Scan f-string text content until `{` (interpolation) or `"` (end).
    /// `on_brace`/`on_quote` determine the emitted token kind.
    fn scan_fstring_text(
        &mut self,
        start: usize,
        on_brace: TokenKind,
        on_quote: TokenKind,
    ) -> Token {
        let text_start = self.pos;
        loop {
            match self.peek() {
                Some(b'{') => {
                    let sym = self.intern_range(text_start, self.pos);
                    let _ = self.advance();
                    return self.emit_sym(on_brace, start, sym);
                }
                Some(b'"') => {
                    let sym = self.intern_range(text_start, self.pos);
                    let _ = self.advance();
                    let _ = self.fstring_depths.pop();
                    return self.emit_sym(on_quote, start, sym);
                }
                Some(b'\\') => self.skip_escape(),
                Some(_) => {
                    let _ = self.advance();
                }
                None => return self.error_token(LexError::UnterminatedFString, start),
            }
        }
    }
}

const fn is_ident_char(b: u8) -> bool {
    b.is_ascii_alphanumeric() || b == b'_'
}

const fn is_hex_digit(b: u8) -> bool {
    b.is_ascii_hexdigit()
}
const fn is_octal_digit(b: u8) -> bool {
    matches!(b, b'0'..=b'7')
}
const fn is_binary_digit(b: u8) -> bool {
    b == b'0' || b == b'1'
}
const fn is_decimal_digit(b: u8) -> bool {
    b.is_ascii_digit()
}
const fn is_exponent(b: u8) -> bool {
    b == b'e' || b == b'E'
}
const fn is_sign(b: u8) -> bool {
    b == b'+' || b == b'-'
}

#[cfg(test)]
mod tests;
