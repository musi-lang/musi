use core::mem;

use music_found::Span;

use crate::cursor::Cursor;
use crate::errors::{LexError, LexResult};
use crate::token::{
    FStrPart, Token, TokenKind, Trivia, TriviaKind, TriviaList, keyword_from_str, single_char_token,
};

const COMPOUNDS: &[(&[u8], TokenKind)] = &[
    (b":?>", TokenKind::ColonQuestionGt),
    (b":=", TokenKind::ColonEq),
    (b"::", TokenKind::ColonColon),
    (b":?", TokenKind::ColonQuestion),
    (b"...", TokenKind::DotDotDot),
    (b"..<", TokenKind::DotDotLt),
    (b"..", TokenKind::DotDot),
    (b".[", TokenKind::DotLBracket),
    (b".{", TokenKind::DotLBrace),
    (b"<-", TokenKind::LtMinus),
    (b"<=", TokenKind::LtEq),
    (b"<:", TokenKind::LtColon),
    (b"->", TokenKind::MinusGt),
    (b"~>", TokenKind::TildeGt),
    (b"=>", TokenKind::EqGt),
    (b"/=", TokenKind::SlashEq),
    (b">=", TokenKind::GtEq),
    (b"??", TokenKind::QuestionQuestion),
    (b"?.", TokenKind::QuestionDot),
    (b"!.", TokenKind::BangDot),
    (b"|>", TokenKind::PipeGt),
    (b"$(", TokenKind::DollarLParen),
    (b"$[", TokenKind::DollarLBracket),
];

const fn is_ident_start(c: char) -> bool {
    c.is_ascii_alphabetic() || c == '_'
}

const fn is_ident_cont(c: char) -> bool {
    c.is_ascii_alphanumeric() || c == '_'
}

const fn is_digit_for_base(c: char, base: u32) -> bool {
    match base {
        2 => matches!(c, '0' | '1'),
        8 => matches!(c, '0'..='7'),
        16 => c.is_ascii_hexdigit(),
        _ => c.is_ascii_digit(),
    }
}

pub struct Lexer<'src> {
    cursor: Cursor<'src>,
    errors: Vec<LexError>,
}

impl<'src> Lexer<'src> {
    #[must_use]
    pub const fn new(src: &'src str) -> Self {
        Self {
            cursor: Cursor::new(src),
            errors: Vec::new(),
        }
    }

    #[must_use]
    pub fn lex(mut self) -> (Vec<Token>, Vec<LexError>) {
        let mut tokens = Vec::new();
        loop {
            let leading = self.collect_trivia();
            if self.cursor.is_eof() {
                tokens.push(Token {
                    kind: TokenKind::Eof,
                    span: self.cursor.span_from(self.cursor.pos()),
                    leading_trivia: leading,
                    trailing_trivia: Vec::new(),
                });
                break;
            }
            let start = self.cursor.pos();
            match self.scan_token() {
                Ok(kind) => {
                    let span = self.cursor.span_from(start);
                    let trailing = self.collect_trailing_trivia();
                    tokens.push(Token {
                        kind,
                        span,
                        leading_trivia: leading,
                        trailing_trivia: trailing,
                    });
                }
                Err(err) => {
                    self.errors.push(err);
                }
            }
        }
        (tokens, self.errors)
    }

    fn try_compound(&mut self) -> Option<TokenKind> {
        let remaining = self.cursor.remaining();
        for (bytes, kind) in COMPOUNDS {
            if remaining.starts_with(bytes) {
                self.cursor.advance_by(bytes.len());
                return Some(kind.clone());
            }
        }
        None
    }

    fn scan_token(&mut self) -> LexResult<TokenKind> {
        if let Some(kind) = self.try_compound() {
            return Ok(kind);
        }
        let start = self.cursor.pos();
        let ch = self
            .cursor
            .advance()
            .expect("scan_token called when not EOF");
        match ch {
            _ if is_ident_start(ch) => self.scan_ident_or_keyword(start),
            '0'..='9' => self.scan_number(start),
            '"' => self.scan_string(start),
            '\'' => self.scan_rune(start),
            '`' => self.scan_escaped_ident(start),
            _ => single_char_token(ch).ok_or_else(|| LexError::UnexpectedChar {
                ch,
                span: self.cursor.span_from(start),
            }),
        }
    }

    fn scan_ident_or_keyword(&mut self, start: u32) -> LexResult<TokenKind> {
        let _ = self.cursor.eat_while(is_ident_cont);
        let text = self.cursor.slice(start);
        if text.len() == 1 && text.starts_with('f') && self.cursor.peek() == Some('"') {
            return self.scan_fstring(start);
        }
        Ok(keyword_from_str(text).unwrap_or(TokenKind::Ident))
    }

    fn scan_number(&mut self, start: u32) -> LexResult<TokenKind> {
        let first_byte = self.cursor.slice(start).as_bytes()[0];
        if first_byte == b'0' {
            if let Some(prefix @ ('x' | 'o' | 'b')) = self.cursor.peek() {
                let _ = self.cursor.advance();
                let base = match prefix {
                    'x' => 16,
                    'o' => 8,
                    _ => 2,
                };
                return self.scan_int_digits(start, base);
            }
        }
        let _ = self.cursor.eat_while(|c| c.is_ascii_digit() || c == '_');
        if self.cursor.peek() == Some('.')
            && self.cursor.peek_next().is_some_and(|c| c.is_ascii_digit())
        {
            return self.scan_float(start);
        }
        if matches!(self.cursor.peek(), Some('e' | 'E')) {
            return self.scan_float(start);
        }
        self.finish_int(start, 10)
    }

    fn scan_int_digits(&mut self, start: u32, base: u32) -> LexResult<TokenKind> {
        let before = self.cursor.pos();
        let _ = self
            .cursor
            .eat_while(|c| is_digit_for_base(c, base) || c == '_');
        if self.cursor.pos() == before {
            return Err(LexError::InvalidNumberPrefix {
                span: self.cursor.span_from(start),
            });
        }
        self.finish_int(start, base)
    }

    fn finish_int(&self, start: u32, base: u32) -> LexResult<TokenKind> {
        let raw = self.cursor.slice(start);
        let skip = if base == 10 { 0 } else { 2 };
        let digits: String = raw.chars().skip(skip).filter(|&c| c != '_').collect();
        if digits.is_empty() {
            return Err(LexError::InvalidNumberPrefix {
                span: self.cursor.span_from(start),
            });
        }
        i64::from_str_radix(&digits, base)
            .map(TokenKind::Int)
            .map_err(|_| LexError::NumberOverflow {
                span: self.cursor.span_from(start),
            })
    }

    fn scan_float(&mut self, start: u32) -> LexResult<TokenKind> {
        if self.cursor.eat('.') {
            let _ = self.cursor.eat_while(|c| c.is_ascii_digit() || c == '_');
        }
        if matches!(self.cursor.peek(), Some('e' | 'E')) {
            let _ = self.cursor.advance();
            let _ = self.cursor.eat('+') || self.cursor.eat('-');
            let before_exp_digits = self.cursor.pos();
            let _ = self.cursor.eat_while(|c| c.is_ascii_digit() || c == '_');
            if self.cursor.pos() == before_exp_digits {
                return Err(LexError::InvalidNumberPrefix {
                    span: self.cursor.span_from(start),
                });
            }
        }
        let raw = self.cursor.slice(start);
        let cleaned: String = raw.chars().filter(|&c| c != '_').collect();
        cleaned
            .parse::<f64>()
            .map(TokenKind::Float)
            .map_err(|_| LexError::NumberOverflow {
                span: self.cursor.span_from(start),
            })
    }

    fn scan_string(&mut self, start: u32) -> LexResult<TokenKind> {
        let mut value = String::new();
        loop {
            match self.cursor.peek() {
                None => {
                    return Err(LexError::UnterminatedString {
                        span: self.cursor.span_from(start),
                    });
                }
                Some('"') => {
                    let _ = self.cursor.advance();
                    return Ok(TokenKind::Str(value));
                }
                Some('\\') => {
                    let _ = self.cursor.advance();
                    value.push(self.scan_escape(start)?);
                }
                Some(ch) => {
                    let _ = self.cursor.advance();
                    value.push(ch);
                }
            }
        }
    }

    fn scan_fstring(&mut self, start: u32) -> LexResult<TokenKind> {
        let _ = self.cursor.advance(); // consume the '"'
        let mut parts = Vec::new();
        let mut lit_buf = String::new();
        loop {
            match self.cursor.peek() {
                None => {
                    return Err(LexError::UnterminatedFString {
                        span: self.cursor.span_from(start),
                    });
                }
                Some('"') => {
                    let _ = self.cursor.advance();
                    if !lit_buf.is_empty() {
                        parts.push(FStrPart::Lit(lit_buf));
                    }
                    return Ok(TokenKind::FStr(parts));
                }
                Some('{') => {
                    let _ = self.cursor.advance();
                    if !lit_buf.is_empty() {
                        parts.push(FStrPart::Lit(mem::take(&mut lit_buf)));
                    }
                    let expr_tokens = self.scan_fstring_expr(start)?;
                    parts.push(FStrPart::Expr(expr_tokens));
                }
                Some('\\') => {
                    let _ = self.cursor.advance();
                    lit_buf.push(self.scan_escape(start)?);
                }
                Some(ch) => {
                    let _ = self.cursor.advance();
                    lit_buf.push(ch);
                }
            }
        }
    }

    fn scan_fstring_expr(&mut self, fstr_start: u32) -> LexResult<Vec<Token>> {
        let mut tokens = Vec::new();
        let mut depth: u32 = 1;
        loop {
            let leading = self.collect_trivia();
            if self.cursor.is_eof() {
                return Err(LexError::UnterminatedFStringExpr {
                    span: self.cursor.span_from(fstr_start),
                });
            }
            if self.cursor.peek() == Some('}') {
                depth -= 1;
                if depth == 0 {
                    let _ = self.cursor.advance();
                    return Ok(tokens);
                }
            }
            let tok_start = self.cursor.pos();
            match self.scan_token() {
                Ok(kind) => {
                    if kind == TokenKind::LBrace {
                        depth += 1;
                    }
                    let span = self.cursor.span_from(tok_start);
                    let trailing = self.collect_trailing_trivia();
                    tokens.push(Token {
                        kind,
                        span,
                        leading_trivia: leading,
                        trailing_trivia: trailing,
                    });
                }
                Err(err) => {
                    self.errors.push(err);
                }
            }
        }
    }

    fn scan_rune(&mut self, start: u32) -> LexResult<TokenKind> {
        let ch = match self.cursor.peek() {
            None | Some('\'') => {
                if self.cursor.peek() == Some('\'') {
                    let _ = self.cursor.advance();
                }
                return Err(LexError::EmptyRune {
                    span: self.cursor.span_from(start),
                });
            }
            Some('\\') => {
                let _ = self.cursor.advance();
                self.scan_escape(start)?
            }
            Some(c) => {
                let _ = self.cursor.advance();
                c
            }
        };
        if self.cursor.peek() == Some('\'') {
            let _ = self.cursor.advance();
            return Ok(TokenKind::Rune(ch));
        }
        // Multi-char or unterminated
        if self.cursor.is_eof() {
            return Err(LexError::UnterminatedRune {
                span: self.cursor.span_from(start),
            });
        }
        // Consume until closing quote or EOF for error recovery
        while let Some(c) = self.cursor.peek() {
            let _ = self.cursor.advance();
            if c == '\'' {
                return Err(LexError::MultiCharRune {
                    span: self.cursor.span_from(start),
                });
            }
        }
        Err(LexError::UnterminatedRune {
            span: self.cursor.span_from(start),
        })
    }

    fn scan_escaped_ident(&mut self, start: u32) -> LexResult<TokenKind> {
        loop {
            match self.cursor.peek() {
                None => {
                    return Err(LexError::UnterminatedEscapedIdent {
                        span: self.cursor.span_from(start),
                    });
                }
                Some('`') => {
                    let _ = self.cursor.advance();
                    return Ok(TokenKind::EscapedIdent);
                }
                Some(_) => {
                    let _ = self.cursor.advance();
                }
            }
        }
    }

    fn scan_escape(&mut self, literal_start: u32) -> LexResult<char> {
        let esc_start = self.cursor.pos() - 1; // backslash position
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
            Some(ch) => Err(LexError::InvalidEscape {
                ch,
                span: self.cursor.span_from(esc_start),
            }),
            None => Err(LexError::InvalidEscape {
                ch: ' ',
                span: self.cursor.span_from(esc_start),
            }),
        }
    }

    fn scan_hex_escape(&mut self, count: u8, _literal_start: u32) -> LexResult<char> {
        let start = self.cursor.pos();
        let mut value: u32 = 0;
        for _ in 0..count {
            match self.cursor.peek() {
                Some(c) if c.is_ascii_hexdigit() => {
                    let _ = self.cursor.advance();
                    value = value * 16 + c.to_digit(16).expect("validated as hex digit");
                }
                _ => {
                    return Err(LexError::InvalidHexEscape {
                        expected: count,
                        span: self.cursor.span_from(start),
                    });
                }
            }
        }
        char::from_u32(value).ok_or_else(|| LexError::InvalidHexEscape {
            expected: count,
            span: self.cursor.span_from(start),
        })
    }

    fn scan_unicode_escape(&mut self, _literal_start: u32) -> LexResult<char> {
        let start = self.cursor.pos();
        if !self.cursor.eat('{') {
            return Err(LexError::InvalidUnicodeEscape {
                span: self.cursor.span_from(start),
            });
        }
        let mut value: u32 = 0;
        let mut digit_count: u32 = 0;
        loop {
            match self.cursor.peek() {
                Some('}') => {
                    let _ = self.cursor.advance();
                    break;
                }
                Some(c) if c.is_ascii_hexdigit() => {
                    let _ = self.cursor.advance();
                    value = value * 16 + c.to_digit(16).expect("validated as hex digit");
                    digit_count += 1;
                    if digit_count > 6 {
                        return Err(LexError::InvalidUnicodeEscape {
                            span: self.cursor.span_from(start),
                        });
                    }
                }
                _ => {
                    return Err(LexError::InvalidUnicodeEscape {
                        span: self.cursor.span_from(start),
                    });
                }
            }
        }
        if digit_count == 0 {
            return Err(LexError::InvalidUnicodeEscape {
                span: self.cursor.span_from(start),
            });
        }
        char::from_u32(value).ok_or_else(|| LexError::InvalidUnicodeEscape {
            span: self.cursor.span_from(start),
        })
    }

    fn collect_trivia(&mut self) -> TriviaList {
        let mut trivia = Vec::new();
        loop {
            match self.cursor.peek() {
                Some(' ' | '\t' | '\r') => trivia.push(self.scan_whitespace()),
                Some('\n') => {
                    let pos = self.cursor.pos();
                    let _ = self.cursor.advance();
                    trivia.push(Trivia {
                        kind: TriviaKind::Newline,
                        span: Span::new(pos, pos + 1),
                    });
                }
                Some('/') if self.cursor.peek_next() == Some('/') => {
                    trivia.push(self.scan_line_comment());
                }
                Some('/') if self.cursor.peek_next() == Some('*') => {
                    match self.scan_block_comment() {
                        Ok(t) => trivia.push(t),
                        Err(err) => self.errors.push(err),
                    }
                }
                _ => break,
            }
        }
        trivia
    }

    fn collect_trailing_trivia(&mut self) -> TriviaList {
        let mut trivia = Vec::new();
        loop {
            match self.cursor.peek() {
                Some(' ' | '\t' | '\r') => trivia.push(self.scan_whitespace()),
                Some('/') if self.cursor.peek_next() == Some('/') => {
                    trivia.push(self.scan_line_comment());
                    break;
                }
                Some('/') if self.cursor.peek_next() == Some('*') => {
                    match self.scan_block_comment() {
                        Ok(t) => trivia.push(t),
                        Err(err) => self.errors.push(err),
                    }
                }
                _ => break,
            }
        }
        trivia
    }

    fn scan_whitespace(&mut self) -> Trivia {
        let start = self.cursor.pos();
        let _ = self
            .cursor
            .eat_while(|c| c == ' ' || c == '\t' || c == '\r');
        Trivia {
            kind: TriviaKind::Whitespace,
            span: self.cursor.span_from(start),
        }
    }

    fn scan_line_comment(&mut self) -> Trivia {
        let start = self.cursor.pos();
        let _ = self.cursor.advance(); // first /
        let _ = self.cursor.advance(); // second /
        let doc = self.cursor.peek() == Some('/');
        if doc {
            let _ = self.cursor.advance();
        }
        let _ = self.cursor.eat_while(|c| c != '\n');
        Trivia {
            kind: TriviaKind::LineComment { doc },
            span: self.cursor.span_from(start),
        }
    }

    fn scan_block_comment(&mut self) -> LexResult<Trivia> {
        let start = self.cursor.pos();
        let _ = self.cursor.advance(); // /
        let _ = self.cursor.advance(); // *
        let doc = self.cursor.peek() == Some('*');
        if doc {
            let _ = self.cursor.advance();
        }
        let mut depth: u32 = 1;
        loop {
            match self.cursor.peek() {
                None => {
                    return Err(LexError::UnterminatedBlockComment {
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
                        return Ok(Trivia {
                            kind: TriviaKind::BlockComment { doc },
                            span: self.cursor.span_from(start),
                        });
                    }
                }
                _ => {
                    let _ = self.cursor.advance();
                }
            }
        }
    }
}

#[cfg(test)]
#[allow(clippy::unwrap_used, clippy::panic)]
mod tests;
