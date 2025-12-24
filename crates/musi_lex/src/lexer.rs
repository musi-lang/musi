use crate::{
    cursor::Cursor,
    error::LexErrorKind,
    token::{KEYWORDS, Token, TokenKind},
};
use musi_basic::{
    diagnostic::{DiagnosticBag, report},
    errors::IntoMusiError,
    interner::Interner,
    source::SourceFile,
    span::Span,
};
use std::str::Chars;

const PREFIX_LEN: usize = "0x".len();
const TEMPLATE_PREFIX: usize = "$\"".len();
const DEC_RADIX: u32 = 10;

/// `(prefix_char, radix, name)`
const NUM_PREFIXES: &[(char, u32, &str)] = &[
    ('x', 16, "hexadecimal"),
    ('o', 8, "octal"),
    ('b', 2, "binary"),
];

pub type TokenStream = (Vec<Token>, DiagnosticBag);

pub fn tokenize(source: &SourceFile, interner: &mut Interner) -> TokenStream {
    let mut lexer = Lexer::new(source, interner);
    let mut tokens = vec![];
    loop {
        let token = lexer.next_token();
        if token.kind == TokenKind::EOF {
            break;
        }
        tokens.push(token);
    }
    (tokens, lexer.errors)
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum BraceKind {
    Normal,
    Template,
}

#[derive(Debug)]
pub struct Lexer<'a> {
    interner: &'a mut Interner,
    errors: DiagnosticBag,
    source: &'a SourceFile,
    cursor: Cursor<'a>,
    braces: Vec<BraceKind>,
}

impl<'a> Lexer<'a> {
    pub fn new(source: &'a SourceFile, interner: &'a mut Interner) -> Self {
        Self {
            interner,
            errors: DiagnosticBag::default(),
            source,
            cursor: Cursor::new(&source.input),
            braces: vec![],
        }
    }

    #[must_use]
    pub const fn errors(&self) -> &DiagnosticBag {
        &self.errors
    }

    pub fn next_token(&mut self) -> Token {
        self.skip_whitespace();
        let start = self.cursor.pos();

        let kind = match self.cursor.peek() {
            Some(c) => match c {
                '#' if self.cursor.peek_nth(1) == Some('!') => self.scan_line_comment(),
                '/' if matches!(self.cursor.peek_nth(1), Some('/' | '*')) => {
                    self.scan_slash_seq(start)
                }
                '}' if self.braces.last() == Some(&BraceKind::Template) => {
                    let _ = self.braces.pop();
                    self.scan_template_lit(1, start)
                }
                '"' => self.scan_text_lit(),
                '\'' => self.scan_rune_lit(),
                '`' => self.scan_escaped_ident(),
                '$' if self.cursor.peek_nth(1) == Some('"') => self.scan_template_lit(2, start),
                c if c.is_ascii_alphabetic() || c == '_' => self.scan_ident(),
                '0'..='9' => self.scan_number(),
                _ => self.scan_symbol(),
            },
            None => TokenKind::EOF,
        };

        Token::new(kind, self.span(start, self.cursor.pos()))
    }

    fn skip_whitespace(&mut self) {
        self.cursor.eat_while(|c| " \t\r\n".contains(c));
    }

    fn scan_line_comment(&mut self) -> TokenKind {
        self.cursor.eat_while(|c| c != '\n');
        self.next_token().kind
    }

    fn scan_slash_seq(&mut self, start: usize) -> TokenKind {
        match self.cursor.peek_nth(1) {
            Some('/') => self.scan_line_comment(),
            Some('*') => self.scan_block_comment(start),
            _ => self.scan_symbol(),
        }
    }

    fn scan_block_comment(&mut self, start: usize) -> TokenKind {
        let mut depth = 1;
        self.cursor.bump_n(2);
        while depth > 0 {
            match (self.cursor.peek(), self.cursor.peek_nth(1)) {
                (Some('/'), Some('*')) => {
                    depth += 1;
                    self.cursor.bump_n(2);
                }
                (Some('*'), Some('/')) => {
                    depth -= 1;
                    self.cursor.bump_n(2);
                }
                (Some(_), _) => {
                    let _: Option<char> = self.cursor.bump();
                }
                (None, _) => break,
            }
        }
        if depth > 0 {
            self.report(LexErrorKind::UnclosedComment, start, self.cursor.pos());
        }
        self.next_token().kind
    }

    fn scan_ident(&mut self) -> TokenKind {
        let start = self.cursor.pos();
        self.cursor
            .eat_while(|c| c.is_ascii_alphanumeric() || c == '_');
        let text = self.source.input.get(start..self.cursor.pos()).unwrap();
        if text == "_" {
            return TokenKind::Underscore;
        }

        match KEYWORDS.binary_search_by_key(&text, |k| k.0) {
            Ok(i) => KEYWORDS[i].1,
            Err(_) => TokenKind::Ident(self.interner.intern(text)),
        }
    }

    fn scan_number(&mut self) -> TokenKind {
        let start = self.cursor.pos();

        let (base, prefix_len, name) = if self.cursor.peek() == Some('0') {
            if let Some((_, b, n)) = NUM_PREFIXES
                .iter()
                .find(|(c, ..)| self.cursor.peek_nth(1) == Some(*c))
            {
                (*b, PREFIX_LEN, *n)
            } else {
                (DEC_RADIX, 0, "decimal")
            }
        } else {
            (DEC_RADIX, 0, "decimal")
        };

        self.cursor.bump_n(prefix_len);
        self.cursor.eat_while(|c| c.is_digit(base) || c == '_');

        let mut end = self.cursor.pos();
        if end == start + prefix_len {
            self.report(LexErrorKind::MalformedNumber, start, end);
            return TokenKind::LitInt(0);
        }

        let is_real = if base == DEC_RADIX {
            let has_frac = if self.cursor.is_next('.') && self.cursor.peek_nth(1) != Some('.') {
                let _: Option<char> = self.cursor.bump();
                if !self.consume_digits() {
                    self.report(LexErrorKind::MalformedNumber, start, self.cursor.pos());
                }
                true
            } else {
                false
            };
            let has_exp = if matches!(self.cursor.peek(), Some('e' | 'E')) {
                let _: Option<char> = self.cursor.bump();
                if matches!(self.cursor.peek(), Some('+' | '-')) {
                    let _: Option<char> = self.cursor.bump();
                }
                if !self.consume_digits() {
                    self.report(LexErrorKind::MalformedNumber, start, self.cursor.pos());
                }
                true
            } else {
                false
            };
            end = self.cursor.pos();
            has_frac || has_exp
        } else {
            false
        };

        let (val, valid) = self.unitize_numeric(start, end);
        if !valid {
            self.report(
                LexErrorKind::MalformedUnderscore((if is_real { "real" } else { name }).into()),
                start,
                end,
            );
        }

        if is_real {
            TokenKind::LitReal(self.interner.intern(&val))
        } else {
            TokenKind::LitInt(self.interner.intern(&val))
        }
    }

    fn consume_digits(&mut self) -> bool {
        let before = self.cursor.pos();
        self.cursor.eat_while(|c| c.is_ascii_digit() || c == '_');
        self.cursor.pos() > before
    }

    fn unitize_numeric(&self, start: usize, end: usize) -> (String, bool) {
        let mut out = String::with_capacity(end - start);
        let mut prev_under = false;
        let mut valid = true;
        let s = self.source.input.get(start..end).unwrap();
        for (i, c) in s.char_indices() {
            if c == '_' {
                if i == 0 || i == s.len() - 1 || prev_under {
                    valid = false;
                }
                prev_under = true;
            } else {
                prev_under = false;
                out.push(c);
            }
        }
        (out, valid)
    }

    fn scan_symbol(&mut self) -> TokenKind {
        match self.cursor.peek() {
            Some('.') => match self.cursor.peek_nth(1) {
                Some('.') => self.match_tri(2, '<', TokenKind::DotDotLt, TokenKind::DotDot),
                Some('^') => self.compound(2, TokenKind::DotCaret),
                _ => self.one(TokenKind::Dot),
            },
            Some('<') => match self.cursor.peek_nth(1) {
                Some('<') => self.compound(2, TokenKind::LtLt),
                Some('=') => self.compound(2, TokenKind::LtEq),
                Some('-') => self.compound(2, TokenKind::LtMinus),
                _ => self.one(TokenKind::Lt),
            },
            Some(':') => self.match_bi(
                1,
                ':',
                TokenKind::ColonColon,
                '=',
                TokenKind::ColonEq,
                TokenKind::Colon,
            ),
            Some('=') => self.match_maybe(1, '>', TokenKind::EqGt, TokenKind::Eq),
            Some('>') => match self.cursor.peek_nth(1) {
                Some('=') => self.compound(2, TokenKind::GtEq),
                Some('>') => self.compound(2, TokenKind::GtGt),
                Some(']') => self.compound(2, TokenKind::GtRBrack),
                _ => self.one(TokenKind::Gt),
            },
            Some('?') => self.match_maybe(1, '?', TokenKind::QuestionQuestion, TokenKind::Question),
            Some('/') => self.match_maybe(1, '=', TokenKind::SlashEq, TokenKind::Slash),
            Some('-') => self.match_maybe(1, '>', TokenKind::MinusGt, TokenKind::Minus),
            Some('*') => self.match_maybe(1, '*', TokenKind::StarStar, TokenKind::Star),
            Some('[') => self.match_maybe(1, '<', TokenKind::LBrackLt, TokenKind::LBrack),
            Some('|') => self.match_maybe(1, '>', TokenKind::BarGt, TokenKind::Bar),
            Some('%') => self.one(TokenKind::Percent),
            Some('&') => self.one(TokenKind::Amp),
            Some('(') => self.one(TokenKind::LParen),
            Some(')') => self.one(TokenKind::RParen),
            Some('+') => self.one(TokenKind::Plus),
            Some(',') => self.one(TokenKind::Comma),
            Some(';') => self.one(TokenKind::Semicolon),
            Some('@') => self.one(TokenKind::At),
            Some(']') => self.one(TokenKind::RBrack),
            Some('^') => self.one(TokenKind::Caret),
            Some('_') => self.one(TokenKind::Underscore),
            Some('{') => {
                self.braces.push(BraceKind::Normal);
                self.one(TokenKind::LBrace)
            }
            Some('}') => {
                if self.braces.last() == Some(&BraceKind::Normal) {
                    let _: Option<BraceKind> = self.braces.pop();
                }
                self.one(TokenKind::RBrace)
            }
            Some('~') => self.one(TokenKind::Tilde),
            Some('$') => self.one(TokenKind::Dollar),
            Some(c) => {
                let _: Option<char> = self.cursor.bump();
                self.report(
                    LexErrorKind::UnknownChar(c),
                    self.cursor.pos() - c.len_utf8(),
                    self.cursor.pos(),
                );
                TokenKind::Invalid(self.interner.intern(c.to_string().as_str()))
            }
            None => TokenKind::EOF,
        }
    }

    fn one(&mut self, kind: TokenKind) -> TokenKind {
        let _: Option<char> = self.cursor.bump();
        kind
    }

    fn compound(&mut self, len: usize, kind: TokenKind) -> TokenKind {
        self.cursor.bump_n(len);
        kind
    }

    fn match_maybe(&mut self, offset: usize, c: char, yes: TokenKind, no: TokenKind) -> TokenKind {
        if self.cursor.peek_nth(offset) == Some(c) {
            self.cursor.bump_n(offset + 1);
            yes
        } else {
            let _: Option<char> = self.cursor.bump();
            no
        }
    }

    fn match_tri(&mut self, offset: usize, c: char, yes: TokenKind, no: TokenKind) -> TokenKind {
        if self.cursor.peek_nth(offset) == Some(c) {
            self.cursor.bump_n(offset + 1);
            yes
        } else {
            self.cursor.bump_n(offset);
            no
        }
    }

    fn match_bi(
        &mut self,
        offset: usize,
        c1: char,
        k1: TokenKind,
        c2: char,
        k2: TokenKind,
        def: TokenKind,
    ) -> TokenKind {
        match self.cursor.peek_nth(offset) {
            Some(c) if c == c1 => {
                self.cursor.bump_n(offset + 1);
                k1
            }
            Some(c) if c == c2 => {
                self.cursor.bump_n(offset + 1);
                k2
            }
            _ => {
                let _: Option<char> = self.cursor.bump();
                def
            }
        }
    }

    fn scan_text_lit(&mut self) -> TokenKind {
        let start = self.cursor.pos();
        if let Some(content_end) =
            self.consume_quoted(start, 1, &['"'], LexErrorKind::UnclosedString)
        {
            let raw = self.source.input.get(start + 1..content_end).unwrap();
            let val = unescape(raw, start + 1, &mut self.errors);
            TokenKind::LitString(self.interner.intern(&val))
        } else {
            TokenKind::Invalid(
                self.interner
                    .intern(self.source.input.get(start..self.cursor.pos()).unwrap()),
            )
        }
    }

    fn scan_rune_lit(&mut self) -> TokenKind {
        let start = self.cursor.pos();
        let _: Option<char> = self.cursor.bump();

        let mut val = '\0';

        match self.cursor.peek() {
            Some('\'') => {
                self.report(LexErrorKind::InvalidRune, start, start + 2);
                let _: Option<char> = self.cursor.bump();
            }
            Some('\\') => {
                let _: Option<char> = self.cursor.bump();
                let c_start = self.cursor.pos();
                let mut rest_chars = self.cursor.rest().chars();
                match scan_escape(&mut rest_chars) {
                    Ok((c, len)) => {
                        val = c;
                        self.cursor.bump_n(len);
                    }
                    Err((msg, len)) => {
                        self.report(
                            LexErrorKind::UnknownEscape(msg),
                            c_start - 1,
                            c_start - 1 + 1 + len,
                        );
                        self.cursor.bump_n(len);
                    }
                }
            }
            Some(c) => {
                val = c;
                let _: Option<char> = self.cursor.bump();
            }
            None => {}
        }

        if self.cursor.is_next('\'') {
            let _: Option<char> = self.cursor.bump();
        } else {
            self.cursor.eat_while(|c| c != '\'');
            if self.cursor.is_next('\'') {
                let _: Option<char> = self.cursor.bump();
                self.report(LexErrorKind::InvalidRune, start, self.cursor.pos());
            } else {
                self.report(LexErrorKind::UnclosedRune, start, start + 1);
            }
        }

        TokenKind::LitRune(val)
    }

    fn scan_template_lit(&mut self, offset: usize, start: usize) -> TokenKind {
        if let Some(content_end) =
            self.consume_quoted(start, offset, &['"', '{'], LexErrorKind::UnclosedTemplate)
        {
            let raw = self.source.input.get(start + offset..content_end).unwrap();
            let val = unescape(raw, start + offset, &mut self.errors);
            let s = self.interner.intern(&val);

            let is_head = offset == TEMPLATE_PREFIX;
            let follows_brace = self
                .source
                .input
                .get(content_end..)
                .is_some_and(|s| s.starts_with('{'));
            if follows_brace {
                self.braces.push(BraceKind::Template);
                if is_head {
                    TokenKind::TemplateHead(s)
                } else {
                    TokenKind::TemplateMiddle(s)
                }
            } else if is_head {
                TokenKind::LitTemplateNoSubst(s)
            } else {
                TokenKind::TemplateTail(s)
            }
        } else {
            TokenKind::Invalid(
                self.interner
                    .intern(self.source.input.get(start..self.cursor.pos()).unwrap()),
            )
        }
    }

    fn scan_escaped_ident(&mut self) -> TokenKind {
        let start = self.cursor.pos();
        if let Some(content_end) =
            self.consume_quoted(start, 1, &['`'], LexErrorKind::UnclosedEscapedIdent)
        {
            let s = self.source.input.get(start + 1..content_end).unwrap();
            if s.is_empty() {
                self.report(LexErrorKind::InvalidIdent, start, self.cursor.pos());
                TokenKind::Invalid(self.interner.intern(""))
            } else {
                TokenKind::Ident(self.interner.intern(s))
            }
        } else {
            TokenKind::Invalid(
                self.interner
                    .intern(self.source.input.get(start..self.cursor.pos()).unwrap()),
            )
        }
    }

    fn consume_quoted(
        &mut self,
        start: usize,
        offset: usize,
        terms: &[char],
        err: LexErrorKind,
    ) -> Option<usize> {
        self.cursor.bump_n(offset);

        loop {
            match self.cursor.peek() {
                Some(c) if terms.contains(&c) => {
                    let pos = self.cursor.pos();
                    let _: Option<char> = self.cursor.bump();
                    return Some(pos);
                }
                Some('\\') => {
                    let _: Option<char> = self.cursor.bump();
                    let _: Option<char> = self.cursor.bump();
                }
                Some(_) => {
                    let _: Option<char> = self.cursor.bump();
                }
                None => {
                    self.report(err, start, start + 1);
                    return None;
                }
            }
        }
    }

    fn report(&mut self, err: LexErrorKind, start: usize, end: usize) {
        self.errors
            .add(report(err.into_musi_error(self.span(start, end))));
    }

    fn span(&self, lo: usize, hi: usize) -> Span {
        Span::new(
            self.source.start + u32::try_from(lo).unwrap(),
            self.source.start + u32::try_from(hi).unwrap(),
        )
    }
}

#[inline]
/// Unescape string literal, reporting any errors into diagnostic bag.
///
/// # Panics
///
/// Panics if `start_pos + offset` or `start_pos + offset + 1 + len` exceeds `u32::MAX`.
pub fn unescape(s: &str, start_pos: usize, errors: &mut DiagnosticBag) -> String {
    let mut out = String::with_capacity(s.len());
    let mut chars = s.chars();
    let mut offset = 0;
    while let Some(ch) = chars.next() {
        if ch == '\\' {
            match scan_escape(&mut chars) {
                Ok((esc, len)) => {
                    out.push(esc);
                    offset += 1 + len;
                }
                Err((esc_err, len)) => {
                    let err_span = Span::new(
                        u32::try_from(start_pos + offset).expect("offset out of range"),
                        u32::try_from(start_pos + offset + 1 + len).expect("offset out of range"),
                    );
                    errors.add(report(
                        LexErrorKind::UnknownEscape(esc_err).into_musi_error(err_span),
                    ));
                    offset += 1 + len;
                }
            }
        } else {
            out.push(ch);
            offset += ch.len_utf8();
        }
    }
    out
}

/// Parses escape sequence from character iterator.
///
/// # Errors
///
/// Returns `Err((invalid_sequence, len))` if escape is malformed.
#[inline]
pub fn scan_escape(chars: &mut Chars<'_>) -> Result<(char, usize), (String, usize)> {
    let Some(ch) = chars.next() else {
        return Ok(('\0', 0));
    };

    if let Ok(i) = ESCAPES.binary_search_by_key(&ch, |e| e.0) {
        return Ok((ESCAPES[i].1, 1));
    }

    match ch {
        'x' => {
            let d1 = chars.next();
            let d2 = chars.next();

            match (d1, d2) {
                (Some(c1), Some(c2)) => {
                    if let (Some(d1), Some(d2)) = (c1.to_digit(16), c2.to_digit(16))
                        && let Some(c) = char::from_u32((d1 << 4) | d2)
                    {
                        return Ok((c, 3));
                    }
                    Err((format!("x{c1}{c2}"), 3))
                }
                (Some(c1), None) => Err((format!("x{c1}"), 2)),
                _ => Err(("x".into(), 1)),
            }
        }
        'u' => {
            if chars.clone().next() == Some('{') {
                let mut err_iter = chars.clone();
                let _: Option<char> = chars.next();
                let _: Option<char> = err_iter.next();

                let (mut val, mut len) = (0, 3);
                let mut valid = false;

                for ch in chars.by_ref() {
                    if ch == '}' {
                        if !valid {
                            return Err(("u{}".into(), len));
                        }
                        match char::from_u32(val) {
                            Some(c) => return Ok((c, len)),
                            None => break, // bad unicode scalar
                        };
                    }
                    if let Some(d) = ch.to_digit(16) {
                        val = (val << 4) | d;
                        len += 1;
                        valid = true;
                        if len > 8 {
                            break;
                        }
                    } else {
                        break;
                    }
                }

                let mut s = String::with_capacity(len);
                s.push('u');
                for _ in 0..(len - 1) {
                    if let Some(c) = err_iter.next() {
                        s.push(c);
                    }
                }
                Err((s, len))
            } else {
                Err(("u".into(), 1))
            }
        }
        _ => Err((ch.to_string(), 1)),
    }
}

const ESCAPES: &[(char, char)] = &[
    ('0', '\0'),
    ('"', '\"'),
    ('\'', '\''),
    ('\\', '\\'),
    ('a', '\x07'),
    ('b', '\x08'),
    ('e', '\x1b'),
    ('f', '\x0c'),
    ('n', '\n'),
    ('r', '\r'),
    ('t', '\t'),
    ('v', '\x0b'),
];
