use crate::basic::{
    diagnostic::{DiagnosticBag, report},
    errors::{Error, LexErrorKind},
    interner::Interner,
    source::SourceFile,
    span::Span,
};
use crate::lex::token::{KEYWORDS, SYMBOLS, Token};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum BraceKind {
    Normal,
    Template,
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

pub struct Lexer<'a> {
    interner: &'a mut Interner,
    errors: DiagnosticBag,
    source: &'a SourceFile,
    cursor: usize,
    braces: Vec<BraceKind>,
}

impl<'a> Lexer<'a> {
    pub fn new(source: &'a SourceFile, interner: &'a mut Interner) -> Self {
        Self {
            interner,
            errors: Default::default(),
            source,
            cursor: 0,
            braces: vec![],
        }
    }

    pub const fn errors(&self) -> &DiagnosticBag {
        &self.errors
    }

    pub fn next_token(&mut self) -> (Token, Span) {
        self.skip_whitespace();
        let start = self.cursor;
        let c = match self.peek_by(0) {
            Some(c) => c,
            None => return (Token::EOF, self.span(start, start)),
        };
        match c {
            '#' if self.peek_by(1) == Some('!') => self.scan_line_comment(),
            '/' if matches!(self.peek_by(1), Some('/' | '*')) => self.scan_slash_seq(start),
            '}' if self.braces.last() == Some(&BraceKind::Template) => {
                let _ = self.braces.pop();
                self.advance_by(1);
                (self.scan_template_lit(0), self.span(start, self.cursor))
            }
            '"' => (self.scan_text_lit(), self.span(start, self.cursor)),
            '\'' => (self.scan_rune_lit(), self.span(start, self.cursor)),
            '`' => (self.scan_escaped_ident(), self.span(start, self.cursor)),
            '$' if self.peek_by(1) == Some('"') => {
                (self.scan_template_lit(2), self.span(start, self.cursor))
            }
            c if c == '_' || c.is_ascii_alphabetic() => {
                (self.scan_ident(), self.span(start, self.cursor))
            }
            '0'..='9' => (self.scan_number(), self.span(start, self.cursor)),
            _ => (
                self.scan_symbol()
                    .unwrap_or_else(|| self.report_invalid_char(start, c).0),
                self.span(start, self.cursor),
            ),
        }
    }

    fn skip_whitespace(&mut self) {
        self.cursor = self.consume_while(self.cursor, |c| " \t\r\n".contains(c));
    }

    fn scan_line_comment(&mut self) -> (Token, Span) {
        self.cursor = self.consume_while(self.cursor, |c| c != '\n');
        self.next_token()
    }

    fn scan_block_comment(&mut self, start: usize) -> (Token, Span) {
        let mut depth = 1;
        self.cursor += 2;
        while depth > 0 && self.cursor < self.source.input.len() {
            if self.source.input[self.cursor..].starts_with("/*") {
                depth += 1;
                self.cursor += 2;
            } else if self.source.input[self.cursor..].starts_with("*/") {
                depth -= 1;
                self.cursor += 2;
            } else {
                self.cursor += self.peek_by(0).map_or(1, char::len_utf8);
            }
        }
        if depth > 0 {
            self.report(LexErrorKind::UnclosedComment, start, self.cursor);
        }
        self.next_token()
    }

    fn scan_slash_seq(&mut self, start: usize) -> (Token, Span) {
        match self.peek_by(1) {
            Some('/') => self.scan_line_comment(),
            Some('*') => self.scan_block_comment(start),
            _ => (self.scan_symbol().unwrap(), self.span(start, self.cursor)),
        }
    }

    fn scan_ident(&mut self) -> Token {
        let start = self.cursor;
        self.cursor = self.consume_while(start, |c| c == '_' || c.is_ascii_alphanumeric());
        let s = &self.source.input[start..self.cursor];
        if s == "_" {
            return Token::Underscore;
        }
        KEYWORDS
            .binary_search_by_key(&s, |k| k.0)
            .map(|i| KEYWORDS[i].1.clone())
            .unwrap_or_else(|_| Token::Ident(self.interner.intern(s)))
    }

    fn scan_number(&mut self) -> Token {
        let (start, (base, prefix, name)) = (self.cursor, self.scan_numeric_base());
        let mut end = self.consume_while(start + prefix, |c| c.is_digit(base) || c == '_');
        if end == start + prefix {
            self.report(LexErrorKind::MalformedNumber, start, end);
            self.cursor = end;
            return Token::LitInt(0);
        }
        let mut is_real = false;
        if base == 10 {
            end = self.scan_real_part(start, end);
            is_real = end > start + prefix
                && self.source.input[start + prefix..end].contains(['.', 'e', 'E']);
        }
        self.cursor = end;
        let (val, valid) = self.unitize_numeric(start, end);
        if !valid {
            self.report(
                LexErrorKind::MalformedUnderscore((if is_real { "real" } else { name }).into()),
                start,
                end,
            );
        }
        if is_real {
            Token::LitReal(self.interner.intern(&val))
        } else {
            Token::LitInt(self.interner.intern(&val))
        }
    }

    fn scan_text_lit(&mut self) -> Token {
        let start = self.cursor;
        match self.consume_quoted(start, 1, &['"'], LexErrorKind::UnclosedString) {
            Some(end) => {
                let content = self.unescape(&self.source.input[start + 1..end], start + 1);
                Token::LitString(self.interner.intern(&content))
            }
            None => self.invalid_token(start),
        }
    }

    fn scan_template_lit(&mut self, offset: usize) -> Token {
        let start = self.cursor;
        match self.consume_quoted(start, offset, &['"', '{'], LexErrorKind::UnclosedTemplate) {
            Some(end) => {
                let content =
                    self.unescape(&self.source.input[start + offset..end], start + offset);
                let s = self.interner.intern(&content);
                let is_head = offset == 2;
                match (is_head, self.source.input[end..].starts_with('{')) {
                    (true, true) => {
                        self.braces.push(BraceKind::Template);
                        Token::TemplateHead(s)
                    }
                    (false, true) => {
                        self.braces.push(BraceKind::Template);
                        Token::TemplateMiddle(s)
                    }
                    (true, false) => Token::LitTemplateNoSubst(s),
                    (false, false) => Token::TemplateTail(s),
                }
            }
            None => self.invalid_token(start),
        }
    }

    fn scan_rune_lit(&mut self) -> Token {
        let start = self.cursor;
        let mut chars = self.source.input[start + 1..].chars();
        let (c, len) = match chars.next() {
            Some('\'') => {
                self.report(LexErrorKind::InvalidRune, start, start + 2);
                self.cursor += 2;
                return Token::LitRune('\0');
            }
            Some('\\') => match self.scan_escape(&mut chars) {
                Ok((c, len)) => (c, len),
                Err((esc, len)) => {
                    self.report(
                        LexErrorKind::UnknownEscape(esc),
                        self.cursor + 1,
                        self.cursor + 1 + len,
                    );
                    ('\0', len)
                }
            },
            Some(c) => (c, c.len_utf8()),
            None => {
                self.report(LexErrorKind::UnclosedRune, start, start + 1);
                self.cursor = self.source.input.len();
                return Token::LitRune('\0');
            }
        };
        if self.source.input[start + 1 + len..].starts_with('\'') {
            self.cursor = start + len + 2;
            Token::LitRune(c)
        } else {
            let end = self.consume_while(start + 1 + len, |c| c != '\'');
            let unclosed = !self.source.input[end..].starts_with('\'');
            self.cursor = if unclosed { end } else { end + 1 };
            self.report(
                if unclosed {
                    LexErrorKind::UnclosedRune
                } else {
                    LexErrorKind::InvalidRune
                },
                start,
                self.cursor,
            );
            Token::LitRune('\0')
        }
    }

    fn scan_escaped_ident(&mut self) -> Token {
        let start = self.cursor;
        match self.consume_quoted(start, 1, &['`'], LexErrorKind::UnclosedEscapedIdent) {
            Some(end) => {
                let s = &self.source.input[start + 1..end];
                if s.is_empty() {
                    self.report(LexErrorKind::InvalidIdent, start, self.cursor);
                    self.invalid_token(start)
                } else {
                    Token::Ident(self.interner.intern(s))
                }
            }
            None => self.invalid_token(start),
        }
    }

    fn scan_symbol(&mut self) -> Option<Token> {
        let input = &self.source.input[self.cursor..];
        for (sym, tok) in SYMBOLS {
            if input.starts_with(sym) {
                self.cursor += sym.len();
                if *tok == Token::LBrace {
                    self.braces.push(BraceKind::Normal);
                } else if *tok == Token::RBrace && self.braces.last() == Some(&BraceKind::Normal) {
                    let _ = self.braces.pop();
                }
                return Some(tok.clone());
            }
        }
        None
    }

    fn consume_quoted(
        &mut self,
        start: usize,
        offset: usize,
        terms: &[char],
        err: LexErrorKind,
    ) -> Option<usize> {
        let mut pos = start + offset;
        let mut chars = self.source.input[pos..].chars();
        while let Some(c) = chars.next() {
            if terms.contains(&c) {
                self.cursor = pos + 1;
                return Some(pos);
            }
            pos += c.len_utf8();
            if c == '\\' {
                if let Some(n) = chars.next() {
                    pos += n.len_utf8();
                }
            }
        }
        self.cursor = self.source.input.len();
        self.report(err, start, start + 1);
        None
    }

    fn consume_while<F>(&self, start: usize, f: F) -> usize
    where
        F: Fn(char) -> bool,
    {
        self.source.input[start..]
            .char_indices()
            .find(|&(_, c)| !f(c))
            .map_or(self.source.input.len(), |(i, _)| start + i)
    }

    fn unitize_numeric(&self, start: usize, end: usize) -> (String, bool) {
        let s = &self.source.input[start..end];
        let mut out = String::with_capacity(s.len());
        let (mut prev, mut valid) = (false, true);
        for (i, c) in s.char_indices() {
            if c == '_' {
                if i == 0 || i == s.len() - 1 || prev {
                    valid = false;
                }
                prev = true;
            } else {
                prev = false;
                out.push(c);
            }
        }
        (out, valid)
    }

    fn unescape(&mut self, s: &str, start_pos: usize) -> String {
        let mut out = String::with_capacity(s.len());
        let mut chars = s.chars();
        let mut offset = 0;
        while let Some(c) = chars.next() {
            if c == '\\' {
                match self.scan_escape(&mut chars) {
                    Ok((esc, len)) => {
                        out.push(esc);
                        offset += 1 + len;
                    }
                    Err((esc, len)) => {
                        self.report(
                            LexErrorKind::UnknownEscape(esc),
                            start_pos + offset,
                            start_pos + offset + 1 + len,
                        );
                        offset += 1 + len;
                    }
                }
            } else {
                out.push(c);
                offset += c.len_utf8();
            }
        }
        out
    }

    fn scan_escape(
        &self,
        chars: &mut std::str::Chars<'_>,
    ) -> Result<(char, usize), (String, usize)> {
        let c = match chars.next() {
            Some(c) => c,
            None => return Ok(('\0', 0)),
        };
        if let Ok(i) = ESCAPES.binary_search_by_key(&c, |e| e.0) {
            return Ok((ESCAPES[i].1, 1));
        }
        match c {
            'x' => {
                let s: String = chars.by_ref().take(2).collect();
                match u32::from_str_radix(&s, 16).ok().and_then(char::from_u32) {
                    Some(c) => Ok((c, 3)),
                    None => Err((format!("x{}", s), 1 + s.len())),
                }
            }
            'u' if chars.next() == Some('{') => {
                let s: String = chars.by_ref().take_while(|&c| c != '}').collect();
                match u32::from_str_radix(&s, 16).ok().and_then(char::from_u32) {
                    Some(c) => Ok((c, s.len() + 3)),
                    None => Err((format!("u{{{}}}", s), s.len() + 3)),
                }
            }
            _ => Err((c.to_string(), 1)),
        }
    }

    fn report_invalid_char(&mut self, start: usize, c: char) -> (Token, Span) {
        self.cursor += c.len_utf8();
        self.report(LexErrorKind::UnknownChar(c), start, self.cursor);
        (self.invalid_token(start), self.span(start, self.cursor))
    }

    fn invalid_token(&mut self, start: usize) -> Token {
        Token::Invalid(self.interner.intern(&self.source.input[start..self.cursor]))
    }

    fn scan_numeric_base(&self) -> (u32, usize, &'static str) {
        match (self.peek_by(0), self.peek_by(1)) {
            (Some('0'), Some('x')) => (16, 2, "hexadecimal"),
            (Some('0'), Some('o')) => (8, 2, "octal"),
            (Some('0'), Some('b')) => (2, 2, "binary"),
            _ => (10, 0, "decimal"),
        }
    }

    fn scan_real_part(&mut self, start: usize, mut end: usize) -> usize {
        if self.source.input[end..].starts_with('.') && !self.source.input[end..].starts_with("..")
        {
            let next = end + 1;
            let after_dot = self.consume_while(next, |c| self.is_num_body(c));
            if after_dot == next {
                self.report(LexErrorKind::MalformedNumber, start, next);
            }
            end = after_dot;
        }
        if self.source.input[end..].starts_with(['e', 'E']) {
            let mut exp = end + 1;
            if self.source.input[exp..].starts_with(['+', '-']) {
                exp += 1;
            }
            let after_exp = self.consume_while(exp, |c| self.is_num_body(c));
            if after_exp == exp {
                self.report(LexErrorKind::MalformedNumber, start, after_exp);
            }
            end = after_exp;
        }
        end
    }

    fn report(&mut self, err: LexErrorKind, start: usize, end: usize) {
        self.errors
            .add(report(Error::new(err.into(), self.span(start, end))));
    }

    fn peek_by(&self, n: usize) -> Option<char> {
        self.source.input[self.cursor..].chars().nth(n)
    }

    fn advance_by(&mut self, n: usize) {
        self.cursor += n;
    }

    const fn span(&self, lo: usize, hi: usize) -> Span {
        Span::new(self.source.start + lo as u32, self.source.start + hi as u32)
    }

    const fn is_num_body(&self, c: char) -> bool {
        c.is_ascii_digit() || c == '_'
    }
}
