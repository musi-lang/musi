use crate::basic::{
    diagnostic::{DiagnosticBag, report},
    errors::{Error, LexErrorKind},
    interner::Interner,
    source::SourceFile,
    span::Span,
};
use crate::lex::{
    cursor::Cursor,
    token::{KEYWORDS, SYMBOLS, Token, TokenKind},
};

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
            errors: Default::default(),
            source,
            cursor: Cursor::new(&source.input),
            braces: vec![],
        }
    }

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
                c if is_id_start(c) => self.scan_ident(),
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
                    let _ = self.cursor.bump();
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
        self.cursor.eat_while(is_id_continue);
        let text = &self.source.input[start..self.cursor.pos()];

        if text == "_" {
            return TokenKind::Underscore;
        }

        KEYWORDS
            .binary_search_by_key(&text, |k| k.0)
            .map(|i| KEYWORDS[i].1)
            .unwrap_or_else(|_| TokenKind::Ident(self.interner.intern(text)))
    }

    fn scan_number(&mut self) -> TokenKind {
        let start = self.cursor.pos();
        let (base, prefix, name) = self.scan_numeric_base();
        self.cursor.bump_n(prefix);

        self.cursor.eat_while(|c| c.is_digit(base) || c == '_');
        let mut end = self.cursor.pos();

        if end == start + prefix {
            self.report(LexErrorKind::MalformedNumber, start, end);
            return TokenKind::LitInt(0);
        }

        let mut is_real = false;
        if base == 10 {
            if self.cursor.is_next('.') && self.cursor.peek_nth(1) != Some('.') {
                let _ = self.cursor.bump();
                let before = self.cursor.pos();
                self.cursor.eat_while(is_num_body);
                if self.cursor.pos() == before {
                    self.report(LexErrorKind::MalformedNumber, start, self.cursor.pos());
                }
            }
            if matches!(self.cursor.peek(), Some('e' | 'E')) {
                let _ = self.cursor.bump();
                if matches!(self.cursor.peek(), Some('+' | '-')) {
                    let _ = self.cursor.bump();
                }
                let before = self.cursor.pos();
                self.cursor.eat_while(is_num_body);
                if self.cursor.pos() == before {
                    self.report(LexErrorKind::MalformedNumber, start, self.cursor.pos());
                }
            }
            end = self.cursor.pos();
            let slice = &self.source.input[start + prefix..end];
            is_real = slice.contains('.') || slice.contains('e') || slice.contains('E');
        }

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

    fn scan_numeric_base(&self) -> (u32, usize, &'static str) {
        match (self.cursor.peek(), self.cursor.peek_nth(1)) {
            (Some('0'), Some('x')) => (16, 2, "hexadecimal"),
            (Some('0'), Some('o')) => (8, 2, "octal"),
            (Some('0'), Some('b')) => (2, 2, "binary"),
            _ => (10, 0, "decimal"),
        }
    }

    fn unitize_numeric(&self, start: usize, end: usize) -> (String, bool) {
        let s = &self.source.input[start..end];
        let mut out = String::with_capacity(s.len());
        let (mut prev_under, mut valid) = (false, true);
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
        let rest = self.cursor.rest();
        for (sym, kind) in SYMBOLS {
            if rest.starts_with(sym) {
                self.cursor.bump_n(sym.len());
                if *kind == TokenKind::LBrace {
                    self.braces.push(BraceKind::Normal);
                } else if *kind == TokenKind::RBrace
                    && self.braces.last() == Some(&BraceKind::Normal)
                {
                    let _ = self.braces.pop();
                }
                return *kind;
            }
        }

        let c = self.cursor.bump().unwrap_or('\0');
        self.report(
            LexErrorKind::UnknownChar(c),
            self.cursor.pos() - c.len_utf8(),
            self.cursor.pos(),
        );
        TokenKind::Invalid(self.interner.intern(c.to_string().as_str()))
    }

    fn scan_text_lit(&mut self) -> TokenKind {
        let start = self.cursor.pos();
        if let Some(content_end) =
            self.consume_quoted(start, 1, &['"'], LexErrorKind::UnclosedString)
        {
            let raw = &self.source.input[start + 1..content_end];
            let val = unescape(raw, start + 1, &mut self.errors);
            TokenKind::LitString(self.interner.intern(&val))
        } else {
            TokenKind::Invalid(
                self.interner
                    .intern(&self.source.input[start..self.cursor.pos()]),
            )
        }
    }

    fn scan_rune_lit(&mut self) -> TokenKind {
        let start = self.cursor.pos();
        let _ = self.cursor.bump();

        let mut val = '\0';

        match self.cursor.peek() {
            Some('\'') => {
                self.report(LexErrorKind::InvalidRune, start, start + 2);
                let _ = self.cursor.bump();
            }
            Some('\\') => {
                let _ = self.cursor.bump();
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
                let _ = self.cursor.bump();
            }
            None => {}
        }

        if self.cursor.is_next('\'') {
            let _ = self.cursor.bump();
        } else {
            self.cursor.eat_while(|c| c != '\'');
            if self.cursor.is_next('\'') {
                let _ = self.cursor.bump();
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
            let raw = &self.source.input[start + offset..content_end];
            let val = unescape(raw, start + offset, &mut self.errors);
            let s = self.interner.intern(&val);

            let is_head = offset == 2;
            let follows_brace = self.source.input[content_end..].starts_with('{');
            if follows_brace {
                self.braces.push(BraceKind::Template);
                match is_head {
                    true => TokenKind::TemplateHead(s),
                    false => TokenKind::TemplateMiddle(s),
                }
            } else {
                match is_head {
                    true => TokenKind::LitTemplateNoSubst(s),
                    false => TokenKind::TemplateTail(s),
                }
            }
        } else {
            TokenKind::Invalid(
                self.interner
                    .intern(&self.source.input[start..self.cursor.pos()]),
            )
        }
    }

    fn scan_escaped_ident(&mut self) -> TokenKind {
        let start = self.cursor.pos();
        if let Some(content_end) =
            self.consume_quoted(start, 1, &['`'], LexErrorKind::UnclosedEscapedIdent)
        {
            let s = &self.source.input[start + 1..content_end];
            if s.is_empty() {
                self.report(LexErrorKind::InvalidIdent, start, self.cursor.pos());
                TokenKind::Invalid(self.interner.intern(""))
            } else {
                TokenKind::Ident(self.interner.intern(s))
            }
        } else {
            TokenKind::Invalid(
                self.interner
                    .intern(&self.source.input[start..self.cursor.pos()]),
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
                    let _ = self.cursor.bump();
                    return Some(pos);
                }
                Some('\\') => {
                    let _ = self.cursor.bump();
                    let _ = self.cursor.bump();
                }
                Some(_) => {
                    let _ = self.cursor.bump();
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
            .add(report(Error::new(err.into(), self.span(start, end))));
    }

    const fn span(&self, lo: usize, hi: usize) -> Span {
        Span::new(self.source.start + lo as u32, self.source.start + hi as u32)
    }
}

const fn is_id_start(c: char) -> bool {
    c.is_ascii_alphabetic() || c == '_'
}

const fn is_id_continue(c: char) -> bool {
    c.is_ascii_alphanumeric() || c == '_'
}

const fn is_num_body(c: char) -> bool {
    c.is_ascii_digit() || c == '_'
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
                        (start_pos + offset) as u32,
                        (start_pos + offset + 1 + len) as u32,
                    );
                    errors.add(report(Error::new(
                        LexErrorKind::UnknownEscape(esc_err).into(),
                        err_span,
                    )));
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

pub fn scan_escape(chars: &mut std::str::Chars<'_>) -> Result<(char, usize), (String, usize)> {
    let ch = match chars.next() {
        Some(c) => c,
        None => return Ok(('\0', 0)),
    };

    if let Ok(i) = ESCAPES.binary_search_by_key(&ch, |e| e.0) {
        return Ok((ESCAPES[i].1, 1));
    }

    match ch {
        'x' => {
            let s: String = chars.clone().take(2).collect();
            if s.len() < 2 {
                return Err((format!("x{s}"), 1 + s.len()));
            }

            let _ = chars.next();
            let _ = chars.next();

            match u32::from_str_radix(&s, 16).ok().and_then(char::from_u32) {
                Some(c) => Ok((c, 3)),
                None => Err((format!("x{s}"), 3)),
            }
        }
        'u' => {
            if chars.clone().next() == Some('{') {
                let _ = chars.next();
                let mut s = String::new();
                let mut len = 0;
                while let Some(c) = chars.next() {
                    if c == '}' {
                        match u32::from_str_radix(&s, 16).ok().and_then(char::from_u32) {
                            Some(c) => return Ok((c, s.len() + 3)),
                            None => return Err((format!("u{{{s}}}"), s.len() + 3)),
                        }
                    }
                    s.push(c);
                    len += c.len_utf8();
                    if s.len() > 6 {
                        break;
                    }
                }
                // len used here for error span calcing
                Err((format!("u{{{s}}}"), len + 2))
            } else {
                Err(("u".to_string(), 1))
            }
        }
        _ => Err((ch.to_string(), 1)),
    }
}
