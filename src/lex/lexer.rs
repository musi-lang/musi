use crate::basic::{
    diagnostic::{DiagnosticBag, report},
    errors::{Error, LexErrorKind},
    interner::Interner,
    source::SourceFile,
    span::Span,
};
use crate::lex::token::Token;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum BraceKind {
    Normal,
    Template,
}

const KEYWORDS: &[(&str, Token)] = &[
    ("alias", Token::KwAlias),
    ("and", Token::KwAnd),
    ("as", Token::KwAs),
    ("break", Token::KwBreak),
    ("case", Token::KwCase),
    ("cycle", Token::KwCycle),
    ("defer", Token::KwDefer),
    ("else", Token::KwElse),
    ("extern", Token::KwExtern),
    ("false", Token::KwFalse),
    ("fn", Token::KwFn),
    ("for", Token::KwFor),
    ("if", Token::KwIf),
    ("import", Token::KwImport),
    ("in", Token::KwIn),
    ("is", Token::KwIs),
    ("match", Token::KwMatch),
    ("mod", Token::KwMod),
    ("not", Token::KwNot),
    ("or", Token::KwOr),
    ("record", Token::KwRecord),
    ("return", Token::KwReturn),
    ("sum", Token::KwSum),
    ("true", Token::KwTrue),
    ("try", Token::KwTry),
    ("unsafe", Token::KwUnsafe),
    ("val", Token::KwVal),
    ("var", Token::KwVar),
    ("while", Token::KwWhile),
    ("with", Token::KwWith),
];

const SYMBOLS: &[(&str, Token)] = &[
    ("..<", Token::DotDotLt),
    ("<<", Token::LtLt),
    ("<=", Token::LtEq),
    ("<-", Token::LtMinus),
    ("::", Token::ColonColon),
    (":=", Token::ColonEq),
    ("=>", Token::EqGt),
    (">=", Token::GtEq),
    (">>", Token::GtGt),
    (">]", Token::GtRBrack),
    ("??", Token::QuestionQuestion),
    (".^", Token::DotCaret),
    ("..", Token::DotDot),
    ("/=", Token::SlashEq),
    ("->", Token::MinusGt),
    ("**", Token::StarStar),
    ("[<", Token::LBrackLt),
    ("|>", Token::BarGt),
    ("%", Token::Percent),
    ("&", Token::Amp),
    ("(", Token::LParen),
    (")", Token::RParen),
    ("*", Token::Star),
    ("+", Token::Plus),
    (",", Token::Comma),
    ("-", Token::Minus),
    (".", Token::Dot),
    ("/", Token::Slash),
    (":", Token::Colon),
    (";", Token::Semicolon),
    ("<", Token::Lt),
    ("=", Token::Eq),
    (">", Token::Gt),
    ("?", Token::Question),
    ("@", Token::At),
    ("[", Token::LBrack),
    ("]", Token::RBrack),
    ("^", Token::Caret),
    ("_", Token::Underscore),
    ("{", Token::LBrace),
    ("|", Token::Bar),
    ("}", Token::RBrace),
    ("~", Token::Tilde),
    ("$", Token::Dollar),
];

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
            c if c == '_' || c.is_alphabetic() => {
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
        self.advance_by(2);
        while depth > 0 && self.cursor < self.source.input.len() {
            if self.source.input[self.cursor..].starts_with("/*") {
                depth += 1;
                self.advance_by(2);
            } else if self.source.input[self.cursor..].starts_with("*/") {
                depth -= 1;
                self.advance_by(2);
            } else {
                self.advance_by(self.peek_by(0).map_or(1, char::len_utf8));
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
        self.cursor = self.consume_while(start, |c| c == '_' || c.is_alphanumeric());
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
        let start = self.cursor;
        let (base, prefix, name) = match (self.peek_by(0), self.peek_by(1)) {
            (Some('0'), Some('x')) => (16, 2, "hexadecimal"),
            (Some('0'), Some('o')) => (8, 2, "octal"),
            (Some('0'), Some('b')) => (2, 2, "binary"),
            _ => (10, 0, "decimal"),
        };
        let mut end = self.consume_while(start + prefix, |c| c.is_digit(base) || c == '_');
        if end == start + prefix {
            self.report(LexErrorKind::InvalidLiteral(name.into()), start, start + 1);
            return Token::LitInt(0);
        }
        let mut is_real = false;
        if base == 10 {
            if self.source.input[end..].starts_with('.')
                && !self.source.input[end..].starts_with("..")
            {
                is_real = true;
                end = self.consume_while(end + 1, |c| c.is_ascii_digit() || c == '_');
            }
            if self.source.input[end..].starts_with(['e', 'E']) {
                is_real = true;
                let mut exp = end + 1;
                if self.source.input[exp..].starts_with(['+', '-']) {
                    exp += 1;
                }
                end = self.consume_while(exp, |c| c.is_ascii_digit() || c == '_');
            }
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
            Some(end) => Token::LitString(
                self.interner
                    .intern(&self.unescape(&self.source.input[start + 1..end])),
            ),
            None => self.invalid_token(start),
        }
    }

    fn scan_template_lit(&mut self, offset: usize) -> Token {
        let start = self.cursor;
        match self.consume_quoted(start, offset, &['"', '{'], LexErrorKind::UnclosedTemplate) {
            Some(end) => {
                let s = self
                    .interner
                    .intern(&self.unescape(&self.source.input[start + offset..end]));
                if self.source.input[end..].starts_with('{') {
                    self.braces.push(BraceKind::Template);
                    if offset == 2 {
                        Token::TemplateHead(s)
                    } else {
                        Token::TemplateMiddle(s)
                    }
                } else if offset == 2 {
                    Token::LitTemplateNoSubst(s)
                } else {
                    Token::TemplateTail(s)
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
                self.report(LexErrorKind::EmptyRune, start, start + 2);
                self.cursor += 2;
                return Token::LitRune('\0');
            }
            Some('\\') => self.scan_escape(&mut chars),
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
                    LexErrorKind::MultiCharRune
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
                match tok {
                    Token::LBrace => self.braces.push(BraceKind::Normal),
                    Token::RBrace if self.braces.last() == Some(&BraceKind::Normal) => {
                        self.braces.pop();
                    }
                    _ => {}
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

    fn unescape(&self, s: &str) -> String {
        let mut out = String::with_capacity(s.len());
        let mut chars = s.chars();
        while let Some(c) = chars.next() {
            if c == '\\' {
                out.push(self.scan_escape(&mut chars).0);
            } else {
                out.push(c);
            }
        }
        out
    }

    fn scan_escape(&self, chars: &mut std::str::Chars<'_>) -> (char, usize) {
        let c = match chars.next() {
            Some(c) => c,
            None => return ('\0', 0),
        };
        if let Ok(i) = ESCAPES.binary_search_by_key(&c, |e| e.0) {
            return (ESCAPES[i].1, 1);
        }
        match c {
            'x' => {
                let s: String = chars.by_ref().take(2).collect();
                (
                    u32::from_str_radix(&s, 16)
                        .ok()
                        .and_then(char::from_u32)
                        .unwrap_or('\0'),
                    3,
                )
            }
            'u' if chars.next() == Some('{') => {
                let s: String = chars.by_ref().take_while(|&c| c != '}').collect();
                (
                    u32::from_str_radix(&s, 16)
                        .ok()
                        .and_then(char::from_u32)
                        .unwrap_or('\0'),
                    s.len() + 3,
                )
            }
            _ => (c, 1),
        }
    }

    fn report_invalid_char(&mut self, start: usize, c: char) -> (Token, Span) {
        self.advance_by(c.len_utf8());
        self.report(LexErrorKind::InvalidChar(c), start, self.cursor);
        (self.invalid_token(start), self.span(start, self.cursor))
    }

    fn invalid_token(&mut self, start: usize) -> Token {
        Token::Invalid(self.interner.intern(&self.source.input[start..self.cursor]))
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
}
