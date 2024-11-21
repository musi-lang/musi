use crate::{
    errors::{LexicalError, MusiError, MusiResult},
    span::Span,
    token::{Token, TokenKind},
};

pub struct Lexer {
    input: Vec<char>,
    start: usize,
    current: usize,

    line: usize,
    column: usize,

    indent_stack: Vec<usize>,
    line_start: bool,
}

impl Lexer {
    pub fn new(source: &str) -> Self {
        Self {
            input: source.chars().collect(),
            start: 0,
            current: 0,

            line: 1,
            column: 1,

            indent_stack: vec![0],
            line_start: true,
        }
    }

    pub fn tokenise(&mut self) -> MusiResult<Vec<Token>> {
        let mut tokens = vec![];

        while !self.is_at_end() {
            let token = self.next_token()?;
            tokens.push(token);
        }

        while self.indent_stack.len() > 1 {
            self.indent_stack.pop();
            tokens.push(self.make_token(TokenKind::Dedent, Some(""))?);
        }

        tokens.push(self.make_token(TokenKind::Eof, Some(""))?);
        Ok(tokens)
    }

    fn next_token(&mut self) -> MusiResult<Token> {
        if self.is_at_end() {
            self.make_token(TokenKind::Eof, None)?;
        }

        let ws_token = self.skip_whitespace()?;
        if ws_token.kind != TokenKind::Unknown {
            return Ok(ws_token);
        }

        self.start = self.current;

        match self.advance() {
            '(' => self.make_token(TokenKind::LeftParen, None),
            ')' => self.make_token(TokenKind::RightParen, None),
            '{' => self.make_token(TokenKind::LeftBrace, None),
            '}' => self.make_token(TokenKind::RightBrace, None),
            '[' => self.make_token(TokenKind::LeftBracket, None),
            ']' => self.make_token(TokenKind::RightBracket, None),
            ',' => self.make_token(TokenKind::Comma, None),
            ':' => {
                if self.match_char('=') {
                    self.make_token(TokenKind::ColonEquals, None)
                } else {
                    self.make_token(TokenKind::Colon, None)
                }
            }
            '.' => {
                if self.match_char('.') {
                    if self.match_char('=') {
                        self.make_token(TokenKind::DotDotEquals, None)
                    } else {
                        self.make_token(TokenKind::DotDot, None)
                    }
                } else {
                    self.make_token(TokenKind::Dot, None)
                }
            }

            '+' => {
                if self.match_char('=') {
                    self.make_token(TokenKind::PlusEquals, None)
                } else if self.match_char('+') {
                    self.make_token(TokenKind::PlusPlus, None)
                } else {
                    self.make_token(TokenKind::Plus, None)
                }
            }
            '-' => {
                if self.match_char('=') {
                    self.make_token(TokenKind::MinusEquals, None)
                } else {
                    self.make_token(TokenKind::Minus, None)
                }
            }
            '*' => {
                if self.match_char('*') {
                    if self.match_char('=') {
                        self.make_token(TokenKind::StarStarEquals, None)
                    } else {
                        self.make_token(TokenKind::StarStar, None)
                    }
                } else if self.match_char('=') {
                    self.make_token(TokenKind::StarEquals, None)
                } else {
                    self.make_token(TokenKind::Star, None)
                }
            }
            '/' => {
                if self.match_char('/') {
                    if self.match_char('=') {
                        self.make_token(TokenKind::SlashSlashEquals, None)
                    } else {
                        self.make_token(TokenKind::SlashSlash, None)
                    }
                } else if self.match_char('=') {
                    self.make_token(TokenKind::SlashEquals, None)
                } else {
                    self.make_token(TokenKind::Slash, None)
                }
            }
            '%' => {
                if self.match_char('=') {
                    self.make_token(TokenKind::PercentEquals, None)
                } else {
                    self.make_token(TokenKind::Percent, None)
                }
            }
            '&' => {
                if self.match_char('=') {
                    self.make_token(TokenKind::AmpersandEquals, None)
                } else {
                    self.make_token(TokenKind::Ampersand, None)
                }
            }
            '|' => {
                if self.match_char('=') {
                    self.make_token(TokenKind::PipeEquals, None)
                } else {
                    self.make_token(TokenKind::Pipe, None)
                }
            }
            '^' => {
                if self.match_char('=') {
                    self.make_token(TokenKind::CaretEquals, None)
                } else {
                    self.make_token(TokenKind::Caret, None)
                }
            }
            '~' => self.make_token(TokenKind::Tilde, None),
            '=' => {
                if self.match_char('=') {
                    self.make_token(TokenKind::EqualsEquals, None)
                } else {
                    self.make_token(TokenKind::Equals, None)
                }
            }
            '!' => {
                if self.match_char('=') {
                    self.make_token(TokenKind::BangEquals, None)
                } else {
                    self.make_token(TokenKind::Unknown, None)
                }
            }
            '<' => {
                if self.match_char('<') {
                    if self.match_char('=') {
                        self.make_token(TokenKind::LessLessEquals, None)
                    } else {
                        self.make_token(TokenKind::LessLess, None)
                    }
                } else if self.match_char('=') {
                    if self.match_char('>') {
                        self.make_token(TokenKind::LessEqualsLess, None)
                    } else {
                        self.make_token(TokenKind::LessEquals, None)
                    }
                } else {
                    self.make_token(TokenKind::Less, None)
                }
            }
            '>' => {
                if self.match_char('>') {
                    if self.match_char('=') {
                        self.make_token(TokenKind::GreaterGreaterEquals, None)
                    } else {
                        self.make_token(TokenKind::GreaterGreater, None)
                    }
                } else if self.match_char('=') {
                    self.make_token(TokenKind::GreaterEquals, None)
                } else {
                    self.make_token(TokenKind::Greater, None)
                }
            }

            '"' => {
                if self.peek() == 'r' {
                    self.read_raw_string()
                } else {
                    self.read_string()
                }
            }
            c => {
                if c.is_ascii_digit() {
                    self.read_number()
                } else if is_identifier_start(c) {
                    self.read_identifier()
                } else {
                    self.make_token(TokenKind::Unknown, None)
                }
            }
        }
    }
}

impl Lexer {
    #[inline]
    fn skip_whitespace(&mut self) -> MusiResult<Token> {
        let mut spaces = 0;

        if self.line_start {
            while !self.is_at_end() && self.peek().is_ascii_whitespace() && self.peek() != '\n' {
                spaces += 1;
                self.advance();
            }

            if !self.is_at_end() && self.peek() != '\n' {
                let current_indent = *self.indent_stack.last().unwrap();

                match spaces.cmp(&current_indent) {
                    std::cmp::Ordering::Greater => {
                        self.indent_stack.push(spaces);
                        return self.make_token(TokenKind::Indent, Some(&format!("{spaces}")));
                    }
                    std::cmp::Ordering::Less => {
                        if let Some(matching_level) =
                            self.indent_stack.iter().rev().position(|&x| x == spaces)
                        {
                            let dedents = self.indent_stack.len() - matching_level - 1;
                            self.indent_stack.truncate(matching_level + 1);
                            if dedents > 0 {
                                return self.make_token(TokenKind::Dedent, Some(""));
                            }
                        }
                    }
                    std::cmp::Ordering::Equal => {}
                }
            }
        }

        while !self.is_at_end() && self.peek().is_ascii_whitespace() {
            if self.peek() == '\n' {
                self.advance();
                self.line += 1;
                self.column = 1;
                self.line_start = true;
                return self.make_token(TokenKind::Newline, Some("\n"));
            }
            self.advance();
        }

        self.line_start = false;
        self.make_token(TokenKind::Unknown, None)
    }

    fn read_string(&mut self) -> MusiResult<Token> {
        let mut string = String::new();

        while !self.is_at_end() && self.peek() != '"' {
            let c = if self.peek() == '\\' {
                self.advance();
                self.read_escape_sequence()?
            } else {
                self.advance()
            };
            string.push(c);
        }

        if self.is_at_end() {
            return Err(MusiError::Lexical(LexicalError {
                message: "unclosed string literal".to_string(),
                span: self.make_span(),
            }));
        }

        self.make_token(TokenKind::StringLiteral, Some(&string))
    }

    fn read_raw_string(&mut self) -> MusiResult<Token> {
        self.advance_by(2); // r"
        let mut string = String::new();

        while !self.is_at_end() && self.peek() != '"' {
            string.push(self.advance());
        }

        if self.is_at_end() {
            return Err(MusiError::Lexical(LexicalError {
                message: "unclosed raw string literal".to_string(),
                span: self.make_span(),
            }));
        }

        self.make_token(TokenKind::StringLiteral, Some(&string))
    }

    fn read_escape_sequence(&mut self) -> MusiResult<char> {
        match self.advance() {
            'n' => Ok('\n'),
            'r' => Ok('\r'),
            't' => Ok('\t'),
            '0' => Ok('\0'),
            '"' => Ok('"'),
            '\'' => Ok('\''),
            '\\' => Ok('\\'),
            'x' => self.read_hex_escape(),
            'u' => self.read_unicode_escape(),
            found => Err(MusiError::Lexical(LexicalError {
                message: format!("invalid escape sequence '\\{found}'"),
                span: self.make_span(),
            })),
        }
    }

    fn read_hex_escape(&mut self) -> MusiResult<char> {
        let hex1 = self.advance().to_digit(16);
        let hex2 = self.advance().to_digit(16);

        match (hex1, hex2) {
            (Some(d1), Some(d2)) => char::from_u32(d1 * 16 + d2).ok_or_else(|| {
                MusiError::Lexical(LexicalError {
                    message: "invalid hex escape sequence".to_string(),
                    span: self.make_span(),
                })
            }),
            _ => Err(MusiError::Lexical(LexicalError {
                message: "invalid hex escape sequence".to_string(),
                span: self.make_span(),
            })),
        }
    }

    fn read_unicode_escape(&mut self) -> MusiResult<char> {
        if self.peek() != '{' {
            return Err(MusiError::Lexical(LexicalError {
                message: "expected '{' in unicode escape".to_string(),
                span: self.make_span(),
            }));
        }

        self.advance(); // {
        let mut value = 0u32;
        let mut digits = 0;

        while !self.is_at_end() && self.peek() != '}' && digits < 6 {
            if let Some(digit) = self.peek().to_digit(16) {
                value = value * 16 + digit;
                self.advance();
                digits += 1;
            } else {
                return Err(MusiError::Lexical(LexicalError {
                    message: "invalid unicode escape sequence".to_string(),
                    span: self.make_span(),
                }));
            }
        }

        if self.peek() != '}' {
            return Err(MusiError::Lexical(LexicalError {
                message: "unclosed unicode escape sequence".to_string(),
                span: self.make_span(),
            }));
        }

        self.advance(); // }
        char::from_u32(value).ok_or_else(|| {
            MusiError::Lexical(LexicalError {
                message: "invalid unicode code point".to_string(),
                span: self.make_span(),
            })
        })
    }

    fn read_number(&mut self) -> MusiResult<Token> {
        if self.peek() == '0' {
            match self.peek_next() {
                'x' | 'X' => return self.read_number_base(16),
                'o' | 'O' => return self.read_number_base(8),
                'b' | 'B' => return self.read_number_base(2),
                _ => {}
            }
        }

        while self.peek().is_ascii_digit() || self.peek() == '_' {
            self.advance();
        }

        if self.peek() == '.' {
            if self.peek_next() == '.' {
                return self.make_token(TokenKind::NumberLiteral, None);
            }
            if self.peek_next().is_ascii_digit() {
                self.advance();

                while self.peek().is_ascii_digit() || self.peek() == '_' {
                    self.advance();
                }
            }
        }

        if self.peek() == 'e' || self.peek() == 'E' {
            self.advance();
            if self.peek() == '-' || self.peek() == '+' {
                self.advance();
            }
            while self.peek().is_ascii_digit() || self.peek() == '_' {
                self.advance();
            }
        }

        self.make_token(TokenKind::NumberLiteral, None)
    }

    fn read_number_base(&mut self, base: u32) -> MusiResult<Token> {
        self.advance_by(2);

        let is_valid_digit = |c: char| match base {
            16 => c.is_ascii_hexdigit(),
            8 => ('0'..='7').contains(&c),
            2 => c == '0' || c == '1',
            _ => c.is_ascii_digit(),
        };

        let mut has_digits = false;
        while is_valid_digit(self.peek()) || self.peek() == '_' {
            if self.peek() != '_' {
                has_digits = true;
            }
            self.advance();
        }

        if !has_digits {
            return self.make_token(TokenKind::Unknown, None);
        }

        self.make_token(TokenKind::NumberLiteral, None)
    }

    fn read_identifier(&mut self) -> MusiResult<Token> {
        while is_identifier_continue(self.peek()) {
            self.advance();
        }

        let lexeme = self.input[self.start..self.current]
            .iter()
            .collect::<String>();
        let lexeme_str = lexeme.as_str();
        let kind = match lexeme_str {
            "and" => TokenKind::And,
            "as" => TokenKind::As,
            "async" => TokenKind::Async,
            "await" => TokenKind::Await,
            "break" => TokenKind::Break,
            "class" => TokenKind::Class,
            "const" => TokenKind::Const,
            "continue" => TokenKind::Continue,
            "def" => TokenKind::Def,
            "do" => TokenKind::Do,
            "else" => TokenKind::Else,
            "false" => TokenKind::False,
            "for" => TokenKind::For,
            "foreign" => TokenKind::Foreign,
            "from" => TokenKind::From,
            "if" => TokenKind::If,
            "implements" => TokenKind::Implements,
            "import" => TokenKind::Import,
            "in" => TokenKind::In,
            "inline" => TokenKind::Inline,
            "is" => TokenKind::Is,
            "inherits" => TokenKind::Inherits,
            "let" => TokenKind::Let,
            "match" => TokenKind::Match,
            "new" => TokenKind::New,
            "not" => TokenKind::Not,
            "or" => TokenKind::Or,
            "override" => TokenKind::Override,
            "protected" => TokenKind::Protected,
            "public" => TokenKind::Public,
            "ref" => TokenKind::Ref,
            "repeat" => TokenKind::Repeat,
            "return" => TokenKind::Return,
            "self" => TokenKind::Self_,
            "super" => TokenKind::Super,
            "then" => TokenKind::Then,
            "trait" => TokenKind::Trait,
            "true" => TokenKind::True,
            "type" => TokenKind::Type,
            "unsafe" => TokenKind::Unsafe,
            "until" => TokenKind::Until,
            "var" => TokenKind::Var,
            "when" => TokenKind::When,
            "where" => TokenKind::Where,
            "while" => TokenKind::While,
            "with" => TokenKind::With,
            "yield" => TokenKind::Yield,
            _ => TokenKind::Identifier,
        };

        self.make_token(kind, Some(lexeme_str))
    }
}

impl Lexer {
    #[inline]
    fn is_at_end(&self) -> bool {
        self.current >= self.input.len()
    }

    #[inline]
    fn peek(&self) -> char {
        self.input.get(self.current).copied().unwrap_or('\0')
    }

    #[inline]
    fn peek_next(&self) -> char {
        self.input.get(self.current + 1).copied().unwrap_or('\0')
    }

    #[inline]
    fn advance(&mut self) -> char {
        let c = self.peek();
        self.current += 1;
        self.column += 1;
        c
    }

    #[inline]
    fn advance_by(&mut self, n: usize) {
        self.current += n;
        self.column += n;
    }

    #[inline]
    fn match_char(&mut self, expected: char) -> bool {
        if self.is_at_end() || self.peek() != expected {
            false
        } else {
            self.advance();
            true
        }
    }

    fn make_span(&self) -> Span {
        Span::new(self.start, self.current, self.line, self.column)
    }

    fn make_token(&self, kind: TokenKind, lexeme: Option<&str>) -> MusiResult<Token> {
        let token = Token::new(
            kind,
            match lexeme {
                Some(lexeme) => lexeme.to_string(),
                None => self.input[self.start..self.current].iter().collect(),
            },
            self.make_span(),
        );
        Ok(token)
    }
}

#[inline]
const fn is_identifier_start(c: char) -> bool {
    c.is_ascii_alphabetic() || c == '_'
}

#[inline]
const fn is_identifier_continue(c: char) -> bool {
    c.is_ascii_alphanumeric() || c == '_'
}
