use crate::{
    ast::{
        Expression, ExpressionKind, Node, Program, Statement, StatementKind, VariableDeclarator,
    },
    errors::{MusiError, MusiResult, SyntaxError},
    span::Span,
    token::{Kind, LiteralKind, Token},
    value::Value,
};

#[derive(Debug, Clone, Copy, PartialEq, PartialOrd)]
enum Precedence {
    None,
    Assignment,  // := <-
    Conditional, // if ... then ... else if ... else ...
    Or,
    And,
    Equality,   // = /=
    Comparison, // < > <= <=> >=
    Term,       // + -
    Factor,     // * /
    Unary,      // not -
    Call,       // . ()
    Primary,
}

pub struct Parser {
    tokens: Vec<Token>,
    current: usize,
}

impl Parser {
    pub const fn new(tokens: Vec<Token>) -> Self {
        Self { tokens, current: 0 }
    }

    pub fn parse(&mut self) -> MusiResult<Program> {
        let start_span = self.peek().span;
        let mut body = vec![];

        while !self.is_at_end() {
            match self.peek().kind {
                Kind::Eof => break,
                Kind::Newline => {
                    self.advance();
                    continue;
                }
                _ => body.push(self.parse_declaration()?),
            }
        }

        Ok(Node {
            kind: body,
            span: Span {
                start: start_span.start,
                end: self.previous().span.end,
            },
        })
    }

    const fn precedence(token: &Token) -> Precedence {
        match token.kind {
            Kind::LessMinus => Precedence::Assignment,
            Kind::Plus | Kind::Minus => Precedence::Term,
            Kind::Star | Kind::Slash => Precedence::Factor,
            _ => Precedence::None,
        }
    }

    fn parse_declaration(&mut self) -> MusiResult<Statement> {
        let start_span = self.peek().span;
        let kind = match self.peek().kind {
            Kind::Let => self.parse_variable_declaration(false)?,
            Kind::Var => self.parse_variable_declaration(true)?,
            _ => self.parse_statement()?,
        };

        Ok(Node {
            kind,
            span: Span {
                start: start_span.start,
                end: self.previous().span.end,
            },
        })
    }

    fn parse_expression(&mut self) -> MusiResult<Expression> {
        self.parse_precedence(Precedence::Assignment)
    }

    fn parse_precedence(&mut self, precedence: Precedence) -> MusiResult<Expression> {
        let mut expression = self.parse_primary()?;

        while precedence <= Self::precedence(self.peek()) {
            let operator = self.advance().kind;
            let right = self.parse_expression()?;

            expression = Node {
                span: Span {
                    start: expression.span.start,
                    end: right.span.end,
                },
                kind: ExpressionKind::Operation {
                    left: Box::new(expression),
                    operator,
                    right: Box::new(right),
                },
            };
        }

        Ok(expression)
    }

    fn parse_primary(&mut self) -> MusiResult<Expression> {
        let token = self.advance().clone();
        let span = token.span;

        let kind = match token.kind {
            Kind::Identifier => ExpressionKind::Identifier(token.lexeme.to_string()),
            Kind::Literal(kind) => {
                ExpressionKind::Literal(Self::parse_literal_value(&token, kind)?)
            }
            Kind::True => ExpressionKind::Literal(Value::Boolean(true)),
            Kind::False => ExpressionKind::Literal(Value::Boolean(false)),
            unexpected => {
                return Err(MusiError::Syntax(SyntaxError {
                    message: Box::leak(
                        format!("expected expression, found '{unexpected:?}'").into_boxed_str(),
                    ),
                }))
            }
        };

        Ok(Node { kind, span })
    }

    fn parse_statement(&mut self) -> MusiResult<StatementKind> {
        Ok(StatementKind::Expression(self.parse_expression()?))
    }

    fn parse_literal_value(token: &Token, kind: LiteralKind) -> MusiResult<Value> {
        match kind {
            LiteralKind::Number => Self::parse_number_literal(token),
            LiteralKind::String => Self::parse_string_literal(token),
            LiteralKind::Character => Self::parse_character_literal(token),
        }
    }

    fn parse_number_literal(token: &Token) -> MusiResult<Value> {
        let lexeme = &token.lexeme;

        if lexeme.contains('.') {
            lexeme
                .parse::<f64>()
                .map(|n| {
                    if n.is_finite() && (f64::MIN..=f64::MAX).contains(&n) {
                        Ok(Value::Real(n))
                    } else {
                        Err(MusiError::Syntax(SyntaxError {
                            message: "number literal out of range",
                        }))
                    }
                })
                .map_err(|_| {
                    MusiError::Syntax(SyntaxError {
                        message: "invalid number literal",
                    })
                })?
        } else if lexeme.starts_with('-') {
            lexeme
                .parse::<i64>()
                .map(|n| {
                    if (i64::MIN..=i64::MAX).contains(&n) {
                        Ok(Value::Integer(n))
                    } else {
                        Err(MusiError::Syntax(SyntaxError {
                            message: "number literal out of range",
                        }))
                    }
                })
                .map_err(|_| {
                    MusiError::Syntax(SyntaxError {
                        message: "invalid number literal",
                    })
                })?
        } else {
            lexeme
                .parse::<u64>()
                .map(|n| Ok(Value::Natural(n)))
                .map_err(|_| {
                    MusiError::Syntax(SyntaxError {
                        message: "invalid number literal",
                    })
                })?
        }
    }

    fn parse_string_literal(token: &Token) -> MusiResult<Value> {
        let lexeme = token.lexeme.trim_matches('"');

        if lexeme.len() > usize::MAX / 4 {
            return Err(MusiError::Syntax(SyntaxError {
                message: "string literal too long",
            }));
        }

        let valid_utf8 = lexeme.chars().all(|c| {
            let bytes = c.to_string().as_bytes().to_vec();
            String::from_utf8(bytes).is_ok()
        });
        if !valid_utf8 {
            return Err(MusiError::Syntax(SyntaxError {
                message: "invalid UTF-8 sequence in string literal",
            }));
        }

        let chars: Vec<char> = lexeme.chars().collect();
        if chars.iter().any(|&c| c as u32 > 0x0010_FFFF) {
            return Err(MusiError::Syntax(SyntaxError {
                message: "invalid unicode character in string literal",
            }));
        }

        Ok(Value::String(chars))
    }

    fn parse_character_literal(token: &Token) -> MusiResult<Value> {
        let lexeme = token.lexeme.trim_matches('\'');

        let ch = match lexeme.chars().next() {
            Some(c) if c as u32 <= 0x0010_FFFF => c,
            Some(_) => {
                return Err(MusiError::Syntax(SyntaxError {
                    message: "invalid unicode character in character literal",
                }))
            }
            None => unreachable!("empty character literal should be caught by lexer"),
        };

        if lexeme.chars().count() > 1 {
            return Err(MusiError::Syntax(SyntaxError {
                message: "character literal contains multiple characters",
            }));
        }

        Ok(Value::Character(ch))
    }

    fn parse_variable_declaration(&mut self, mutable: bool) -> MusiResult<StatementKind> {
        self.advance(); // consume 'let' or 'var'

        let mut declarations = Vec::new();

        loop {
            let name = self
                .consume(Kind::Identifier, "expected variable name")?
                .lexeme
                .to_string();

            self.consume(Kind::ColonEquals, "expected ':=' after variable name")?;

            let initialiser = self.parse_expression()?;

            declarations.push(VariableDeclarator { name, initialiser });

            if self.peek().kind != Kind::Comma {
                break;
            }
            self.advance();
        }

        Ok(StatementKind::VariableDeclaration {
            declarations,
            mutable,
        })
    }
}

impl Parser {
    #[inline]
    fn peek(&self) -> &Token {
        &self.tokens[self.current]
    }

    #[inline]
    fn is_at_end(&self) -> bool {
        self.peek().kind == Kind::Eof
    }

    #[inline]
    fn advance(&mut self) -> &Token {
        if !self.is_at_end() {
            self.current += 1;
        }
        self.previous()
    }

    #[inline]
    fn previous(&self) -> &Token {
        &self.tokens[self.current - 1]
    }

    #[inline]
    fn consume(&mut self, kind: Kind, message: &'static str) -> MusiResult<&Token> {
        if self.peek().kind == kind {
            Ok(self.advance())
        } else {
            Err(MusiError::Syntax(SyntaxError { message }))
        }
    }
}
