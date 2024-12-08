use crate::{
    ast::{Expression, ExpressionKind, Program, Statement, StatementKind, VariableDeclarator},
    errors::{MusiError, MusiResult, SyntaxError},
    span::Span,
    token::{Kind, LiteralKind, Token},
    value::Value,
};

#[derive(Debug, Clone, Copy, PartialEq, PartialOrd)]
enum Precedence {
    None,
    Assignment, // <-
    Or,         // or
    And,        // and
    Equality,   // = /=
    Comparison, // < > <= <=> >=
    Term,       // + -
    Factor,     // * /
    Unary,      // not -
    Call,       // . ()
}

impl From<Kind> for Precedence {
    fn from(kind: Kind) -> Self {
        match kind {
            Kind::LessMinus => Self::Assignment,

            Kind::Or | Kind::Xor => Self::Or,
            Kind::And => Self::And,

            Kind::Equals | Kind::SlashEquals | Kind::TildeEquals => Self::Equality,

            Kind::Less
            | Kind::Greater
            | Kind::LessEquals
            | Kind::GreaterEquals
            | Kind::LessEqualsGreater => Self::Comparison,

            Kind::Plus | Kind::Minus => Self::Term,
            Kind::Star | Kind::Slash => Self::Factor,

            Kind::Not => Self::Unary,
            Kind::Dot | Kind::LeftParen => Self::Call,

            _ => Self::None,
        }
    }
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
        let mut statements = vec![];

        while !self.is_at_end() {
            match self.peek().kind {
                Kind::Eof => break,
                Kind::Newline | Kind::Indent | Kind::Dedent => {
                    self.advance();
                    continue;
                }
                _ => statements.push(self.parse_declaration()?),
            }
        }

        Ok(Program {
            statements,
            span: Span {
                start: start_span.start,
                end: self.previous().span.end,
            },
        })
    }

    fn skip_whitespace(&mut self) {
        while matches!(
            self.peek().kind,
            Kind::Newline | Kind::Indent | Kind::Dedent
        ) {
            self.advance();
        }
    }

    fn parse_declaration(&mut self) -> MusiResult<Statement> {
        let start_span = self.peek().span;
        let kind = match self.peek().kind {
            Kind::Const => self.parse_variable_declaration(false)?,
            Kind::Var => self.parse_variable_declaration(true)?,
            Kind::If => StatementKind::Expression(self.parse_conditional()?),
            _ => self.parse_statement()?,
        };

        Ok(Statement {
            kind,
            span: Span {
                start: start_span.start,
                end: self.previous().span.end,
            },
        })
    }

    fn parse_variable_declaration(&mut self, mutable: bool) -> MusiResult<StatementKind> {
        let start_span = self.peek().span;
        self.advance(); // consume 'const' or 'var'

        let mut declarations = vec![];

        loop {
            let name_token = self.consume(Kind::Identifier, "expected variable name")?;
            let name = name_token.lexeme.clone();
            let declaration_span = name_token.span;

            self.consume(Kind::ColonEquals, "expected ':=' after variable name")?;

            let initialiser = self.parse_expression()?;

            declarations.push(VariableDeclarator {
                name,
                initialiser,
                span: declaration_span,
            });

            if self.peek().kind != Kind::Comma {
                break;
            }
            self.advance();
        }

        Ok(StatementKind::VariableDeclaration {
            declarations,
            mutable,
            span: Span {
                start: start_span.start,
                end: self.previous().span.end,
            },
        })
    }

    fn parse_conditional(&mut self) -> MusiResult<Expression> {
        let start_span = self.peek().span;
        self.advance(); // consume 'if'

        let condition = self.parse_expression()?;

        self.skip_whitespace();
        self.consume(Kind::Then, "expected 'then' after condition")?;
        self.skip_whitespace();

        let consequence = self.parse_expression()?;

        self.skip_whitespace();

        let alternative = if self.peek().kind == Kind::Else {
            self.advance();

            self.skip_whitespace();

            match self.peek().kind {
                Kind::If => self.parse_conditional()?, // else-if
                _ => self.parse_expression()?,
            }
        } else {
            Expression {
                kind: ExpressionKind::Literal {
                    value: Value::Unit,
                    span: self.previous().span,
                },
                span: self.previous().span,
            }
        };

        let span = Span {
            start: start_span.start,
            end: self.previous().span.end,
        };

        Ok(Expression {
            kind: ExpressionKind::Conditional {
                condition: Box::new(condition),
                consequence: Box::new(consequence),
                alternative: Some(Box::new(alternative)),
                span,
            },
            span,
        })
    }

    fn parse_expression(&mut self) -> MusiResult<Expression> {
        self.parse_precedence(Precedence::Assignment)
    }

    fn parse_precedence(&mut self, precedence: Precedence) -> MusiResult<Expression> {
        let mut expression = self.parse_assignment()?;

        while precedence <= Precedence::from(self.peek().kind) {
            let start_span = expression.span;
            expression = match self.peek().kind {
                Kind::LeftParen => self.parse_function_call(expression)?,
                Kind::Dot => self.parse_call(start_span, expression)?,
                _ => break,
            };
        }

        Ok(expression)
    }

    fn parse_assignment(&mut self) -> MusiResult<Expression> {
        let mut expression = self.parse_or()?;

        if matches!(self.peek().kind, Kind::LessMinus) {
            let start_span = expression.span;

            // left-hand side is identifier
            if !matches!(expression.kind, ExpressionKind::Identifier { .. }) {
                return Err(MusiError::Syntax(SyntaxError {
                    message: "invalid assignment target",
                }));
            }

            let value = self.parse_assignment()?; // R associative
            let span = Span {
                start: start_span.start,
                end: value.span.end,
            };

            expression = Expression {
                span,
                kind: ExpressionKind::Assignment {
                    target: Box::new(expression),
                    value: Box::new(value),
                    span,
                },
            };
        }

        Ok(expression)
    }

    fn parse_or(&mut self) -> MusiResult<Expression> {
        let mut expression = self.parse_and()?;

        while matches!(self.peek().kind, Kind::Or | Kind::Xor) {
            let operator = self.advance().kind;
            let start_span = expression.span;
            let right = self.parse_and()?;
            let span = Span {
                start: start_span.start,
                end: right.span.end,
            };

            expression = Expression {
                span,
                kind: ExpressionKind::Operation {
                    left: Box::new(expression),
                    operator,
                    right: Box::new(right),
                    span,
                },
            };
        }

        Ok(expression)
    }

    fn parse_and(&mut self) -> MusiResult<Expression> {
        let mut expression = self.parse_equality()?;

        while matches!(self.peek().kind, Kind::And) {
            let operator = self.advance().kind;
            let start_span = expression.span;
            let right = self.parse_equality()?;
            let span = Span {
                start: start_span.start,
                end: right.span.end,
            };

            expression = Expression {
                span,
                kind: ExpressionKind::Operation {
                    left: Box::new(expression),
                    operator,
                    right: Box::new(right),
                    span,
                },
            };
        }

        Ok(expression)
    }

    fn parse_equality(&mut self) -> MusiResult<Expression> {
        let mut expression = self.parse_comparison()?;

        while matches!(
            self.peek().kind,
            Kind::Equals | Kind::SlashEquals | Kind::TildeEquals
        ) {
            let operator = self.advance().kind;
            let start_span = expression.span;
            let right = self.parse_comparison()?;
            let span = Span {
                start: start_span.start,
                end: right.span.end,
            };

            expression = Expression {
                span,
                kind: ExpressionKind::Operation {
                    left: Box::new(expression),
                    operator,
                    right: Box::new(right),
                    span,
                },
            };
        }

        Ok(expression)
    }

    fn parse_comparison(&mut self) -> MusiResult<Expression> {
        let mut expression = self.parse_term()?;

        while matches!(
            self.peek().kind,
            Kind::Less
                | Kind::Greater
                | Kind::LessEquals
                | Kind::GreaterEquals
                | Kind::LessEqualsGreater
        ) {
            let operator = self.advance().kind;
            let start_span = expression.span;
            let right = self.parse_term()?;
            let span = Span {
                start: start_span.start,
                end: right.span.end,
            };

            expression = Expression {
                span,
                kind: ExpressionKind::Operation {
                    left: Box::new(expression),
                    operator,
                    right: Box::new(right),
                    span,
                },
            };
        }

        Ok(expression)
    }

    fn parse_term(&mut self) -> MusiResult<Expression> {
        let mut expression = self.parse_factor()?;

        while matches!(self.peek().kind, Kind::Plus | Kind::Minus) {
            let operator = self.advance().kind;
            let start_span = expression.span;
            let right = self.parse_factor()?;
            let span = Span {
                start: start_span.start,
                end: right.span.end,
            };

            expression = Expression {
                span,
                kind: ExpressionKind::Operation {
                    left: Box::new(expression),
                    operator,
                    right: Box::new(right),
                    span,
                },
            };
        }

        Ok(expression)
    }

    fn parse_factor(&mut self) -> MusiResult<Expression> {
        let mut expression = self.parse_unary()?;

        while matches!(self.peek().kind, Kind::Star | Kind::Slash) {
            let operator = self.advance().kind;
            let start_span = expression.span;
            let right = self.parse_unary()?;
            let span = Span {
                start: start_span.start,
                end: right.span.end,
            };

            expression = Expression {
                span,
                kind: ExpressionKind::Operation {
                    left: Box::new(expression),
                    operator,
                    right: Box::new(right),
                    span,
                },
            };
        }

        Ok(expression)
    }

    fn parse_unary(&mut self) -> MusiResult<Expression> {
        let token = self.peek().clone();
        match token.kind {
            Kind::Not | Kind::Minus => {
                let operator = self.advance().kind;
                let start_span = token.span;
                let operand = self.parse_precedence(Precedence::Unary)?;
                let span = Span {
                    start: start_span.start,
                    end: operand.span.end,
                };

                Ok(Expression {
                    span,
                    kind: ExpressionKind::Operation {
                        left: Box::new(Expression {
                            kind: ExpressionKind::Literal {
                                value: Value::Unit,
                                span: start_span,
                            },
                            span: start_span,
                        }),
                        operator,
                        right: Box::new(operand),
                        span,
                    },
                })
            }
            _ => self.parse_primary(),
        }
    }

    fn parse_call(
        &mut self,
        start_span: Span,
        expression: Expression,
    ) -> Result<Expression, MusiError> {
        self.advance();
        let member = self.consume(Kind::Identifier, "expected identifier after '.'")?;

        Ok(Expression {
            span: Span {
                start: start_span.start,
                end: member.span.end,
            },
            kind: ExpressionKind::Call {
                callee: Box::new(expression),
                arguments: vec![Expression {
                    kind: ExpressionKind::Identifier {
                        name: member.lexeme.clone(),
                        span: member.span,
                    },
                    span: member.span,
                }],
                span: start_span,
            },
        })
    }

    fn parse_function_call(&mut self, callee: Expression) -> MusiResult<Expression> {
        let start_span = callee.span;
        let mut arguments = vec![];

        self.advance(); // consume '('

        if self.peek().kind != Kind::RightParen {
            loop {
                arguments.push(self.parse_expression()?);

                if self.peek().kind != Kind::Comma {
                    break;
                }
                self.advance();
            }
        }

        let end_token = self.consume(Kind::RightParen, "expected ')' after argument(s)")?;

        Ok(Expression {
            span: Span {
                start: start_span.start,
                end: end_token.span.end,
            },
            kind: ExpressionKind::Call {
                callee: Box::new(callee),
                arguments,
                span: start_span,
            },
        })
    }

    fn parse_primary(&mut self) -> MusiResult<Expression> {
        let token = self.advance().clone();
        let span = token.span;

        let kind = match token.kind {
            Kind::Identifier => ExpressionKind::Identifier {
                name: token.lexeme,
                span,
            },
            Kind::Literal(kind) => ExpressionKind::Literal {
                value: Self::parse_literal_value(&token, kind)?,
                span,
            },
            Kind::True => ExpressionKind::Literal {
                value: Value::Boolean(true),
                span,
            },
            Kind::False => ExpressionKind::Literal {
                value: Value::Boolean(false),
                span,
            },
            unexpected => {
                //print span
                log::info!("span during unexpected: {:?}", span);
                return Err(MusiError::Syntax(SyntaxError {
                    message: Box::leak(
                        format!("expected expression, found '{unexpected:?}'").into_boxed_str(),
                    ),
                }));
            }
        };

        Ok(Expression { kind, span })
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
