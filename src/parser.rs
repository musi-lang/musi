use crate::{
    ast::{
        Expression, ExpressionKind, Node, Program, ProgramKind, Statement, StatementKind,
        VariableDeclarator,
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
            kind: ProgramKind { body },
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
            _ => {
                return Err(MusiError::Syntax(SyntaxError {
                    message: "expected expression",
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
            LiteralKind::Number => {
                let lexeme = &token.lexeme;
                if lexeme.contains('.') {
                    lexeme.parse::<f64>().map(Value::Real).map_err(|_| {
                        MusiError::Syntax(SyntaxError {
                            message: "invalid number literal",
                        })
                    })
                } else if lexeme.starts_with('-') {
                    lexeme.parse::<i64>().map(Value::Integer).map_err(|_| {
                        MusiError::Syntax(SyntaxError {
                            message: "invalid number literal",
                        })
                    })
                } else {
                    lexeme.parse::<u64>().map(Value::Natural).map_err(|_| {
                        MusiError::Syntax(SyntaxError {
                            message: "invalid number literal",
                        })
                    })
                }
            }
            _ => Err(MusiError::Syntax(SyntaxError {
                message: "unsupported literal type",
            })),
        }
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
