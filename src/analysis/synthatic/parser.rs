use std::vec;

use crate::{
    analysis::lexical::token::{Kind, Token},
    core::{diagnostics::Diagnostic, span::Span, value::Value, MusiResult},
};

use super::ast::{Expression, ExpressionKind, Program, Statement, StatementKind};

#[derive(Debug, Clone, Copy, PartialEq, PartialOrd)]
enum Precedence {
    None,
    Assignment, // := <-
    Pipeline,   // |>
    LogicalOr,  // or
    LogicalAnd, // and
    Equality,   // = /=
    Comparison, // < > <= >= <=>
    Term,       // + -
    Factor,     // * /
    Power,      // ^
    Unary,      // - not
    Call,       // . ()
    Primary,    //
}

impl From<Kind> for Precedence {
    fn from(kind: Kind) -> Self {
        match kind {
            Kind::ColonEquals | Kind::LessMinus => Self::Assignment,
            Kind::PipeGreater => Self::Pipeline,
            Kind::Or => Self::LogicalOr,
            Kind::And => Self::LogicalAnd,
            Kind::Equals | Kind::SlashEquals => Self::Equality,
            Kind::Less
            | Kind::Greater
            | Kind::LessEquals
            | Kind::GreaterEquals
            | Kind::LessEqualsGreater => Self::Comparison,
            Kind::Plus | Kind::Minus => Self::Term,
            Kind::Star | Kind::Slash => Self::Factor,
            Kind::Caret => Self::Power,
            Kind::LeftParen | Kind::Dot => Self::Call,
            _ => Self::None,
        }
    }
}

pub struct Parser {
    tokens: Vec<Token>,
    current: usize,
    diagnostics: Vec<Diagnostic>,
}

impl Parser {
    #[must_use]
    pub const fn new(tokens: Vec<Token>) -> Self {
        Self {
            tokens,
            current: 0,
            diagnostics: vec![],
        }
    }

    #[allow(clippy::missing_errors_doc)]
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
                _ => match self.parse_statement() {
                    Ok(statement) => statements.push(statement),
                    Err(diagnostic) => {
                        self.diagnostics.push(diagnostic);
                        self.sync();
                    }
                },
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

    fn parse_statement(&mut self) -> MusiResult<Statement> {
        let start_span = self.peek().span;

        let kind = match self.peek().kind {
            Kind::Let | Kind::Var => self.parse_variable_declaration(start_span)?,
            _ => StatementKind::Expression(self.parse_expression()?),
        };

        Ok(Statement {
            kind,
            span: Span {
                start: start_span.start,
                end: self.previous().span.end,
            },
        })
    }

    fn parse_variable_declaration(&mut self, span: Span) -> Result<StatementKind, Diagnostic> {
        let kind = self.advance().kind; // let | var
        let mutable = matches!(kind, Kind::Var);

        let name = String::from_utf8(
            self.consume(Kind::Identifer, "expected identifier")?
                .lexeme
                .clone(),
        )
        .expect("identifier contains invalid UTF-8 character(s)")
        .into();

        self.consume(Kind::ColonEquals, "expected ':=' after identifier")?;
        let initialiser = self.parse_expression()?;

        if matches!(
            initialiser.kind,
            ExpressionKind::Unary {
                operator: Kind::Ref,
                ..
            }
        ) && !mutable
        {
            return Err(self.error("cannot assign mutable reference to immutable variable"));
        }

        Ok(StatementKind::VariableDeclaration {
            name,
            initialiser,
            mutable,
            span,
        })
    }

    fn parse_expression(&mut self) -> MusiResult<Expression> {
        self.parse_precedence(Precedence::Assignment)
    }

    fn parse_precedence(&mut self, precedence: Precedence) -> MusiResult<Expression> {
        let mut left = self.parse_unary()?;

        while precedence <= Precedence::from(self.peek().kind) {
            let operator = self.advance().kind;
            let right = self.parse_precedence(Precedence::from(operator))?;
            let span = Span {
                start: left.span.start,
                end: right.span.end,
            };

            left = match operator {
                Kind::LessMinus => Expression {
                    kind: ExpressionKind::Assignment {
                        target: Box::new(left),
                        value: Box::new(right),
                        span,
                    },
                    span,
                },
                _ => Expression {
                    kind: ExpressionKind::Binary {
                        left: Box::new(left),
                        operator,
                        right: Box::new(right),
                        span,
                    },
                    span,
                },
            };
        }

        Ok(left)
    }

    fn parse_unary(&mut self) -> MusiResult<Expression> {
        let token = self.peek();
        match token.kind {
            Kind::Minus | Kind::Not | Kind::Ref | Kind::Deref => {
                let start_span = token.span;
                let operator = self.advance().kind;

                let precedence = match operator {
                    Kind::Ref | Kind::Deref => Precedence::Unary,
                    _ => Precedence::Term,
                };

                let operand = self.parse_precedence(precedence)?;

                match operator {
                    Kind::Ref => {
                        if !matches!(operand.kind, ExpressionKind::Identifier { .. }) {
                            return Err(self.error("'ref' can only be applied to variables"));
                        }
                    }
                    Kind::Deref => {
                        if !matches!(
                            operand.kind,
                            ExpressionKind::Identifier { .. }
                                | ExpressionKind::Unary {
                                    operator: Kind::Ref,
                                    ..
                                }
                        ) {
                            return Err(self.error("'deref' can only be applied to references"));
                        }
                    }
                    _ => {}
                }

                let span = Span {
                    start: start_span.start,
                    end: operand.span.end,
                };

                Ok(Expression {
                    kind: ExpressionKind::Unary {
                        operator,
                        operand: Box::new(operand),
                        span,
                    },
                    span,
                })
            }
            _ => self.parse_primary(),
        }
    }

    fn parse_primary(&mut self) -> MusiResult<Expression> {
        let token = self.advance().clone();
        let span = token.span;

        let kind = match token.kind {
            Kind::Number => ExpressionKind::Literal {
                value: Self::parse_number(&token.lexeme),
                span,
            },
            Kind::String => ExpressionKind::Literal {
                value: Self::parse_string(&token.lexeme),
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

            Kind::Identifer => ExpressionKind::Identifier {
                name: String::from_utf8(token.lexeme)
                    .expect("identifier contains invalid UTF-8 character(s)")
                    .into(),
                span,
            },

            Kind::LeftParen => {
                let expression = self.parse_expression()?;
                self.consume(Kind::RightParen, "expected ')' after expression")?;
                return Ok(expression);
            }

            _ => {
                return Err(self.error(&format!(
                    "unexpected token '{:?}' in expression",
                    token.kind
                )))
            }
        };

        Ok(Expression { kind, span })
    }

    fn parse_number(lexeme: &[u8]) -> Value {
        let number_str = std::str::from_utf8(lexeme).unwrap();
        match number_str {
            s if s.starts_with("0x") || s.starts_with("0X") => {
                let hex_str = &s[2..];
                let value = i64::from_str_radix(hex_str, 16).unwrap();
                Value::Integer(value)
            }
            s if s.starts_with("0b") || s.starts_with("0B") => {
                let bin_str = &s[2..];
                let value = i64::from_str_radix(bin_str, 2).unwrap();
                Value::Integer(value)
            }
            s if s.starts_with("0o") || s.starts_with("0O") => {
                let oct_str = &s[2..];
                let value = i64::from_str_radix(oct_str, 8).unwrap();
                Value::Integer(value)
            }
            s if s.contains('.') => {
                let value = s.parse::<f64>().unwrap();
                Value::Real(value)
            }
            s => {
                let value = s.parse::<i64>().unwrap();
                Value::Integer(value)
            }
        }
    }

    fn parse_string(lexeme: &[u8]) -> Value {
        let content = &lexeme[1..lexeme.len() - 1];
        Value::String(content.to_vec())
    }
}

impl Parser {
    #[inline]
    fn peek(&self) -> &Token {
        self.tokens
            .get(self.current)
            .expect("position out of bounds")
    }

    #[inline]
    fn previous(&self) -> &Token {
        self.tokens
            .get(self.current.checked_sub(1).expect("position underflow"))
            .expect("position out of bounds")
    }

    #[inline]
    fn advance(&mut self) -> &Token {
        if !self.is_at_end() {
            self.current += 1;
        }
        self.previous()
    }

    #[inline]
    fn is_at_end(&self) -> bool {
        self.peek().kind == Kind::Eof
    }

    #[inline]
    fn check(&self, kind: Kind) -> bool {
        if self.is_at_end() {
            false
        } else {
            self.peek().kind == kind
        }
    }

    #[inline]
    fn consume(&mut self, kind: Kind, message: &str) -> MusiResult<&Token> {
        if self.check(kind) {
            Ok(self.advance())
        } else {
            Err(self.error(message))
        }
    }

    #[inline]
    fn error(&mut self, message: &str) -> Diagnostic {
        let diagnostic = Diagnostic::error(message, self.peek().span);
        self.diagnostics.push(diagnostic.clone());
        diagnostic
    }

    #[inline]
    fn sync(&mut self) {
        self.advance();

        while !self.is_at_end() {
            if self.previous().kind == Kind::Newline {
                return;
            }

            match self.peek().kind {
                Kind::If
                | Kind::Let
                | Kind::Match
                | Kind::Return
                | Kind::Type
                | Kind::While
                | Kind::Var => return,
                _ => {
                    self.advance();
                }
            }
        }
    }
}
