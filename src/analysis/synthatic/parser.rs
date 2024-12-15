use std::vec;

use crate::{
    analysis::lexical::token::{Token, TokenKind},
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

impl From<TokenKind> for Precedence {
    fn from(kind: TokenKind) -> Self {
        match kind {
            TokenKind::ColonEquals | TokenKind::LessMinus => Self::Assignment,
            TokenKind::PipeGreater => Self::Pipeline,
            TokenKind::Or => Self::LogicalOr,
            TokenKind::And => Self::LogicalAnd,
            TokenKind::Equals | TokenKind::SlashEquals => Self::Equality,
            TokenKind::Less
            | TokenKind::Greater
            | TokenKind::LessEquals
            | TokenKind::GreaterEquals
            | TokenKind::LessEqualsGreater => Self::Comparison,
            TokenKind::Plus | TokenKind::Minus => Self::Term,
            TokenKind::Star | TokenKind::Slash => Self::Factor,
            TokenKind::Caret => Self::Power,
            TokenKind::LeftParen | TokenKind::Dot => Self::Call,
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
    #[inline]
    pub const fn new(tokens: Vec<Token>) -> Self {
        Self {
            tokens,
            current: 0,
            diagnostics: vec![],
        }
    }

    #[inline]
    pub fn parse(&mut self) -> MusiResult<Program> {
        let start_span = match self.peek() {
            Some(token) => token.span,
            None => return Err(self.error("unexpected end of file")),
        };
        let mut statements = vec![];

        while !self.is_at_end() {
            match self.peek() {
                Some(token) => match token.kind {
                    TokenKind::Eof => break,
                    TokenKind::Newline | TokenKind::Indent | TokenKind::Dedent => {
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
                },
                None => break,
            }
        }

        Ok(Program {
            statements,
            span: Span {
                start: start_span.start,
                end: self
                    .previous()
                    .map_or(start_span.end, |token| token.span.end),
            },
        })
    }

    fn parse_statement(&mut self) -> MusiResult<Statement> {
        let Some(current_token) = self.peek() else {
            return Err(self.error("unexpected end of file"));
        };
        let start_span = current_token.span;

        let kind = match self.peek() {
            Some(next_token) => match next_token.kind {
                TokenKind::Let | TokenKind::Var => self.parse_variable_declaration(start_span)?,
                _ => StatementKind::Expression(self.parse_expression()?),
            },
            None => return Err(self.error("unexpected end of file")),
        };

        Ok(Statement {
            kind,
            span: Span {
                start: start_span.start,
                end: match self.previous() {
                    Some(previous_token) => previous_token.span.end,
                    None => return Err(self.error("unexpected end of file")),
                },
            },
        })
    }

    fn parse_variable_declaration(&mut self, span: Span) -> Result<StatementKind, Diagnostic> {
        let kind = self.advance().kind; // let | var
        let mutable = matches!(kind, TokenKind::Var);

        let name = String::from_utf8(
            self.consume(TokenKind::Identifer, "expected identifier")?
                .lexeme,
        )
        .map_err(|_error| self.error("identifier contains invalid UTF-8 character(s)"))?
        .into();

        self.consume(TokenKind::ColonEquals, "expected ':=' after identifier")?;
        let initialiser = self.parse_expression()?;

        if matches!(
            initialiser.kind,
            ExpressionKind::Unary {
                operator: TokenKind::Ref,
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

        while let Some(token) = self.peek() {
            if precedence <= Precedence::from(token.kind) {
                let operator = self.advance().kind;
                let right = self.parse_precedence(Precedence::from(operator))?;
                let span = Span {
                    start: left.span.start,
                    end: right.span.end,
                };

                left = match operator {
                    TokenKind::LessMinus => Expression {
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
            } else {
                break;
            }
        }

        Ok(left)
    }

    fn parse_unary(&mut self) -> MusiResult<Expression> {
        let Some(token) = self.peek() else {
            return self.parse_primary();
        };
        match token.kind {
            TokenKind::Minus | TokenKind::Not | TokenKind::Ref | TokenKind::Deref => {
                let start_span = token.span;
                let operator = self.advance().kind;

                let precedence = match operator {
                    TokenKind::Ref | TokenKind::Deref => Precedence::Unary,
                    _ => Precedence::Term,
                };

                let operand = self.parse_precedence(precedence)?;

                match operator {
                    TokenKind::Ref => {
                        if !matches!(operand.kind, ExpressionKind::Identifier { .. }) {
                            return Err(self.error("'ref' can only be applied to variables"));
                        }
                    }
                    TokenKind::Deref => {
                        if !matches!(
                            operand.kind,
                            ExpressionKind::Identifier { .. }
                                | ExpressionKind::Unary {
                                    operator: TokenKind::Ref,
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
        let token = self.advance();
        let lexeme = token.lexeme;
        let span = token.span;

        let kind = match token.kind {
            TokenKind::Number => ExpressionKind::Literal {
                value: {
                    use std::str::from_utf8;

                    let number_str =
                        from_utf8(&lexeme).map_err(|_error| "number must be valid UTF-8");
                    match number_str {
                        Ok(str) if str.starts_with("0x") || str.starts_with("0X") => {
                            let hexadecimal_str = str
                                .get(2..)
                                .ok_or_else(|| self.error("expected '0x' or '0X'"))?;
                            let value = i64::from_str_radix(hexadecimal_str, 16)
                                .map_err(|_error| self.error("malformed hexadecimal integer"))?;
                            Value::Integer(value)
                        }
                        Ok(str) if str.starts_with("0b") || str.starts_with("0B") => {
                            let binary_str = str
                                .get(2..)
                                .ok_or_else(|| self.error("expected '0b' or '0B'"))?;
                            let value = i64::from_str_radix(binary_str, 2)
                                .map_err(|_error| self.error("malformed binary integer"))?;
                            Value::Integer(value)
                        }
                        Ok(str) if str.starts_with("0o") || str.starts_with("0O") => {
                            let octal_str = str
                                .get(2..)
                                .ok_or_else(|| self.error("expected '0o' or '0O'"))?;
                            let value = i64::from_str_radix(octal_str, 8)
                                .map_err(|_error| self.error("malformed octal integer"))?;
                            Value::Integer(value)
                        }
                        Ok(str) if str.contains('.') => {
                            let value = str
                                .parse::<f64>()
                                .map_err(|_error| self.error("malformed real number"))?;
                            Value::Real(value)
                        }
                        Ok(str) => {
                            let value = str
                                .parse::<i64>()
                                .map_err(|_error| self.error("malformed integer"))?;
                            Value::Integer(value)
                        }
                        Err(error) => return Err(self.error(error)),
                    }
                },
                span,
            },
            TokenKind::String => ExpressionKind::Literal {
                value: {
                    let content = lexeme
                        .get(1..lexeme.len().saturating_sub(1))
                        .unwrap_or_default();
                    Value::String(content.to_vec())
                },
                span,
            },
            TokenKind::True => ExpressionKind::Literal {
                value: Value::Boolean(true),
                span,
            },
            TokenKind::False => ExpressionKind::Literal {
                value: Value::Boolean(false),
                span,
            },

            TokenKind::Identifer => ExpressionKind::Identifier {
                name: String::from_utf8(lexeme)
                    .map_err(|_error| self.error("identifier contains invalid UTF-8 character(s)"))?
                    .into(),
                span,
            },

            TokenKind::LeftParen => {
                let expression = self.parse_expression()?;
                self.consume(TokenKind::RightParen, "expected ')' after expression")?;
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

    #[inline]
    fn peek(&self) -> Option<&Token> {
        self.tokens.get(self.current)
    }

    #[inline]
    fn previous(&self) -> Option<&Token> {
        self.tokens.get(self.current.saturating_sub(1))
    }

    #[inline]
    fn advance(&mut self) -> Token {
        if !self.is_at_end() {
            self.current = self.current.saturating_add(1);
        }

        self.previous()
            .cloned()
            .unwrap_or_else(|| Token::new(TokenKind::Eof, &[], Span::default()))
    }

    #[inline]
    fn is_at_end(&self) -> bool {
        matches!(self.peek(), Some(token) if token.kind == TokenKind::Eof)
    }

    #[inline]
    fn check(&self, kind: TokenKind) -> bool {
        if self.is_at_end() {
            false
        } else {
            matches!(self.peek(), Some(token) if token.kind == kind)
        }
    }

    #[inline]
    fn consume(&mut self, kind: TokenKind, message: &str) -> MusiResult<Token> {
        if self.check(kind) {
            Ok(self.advance())
        } else {
            Err(self.error(message))
        }
    }

    #[inline]
    fn error(&mut self, message: &str) -> Diagnostic {
        let diagnostic = Diagnostic::error(
            message,
            self.peek().map_or_else(Span::default, |token| token.span),
        );
        self.diagnostics.push(diagnostic.clone());
        diagnostic
    }

    #[inline]
    fn sync(&mut self) {
        self.advance();

        while !self.is_at_end() {
            if let Some(token) = self.previous() {
                if token.kind == TokenKind::Newline {
                    return;
                }
            }

            if let Some(token) = self.peek() {
                match token.kind {
                    TokenKind::If
                    | TokenKind::Let
                    | TokenKind::Match
                    | TokenKind::Return
                    | TokenKind::Type
                    | TokenKind::While
                    | TokenKind::Var => return,
                    _ => {
                        self.advance();
                    }
                }
            }
        }
    }
}
