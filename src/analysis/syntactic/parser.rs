use std::vec;

use crate::{
    analysis::lexical::token::{Token, TokenKind},
    core::{diagnostics::Diagnostic, span::Span, value::Value, MusiResult},
};

use super::ast::{Expression, ExpressionKind, Program, Statement, StatementKind};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd)]
#[non_exhaustive]
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
}

impl From<TokenKind> for Precedence {
    #[inline]
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
    current_position: usize,
}

impl Parser {
    #[inline]
    #[must_use]
    pub const fn new(tokens: Vec<Token>) -> Self {
        Self {
            tokens,
            current_position: 0,
        }
    }

    #[inline]
    pub fn parse(&mut self) -> MusiResult<Program> {
        let token_start = match self.peek() {
            Some(token) => token.span,
            None => return Err(self.error("unexpected end of file")),
        };
        let mut statements = vec![];

        while !self.is_eof() {
            match self.peek() {
                Some(token) => match token.kind {
                    TokenKind::Eof => break,
                    TokenKind::Newline | TokenKind::Indent | TokenKind::Dedent => {
                        self.consume();
                        continue;
                    }
                    _ => match self.parse_statement() {
                        Ok(statement) => statements.push(statement),
                        Err(_) => continue,
                    },
                },
                None => break,
            }
        }

        Ok(Program {
            statements,
            span: token_start,
        })
    }

    fn parse_statement(&mut self) -> MusiResult<Statement> {
        let Some(current) = self.peek() else {
            return Err(self.error("unexpected end of file while parsing statement"));
        };
        let token_span = current.span;

        let kind = match self.peek() {
            Some(token) => match token.kind {
                TokenKind::Let | TokenKind::Var => self.parse_variable_declaration(token_span)?,
                _ => StatementKind::Expression(self.parse_expression()?),
            },
            None => return Err(self.error("unexpected end of file while parsing statement kind")),
        };

        Ok(Statement {
            kind,
            span: Span {
                start: token_span.start,
                end: match self.previous() {
                    Some(previous) => previous.span.end,
                    None => {
                        return Err(
                            self.error("unexpected end of file while parsing statement span")
                        )
                    }
                },
            },
        })
    }

    fn parse_expression(&mut self) -> MusiResult<Expression> {
        self.parse_precedence(Precedence::Assignment)
    }

    fn parse_precedence(&mut self, precedence: Precedence) -> MusiResult<Expression> {
        let mut left_expression = self.parse_unary()?;

        while let Some(current) = self.peek() {
            if precedence <= Precedence::from(current.kind) {
                let operator = self.consume().kind;
                let right_expression = self.parse_precedence(Precedence::from(operator))?;
                let span = Span {
                    start: left_expression.span.start,
                    end: right_expression.span.end,
                };

                left_expression = match operator {
                    TokenKind::LessMinus => Expression {
                        kind: ExpressionKind::Assignment {
                            target: Box::new(left_expression),
                            value: Box::new(right_expression),
                            span,
                        },
                        span,
                    },
                    _ => Expression {
                        kind: ExpressionKind::Binary {
                            left: Box::new(left_expression),
                            operator,
                            right: Box::new(right_expression),
                            span,
                        },
                        span,
                    },
                };
            } else {
                break;
            }
        }

        Ok(left_expression)
    }

    fn parse_unary(&mut self) -> MusiResult<Expression> {
        let Some(current) = self.peek() else {
            return self.parse_primary();
        };

        match current.kind {
            TokenKind::Minus | TokenKind::Not | TokenKind::Ref | TokenKind::Deref => {
                let deref_token_span = current.span;
                let operator = self.consume().kind;

                let precedence = match operator {
                    TokenKind::Ref | TokenKind::Deref => Precedence::Unary,
                    _ => Precedence::Term,
                };

                let operand = self.parse_precedence(precedence)?;

                match operator {
                    TokenKind::Ref => {
                        matches!(operand.kind, ExpressionKind::Identifier { .. });
                    }
                    TokenKind::Deref => {
                        matches!(
                            operand.kind,
                            ExpressionKind::Identifier { .. }
                                | ExpressionKind::Unary {
                                    operator: TokenKind::Ref,
                                    ..
                                }
                        );
                    }
                    _ => {}
                }

                let span = Span {
                    start: deref_token_span.start,
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
        let current = self.consume();
        let lexeme = current.lexeme;
        let span = current.span;

        let kind = match current.kind {
            TokenKind::NumbericLiteral => ExpressionKind::Literal {
                value: {
                    use std::str::from_utf8;

                    let digit_sequence =
                        from_utf8(&lexeme).map_err(|_error| "number must be valid UTF-8");
                    match digit_sequence {
                        Ok(digit) if digit.starts_with("0x") || digit.starts_with("0X") => {
                            let hexadecimal_digits = digit
                                .get(2..)
                                .ok_or_else(|| self.error("expected '0x' or '0X'"))?;

                            let value = i64::from_str_radix(hexadecimal_digits, 16)
                                .map_err(|_error| self.error("invalid hexadecimal format"))?;
                            Value::Integer(value)
                        }
                        Ok(digit) if digit.starts_with("0b") || digit.starts_with("0B") => {
                            let binary_digits = digit
                                .get(2..)
                                .ok_or_else(|| self.error("expected '0b' or '0B'"))?;

                            let value = i64::from_str_radix(binary_digits, 2)
                                .map_err(|_error| self.error(" invalid binary format"))?;
                            Value::Integer(value)
                        }
                        Ok(digit) if digit.starts_with("0o") || digit.starts_with("0O") => {
                            let octal_digits = digit
                                .get(2..)
                                .ok_or_else(|| self.error("expected '0o' or '0O'"))?;

                            let value = i64::from_str_radix(octal_digits, 8)
                                .map_err(|_error| self.error("invalid octal format"))?;
                            Value::Integer(value)
                        }
                        Ok(digit) if digit.contains('.') => {
                            let value = digit
                                .parse::<f64>()
                                .map_err(|_error| self.error("invalid decimal format"))?;
                            Value::Real(value)
                        }
                        Ok(digit) => {
                            let value = digit
                                .parse::<i64>()
                                .map_err(|_error| self.error("invalid integer format"))?;
                            Value::Integer(value)
                        }
                        Err(error) => return Err(self.error(error)),
                    }
                },
                span,
            },
            TokenKind::StringLiteral => ExpressionKind::Literal {
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
                    .map_err(|_error| self.error("invalid UTF-8 sequence in identifier"))?
                    .into(),
                span,
            },

            TokenKind::LeftParen => {
                let expression = self.parse_expression()?;
                self.expect(TokenKind::RightParen, "unclosed parenthesis in expression")?;
                return Ok(expression);
            }

            _ => return Err(self.error(&format!("expected '{:?}' in expression", current.kind))),
        };

        Ok(Expression { kind, span })
    }

    fn parse_variable_declaration(&mut self, span: Span) -> MusiResult<StatementKind> {
        let kind = self.consume().kind; // let | var
        let mutable = matches!(kind, TokenKind::Var);

        let name = String::from_utf8(
            self.expect(TokenKind::Identifer, "expected variable name")?
                .lexeme,
        )
        .map_err(|_error| self.error("invalid UTF-8 sequence in variable name"))?
        .into();

        self.expect(TokenKind::ColonEquals, "expected ':=' after variable name")?;

        let initialiser = if self.check(TokenKind::LeftParen) {
            if self
                .tokens
                .get(self.current_position..)
                .map_or(false, |tokens| {
                    tokens
                        .iter()
                        .any(|token| token.kind == TokenKind::MinusGreater)
                })
            {
                self.parse_lambda()?
            } else {
                self.parse_expression()?
            }
        } else {
            self.parse_expression()?
        };

        Ok(StatementKind::VariableDeclaration {
            name,
            initialiser,
            mutable,
            span,
        })
    }

    fn parse_lambda(&mut self) -> MusiResult<Expression> {
        let token_span = self.peek().map(|token| token.span).unwrap_or_default();

        self.expect(TokenKind::LeftParen, "expected '(' after lambda")?;

        let mut parameters = vec![];
        while !self.check(TokenKind::RightParen) {
            let parameter_name = String::from_utf8(
                self.expect(TokenKind::Identifer, "expected parameter name")?
                    .lexeme,
            )
            .map_err(|_error| self.error("invalid UTF-8 sequence in parameter name"))?
            .into_boxed_str();

            parameters.push(parameter_name);

            if !self.check(TokenKind::RightParen) {
                self.expect(TokenKind::Comma, "expected ',' between parameters")?;
            }
        }

        self.expect(TokenKind::RightParen, "expected ')' after parameters")?;
        self.expect(TokenKind::MinusGreater, "expected '->' after parameters")?;

        let body = if self.check(TokenKind::Newline) {
            self.consume();
            self.expect(TokenKind::Indent, "expected indented block after '->'")?;

            let body_expression = self.parse_expression()?;

            self.expect(TokenKind::Dedent, "expected dedent after lambda body")?;
            body_expression
        } else {
            self.parse_expression()?
        };

        let expression_span = Span {
            start: token_span.start,
            end: body.span.end,
        };

        Ok(Expression {
            kind: ExpressionKind::Lambda {
                parameters,
                body: Box::new(body),
                span: expression_span,
            },
            span: expression_span,
        })
    }

    #[inline]
    fn peek(&self) -> Option<&Token> {
        self.tokens.get(self.current_position)
    }

    #[inline]
    fn previous(&self) -> Option<&Token> {
        self.tokens.get(self.current_position.saturating_sub(1))
    }

    #[inline]
    fn consume(&mut self) -> Token {
        if !self.is_eof() {
            self.current_position = self.current_position.saturating_add(1);
        }

        self.previous()
            .cloned()
            .unwrap_or_else(|| Token::new(TokenKind::Eof, &[], Span::default()))
    }

    #[inline]
    fn is_eof(&self) -> bool {
        matches!(self.peek(), Some(current) if current.kind == TokenKind::Eof)
    }

    #[inline]
    fn check(&self, kind: TokenKind) -> bool {
        if self.is_eof() {
            false
        } else {
            matches!(self.peek(), Some(current) if current.kind == kind)
        }
    }

    #[inline]
    fn expect(&mut self, kind: TokenKind, message: &str) -> MusiResult<Token> {
        if self.check(kind) {
            Ok(self.consume())
        } else {
            Err(self.error(message))
        }
    }

    #[inline]
    fn error(&self, message: &str) -> Diagnostic {
        Diagnostic::error(
            message.to_owned(),
            self.peek().map_or_else(Span::default, |token| token.span),
        )
    }
}
