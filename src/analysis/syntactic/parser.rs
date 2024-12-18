use std::{str, vec};

use crate::{
    analysis::lexical::token::{Token, TokenKind},
    core::{diagnostics::Diagnostic, span::Span, value::Value, MusiResult},
};

use super::ast::{
    Declaration, Expression, ExpressionKind, Parameter, Program, Statement, StatementKind,
};

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
        let start_span = match self.peek() {
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
            body: statements,
            span: start_span,
        })
    }

    fn parse_statement(&mut self) -> MusiResult<Statement> {
        let start_span = self.peek().map(|token| token.span).unwrap_or_default();

        let kind = match self.peek() {
            Some(token) => match token.kind {
                TokenKind::Let | TokenKind::Var => {
                    StatementKind::Declaration(self.parse_variable_declaration()?)
                }

                TokenKind::If => StatementKind::Expression(self.parse_if_then_expression()?),
                // TokenKind::While => self.parse_while_statement()?,
                // TokenKind::Return => self.parse_return_statement()?,
                _ => StatementKind::Expression(self.parse_expression()?),
            },
            None => return Err(self.error("unexpected end of file")),
        };

        Ok(Statement {
            kind,
            span: start_span,
        })
    }

    fn parse_variable_declaration(&mut self) -> MusiResult<Declaration> {
        let mutable = matches!(self.consume().kind, TokenKind::Var);

        let name = self.expect_identifier()?;

        self.expect(TokenKind::ColonEquals, "expected ':=' after variable name")?;

        let initialiser =
            if !mutable && self.check(TokenKind::LeftParen) || self.check_lambda_parameters() {
                self.parse_lambda_expression()?
            } else {
                self.parse_expression()?
            };

        Ok(Declaration::Variable {
            mutable,
            name,
            initialiser: Some(initialiser),
        })
    }

    fn parse_expression(&mut self) -> MusiResult<Expression> {
        let precedence = self.parse_precedence(Precedence::Assignment)?;
        Ok(precedence)
    }

    fn parse_precedence(&mut self, precedence: Precedence) -> MusiResult<Expression> {
        let mut left = self.parse_unary()?;

        while let Some(current) = self.peek() {
            let operator_precedence = Precedence::from(current.kind);
            if precedence <= operator_precedence {
                let operator = self.consume().kind;
                let right = self.parse_precedence(operator_precedence)?;

                let span = Span {
                    start: left.span.start,
                    end: right.span.end,
                };

                left = Expression {
                    kind: match operator {
                        TokenKind::ColonEquals | TokenKind::LessMinus => {
                            ExpressionKind::Assignment {
                                target: Box::new(left),
                                value: Box::new(right),
                            }
                        }
                        _ => ExpressionKind::Binary {
                            left: Box::new(left),
                            operator,
                            right: Box::new(right),
                        },
                    },
                    span,
                };
            } else {
                break;
            }
        }

        Ok(left)
    }

    fn parse_unary(&mut self) -> MusiResult<Expression> {
        let Some(current) = self.peek() else {
            return self.parse_primary();
        };

        match current.kind {
            TokenKind::Minus | TokenKind::Not | TokenKind::Ref | TokenKind::Deref => {
                let start_spab = current.span;
                let operator = self.consume().kind;

                let operand = self.parse_precedence(Precedence::Unary)?;

                let span = Span {
                    start: start_spab.start,
                    end: operand.span.end,
                };

                Ok(Expression {
                    kind: ExpressionKind::Unary {
                        operator,
                        operand: Box::new(operand),
                    },
                    span,
                })
            }
            _ => self.parse_primary(),
        }
    }

    fn parse_primary(&mut self) -> MusiResult<Expression> {
        let current = self.consume();
        let span = current.span;

        let kind = match current.kind {
            TokenKind::NumbericLiteral => {
                let value = self.parse_numeric_literal(&current.lexeme)?;
                ExpressionKind::Literal(value)
            }
            TokenKind::StringLiteral => {
                let content = current
                    .lexeme
                    .get(1..current.lexeme.len().saturating_sub(1))
                    .unwrap_or_default();

                ExpressionKind::Literal(Value::String(content.to_vec()))
            }
            TokenKind::True => ExpressionKind::Literal(Value::Boolean(true)),
            TokenKind::False => ExpressionKind::Literal(Value::Boolean(false)),
            TokenKind::Identifer => {
                let name = String::from_utf8(current.lexeme)
                    .map_err(|_error| self.error("invalid UTF-8 sequence in identifier"))?
                    .into_boxed_str();
                ExpressionKind::Variable(name)
            }

            TokenKind::LeftParen => {
                if self.check_lambda_parameters() {
                    return self.parse_lambda_expression();
                }

                let expression = self.parse_expression()?;
                self.expect(TokenKind::RightParen, "expected ')' after expression")?;
                return Ok(expression);
            }
            _ => return Err(self.error("unexpected token in expression")),
        };

        Ok(Expression { kind, span })
    }

    fn parse_numeric_literal(&self, lexeme: &[u8]) -> MusiResult<Value> {
        let number_str = str::from_utf8(lexeme)
            .map_err(|_error| self.error("invalid UTF-8 sequence in number"))?;
        match (
            number_str
                .strip_prefix("0x")
                .or_else(|| number_str.strip_prefix("0X")),
            number_str
                .strip_prefix("0b")
                .or_else(|| number_str.strip_prefix("0B")),
            number_str
                .strip_prefix("0o")
                .or_else(|| number_str.strip_prefix("0O")),
        ) {
            (Some(value), _, _) => {
                let hexadecimal = i64::from_str_radix(value, 16)
                    .map_err(|_error| self.error("invalid hexadecimal format"))?;
                return Ok(Value::Integer(hexadecimal));
            }
            (_, Some(value), _) => {
                let binary = i64::from_str_radix(value, 2)
                    .map_err(|_error| self.error("invalid binary format"))?;
                return Ok(Value::Integer(binary));
            }
            (_, _, Some(value)) => {
                let octal = i64::from_str_radix(value, 8)
                    .map_err(|_error| self.error("invalid octal format"))?;
                return Ok(Value::Integer(octal));
            }
            _ => {}
        }

        if number_str.contains('.') {
            let value = number_str
                .parse::<f64>()
                .map_err(|_error| self.error("invalid decimal format"))?;
            return Ok(Value::Real(value));
        }

        let value = number_str
            .parse::<i64>()
            .map_err(|_error| self.error("invalid integer format"))?;
        Ok(Value::Integer(value))
    }

    fn parse_if_then_expression(&mut self) -> MusiResult<Expression> {
        self.consume();

        let start_span = self.peek().map_or_else(Span::default, |token| token.span);

        let condition = Box::new(self.parse_expression()?);
        self.expect(TokenKind::Then, "expected 'then' after condition")?;

        let then_branch = if self.check(TokenKind::Newline) {
            Box::new(self.parse_block_expression()?)
        } else {
            Box::new(self.parse_expression()?)
        };
        let else_branch = if self.check(TokenKind::Else) {
            self.consume();
            if self.check(TokenKind::If) {
                Some(Box::new(self.parse_if_then_expression()?))
            } else {
                Some(Box::new(self.parse_expression()?))
            }
        } else {
            None
        };

        let end_span = self
            .previous()
            .map_or_else(Span::default, |token| token.span);

        Ok(Expression {
            kind: ExpressionKind::If {
                condition,
                then_branch,
                else_branch,
            },
            span: Span {
                start: start_span.start,
                end: end_span.end,
            },
        })
    }

    fn parse_lambda_expression(&mut self) -> MusiResult<Expression> {
        let start_span = self.peek().map(|token| token.span).unwrap_or_default();

        self.expect(TokenKind::LeftParen, "expected '(' after lambda")?;
        let parameters = self.parse_parameters()?;
        self.expect(TokenKind::RightParen, "expected ')' after parameters")?;
        self.expect(TokenKind::MinusGreater, "expected '->' after parameters")?;
        let body = if self.check(TokenKind::Newline) {
            Box::new(self.parse_block_expression()?)
        } else {
            Box::new(self.parse_expression()?)
        };

        let end_span = body.span;

        Ok(Expression {
            kind: ExpressionKind::Lambda { parameters, body },
            span: Span {
                start: start_span.start,
                end: end_span.end,
            },
        })
    }

    fn parse_block_expression(&mut self) -> MusiResult<Expression> {
        let start_span = self
            .previous()
            .map(|token| token.span.start)
            .unwrap_or_default();

        let mut statements = vec![];

        self.expect(TokenKind::Newline, "expected newline before indented block")?;
        self.expect(TokenKind::Indent, "expected indented block")?;

        while !self.check(TokenKind::Dedent) && !self.is_eof() {
            match self.peek() {
                Some(token) => match token.kind {
                    TokenKind::Newline => {
                        self.consume();
                        continue;
                    }
                    _ => statements.push(self.parse_statement()?),
                },
                None => break,
            }
        }

        self.expect(TokenKind::Dedent, "expected dedent after block")?;

        let end_span = self.peek().map(|token| token.span.end).unwrap_or_default();

        Ok(Expression {
            kind: ExpressionKind::Block { body: statements },
            span: Span {
                start: start_span,
                end: end_span,
            },
        })
    }

    fn parse_parameters(&mut self) -> MusiResult<Vec<Parameter>> {
        let mut parameters = vec![];

        while !self.check(TokenKind::RightParen) {
            let name = self.expect_identifier()?;
            parameters.push(Parameter { name });

            if !self.check(TokenKind::RightParen) {
                self.expect(TokenKind::Comma, "expected ',' between parameters")?;
            }
        }

        Ok(parameters)
    }

    fn peek(&self) -> Option<&Token> {
        self.tokens.get(self.current_position)
    }

    fn previous(&self) -> Option<&Token> {
        self.tokens.get(self.current_position.saturating_sub(1))
    }

    fn consume(&mut self) -> Token {
        if !self.is_eof() {
            self.current_position = self.current_position.saturating_add(1);
        }

        self.previous()
            .cloned()
            .unwrap_or_else(|| Token::new(TokenKind::Eof, &[], Span::default()))
    }

    fn is_eof(&self) -> bool {
        matches!(self.peek(), Some(current) if current.kind == TokenKind::Eof)
    }

    fn check(&self, kind: TokenKind) -> bool {
        if self.is_eof() {
            false
        } else {
            matches!(self.peek(), Some(current) if current.kind == kind)
        }
    }

    fn check_lambda_parameters(&self) -> bool {
        if !self.check(TokenKind::LeftParen) {
            return false;
        }

        let mut position = self.current_position.saturating_add(1);

        while position < self.tokens.len() {
            match self.tokens.get(position) {
                Some(delimiter) => {
                    if delimiter.kind == TokenKind::RightParen {
                        return matches!(
                            self.tokens.get(position.saturating_add(1)),
                            Some(operator) if operator.kind == TokenKind::MinusGreater
                        );
                    }

                    position = position.saturating_add(1);
                }
                None => return false,
            }
        }

        false
    }

    fn expect(&mut self, kind: TokenKind, message: &str) -> MusiResult<Token> {
        if self.check(kind) {
            Ok(self.consume())
        } else {
            Err(self.error(message))
        }
    }

    fn expect_identifier(&mut self) -> MusiResult<Box<str>> {
        let identifier = String::from_utf8(
            self.expect(TokenKind::Identifer, "expected identifier")?
                .lexeme,
        )
        .map_err(|_error| self.error("invalid UTF-8 sequence in indentifier"))?;
        Ok(identifier.into())
    }

    fn error(&self, message: &str) -> Diagnostic {
        Diagnostic::error(
            message.to_owned(),
            self.peek().map_or_else(Span::default, |token| token.span),
        )
    }
}
