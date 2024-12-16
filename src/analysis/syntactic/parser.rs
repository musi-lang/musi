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
    token_stream: Vec<Token>,
    token_index: usize,
    diagnostics: Vec<Diagnostic>,
}

impl Parser {
    #[inline]
    #[must_use]
    pub const fn new(token_stream: Vec<Token>) -> Self {
        Self {
            token_stream,
            token_index: 0,
            diagnostics: vec![],
        }
    }

    #[inline]
    pub fn parse(&mut self) -> MusiResult<Program> {
        let start_span = match self.current_token() {
            Some(token) => token.span,
            None => return Err(self.error("unexpected end of file")),
        };
        let mut statements = vec![];

        while !self.is_at_end() {
            match self.current_token() {
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
                            self.recover_from_error();
                        }
                    },
                },
                None => break,
            }
        }

        Ok(Program {
            statements,
            span: start_span,
        })
    }

    fn parse_statement(&mut self) -> MusiResult<Statement> {
        let Some(current) = self.current_token() else {
            return Err(self.error("unexpected end of file"));
        };
        let start_span = current.span;

        let kind = match self.current_token() {
            Some(token) => match token.kind {
                TokenKind::Let | TokenKind::Var => self.parse_variable_declaration(start_span)?,
                _ => StatementKind::Expression(self.parse_expression()?),
            },
            None => return Err(self.error("unexpected end of file")),
        };

        Ok(Statement {
            kind,
            span: Span {
                start: start_span.start,
                end: match self.previous_token() {
                    Some(previous) => previous.span.end,
                    None => return Err(self.error("unexpected end of file")),
                },
            },
        })
    }

    fn parse_variable_declaration(&mut self, span: Span) -> Result<StatementKind, Diagnostic> {
        let kind = self.advance().kind; // let | var
        let mutable = matches!(kind, TokenKind::Var);

        let name = String::from_utf8(
            self.expect_token(TokenKind::Identifer, "expected identifier")?
                .lexeme,
        )
        .map_err(|_error| self.error("identifier contains invalid UTF-8 character(s)"))?
        .into();

        self.expect_token(TokenKind::ColonEquals, "expected ':=' after identifier")?;
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
        let mut left_expression = self.parse_unary()?;

        while let Some(current) = self.current_token() {
            if precedence <= Precedence::from(current.kind) {
                let operator = self.advance().kind;
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
        let Some(current) = self.current_token() else {
            return self.parse_primary();
        };

        match current.kind {
            TokenKind::Minus | TokenKind::Not | TokenKind::Ref | TokenKind::Deref => {
                let start_span = current.span;
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

                    let digit_sequence =
                        from_utf8(&lexeme).map_err(|_error| "number must be valid UTF-8");
                    match digit_sequence {
                        Ok(digit) if digit.starts_with("0x") || digit.starts_with("0X") => {
                            let hexadecimal_digits = digit
                                .get(2..)
                                .ok_or_else(|| self.error("expected '0x' or '0X'"))?;

                            let value = i64::from_str_radix(hexadecimal_digits, 16)
                                .map_err(|_error| self.error("malformed hexadecimal integer"))?;
                            Value::Integer(value)
                        }
                        Ok(digit) if digit.starts_with("0b") || digit.starts_with("0B") => {
                            let binary_digits = digit
                                .get(2..)
                                .ok_or_else(|| self.error("expected '0b' or '0B'"))?;

                            let value = i64::from_str_radix(binary_digits, 2)
                                .map_err(|_error| self.error("malformed binary integer"))?;
                            Value::Integer(value)
                        }
                        Ok(digit) if digit.starts_with("0o") || digit.starts_with("0O") => {
                            let octal_digits = digit
                                .get(2..)
                                .ok_or_else(|| self.error("expected '0o' or '0O'"))?;

                            let value = i64::from_str_radix(octal_digits, 8)
                                .map_err(|_error| self.error("malformed octal integer"))?;
                            Value::Integer(value)
                        }
                        Ok(digit) if digit.contains('.') => {
                            let value = digit
                                .parse::<f64>()
                                .map_err(|_error| self.error("malformed real number"))?;
                            Value::Real(value)
                        }
                        Ok(digit) => {
                            let value = digit
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
                self.expect_token(TokenKind::RightParen, "expected ')' after expression")?;
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
    fn current_token(&self) -> Option<&Token> {
        self.token_stream.get(self.token_index)
    }

    #[inline]
    fn previous_token(&self) -> Option<&Token> {
        self.token_stream.get(self.token_index.saturating_sub(1))
    }

    #[inline]
    fn advance(&mut self) -> Token {
        if !self.is_at_end() {
            self.token_index = self.token_index.saturating_add(1);
        }

        self.previous_token()
            .cloned()
            .unwrap_or_else(|| Token::new(TokenKind::Eof, &[], Span::default()))
    }

    #[inline]
    fn is_at_end(&self) -> bool {
        matches!(self.current_token(), Some(current) if current.kind == TokenKind::Eof)
    }

    #[inline]
    fn match_token_kind(&self, kind: TokenKind) -> bool {
        if self.is_at_end() {
            false
        } else {
            matches!(self.current_token(), Some(current) if current.kind == kind)
        }
    }

    #[inline]
    fn expect_token(&mut self, kind: TokenKind, message: &str) -> MusiResult<Token> {
        if self.match_token_kind(kind) {
            Ok(self.advance())
        } else {
            Err(self.error(message))
        }
    }

    #[inline]
    fn error(&mut self, message: &str) -> Diagnostic {
        let diagnostic = Diagnostic::error(
            message.to_owned(),
            self.current_token()
                .map_or_else(Span::default, |token| token.span),
        );
        self.diagnostics.push(diagnostic.clone());
        diagnostic
    }

    #[inline]
    fn recover_from_error(&mut self) {
        self.advance();

        while !self.is_at_end() {
            if let Some(previous) = self.previous_token() {
                if previous.kind == TokenKind::Newline {
                    return;
                }
            }

            if let Some(current) = self.current_token() {
                match current.kind {
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
