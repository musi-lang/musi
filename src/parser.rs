use crate::{
    ast::{DeclarationKind, ExpressionKind, Node, StatementKind},
    errors::{MusiError, MusiResult, SyntaxError},
    span::Span,
    token::{Kind, LiteralKind, Token},
    value::Value,
};

pub struct Parser {
    tokens: Vec<Token>,
    current: usize,
}

impl Parser {
    pub const fn new(tokens: Vec<Token>) -> Self {
        Self { tokens, current: 0 }
    }

    pub fn parse(&mut self) -> MusiResult<Vec<Node>> {
        let mut nodes = vec![];

        while !self.is_at_end() {
            match self.peek().kind {
                Kind::Eof => break,
                Kind::Newline => {
                    self.advance();
                    continue;
                }
                _ => nodes.push(self.parse_declaration()?),
            }
        }

        Ok(nodes)
    }

    fn parse_declaration(&mut self) -> MusiResult<Node> {
        match self.peek().kind {
            Kind::Let | Kind::Var => self.parse_variable_declaration(),
            _ => self.parse_statement(),
        }
    }

    fn parse_statement(&mut self) -> MusiResult<Node> {
        let expression = self.parse_expression()?;
        let span = expression.span();

        Ok(Node::Statement(Box::new(
            StatementKind::ExpressionStatement {
                kind: expression,
                span,
            },
        )))
    }

    fn parse_expression(&mut self) -> MusiResult<ExpressionKind> {
        let expression = self.parse_primary()?;

        if self.peek().kind == Kind::LessMinus {
            let operator = self.advance().clone();
            let value = self.parse_expression()?;

            return Ok(ExpressionKind::Assignment {
                target: Box::new(expression),
                value: Box::new(value),
                span: Span {
                    start: operator.span.start,
                    end: self.previous().span.end,
                },
            });
        }

        Ok(expression)
    }

    fn parse_primary(&mut self) -> MusiResult<ExpressionKind> {
        match self.peek().kind {
            Kind::Literal(kind) => {
                let token = self.advance();
                Ok(ExpressionKind::Literal {
                    value: match kind {
                        LiteralKind::Number => {
                            let lexeme = &token.lexeme;
                            if lexeme.contains('.') {
                                match lexeme.parse::<f64>() {
                                    Ok(r) => Value::Real(r),
                                    Err(_) => {
                                        return Err(MusiError::Syntax(SyntaxError {
                                            message: "invalid real number literal",
                                        }))
                                    }
                                }
                            } else if lexeme.starts_with('-') {
                                match lexeme.parse::<i64>() {
                                    Ok(i) => Value::Integer(i),
                                    Err(_) => {
                                        return Err(MusiError::Syntax(SyntaxError {
                                            message: "invalid integer literal",
                                        }))
                                    }
                                }
                            } else {
                                match lexeme.parse::<u64>() {
                                    Ok(n) => Value::Natural(n),
                                    Err(_) => {
                                        return Err(MusiError::Syntax(SyntaxError {
                                            message: "invalid natural number literal",
                                        }))
                                    }
                                }
                            }
                        }
                        _ => unimplemented!("other literal types"),
                    },
                    span: token.span,
                })
            }
            Kind::Identifier => {
                let token = self.advance();
                Ok(ExpressionKind::Identifier {
                    name: token.clone(),
                    span: token.span,
                })
            }
            _unexpected => Err(MusiError::Syntax(SyntaxError {
                message: "expected expression",
            })),
        }
    }

    fn parse_variable_declaration(&mut self) -> MusiResult<Node> {
        let start_token = self.advance().clone();
        let mutable = start_token.kind == Kind::Var;

        let name = self
            .consume(Kind::Identifier, "expected IDENTIFIER")?
            .clone();
        self.consume(Kind::ColonEquals, "expected ':=' after IDENTIFIER")?;
        let initialiser = self.parse_expression()?;

        Ok(Node::Declaration(Box::new(DeclarationKind::Variable {
            name,
            mutable,
            initialiser: Some(initialiser),
            span: Span {
                start: start_token.span.start,
                end: self.previous().span.end,
            },
        })))
    }
}

impl Parser {
    #[inline]
    fn peek(&self) -> &Token {
        if self.current >= self.tokens.len() {
            &self.tokens[self.tokens.len() - 1]
        } else {
            &self.tokens[self.current]
        }
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
