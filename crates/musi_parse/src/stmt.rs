use musi_ast::{Prog, Stmt, StmtKind};
use musi_basic::error::MusiResult;
use musi_lex::token::TokenKind;

use crate::Parser;

impl Parser<'_> {
    /// # Errors
    /// Returns `ParseErrorKind` on syntax error.
    pub fn parse_stmt(&mut self) -> MusiResult<Stmt> {
        let expr = self.parse_expr()?;
        let _ = self.expect(TokenKind::Semicolon)?;
        Ok(Stmt {
            kind: StmtKind::Expr(expr),
            span: self.prev_span(),
        })
    }

    pub fn parse_prog(&mut self) -> Prog {
        let mut stmts = vec![];
        while !self.is_eof() {
            match self.parse_stmt() {
                Ok(stmt) => stmts.push(stmt),
                Err(e) => {
                    self.report(e);
                    self.sync_to_stmt();
                }
            }
        }
        Prog { stmts }
    }
}
