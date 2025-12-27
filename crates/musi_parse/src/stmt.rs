use musi_ast::{Prog, StmtId, StmtKind};
use musi_basic::error::MusiResult;
use musi_lex::token::TokenKind;

use crate::Parser;

impl Parser<'_> {
    /// # Errors
    /// Returns `ParseErrorKind` on syntax error.
    pub fn parse_stmt(&mut self) -> MusiResult<StmtId> {
        let expr_id = self.parse_expr()?;
        let _ = self.expect(TokenKind::Semicolon)?;
        let span = self.prev_span();
        Ok(self.arena.alloc_stmt(StmtKind::Expr(expr_id), span))
    }

    pub fn parse_prog(&mut self) -> Prog {
        let mut stmts = vec![];
        while !self.is_eof() {
            match self.parse_stmt() {
                Ok(stmt_id) => stmts.push(stmt_id),
                Err(e) => {
                    self.report(e);
                    self.sync_to_stmt();
                }
            }
        }
        Prog { stmts }
    }
}
