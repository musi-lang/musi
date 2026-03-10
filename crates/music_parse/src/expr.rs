//! Expression parsing: Pratt engine, postfix chains, atoms.

mod atom;
mod compound;
mod postfix;
#[cfg(test)]
mod tests;

use music_ast::expr::{BinOp, Expr, UnaryOp};
use music_ast::ty::Quantifier;
use music_lex::token::TokenKind;

use crate::error::ParseError;
use crate::parser::Parser;

impl Parser<'_> {
    /// Entry point for expression parsing.
    pub(crate) fn parse_expr(&mut self) -> Expr {
        self.parse_pratt(0)
    }

    fn parse_pratt(&mut self, min_bp: u16) -> Expr {
        self.parse_pratt_until(min_bp, None)
    }

    /// Like `parse_pratt(0)` but stops before `in` (for let-in disambiguation).
    pub(crate) fn parse_expr_no_in(&mut self) -> Expr {
        self.parse_pratt_until(0, Some(TokenKind::KwIn))
    }

    /// Like `parse_pratt(0)` but stops before `|` (pipe-separated arms).
    pub(crate) fn parse_arm_body(&mut self) -> Expr {
        self.parse_pratt_until(0, Some(TokenKind::Pipe))
    }

    /// Core Pratt loop: parses infix operators with binding power >= `min_bp`,
    /// stopping early if the current token matches `stop`.
    fn parse_pratt_until(&mut self, min_bp: u16, stop: Option<TokenKind>) -> Expr {
        let start = self.start_span();
        let mut lhs = self.parse_expr_nud_chain();
        lhs = self.parse_expr_postfix_chain(lhs, start);

        loop {
            if let Some(stop_kind) = stop
                && self.at(stop_kind)
            {
                break;
            }
            let Some((l_bp, r_bp, op)) = self.infix_bp() else {
                break;
            };
            if l_bp < min_bp {
                break;
            }
            let _op_tok = self.bump();
            let rhs = self.parse_pratt(r_bp);
            let lhs_idx = self.alloc_expr(lhs);
            let rhs_idx = self.alloc_expr(rhs);
            lhs = Expr::BinOp {
                op,
                left: lhs_idx,
                right: rhs_idx,
                span: self.finish_span(start),
            };
        }

        lhs
    }

    /// Returns `(l_bp, r_bp, op)` for the current token if it's an infix
    /// operator.
    #[must_use]
    fn infix_bp(&self) -> Option<(u16, u16, BinOp)> {
        use BinOp as B;
        use TokenKind as T;

        let (l, r, op) = match self.peek_kind() {
            // BP 10 — assign (right-assoc)
            T::LtDash => (10, 9, B::Assign),
            // BP 15 — nil coalescing (right-assoc)
            T::QuestionQuestion => (15, 14, B::NilCoal),
            // BP 20 — pipe (left-assoc)
            T::PipeGt => (20, 21, B::Pipe),
            // BP 30 — comparison (non-assoc)
            T::Eq => (30, 31, B::Eq),
            T::SlashEq => (30, 31, B::Ne),
            T::Lt => (30, 31, B::Lt),
            T::Gt => (30, 31, B::Gt),
            T::LtEq => (30, 31, B::Le),
            T::GtEq => (30, 31, B::Ge),
            T::KwIn => (30, 31, B::In),
            // BP 40 — or (left-assoc)
            T::KwOr => (40, 41, B::Or),
            // BP 45 — xor (left-assoc)
            T::KwXor => (45, 46, B::Xor),
            // BP 50 — and (left-assoc)
            T::KwAnd => (50, 51, B::And),
            // BP 60 — range (non-assoc)
            T::DotDot => (60, 61, B::RangeInc),
            T::DotDotLt => (60, 61, B::RangeExc),
            // BP 70 — cons (right-assoc)
            T::ColonColon => (70, 69, B::Cons),
            // BP 80 — shift (left-assoc)
            T::LtLt => (80, 81, B::Shl),
            T::GtGt => (80, 81, B::Shr),
            // BP 90 — additive (left-assoc)
            T::Plus => (90, 91, B::Add),
            T::Minus => (90, 91, B::Sub),
            // BP 100 — multiplicative (left-assoc)
            T::Star => (100, 101, B::Mul),
            T::Slash => (100, 101, B::Div),
            T::Percent => (100, 101, B::Rem),
            _ => return None,
        };
        Some((l, r, op))
    }

    fn parse_expr_nud_chain(&mut self) -> Expr {
        match self.peek_kind() {
            // Prefix unary: - expr, not expr (BP 110 > multiplicative)
            TokenKind::Minus => self.parse_expr_unary_op(UnaryOp::Neg, 110),
            TokenKind::KwNot => self.parse_expr_unary_op(UnaryOp::Not, 110),

            // Keyword prefix: defer/spawn/await/try expr (BP 0)
            TokenKind::KwDefer => self.parse_expr_unary_op(UnaryOp::Defer, 0),
            TokenKind::KwSpawn => self.parse_expr_unary_op(UnaryOp::Spawn, 0),
            TokenKind::KwAwait => self.parse_expr_unary_op(UnaryOp::Await, 0),
            TokenKind::KwTry => self.parse_expr_unary_op(UnaryOp::Try, 0),

            // Literals
            TokenKind::IntLit | TokenKind::FloatLit | TokenKind::StringLit | TokenKind::RuneLit => {
                self.parse_expr_lit()
            }

            // F-string
            TokenKind::FStringHead => self.parse_expr_lit_fstr(),

            // Identifier (then check for record literal: Name.{ ... })
            TokenKind::Ident => self.parse_expr_record_or_name(),

            // Parenthesised expression / tuple / block / piecewise / fn literal
            TokenKind::LParen => self.parse_expr_paren(),

            // Array literal
            TokenKind::LBracket => self.parse_array_expr(),

            // Anonymous record: .{ ... }
            TokenKind::DotLBrace => self.parse_expr_record_anon(),

            // Variant constructor: .Name or .Name(args)
            TokenKind::Dot => self.parse_expr_variant(),

            // Bindings
            TokenKind::KwLet => self.parse_expr_let(),
            TokenKind::KwVar => self.parse_expr_binding_mut(),

            // Return
            TokenKind::KwReturn => self.parse_expr_return(),

            // Match
            TokenKind::KwMatch => self.parse_expr_match(),

            // Quantified expressions
            TokenKind::KwForall => self.parse_expr_quantified(Quantifier::Forall),
            TokenKind::KwExists => self.parse_expr_quantified(Quantifier::Exists),

            // Import
            TokenKind::KwImport => self.parse_expr_import(),

            // Export
            TokenKind::KwExport => self.parse_expr_export(),

            // Class / Given / Effect / Foreign
            TokenKind::KwClass => self.parse_expr_class(),
            TokenKind::KwGiven => self.parse_expr_given(),
            TokenKind::KwEffect => self.parse_expr_effect(),
            TokenKind::KwForeign => self.parse_expr_foreign(),

            // Annotated: #[...] decl
            TokenKind::HashLBracket => self.parse_expr_annotated_chain(),

            _ => self.error_expr(&ParseError::unexpected_kind(self.peek_kind())),
        }
    }

    fn parse_expr_unary_op(&mut self, op: UnaryOp, bp: u16) -> Expr {
        let start = self.start_span();
        let _tok = self.bump();
        let inner = self.parse_pratt(bp);
        let operand = self.alloc_expr(inner);
        Expr::UnaryOp {
            op,
            operand,
            span: self.finish_span(start),
        }
    }
}
