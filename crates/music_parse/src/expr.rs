//! Expression parsing: Pratt engine, postfix chains, atoms.

#[cfg(test)]
mod tests;

use music_ast::expr::{
    Arg, ArrayElem, Arrow, BinOp, Expr, FieldKey, MatchArm, PwArm, PwGuard, RecField, UnaryOp,
};
use music_ast::lit::{FStrPart, Lit};
use music_ast::ty::Quantifier;
use music_lex::token::TokenKind;
use music_shared::{Idx, Symbol};

use crate::error::ParseError;
use crate::parser::Parser;

impl Parser<'_> {
    /// Entry point for expression parsing.
    pub(crate) fn parse_expr(&mut self) -> Expr {
        self.parse_pratt(0)
    }

    fn parse_pratt(&mut self, min_bp: u16) -> Expr {
        let start = self.start_span();
        let mut lhs = self.parse_expr_nud_chain();
        lhs = self.parse_expr_postfix_chain(lhs, start);

        loop {
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

    /// Like `parse_pratt(0)` but stops before `in` (for let-in disambiguation).
    pub(crate) fn parse_expr_no_in(&mut self) -> Expr {
        let start = self.start_span();
        let mut lhs = self.parse_expr_nud_chain();
        lhs = self.parse_expr_postfix_chain(lhs, start);

        loop {
            if self.at(TokenKind::KwIn) {
                break;
            }
            let Some((_l_bp, r_bp, op)) = self.infix_bp() else {
                break;
            };
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

    /// Like `parse_pratt(0)` but stops before `|` (pipe-separated arms).
    pub(crate) fn parse_arm_body(&mut self) -> Expr {
        let start = self.start_span();
        let mut lhs = self.parse_expr_nud_chain();
        lhs = self.parse_expr_postfix_chain(lhs, start);

        loop {
            if self.at(TokenKind::Pipe) {
                break;
            }
            let Some((_l_bp, r_bp, op)) = self.infix_bp() else {
                break;
            };
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

            // Class / Given / Effect
            TokenKind::KwClass => self.parse_expr_class(),
            TokenKind::KwGiven => self.parse_expr_given(),
            TokenKind::KwEffect => self.parse_expr_effect(),

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

    fn parse_expr_lit(&mut self) -> Expr {
        let start = self.start_span();
        let lit = self.parse_lit_value();
        Expr::Lit {
            lit,
            span: self.finish_span(start),
        }
    }

    fn parse_expr_lit_fstr(&mut self) -> Expr {
        let start = self.start_span();
        let mut parts = vec![];

        let head_tok = self.bump();
        push_fstr_text(head_tok, &mut parts);

        // Middle/Tail loop
        loop {
            let expr = self.parse_expr();
            let expr_idx = self.alloc_expr(expr);
            let expr_span = self.finish_span(start);
            parts.push(FStrPart::Interpolated {
                expr: expr_idx,
                span: expr_span,
            });

            match self.peek_kind() {
                TokenKind::FStringMiddle => {
                    let mid = self.bump();
                    push_fstr_text(mid, &mut parts);
                }
                TokenKind::FStringTail => {
                    let tail = self.bump();
                    push_fstr_text(tail, &mut parts);
                    break;
                }
                _ => {
                    let _diag = self.diags.report(
                        &ParseError::UnterminatedFString,
                        self.peek().span,
                        self.file_id,
                    );
                    break;
                }
            }
        }

        let span = self.finish_span(start);
        Expr::Lit {
            lit: Lit::FStr { parts, span },
            span,
        }
    }

    fn parse_expr_record_or_name(&mut self) -> Expr {
        let start = self.start_span();
        let tok = self.bump();
        let span = tok.span;
        let sym = tok.symbol.unwrap_or(Symbol(u32::MAX));

        // check for named record literal: Name.{ ... }
        if self.at(TokenKind::DotLBrace) {
            let _dlb = self.bump();
            let fields = self.comma_sep(TokenKind::RBrace, Self::parse_rec_field);
            let _rb = self.expect(TokenKind::RBrace);
            return Expr::Record {
                ty_name: Some(sym),
                fields,
                span: self.finish_span(start),
            };
        }

        Expr::Name {
            name: sym,
            span: self.finish_span(span.start),
        }
    }

    fn parse_rec_field(&mut self) -> RecField {
        let start = self.start_span();

        // Spread: ... expr
        if self.eat(TokenKind::DotDotDot) {
            let expr = self.parse_alloc_expr();
            return RecField::Spread {
                expr,
                span: self.finish_span(start),
            };
        }

        let name = self.expect_symbol();
        let value = if self.eat(TokenKind::ColonEq) {
            Some(self.parse_alloc_expr())
        } else {
            None
        };
        RecField::Named {
            name,
            value,
            span: self.finish_span(start),
        }
    }

    fn parse_expr_paren(&mut self) -> Expr {
        let start = self.start_span();
        let _lp = self.bump();

        // () -> unit
        if self.eat(TokenKind::RParen) {
            return self.maybe_expr_lit_fn(vec![], start);
        }

        // (,) -> empty tuple
        if self.at(TokenKind::Comma) && self.peek2() == TokenKind::RParen {
            let _comma = self.bump();
            let _rp = self.bump();
            return Expr::Tuple {
                elems: vec![],
                span: self.finish_span(start),
            };
        }

        // (;) -> empty block
        if self.at(TokenKind::Semi) && self.peek2() == TokenKind::RParen {
            let _semi = self.bump();
            let _rp = self.bump();
            return Expr::Block {
                stmts: vec![],
                tail: None,
                span: self.finish_span(start),
            };
        }

        let first = self.parse_expr();

        match self.peek_kind() {
            TokenKind::Comma => self.parse_expr_tuple_tail(first, start),
            TokenKind::Semi => {
                let _semi = self.bump();
                let first_idx = self.alloc_expr(first);
                self.parse_expr_block_tail(vec![first_idx], start)
            }
            TokenKind::KwIf => {
                let first_idx = self.alloc_expr(first);
                self.parse_expr_piecewise_tail(first_idx, start)
            }
            TokenKind::RParen => self.parse_expr_paren_close(first, start),
            _ => {
                let _rp = self.expect(TokenKind::RParen);
                let inner = self.alloc_expr(first);
                Expr::Paren {
                    inner,
                    span: self.finish_span(start),
                }
            }
        }
    }

    /// Parses `(a, b, ...)` tuple after the first element and opening comma.
    fn parse_expr_tuple_tail(&mut self, first: Expr, start: u32) -> Expr {
        let _comma = self.bump();
        let mut elems = vec![self.alloc_expr(first)];
        if !self.at(TokenKind::RParen) {
            loop {
                elems.push(self.parse_alloc_expr());
                if !self.eat(TokenKind::Comma) {
                    break;
                }
                if self.at(TokenKind::RParen) {
                    break;
                }
            }
        }
        let _rp = self.expect(TokenKind::RParen);
        Expr::Tuple {
            elems,
            span: self.finish_span(start),
        }
    }

    /// Handles `)` after a single expression: either fn literal or parenthesised expr.
    fn parse_expr_paren_close(&mut self, first: Expr, start: u32) -> Expr {
        let _rp = self.bump();

        if self.at(TokenKind::DashGt) || self.at(TokenKind::TildeGt) || self.at(TokenKind::Colon) {
            return self.parse_expr_fn_after_paren(&[first], start);
        }

        let inner = self.alloc_expr(first);
        Expr::Paren {
            inner,
            span: self.finish_span(start),
        }
    }

    fn maybe_expr_lit_fn(&mut self, _exprs: Vec<Expr>, start: u32) -> Expr {
        // () followed by -> or ~> or : means fn literal
        if self.at(TokenKind::DashGt) || self.at(TokenKind::TildeGt) || self.at(TokenKind::Colon) {
            return self.parse_expr_fn_after_paren(&[], start);
        }
        Expr::Lit {
            lit: Lit::Unit {
                span: self.finish_span(start),
            },
            span: self.finish_span(start),
        }
    }

    fn parse_expr_fn_after_paren(&mut self, paren_exprs: &[Expr], start: u32) -> Expr {
        let params = self.reinterpret_as_params(paren_exprs);
        let ret_ty = self.parse_opt_ty_annot();
        let arrow = if self.eat(TokenKind::DashGt) {
            Arrow::Pure
        } else if self.eat(TokenKind::TildeGt) {
            Arrow::Effectful
        } else {
            let _span = self.expect(TokenKind::DashGt);
            Arrow::Pure
        };
        let body = self.parse_alloc_expr();
        Expr::Fn {
            params,
            arrow,
            ret_ty,
            body,
            span: self.finish_span(start),
        }
    }

    fn parse_expr_block_tail(&mut self, mut stmts: Vec<Idx<Expr>>, start: u32) -> Expr {
        while !self.at(TokenKind::RParen) && !self.at(TokenKind::Eof) {
            let e = self.parse_expr();
            if self.eat(TokenKind::Semi) {
                stmts.push(self.alloc_expr(e));
            } else {
                let tail = self.alloc_expr(e);
                let _rp = self.expect(TokenKind::RParen);
                return Expr::Block {
                    stmts,
                    tail: Some(tail),
                    span: self.finish_span(start),
                };
            }
        }
        let _rp = self.expect(TokenKind::RParen);
        Expr::Block {
            stmts,
            tail: None,
            span: self.finish_span(start),
        }
    }

    fn parse_expr_piecewise_tail(&mut self, first_result: Idx<Expr>, start: u32) -> Expr {
        // first_result if guard | ...
        let _if = self.expect(TokenKind::KwIf);
        let first_guard = self.parse_pw_guard();
        let first_span = self.finish_span(start);
        let mut arms = vec![PwArm {
            result: first_result,
            guard: first_guard,
            span: first_span,
        }];

        while self.eat(TokenKind::Pipe) {
            let arm_start = self.start_span();
            let result = self.parse_alloc_expr();
            let _if = self.expect(TokenKind::KwIf);
            let guard = self.parse_pw_guard();
            arms.push(PwArm {
                result,
                guard,
                span: self.finish_span(arm_start),
            });
        }

        let _rp = self.expect(TokenKind::RParen);
        Expr::Piecewise {
            arms,
            span: self.finish_span(start),
        }
    }

    fn parse_pw_guard(&mut self) -> PwGuard {
        if self.at(TokenKind::Underscore) {
            let start = self.start_span();
            let _us = self.bump();
            PwGuard::Any {
                span: self.finish_span(start),
            }
        } else {
            let expr = self.parse_alloc_expr();
            PwGuard::When {
                expr,
                span: self.peek().span,
            }
        }
    }

    fn parse_array_expr(&mut self) -> Expr {
        let start = self.start_span();
        let _lb = self.bump();
        let mut elems = vec![];

        if !self.at(TokenKind::RBracket) {
            loop {
                if self.eat(TokenKind::DotDotDot) {
                    let expr = self.parse_alloc_expr();
                    let span = self.finish_span(start);
                    elems.push(ArrayElem::Spread { expr, span });
                } else {
                    let expr = self.parse_alloc_expr();
                    let span = self.finish_span(start);
                    elems.push(ArrayElem::Elem { expr, span });
                }
                if !self.eat(TokenKind::Comma) {
                    break;
                }
                if self.at(TokenKind::RBracket) {
                    break;
                }
            }
        }
        let _rb = self.expect(TokenKind::RBracket);
        Expr::Array {
            elems,
            span: self.finish_span(start),
        }
    }

    fn parse_expr_record_anon(&mut self) -> Expr {
        let start = self.start_span();
        let _dlb = self.bump();
        let fields = self.comma_sep(TokenKind::RBrace, Self::parse_rec_field);
        let _rb = self.expect(TokenKind::RBrace);
        Expr::Record {
            ty_name: None,
            fields,
            span: self.finish_span(start),
        }
    }

    fn parse_expr_variant(&mut self) -> Expr {
        let start = self.start_span();
        let _dot = self.bump();
        let name = self.expect_symbol();
        let args = if self.at(TokenKind::LParen) {
            self.delimited(TokenKind::LParen, TokenKind::RParen, Self::parse_alloc_expr)
        } else {
            vec![]
        };
        Expr::Variant {
            name,
            args,
            span: self.finish_span(start),
        }
    }

    fn parse_expr_return(&mut self) -> Expr {
        let start = self.start_span();
        let _ret = self.bump();
        let value = self.parse_opt_expr();
        Expr::Return {
            value,
            span: self.finish_span(start),
        }
    }

    fn parse_expr_match(&mut self) -> Expr {
        let start = self.start_span();
        let _match = self.bump();
        // scrutinee is parsed without postfix to avoid `match x (...)` being
        // interpreted as `match (x(...))`. `(` after scrutinee starts match arms
        let scrut_expr = self.parse_expr_nud_chain();
        let scrutinee = self.alloc_expr(scrut_expr);
        let _lp = self.expect(TokenKind::LParen);
        let arms = self.pipe_sep(TokenKind::RParen, Self::parse_match_arm);
        let _rp = self.expect(TokenKind::RParen);
        Expr::Match {
            scrutinee,
            arms,
            span: self.finish_span(start),
        }
    }

    fn parse_match_arm(&mut self) -> MatchArm {
        let arm_start = self.start_span();
        let attrs = self.parse_attrs();
        let pat = self.parse_alloc_pat();
        let guard = if self.eat(TokenKind::KwIf) {
            Some(self.parse_alloc_expr())
        } else {
            None
        };
        let _arrow = self.expect(TokenKind::EqGt);
        let result_expr = self.parse_arm_body();
        let result = self.alloc_expr(result_expr);
        MatchArm {
            attrs,
            pat,
            guard,
            result,
            span: self.finish_span(arm_start),
        }
    }

    fn parse_expr_import(&mut self) -> Expr {
        let start = self.start_span();
        let _import = self.bump();
        let tok = self.bump();
        let span = tok.span;
        let path = tok.symbol.unwrap_or_else(|| {
            let _diag = self
                .diags
                .report(&ParseError::ExpectedImportPath, span, self.file_id);
            Symbol(u32::MAX)
        });
        Expr::Import {
            path,
            span: self.finish_span(start),
        }
    }

    fn parse_expr_postfix_chain(&mut self, mut lhs: Expr, start: u32) -> Expr {
        loop {
            lhs = match self.peek_kind() {
                TokenKind::LParen => self.parse_expr_call(lhs, start),
                TokenKind::DotLBracket => self.parse_expr_index(lhs, start),
                TokenKind::DotLBrace => self.parse_expr_update(lhs, start),
                TokenKind::Dot => self.parse_expr_field(lhs, start, false),
                TokenKind::QuestionDot => self.parse_expr_field(lhs, start, true),
                _ => break,
            };
        }
        lhs
    }

    fn parse_expr_call(&mut self, lhs: Expr, start: u32) -> Expr {
        let _lp = self.bump();
        let args = self.comma_sep(TokenKind::RParen, Self::parse_arg);
        let _rp = self.expect(TokenKind::RParen);
        let callee = self.alloc_expr(lhs);
        Expr::Call {
            callee,
            args,
            span: self.finish_span(start),
        }
    }

    fn parse_expr_index(&mut self, lhs: Expr, start: u32) -> Expr {
        let _dlb = self.bump();
        let index = self.parse_alloc_expr();
        let _rb = self.expect(TokenKind::RBracket);
        let object = self.alloc_expr(lhs);
        Expr::Index {
            object,
            index,
            span: self.finish_span(start),
        }
    }

    fn parse_expr_update(&mut self, lhs: Expr, start: u32) -> Expr {
        let _dlb = self.bump();
        let fields = self.comma_sep(TokenKind::RBrace, Self::parse_rec_field);
        let _rb = self.expect(TokenKind::RBrace);
        let base = self.alloc_expr(lhs);
        Expr::Update {
            base,
            fields,
            span: self.finish_span(start),
        }
    }

    fn parse_expr_field(&mut self, lhs: Expr, start: u32, safe: bool) -> Expr {
        let _tok = self.bump();
        let field = self.parse_field_key();
        let object = self.alloc_expr(lhs);
        Expr::Field {
            object,
            field,
            safe,
            span: self.finish_span(start),
        }
    }

    fn parse_field_key(&mut self) -> FieldKey {
        let start = self.start_span();
        if self.at(TokenKind::IntLit) {
            let tok = self.bump();
            let sym = tok.symbol.unwrap_or(Symbol(u32::MAX));
            let text = self.resolve(sym);
            let index = text.parse::<u32>().unwrap_or(0);
            FieldKey::Pos {
                index,
                span: self.finish_span(start),
            }
        } else {
            let name = self.expect_symbol();
            FieldKey::Name {
                name,
                span: self.finish_span(start),
            }
        }
    }

    fn parse_arg(&mut self) -> Arg {
        let start = self.start_span();
        // Hole: ...
        if self.eat(TokenKind::DotDotDot) {
            return Arg::Hole {
                span: self.finish_span(start),
            };
        }
        let expr = self.parse_alloc_expr();
        Arg::Pos {
            expr,
            span: self.finish_span(start),
        }
    }
}

use music_lex::token::Token;

/// Pushes an `FStrPart::Text` if the token carries interned text.
fn push_fstr_text(tok: &Token, parts: &mut Vec<FStrPart>) {
    if let Some(sym) = tok.symbol {
        parts.push(FStrPart::Text {
            raw: sym,
            span: tok.span,
        });
    }
}
