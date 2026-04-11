use super::*;

impl<'tree, 'src> Resolver<'_, '_, 'tree, 'src>
where
    'tree: 'src,
{
    pub(super) fn lower_prefix_expr(&mut self, node: SyntaxNode<'tree, 'src>) -> HirExprId {
        let origin = self.origin_node(node);
        let op_tok = node.child_tokens().next();
        let op = match op_tok.map(SyntaxToken::kind) {
            Some(TokenKind::Minus) => HirPrefixOp::Neg,
            Some(TokenKind::KwMut) => HirPrefixOp::Mut,
            _ => HirPrefixOp::Not,
        };
        let expr = self.lower_opt_expr(origin, node.child_nodes().next());
        self.alloc_expr(origin, HirExprKind::Prefix { op, expr })
    }

    pub(super) fn lower_binary_expr(&mut self, node: SyntaxNode<'tree, 'src>) -> HirExprId {
        let origin = self.origin_node(node);
        let mut nodes = node.child_nodes();
        let left = self.lower_opt_expr(origin, nodes.next());
        let right = self.lower_opt_expr(origin, nodes.next());

        let op_tok = node.child_tokens().find(|t| t.kind() != TokenKind::Eof);
        let op = match op_tok.map(SyntaxToken::kind) {
            Some(TokenKind::ColonEq) => HirBinaryOp::Assign,
            Some(TokenKind::PipeGt) => HirBinaryOp::Pipe,
            Some(TokenKind::MinusGt) => HirBinaryOp::Arrow,
            Some(TokenKind::TildeGt) => HirBinaryOp::EffectArrow,
            Some(TokenKind::KwXor) => HirBinaryOp::Xor,
            Some(TokenKind::KwAnd) => HirBinaryOp::And,
            Some(TokenKind::Eq) => HirBinaryOp::Eq,
            Some(TokenKind::SlashEq) => HirBinaryOp::Ne,
            Some(TokenKind::Lt) => HirBinaryOp::Lt,
            Some(TokenKind::Gt) => HirBinaryOp::Gt,
            Some(TokenKind::LtEq) => HirBinaryOp::Le,
            Some(TokenKind::GtEq) => HirBinaryOp::Ge,
            Some(TokenKind::KwIn) => HirBinaryOp::In,
            Some(TokenKind::KwShl) => HirBinaryOp::Shl,
            Some(TokenKind::KwShr) => HirBinaryOp::Shr,
            Some(TokenKind::Plus) => HirBinaryOp::Add,
            Some(TokenKind::Minus) => HirBinaryOp::Sub,
            Some(TokenKind::Star) => HirBinaryOp::Mul,
            Some(TokenKind::Slash) => HirBinaryOp::Div,
            Some(TokenKind::Percent) => HirBinaryOp::Rem,
            Some(TokenKind::SymbolicOp) => {
                let tok = op_tok.expect("symbolic op token must exist");
                let raw = tok.text().unwrap_or("");
                let ident = self.intern_ident_text(tok.kind(), raw, tok.span());
                self.record_use(ident);
                HirBinaryOp::UserOp(ident)
            }
            _ => HirBinaryOp::Or,
        };
        self.alloc_expr(origin, HirExprKind::Binary { op, left, right })
    }
}
