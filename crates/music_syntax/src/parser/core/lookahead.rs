use super::*;

impl Parser<'_> {
    pub(crate) fn is_pi_paren(&self) -> bool {
        if !matches!(self.nth_kind(1), TokenKind::Ident) {
            return false;
        }
        if self.nth_kind(2) != TokenKind::Colon {
            return false;
        }
        let Some(close) = self.lparen_match.get(self.pos).copied().flatten() else {
            return false;
        };
        self.tokens
            .get(close + 1)
            .is_some_and(|token| matches!(token.kind, TokenKind::MinusGt | TokenKind::TildeGt))
    }

    pub(crate) fn is_comparison_expr(&self, node: SyntaxNodeId) -> bool {
        self.comparison_exprs.contains(&node)
    }
}
