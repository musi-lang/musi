use super::*;

impl<'tree, 'src> Resolver<'_, '_, 'tree, 'src>
where
    'tree: 'src,
{
    pub(super) fn lower_prefix_expr(&mut self, node: SyntaxNode<'tree, 'src>) -> HirExprId {
        let origin = self.origin_node(node);
        let op_tok = node.child_tokens().next();
        let Some(kind) = op_tok.map(SyntaxToken::kind) else {
            return self.error_expr(origin);
        };
        if matches!(kind, TokenKind::DotDot | TokenKind::DotDotLt) {
            let expr = self.lower_opt_expr(origin, node.child_nodes().next());
            let partial_kind = match kind {
                TokenKind::DotDot => HirPartialRangeKind::UpTo {
                    include_upper: true,
                },
                TokenKind::DotDotLt => HirPartialRangeKind::UpTo {
                    include_upper: false,
                },
                _ => return self.error_expr(origin),
            };
            return self.alloc_expr(
                origin,
                HirExprKind::PartialRange {
                    kind: partial_kind,
                    expr,
                },
            );
        }
        let op = match kind {
            TokenKind::Minus => HirPrefixOp::Neg,
            TokenKind::KwKnown => HirPrefixOp::Known,
            TokenKind::KwMut => HirPrefixOp::Mut,
            TokenKind::KwAny => HirPrefixOp::Any,
            TokenKind::KwSome => HirPrefixOp::Some,
            _ => HirPrefixOp::Not,
        };
        let expr = self.lower_opt_expr(origin, node.child_nodes().next());
        self.alloc_expr(origin, HirExprKind::Prefix { op, expr })
    }

    pub(super) fn lower_postfix_expr(&mut self, node: SyntaxNode<'tree, 'src>) -> HirExprId {
        let origin = self.origin_node(node);
        let expr = self.lower_opt_expr(origin, node.child_nodes().next());
        self.alloc_expr(
            origin,
            HirExprKind::PartialRange {
                kind: match node.child_tokens().last().map(SyntaxToken::kind) {
                    Some(TokenKind::LtDotDot) => HirPartialRangeKind::From {
                        include_lower: false,
                    },
                    _ => HirPartialRangeKind::From {
                        include_lower: true,
                    },
                },
                expr,
            },
        )
    }

    pub(super) fn lower_binary_expr(&mut self, node: SyntaxNode<'tree, 'src>) -> HirExprId {
        let origin = self.origin_node(node);
        let mut nodes = node.child_nodes();
        let left = self.lower_opt_expr(origin, nodes.next());
        let Some(right_node) = nodes.next() else {
            return left;
        };
        let right = self.lower_opt_expr(origin, Some(right_node));

        let op_tok = node.child_tokens().find(|t| t.kind() != TokenKind::Eof);
        if matches!(op_tok.map(SyntaxToken::kind), Some(TokenKind::PipeGt)) {
            return self.alloc_call_expr_with_piped_value(origin, left, right);
        }

        let op = self.lower_binary_op(op_tok);
        self.alloc_expr(origin, HirExprKind::Binary { op, left, right })
    }

    fn lower_binary_op(&mut self, op_tok: Option<SyntaxToken<'tree, 'src>>) -> HirBinaryOp {
        let Some(tok) = op_tok else {
            return HirBinaryOp::Or;
        };
        lower_fixed_binary_op(tok.kind()).unwrap_or_else(|| self.lower_fallback_binary_op(tok))
    }

    fn lower_fallback_binary_op(&mut self, tok: SyntaxToken<'tree, 'src>) -> HirBinaryOp {
        match tok.kind() {
            TokenKind::SymbolicOp => {
                let raw = tok.text().unwrap_or("");
                let ident = self.intern_ident_text(tok.kind(), raw, tok.span());
                self.record_use(ident);
                HirBinaryOp::UserOp(ident)
            }
            _ => HirBinaryOp::Or,
        }
    }
}

fn lower_fixed_binary_op(kind: TokenKind) -> Option<HirBinaryOp> {
    fixed_binary_ops()
        .iter()
        .find_map(|(token, op)| (*token == kind).then(|| op.clone()))
}

const fn fixed_binary_ops() -> &'static [(TokenKind, HirBinaryOp)] {
    &[
        (TokenKind::ColonEq, HirBinaryOp::Assign),
        (TokenKind::MinusGt, HirBinaryOp::Arrow),
        (TokenKind::TildeGt, HirBinaryOp::EffectArrow),
        (TokenKind::TildeEq, HirBinaryOp::TypeEq),
        (TokenKind::KwXor, HirBinaryOp::Xor),
        (TokenKind::KwAnd, HirBinaryOp::And),
        (TokenKind::Eq, HirBinaryOp::Eq),
        (TokenKind::SlashEq, HirBinaryOp::Ne),
        (TokenKind::Lt, HirBinaryOp::Lt),
        (TokenKind::Gt, HirBinaryOp::Gt),
        (TokenKind::LtEq, HirBinaryOp::Le),
        (TokenKind::GtEq, HirBinaryOp::Ge),
        (
            TokenKind::DotDot,
            HirBinaryOp::Range {
                include_lower: true,
                include_upper: true,
            },
        ),
        (
            TokenKind::DotDotLt,
            HirBinaryOp::Range {
                include_lower: true,
                include_upper: false,
            },
        ),
        (
            TokenKind::LtDotDot,
            HirBinaryOp::Range {
                include_lower: false,
                include_upper: true,
            },
        ),
        (
            TokenKind::LtDotDotLt,
            HirBinaryOp::Range {
                include_lower: false,
                include_upper: false,
            },
        ),
        (TokenKind::KwIn, HirBinaryOp::In),
        (TokenKind::KwShl, HirBinaryOp::Shl),
        (TokenKind::KwShr, HirBinaryOp::Shr),
        (TokenKind::Plus, HirBinaryOp::Add),
        (TokenKind::Minus, HirBinaryOp::Sub),
        (TokenKind::Star, HirBinaryOp::Mul),
        (TokenKind::Slash, HirBinaryOp::Div),
        (TokenKind::Percent, HirBinaryOp::Rem),
    ]
}
