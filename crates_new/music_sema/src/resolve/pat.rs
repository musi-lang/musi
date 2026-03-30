use music_ast::{SyntaxNode, SyntaxNodeKind};
use music_hir::{HirLit, HirPat, HirPatId, HirPatKind, HirPatKind::*, HirRecordPatField};
use music_lex::TokenKind;

use super::Resolver;

impl<'a, 'tree> Resolver<'a, 'tree> {
    pub(super) fn lower_pat(&mut self, node: SyntaxNode<'tree>, bind_names: bool) -> HirPatId {
        let origin = self.origin_node(node);

        let kind = match node.kind() {
            SyntaxNodeKind::WildcardPat => Wildcard,
            SyntaxNodeKind::BindPat => self.lower_bind_pat(node, bind_names),
            SyntaxNodeKind::LiteralPat => {
                let token = match node.child_tokens().next() {
                    Some(token) => token,
                    None => {
                        self.error(node.span(), "expected literal token");
                        return self.alloc_pat(origin, Error);
                    }
                };
                Lit {
                    lit: self.lower_lit_token(token),
                }
            }
            SyntaxNodeKind::VariantPat => {
                let mut tokens = node.child_tokens();
                let _dot = tokens.next();
                let Some(name_tok) = tokens.next() else {
                    self.error(node.span(), "expected variant name");
                    return self.alloc_pat(origin, Error);
                };
                let name = self.intern_ident_token(name_tok);
                let args: Vec<HirPatId> = node
                    .child_nodes()
                    .filter(|n| n.kind().is_pat())
                    .map(|pat| self.lower_pat(pat, bind_names))
                    .collect();
                Variant {
                    name,
                    args: args.into_boxed_slice(),
                }
            }
            SyntaxNodeKind::RecordPat => {
                let mut fields = Vec::new();
                let mut it = node.children().peekable();
                while let Some(el) = it.next() {
                    let Some(tok) = el.into_token() else {
                        continue;
                    };
                    if matches!(tok.kind(), TokenKind::RBrace | TokenKind::DotLBrace | TokenKind::LBrace) {
                        continue;
                    }
                    let (mutable, name_tok) = if matches!(tok.kind(), TokenKind::KwMut) {
                        let Some(name_tok) = it.next().and_then(|e| e.into_token()) else {
                            self.error(node.span(), "expected record pattern field name");
                            break;
                        };
                        (true, name_tok)
                    } else {
                        (false, tok)
                    };

                    if !matches!(name_tok.kind(), TokenKind::Ident | TokenKind::EscapedIdent) {
                        continue;
                    }
                    let name = self.intern_ident_token(name_tok);

                    // If a ':' follows, the next pat node is the subpattern.
                    let sub = if it.peek().and_then(|e| e.into_token()).is_some_and(|t| matches!(t.kind(), TokenKind::Colon)) {
                        let _ = it.next();
                        node.child_nodes()
                            .filter(|n| n.kind().is_pat())
                            .find(|pat| pat.span().start >= name_tok.span().end)
                            .map(|pat| self.lower_pat(pat, bind_names))
                    } else {
                        // Shorthand binds the field name.
                        if bind_names {
                            self.define(name);
                        }
                        None
                    };

                    fields.push(HirRecordPatField {
                        origin: self.origin_node(node),
                        mutable,
                        name,
                        sub,
                    });
                }

                Record {
                    fields: fields.into_boxed_slice(),
                }
            }
            SyntaxNodeKind::TuplePat => {
                let items: Vec<HirPatId> = node
                    .child_nodes()
                    .filter(|n| n.kind().is_pat())
                    .map(|pat| self.lower_pat(pat, bind_names))
                    .collect();
                Tuple {
                    items: items.into_boxed_slice(),
                }
            }
            SyntaxNodeKind::ArrayPat => {
                let items: Vec<HirPatId> = node
                    .child_nodes()
                    .filter(|n| n.kind().is_pat())
                    .map(|pat| self.lower_pat(pat, bind_names))
                    .collect();
                Array {
                    items: items.into_boxed_slice(),
                }
            }
            SyntaxNodeKind::OrPat => {
                let mut alts = Vec::new();
                self.collect_or_pats(node, bind_names, &mut alts);
                Or {
                    alts: alts.into_boxed_slice(),
                }
            }
            _ => {
                self.error(node.span(), "expected pattern");
                Error
            }
        };

        self.alloc_pat(origin, kind)
    }
    pub(super) fn alloc_pat(&mut self, origin: music_hir::HirOrigin, kind: HirPatKind) -> HirPatId {
        self.store.pats.alloc(HirPat {
            origin,
            ty: self.error_ty,
            kind,
        })
    }

    fn lower_bind_pat(&mut self, node: SyntaxNode<'tree>, bind_names: bool) -> HirPatKind {
        let mut tokens = node.child_tokens();
        let Some(name_tok) = tokens.next() else {
            self.error(node.span(), "expected binding name");
            return Error;
        };
        let name = self.intern_ident_token(name_tok);
        if bind_names {
            self.define(name);
        }

        let sub = node
            .child_nodes()
            .find(|n| n.kind().is_pat())
            .map(|pat| self.lower_pat(pat, bind_names));

        Bind { name, sub }
    }

    fn collect_or_pats(
        &mut self,
        node: SyntaxNode<'tree>,
        bind_names: bool,
        out: &mut Vec<HirPatId>,
    ) {
        if node.kind() == SyntaxNodeKind::OrPat {
            let mut children = node.child_nodes().filter(|n| n.kind().is_pat());
            if let Some(left) = children.next() {
                self.collect_or_pats(left, bind_names, out);
            }
            if let Some(right) = children.next() {
                self.collect_or_pats(right, bind_names, out);
            }
            return;
        }

        out.push(self.lower_pat(node, bind_names));
    }

    pub(super) fn lower_lit_token(&mut self, token: music_ast::SyntaxToken<'tree>) -> HirLit {
        let kind = match token.kind() {
            TokenKind::IntLit => music_hir::HirLitKind::Int {
                span: token.span(),
                syntax: Some(token.id()),
            },
            TokenKind::FloatLit => music_hir::HirLitKind::Float {
                span: token.span(),
                syntax: Some(token.id()),
            },
            TokenKind::RuneLit => music_hir::HirLitKind::Rune {
                span: token.span(),
                syntax: Some(token.id()),
            },
            TokenKind::StringLit => music_hir::HirLitKind::String(music_hir::HirStringLit::new(
                token.span(),
                Some(token.id()),
            )),
            TokenKind::FStringLit(parts) => {
                let mut hir_parts = Vec::new();
                for part in parts {
                    match part.kind {
                        music_lex::FStringPartKind::Literal => {
                            hir_parts.push(music_hir::HirFStringPart::Literal { span: part.span });
                        }
                        music_lex::FStringPartKind::Interpolation => {
                            // Resolver-only lowering: interpolation expressions are not parsed yet.
                            // The span is still preserved so later layers can re-parse from source.
                            let origin = music_hir::HirOrigin::new(part.span, None);
                            let expr = self.alloc_expr(origin, music_hir::HirExprKind::Error);
                            hir_parts.push(music_hir::HirFStringPart::Expr { origin, expr });
                        }
                    }
                }
                music_hir::HirLitKind::FString {
                    span: token.span(),
                    syntax: Some(token.id()),
                    parts: hir_parts.into_boxed_slice(),
                }
            }
            _ => {
                self.error(token.span(), "expected literal token");
                music_hir::HirLitKind::String(music_hir::HirStringLit::new(
                    token.span(),
                    Some(token.id()),
                ))
            }
        };

        HirLit { kind }
    }
}
