use std::collections::BTreeMap;

use music_ast::{SyntaxNode, SyntaxNodeKind};
use music_basic::Span;
use music_hir::{HirLit, HirPat, HirPatId, HirPatKind, HirPatKind::*, HirRecordPatField};
use music_lex::TokenKind;
use music_names::{Ident, NameBindingKind, Symbol};

use crate::{ResolveError, ResolveErrorKind};

use super::Resolver;

impl<'a, 'tree, 'env> Resolver<'a, 'tree, 'env> {
    pub(super) fn lower_pat(
        &mut self,
        node: SyntaxNode<'tree>,
        bind_names: bool,
        bind_kind: NameBindingKind,
    ) -> HirPatId {
        let origin = self.origin_node(node);

        let kind = match node.kind() {
            SyntaxNodeKind::WildcardPat => Wildcard,
            SyntaxNodeKind::BindPat => self.lower_bind_pat(node, bind_names, bind_kind),
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
                    .map(|pat| self.lower_pat(pat, bind_names, bind_kind))
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
                    if matches!(
                        tok.kind(),
                        TokenKind::RBrace | TokenKind::DotLBrace | TokenKind::LBrace
                    ) {
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
                    let sub = if it
                        .peek()
                        .and_then(|e| e.into_token())
                        .is_some_and(|t| matches!(t.kind(), TokenKind::Colon))
                    {
                        let _ = it.next();
                        node.child_nodes()
                            .filter(|n| n.kind().is_pat())
                            .find(|pat| pat.span().start >= name_tok.span().end)
                            .map(|pat| self.lower_pat(pat, bind_names, bind_kind))
                    } else {
                        // Shorthand binds the field name.
                        if bind_names {
                            self.define(bind_kind, name);
                        } else {
                            self.check_use(name);
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
                    .map(|pat| self.lower_pat(pat, bind_names, bind_kind))
                    .collect();
                Tuple {
                    items: items.into_boxed_slice(),
                }
            }
            SyntaxNodeKind::ArrayPat => {
                let items: Vec<HirPatId> = node
                    .child_nodes()
                    .filter(|n| n.kind().is_pat())
                    .map(|pat| self.lower_pat(pat, bind_names, bind_kind))
                    .collect();
                Array {
                    items: items.into_boxed_slice(),
                }
            }
            SyntaxNodeKind::OrPat => {
                let mut alt_nodes = Vec::new();
                self.collect_or_pat_nodes(node, &mut alt_nodes);
                let alts = self.lower_or_pat_alts(bind_names, bind_kind, &alt_nodes);
                Or { alts }
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

    fn lower_bind_pat(
        &mut self,
        node: SyntaxNode<'tree>,
        bind_names: bool,
        bind_kind: NameBindingKind,
    ) -> HirPatKind {
        let mut tokens = node.child_tokens();
        let Some(name_tok) = tokens.next() else {
            self.error(node.span(), "expected binding name");
            return Error;
        };
        let name = if matches!(name_tok.kind(), TokenKind::LParen) {
            let op_tok = tokens.next();
            let _close = tokens.next();
            op_tok.map(|t| self.intern_op_token(t)).unwrap_or_else(|| {
                self.error(node.span(), "expected binding operator name");
                Ident::dummy(Symbol::synthetic(u32::MAX))
            })
        } else {
            self.intern_ident_token(name_tok)
        };
        if bind_names {
            self.define(bind_kind, name);
        } else {
            self.check_use(name);
        }

        let sub = node
            .child_nodes()
            .find(|n| n.kind().is_pat())
            .map(|pat| self.lower_pat(pat, bind_names, bind_kind));

        Bind { name, sub }
    }

    fn collect_or_pat_nodes(&mut self, node: SyntaxNode<'tree>, out: &mut Vec<SyntaxNode<'tree>>) {
        if node.kind() == SyntaxNodeKind::OrPat {
            let mut children = node.child_nodes().filter(|n| n.kind().is_pat());
            if let Some(left) = children.next() {
                self.collect_or_pat_nodes(left, out);
            }
            if let Some(right) = children.next() {
                self.collect_or_pat_nodes(right, out);
            }
            return;
        }

        out.push(node);
    }

    fn lower_or_pat_alts(
        &mut self,
        bind_names: bool,
        bind_kind: NameBindingKind,
        alt_nodes: &[SyntaxNode<'tree>],
    ) -> Box<[HirPatId]> {
        if !bind_names {
            let out: Vec<_> = alt_nodes
                .iter()
                .copied()
                .map(|n| self.lower_pat(n, false, bind_kind))
                .collect();
            return out.into_boxed_slice();
        }

        let mut binds_per_alt: Vec<BTreeMap<Symbol, Span>> = Vec::new();
        for alt in alt_nodes {
            let mut binds = BTreeMap::new();
            self.collect_pat_binds(*alt, &mut binds);
            binds_per_alt.push(binds);
        }

        let Some(first) = binds_per_alt.first() else {
            return Box::new([]);
        };

        for binds in &binds_per_alt[1..] {
            if binds.keys().ne(first.keys()) {
                self.errors.push(ResolveError {
                    kind: ResolveErrorKind::OrPatternBindingsMismatch,
                    source_id: self.source_id,
                    span: alt_nodes[0].span(),
                });
                break;
            }
        }

        for (&sym, &span) in first {
            self.define(bind_kind, music_names::Ident::new(sym, span));
        }

        let out: Vec<_> = alt_nodes
            .iter()
            .copied()
            .map(|n| self.lower_pat(n, false, bind_kind))
            .collect();
        out.into_boxed_slice()
    }

    fn collect_pat_binds(&mut self, node: SyntaxNode<'tree>, out: &mut BTreeMap<Symbol, Span>) {
        match node.kind() {
            SyntaxNodeKind::BindPat => {
                let Some(name_tok) = node.child_tokens().next() else {
                    return;
                };
                if !matches!(name_tok.kind(), TokenKind::Ident | TokenKind::EscapedIdent) {
                    return;
                }
                let ident = self.intern_ident_token(name_tok);
                if let Some(first) = out.insert(ident.name, ident.span) {
                    self.errors.push(ResolveError {
                        kind: ResolveErrorKind::DuplicateBinding {
                            name: self.interner.resolve(ident.name).to_string(),
                            first,
                        },
                        source_id: self.source_id,
                        span: ident.span,
                    });
                }
                if let Some(sub) = node.child_nodes().find(|n| n.kind().is_pat()) {
                    self.collect_pat_binds(sub, out);
                }
            }
            SyntaxNodeKind::RecordPat => {
                let mut it = node.children().peekable();
                while let Some(el) = it.next() {
                    let Some(tok) = el.into_token() else {
                        continue;
                    };
                    if matches!(
                        tok.kind(),
                        TokenKind::RBrace | TokenKind::DotLBrace | TokenKind::LBrace
                    ) {
                        continue;
                    }
                    let name_tok = if matches!(tok.kind(), TokenKind::KwMut) {
                        it.next().and_then(|e| e.into_token())
                    } else {
                        Some(tok)
                    };
                    let Some(name_tok) = name_tok else {
                        break;
                    };
                    if !matches!(name_tok.kind(), TokenKind::Ident | TokenKind::EscapedIdent) {
                        continue;
                    }
                    let ident = self.intern_ident_token(name_tok);

                    let has_sub = it
                        .peek()
                        .and_then(|e| e.into_token())
                        .is_some_and(|t| matches!(t.kind(), TokenKind::Colon));
                    if has_sub {
                        let _ = it.next();
                        if let Some(sub) = node
                            .child_nodes()
                            .filter(|n| n.kind().is_pat())
                            .find(|pat| pat.span().start >= name_tok.span().end)
                        {
                            self.collect_pat_binds(sub, out);
                        }
                    } else {
                        if let Some(first) = out.insert(ident.name, ident.span) {
                            self.errors.push(ResolveError {
                                kind: ResolveErrorKind::DuplicateBinding {
                                    name: self.interner.resolve(ident.name).to_string(),
                                    first,
                                },
                                source_id: self.source_id,
                                span: ident.span,
                            });
                        }
                    }
                }
            }
            SyntaxNodeKind::VariantPat | SyntaxNodeKind::TuplePat | SyntaxNodeKind::ArrayPat => {
                for child in node.child_nodes().filter(|n| n.kind().is_pat()) {
                    self.collect_pat_binds(child, out);
                }
            }
            SyntaxNodeKind::OrPat => {
                // Bind mismatch is handled by the caller that lowers the `OrPat`.
            }
            _ => {}
        }
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
