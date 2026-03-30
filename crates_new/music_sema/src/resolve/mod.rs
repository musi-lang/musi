mod attr;
mod cursor;
mod expr;
mod member;
mod pat;
mod ty;

use std::collections::HashMap;

use music_ast::{SyntaxNode, SyntaxNodeKind, SyntaxToken, SyntaxTree};
use music_basic::{SourceId, SourceMap, Span};
use music_hir::{
    HirExpr, HirExprId, HirExprKind, HirModule, HirOrigin, HirStore, HirTy, HirTyId, HirTyKind,
};
use music_lex::TokenKind;
use music_names::{Ident, Interner, Symbol};

use crate::{SemaError, SemaErrorKind};

#[derive(Debug, Default)]
pub struct ResolveOptions {
    pub prelude: Vec<Symbol>,
}

impl ResolveOptions {
    #[must_use]
    pub fn with_prelude<'a>(
        interner: &mut Interner,
        names: impl IntoIterator<Item = &'a str>,
    ) -> Self {
        Self {
            prelude: names.into_iter().map(|s| interner.intern(s)).collect(),
        }
    }
}

#[derive(Debug)]
pub struct ResolvedModule {
    pub module: HirModule,
    pub errors: Vec<SemaError>,
}

#[must_use]
pub fn resolve_module(
    tree: &SyntaxTree,
    sources: &SourceMap,
    interner: &mut Interner,
    options: ResolveOptions,
) -> ResolvedModule {
    let mut resolver = Resolver::new(tree, sources, interner, options);
    let root = resolver.lower_source_file();
    let module = HirModule::new(resolver.store, root);
    ResolvedModule {
        module,
        errors: resolver.errors,
    }
}

#[derive(Debug, Default)]
struct Scope {
    names: HashMap<Symbol, Span>,
}

struct Resolver<'a, 'tree> {
    tree: &'tree SyntaxTree,
    sources: &'a SourceMap,
    interner: &'a mut Interner,
    source_id: SourceId,

    store: HirStore,
    error_ty: HirTyId,

    errors: Vec<SemaError>,
    scopes: Vec<Scope>,
}

impl<'a, 'tree> Resolver<'a, 'tree> {
    fn new(
        tree: &'tree SyntaxTree,
        sources: &'a SourceMap,
        interner: &'a mut Interner,
        options: ResolveOptions,
    ) -> Self {
        let mut store = HirStore::new();
        let error_ty = store.tys.alloc(HirTy {
            origin: HirOrigin::dummy(),
            kind: HirTyKind::Error,
        });

        let mut root = Scope::default();
        for sym in options.prelude {
            let _prev = root.names.insert(sym, Span::DUMMY);
        }

        Self {
            tree,
            sources,
            interner,
            source_id: tree.source_id(),
            store,
            error_ty,
            errors: vec![],
            scopes: vec![root],
        }
    }

    fn lower_source_file(&mut self) -> HirExprId {
        let root = self.tree.root();
        if root.kind() != SyntaxNodeKind::SourceFile {
            return self.alloc_expr(HirOrigin::new(root.span(), Some(root.id())), HirExprKind::Error);
        }

        let mut exprs = Vec::new();

        for node in root.child_nodes() {
            if let Some(stmt_expr) = unwrap_stmt_expr(node) {
                exprs.push(self.lower_expr(stmt_expr));
            } else {
                exprs.push(self.lower_expr(node));
            }
        }

        self.alloc_expr(
            HirOrigin::new(root.span(), Some(root.id())),
            HirExprKind::Sequence {
                exprs: exprs.into_boxed_slice(),
                yields_unit: true,
            },
        )
    }

    fn alloc_expr(&mut self, origin: HirOrigin, kind: HirExprKind) -> HirExprId {
        self.store.exprs.alloc(HirExpr {
            origin,
            ty: self.error_ty,
            kind,
        })
    }

    fn push_scope(&mut self) {
        self.scopes.push(Scope::default());
    }

    fn pop_scope(&mut self) {
        let _ = self.scopes.pop();
    }

    fn define(&mut self, ident: Ident) {
        let Some(scope) = self.scopes.last_mut() else {
            return;
        };

        if let Some(first) = scope.names.get(&ident.name).copied() {
            self.errors.push(SemaError {
                kind: SemaErrorKind::DuplicateBinding {
                    name: self.interner.resolve(ident.name).to_string(),
                    first,
                },
                source_id: self.source_id,
                span: ident.span,
            });
            return;
        }

        let _prev = scope.names.insert(ident.name, ident.span);
    }

    fn is_defined(&self, sym: Symbol) -> bool {
        self.scopes.iter().rev().any(|scope| scope.names.contains_key(&sym))
    }

    fn check_use(&mut self, ident: Ident) {
        if self.is_defined(ident.name) {
            return;
        }

        self.errors.push(SemaError {
            kind: SemaErrorKind::UndefinedBinding {
                name: self.interner.resolve(ident.name).to_string(),
            },
            source_id: self.source_id,
            span: ident.span,
        });
    }

    fn error(&mut self, span: Span, what: &'static str) {
        self.errors.push(SemaError {
            kind: SemaErrorKind::MalformedSyntax { what },
            source_id: self.source_id,
            span,
        });
    }

    fn origin_node(&self, node: SyntaxNode<'tree>) -> HirOrigin {
        HirOrigin::new(node.span(), Some(node.id()))
    }

    fn slice(&mut self, span: Span) -> &'a str {
        let Some(source) = self.sources.get(self.source_id) else {
            self.errors.push(SemaError {
                kind: SemaErrorKind::MalformedSyntax { what: "missing source text" },
                source_id: self.source_id,
                span,
            });
            return "";
        };
        let start = usize::try_from(span.start).unwrap_or(0);
        let end = usize::try_from(span.end).unwrap_or(start);
        source.text().get(start..end).unwrap_or("")
    }

    fn intern_ident_token(&mut self, token: SyntaxToken<'tree>) -> Ident {
        let raw = self.slice(token.span());
        let name = match token.kind() {
            TokenKind::EscapedIdent => raw
                .strip_prefix('`')
                .and_then(|s| s.strip_suffix('`'))
                .unwrap_or(raw),
            _ => raw,
        };
        let sym = self.interner.intern(name);
        Ident::new(sym, token.span())
    }

    fn intern_op_token(&mut self, token: SyntaxToken<'tree>) -> Ident {
        let raw = self.slice(token.span());
        let sym = self.interner.intern(raw);
        Ident::new(sym, token.span())
    }
}

fn unwrap_stmt_expr(node: SyntaxNode<'_>) -> Option<SyntaxNode<'_>> {
    if node.kind() != SyntaxNodeKind::SequenceExpr {
        return None;
    }

    // Statement wrappers are `expr ';'` with no parens.
    let has_lparen = node
        .child_tokens()
        .any(|token| matches!(token.kind(), TokenKind::LParen));
    if has_lparen {
        return None;
    }

    node.child_nodes().find(|child| child.kind().is_expr())
}
