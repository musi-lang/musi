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
    HirExpr, HirExprId, HirExprKind, HirModule, HirOrigin, HirPatId, HirPatKind, HirStore, HirTy,
    HirTyId, HirTyKind,
};
use music_known::KnownSymbols;
use music_lex::TokenKind;
use music_names::{
    Ident, Interner, NameBinding, NameBindingId, NameBindingKind, NameResolution, NameSite, Symbol,
};

use crate::{ResolveError, ResolveErrorKind};

pub trait ImportEnv {
    fn has_module(&self, from: SourceId, path: &str) -> bool;
    fn for_each_export(&self, from: SourceId, path: &str, f: &mut dyn FnMut(&str));
}

pub struct ResolveOptions<'env> {
    pub prelude: Vec<Symbol>,
    pub import_env: Option<&'env dyn ImportEnv>,
}

impl<'env> Default for ResolveOptions<'env> {
    fn default() -> Self {
        Self {
            prelude: vec![],
            import_env: None,
        }
    }
}

impl<'env> ResolveOptions<'env> {
    #[must_use]
    pub fn with_prelude<'a>(
        interner: &mut Interner,
        names: impl IntoIterator<Item = &'a str>,
    ) -> Self {
        Self {
            prelude: names.into_iter().map(|s| interner.intern(s)).collect(),
            import_env: None,
        }
    }

    #[must_use]
    pub const fn with_import_env(mut self, import_env: &'env dyn ImportEnv) -> Self {
        self.import_env = Some(import_env);
        self
    }
}

#[derive(Debug)]
pub struct ResolvedModule {
    pub module: HirModule,
    pub exports: Box<[Symbol]>,
    pub names: NameResolution,
    pub errors: Vec<ResolveError>,
}

#[must_use]
pub fn resolve_module(
    tree: &SyntaxTree,
    sources: &SourceMap,
    interner: &mut Interner,
    options: ResolveOptions<'_>,
) -> ResolvedModule {
    let mut resolver = Resolver::new(tree, sources, interner, options);
    let root = resolver.lower_source_file();
    let module = HirModule::new(resolver.store, root);
    let exports = resolver.exports.into_boxed_slice();
    ResolvedModule {
        module,
        exports,
        names: resolver.names,
        errors: resolver.errors,
    }
}

#[derive(Debug, Default)]
struct Scope {
    names: HashMap<Symbol, NameBindingId>,
}

struct Resolver<'a, 'tree, 'env> {
    tree: &'tree SyntaxTree,
    sources: &'a SourceMap,
    interner: &'a mut Interner,
    source_id: SourceId,

    import_env: Option<&'env dyn ImportEnv>,

    store: HirStore,
    error_ty: HirTyId,

    names: NameResolution,
    exports: Vec<Symbol>,
    errors: Vec<ResolveError>,
    scopes: Vec<Scope>,
}

impl<'a, 'tree, 'env> Resolver<'a, 'tree, 'env> {
    fn new(
        tree: &'tree SyntaxTree,
        sources: &'a SourceMap,
        interner: &'a mut Interner,
        options: ResolveOptions<'env>,
    ) -> Self {
        let source_id = tree.source_id();

        let mut store = HirStore::new();
        let error_ty = store.tys.alloc(HirTy {
            origin: HirOrigin::dummy(),
            kind: HirTyKind::Error,
        });

        let mut names = NameResolution::new();
        let mut root = Scope::default();

        let known = KnownSymbols::new(interner);
        for sym in known.compiler_prelude() {
            let binding = names.alloc_binding(NameBinding {
                name: sym,
                site: NameSite::new(source_id, Span::DUMMY),
                kind: NameBindingKind::Prelude,
            });
            let _prev = root.names.insert(sym, binding);
        }

        // Project/tooling prelude names (additional).
        for sym in options.prelude {
            if root.names.contains_key(&sym) {
                continue;
            }
            let binding = names.alloc_binding(NameBinding {
                name: sym,
                site: NameSite::new(source_id, Span::DUMMY),
                kind: NameBindingKind::Prelude,
            });
            let _prev = root.names.insert(sym, binding);
        }

        Self {
            tree,
            sources,
            interner,
            source_id,
            import_env: options.import_env,
            store,
            error_ty,
            names,
            exports: vec![],
            errors: vec![],
            scopes: vec![root],
        }
    }

    fn lower_source_file(&mut self) -> HirExprId {
        let root = self.tree.root();
        if root.kind() != SyntaxNodeKind::SourceFile {
            return self.alloc_expr(
                HirOrigin::new(root.span(), Some(root.id())),
                HirExprKind::Error,
            );
        }

        let mut exprs = Vec::new();

        for node in root.child_nodes() {
            if is_stmt_wrapper(node) {
                let Some(stmt_expr) = node.child_nodes().find(|child| child.kind().is_expr())
                else {
                    exprs.push(self.lower_expr(node));
                    continue;
                };

                if stmt_expr.kind() == SyntaxNodeKind::ImportExpr {
                    self.open_import_expr(stmt_expr);
                }

                exprs.push(self.lower_expr(stmt_expr));
                continue;
            }

            exprs.push(self.lower_expr(node));
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

    fn record_exports_from_pat(&mut self, pat: HirPatId) {
        let mut stack = vec![pat];
        while let Some(pat_id) = stack.pop() {
            let pat = self.store.pats[pat_id].kind.clone();
            match pat {
                HirPatKind::Wildcard | HirPatKind::Lit { .. } | HirPatKind::Error => {}
                HirPatKind::Bind { name, sub } => {
                    self.record_export_symbol(name.name);
                    if let Some(sub) = sub {
                        stack.push(sub);
                    }
                }
                HirPatKind::Variant { args, .. } => stack.extend(args.iter().copied()),
                HirPatKind::Record { fields } => {
                    for field in fields {
                        // Shorthand record pattern fields bind the field name.
                        if field.sub.is_none() {
                            self.record_export_symbol(field.name.name);
                        }
                        if let Some(sub) = field.sub {
                            stack.push(sub);
                        }
                    }
                }
                HirPatKind::Tuple { items }
                | HirPatKind::Array { items }
                | HirPatKind::Or { alts: items } => {
                    stack.extend(items.iter().copied());
                }
            }
        }
    }

    fn record_export_symbol(&mut self, sym: Symbol) {
        if self.exports.contains(&sym) {
            return;
        }
        self.exports.push(sym);
    }

    fn push_scope(&mut self) {
        self.scopes.push(Scope::default());
    }

    fn pop_scope(&mut self) {
        let _ = self.scopes.pop();
    }

    fn define(&mut self, kind: NameBindingKind, ident: Ident) {
        let Some(scope) = self.scopes.last_mut() else {
            return;
        };

        if let Some(first_id) = scope.names.get(&ident.name).copied() {
            let first_span = self.names.bindings.get(first_id).site.span;
            self.errors.push(ResolveError {
                kind: ResolveErrorKind::DuplicateBinding {
                    name: self.interner.resolve(ident.name).to_string(),
                    first: first_span,
                },
                source_id: self.source_id,
                span: ident.span,
            });
            return;
        }

        let binding = self.names.alloc_binding(NameBinding {
            name: ident.name,
            site: NameSite::new(self.source_id, ident.span),
            kind,
        });
        let _prev = scope.names.insert(ident.name, binding);
    }

    fn lookup(&self, sym: Symbol) -> Option<NameBindingId> {
        self.scopes
            .iter()
            .rev()
            .find_map(|scope| scope.names.get(&sym).copied())
    }

    fn check_use(&mut self, ident: Ident) {
        let Some(binding) = self.lookup(ident.name) else {
            self.errors.push(ResolveError {
                kind: ResolveErrorKind::UndefinedBinding {
                    name: self.interner.resolve(ident.name).to_string(),
                },
                source_id: self.source_id,
                span: ident.span,
            });
            return;
        };

        self.names
            .record_ref(NameSite::new(self.source_id, ident.span), binding);
    }

    fn error(&mut self, span: Span, what: &'static str) {
        self.errors.push(ResolveError {
            kind: ResolveErrorKind::MalformedSyntax { what },
            source_id: self.source_id,
            span,
        });
    }

    fn origin_node(&self, node: SyntaxNode<'tree>) -> HirOrigin {
        HirOrigin::new(node.span(), Some(node.id()))
    }

    fn slice(&mut self, span: Span) -> &'a str {
        let Some(source) = self.sources.get(self.source_id) else {
            self.errors.push(ResolveError {
                kind: ResolveErrorKind::MalformedSyntax {
                    what: "missing source text",
                },
                source_id: self.source_id,
                span,
            });
            return "";
        };
        let start = usize::try_from(span.start).unwrap_or(0);
        let end = usize::try_from(span.end).unwrap_or(start);
        source.text().get(start..end).unwrap_or("")
    }

    fn decode_string_lit_span(&mut self, span: Span) -> String {
        music_basic::string_lit::decode(self.slice(span))
    }

    fn open_import_expr(&mut self, node: SyntaxNode<'tree>) {
        let Some(path_tok) = node
            .child_tokens()
            .find(|t| matches!(t.kind(), TokenKind::StringLit))
        else {
            self.error(node.span(), "expected string literal import path");
            return;
        };

        let path = self.decode_string_lit_span(path_tok.span());
        let Some(env) = self.import_env else {
            self.errors.push(ResolveError {
                kind: ResolveErrorKind::UnresolvedImport { path },
                source_id: self.source_id,
                span: node.span(),
            });
            return;
        };

        if !env.has_module(self.source_id, &path) {
            self.errors.push(ResolveError {
                kind: ResolveErrorKind::UnresolvedImport { path },
                source_id: self.source_id,
                span: node.span(),
            });
            return;
        }

        let source_id = self.source_id;
        let site_span = node.span();
        let mut insert = |name: &str| {
            let sym = self.interner.intern(name);
            self.define(NameBindingKind::Import, Ident::new(sym, site_span));
        };
        env.for_each_export(source_id, &path, &mut insert);
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

fn is_stmt_wrapper(node: SyntaxNode<'_>) -> bool {
    if node.kind() != SyntaxNodeKind::SequenceExpr {
        return false;
    }

    // Statement wrappers are `expr ';'` with no parens.
    let has_lparen = node
        .child_tokens()
        .any(|token| matches!(token.kind(), TokenKind::LParen));
    if has_lparen {
        return false;
    }

    true
}
