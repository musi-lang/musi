use std::collections::HashMap;

use music_base::diag::Diag;
use music_base::{SourceId, Span};
use music_hir::{HirExpr, HirExprId, HirExprKind, HirModule, HirOrigin, HirStore};
use music_module::{
    ImportEnv, ModuleExportSummary, ModuleKey, ModuleSpecifier, collect_export_summary,
};
use music_names::{
    Ident, Interner, KnownSymbols, NameBinding, NameBindingId, NameBindingKind, NameResolution,
    NameSite, Symbol,
};
use music_syntax::{SyntaxNode, SyntaxToken, SyntaxTree, TokenKind, canonical_name_text};

mod expr;
mod imports;
mod names;
mod pat;
mod stmt;
mod util;

pub type ResolvedImportList = Vec<ResolvedImport>;
pub type ResolveDiagList = Vec<Diag>;

#[derive(Default)]
pub struct ResolveOptions<'env> {
    pub prelude: Vec<Symbol>,
    pub import_env: Option<&'env dyn ImportEnv>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ResolvedImport {
    pub span: Span,
    pub spec: ModuleSpecifier,
    pub to: ModuleKey,
}

#[derive(Debug, Clone)]
pub struct ResolvedModule {
    pub module_key: ModuleKey,
    pub module: HirModule,
    pub imports: ResolvedImportList,
    pub export_summary: ModuleExportSummary,
    pub names: NameResolution,
    pub diags: ResolveDiagList,
}

#[must_use]
pub fn resolve_module(
    source_id: SourceId,
    module_key: &ModuleKey,
    tree: &SyntaxTree<'_>,
    interner: &mut Interner,
    options: ResolveOptions<'_>,
) -> ResolvedModule {
    let mut resolver = Resolver::new(source_id, tree, interner, options);
    let imports = resolver.discover_imports(module_key);
    let root = resolver.lower_source_file();
    let module = HirModule::new(source_id, resolver.store, root);
    let export_summary = collect_export_summary(source_id, tree);
    ResolvedModule {
        module_key: module_key.clone(),
        module,
        imports,
        export_summary,
        names: resolver.names,
        diags: resolver.diags,
    }
}

#[derive(Debug, Default)]
struct Scope {
    names: HashMap<Symbol, NameBindingId>,
}

struct Resolver<'a, 'env, 'tree, 'src> {
    source_id: SourceId,
    tree: &'tree SyntaxTree<'src>,
    interner: &'a mut Interner,
    import_env: Option<&'env dyn ImportEnv>,

    store: HirStore,

    names: NameResolution,
    diags: ResolveDiagList,
    scopes: Vec<Scope>,
}

impl<'a, 'env, 'tree, 'src> Resolver<'a, 'env, 'tree, 'src> {
    fn new(
        source_id: SourceId,
        tree: &'tree SyntaxTree<'src>,
        interner: &'a mut Interner,
        options: ResolveOptions<'env>,
    ) -> Self {
        let store = HirStore::new();

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
            source_id,
            tree,
            interner,
            import_env: options.import_env,
            store,
            names,
            diags: Vec::new(),
            scopes: vec![root],
        }
    }

    fn alloc_expr(&mut self, origin: HirOrigin, kind: HirExprKind) -> HirExprId {
        self.store.alloc_expr(HirExpr { origin, kind })
    }

    fn error_expr(&mut self, origin: HirOrigin) -> HirExprId {
        self.alloc_expr(origin, HirExprKind::Error)
    }

    fn lower_opt_expr(
        &mut self,
        origin: HirOrigin,
        node: Option<SyntaxNode<'tree, 'src>>,
    ) -> HirExprId {
        match node {
            Some(node) => self.lower_expr(node),
            None => self.error_expr(origin),
        }
    }

    fn origin_node(&self, node: SyntaxNode<'tree, 'src>) -> HirOrigin {
        HirOrigin::new(self.source_id, node.span())
    }

    fn origin_token(&self, tok: SyntaxToken<'tree, 'src>) -> HirOrigin {
        HirOrigin::new(self.source_id, tok.span())
    }

    fn intern_ident_text(&mut self, token_kind: TokenKind, raw: &str, span: Span) -> Ident {
        let canon = canonical_name_text(token_kind, raw);
        let sym = self.interner.intern(canon);
        Ident::new(sym, span)
    }

    fn placeholder_ident(&mut self, span: Span) -> Ident {
        Ident::new(self.interner.intern("_"), span)
    }

    fn intern_ident_token_or_placeholder(
        &mut self,
        tok: Option<SyntaxToken<'tree, 'src>>,
        span: Span,
    ) -> Ident {
        tok.and_then(|t| self.intern_ident_token(t))
            .unwrap_or_else(|| self.placeholder_ident(span))
    }
}
