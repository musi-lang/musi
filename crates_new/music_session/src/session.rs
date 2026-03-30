use std::path::PathBuf;

use music_basic::{SourceId, SourceMap};
use music_check::AnalyzedModule;
use music_known::KnownSymbols;
use music_lex::{LexError, LexedSource, Lexer};
use music_names::{Interner, Symbol};
use music_parse::ParsedSource;
use music_resolve::ResolveOptions;

use crate::{SessionImportEnv, SessionImportModule};

pub struct SessionParsedSource {
    pub source_id: SourceId,
    pub lex_errors: Vec<LexError>,
    pub parsed: ParsedSource,
}

pub struct SessionAnalyzedSource {
    pub parsed: SessionParsedSource,
    pub analyzed: AnalyzedModule,
}

/// Session owns the shared compiler context needed to compile a set of sources.
#[derive(Debug)]
pub struct Session {
    sources: SourceMap,
    interner: Interner,
    known: KnownSymbols,
    import_env: SessionImportEnv,
    prelude: Vec<Symbol>,
}

impl Default for Session {
    fn default() -> Self {
        Self::new()
    }
}

impl Session {
    #[must_use]
    pub fn new() -> Self {
        let mut interner = Interner::new();
        let known = KnownSymbols::new(&mut interner);
        let prelude = known.compiler_prelude().to_vec();
        Self {
            sources: SourceMap::new(),
            interner,
            known,
            import_env: SessionImportEnv::new(),
            prelude,
        }
    }

    #[must_use]
    pub const fn known(&self) -> KnownSymbols {
        self.known
    }

    #[must_use]
    pub fn sources(&self) -> &SourceMap {
        &self.sources
    }

    #[must_use]
    pub fn interner(&self) -> &Interner {
        &self.interner
    }

    #[must_use]
    pub fn interner_mut(&mut self) -> &mut Interner {
        &mut self.interner
    }

    #[must_use]
    pub fn import_env(&self) -> &SessionImportEnv {
        &self.import_env
    }

    #[must_use]
    pub fn import_env_mut(&mut self) -> &mut SessionImportEnv {
        &mut self.import_env
    }

    pub fn add_source(&mut self, path: impl Into<PathBuf>, text: impl Into<String>) -> SourceId {
        self.sources.add(path, text)
    }

    #[must_use]
    pub fn lex(&self, source_id: SourceId) -> Option<LexedSource<'_>> {
        let source = self.sources.get(source_id)?;
        Some(Lexer::new(source.text()).lex())
    }

    #[must_use]
    pub fn parse(&self, source_id: SourceId) -> Option<SessionParsedSource> {
        let source = self.sources.get(source_id)?;
        let lexed = Lexer::new(source.text()).lex();
        let lex_errors = lexed.errors().to_vec();
        let parsed = music_parse::parse(source_id, &lexed);
        Some(SessionParsedSource {
            source_id,
            lex_errors,
            parsed,
        })
    }

    #[must_use]
    pub fn analyze(&mut self, source_id: SourceId) -> Option<SessionAnalyzedSource> {
        let parsed = self.parse(source_id)?;
        let options = ResolveOptions {
            prelude: self.prelude.clone(),
            import_env: Some(&self.import_env),
        };

        let analyzed = music_check::analyze_module(
            parsed.parsed.tree(),
            &self.sources,
            &mut self.interner,
            options,
        );

        self.register_analyzed_module(source_id, &analyzed);
        Some(SessionAnalyzedSource { parsed, analyzed })
    }

    fn register_analyzed_module(&mut self, source_id: SourceId, analyzed: &AnalyzedModule) {
        let Some(source) = self.sources.get(source_id) else {
            return;
        };
        let path = source.path().to_string_lossy().into_owned();
        if path.is_empty() {
            return;
        }

        let exports = analyzed
            .exports
            .iter()
            .map(|&sym| String::from(self.interner.resolve(sym)))
            .collect::<Vec<_>>();

        let opaque_exports = collect_top_level_opaque_exports(&analyzed.module, &self.interner);
        self.import_env.insert(
            path,
            SessionImportModule::with_opaque_exports(exports, opaque_exports),
        );
    }
}

fn collect_top_level_opaque_exports(module: &music_hir::HirModule, interner: &Interner) -> Vec<String> {
    let mut out = Vec::new();
    let root = module.store.exprs.get(module.root);
    let music_hir::HirExprKind::Sequence { exprs, .. } = &root.kind else {
        return out;
    };

    for expr_id in exprs.iter().copied() {
        let expr = module.store.exprs.get(expr_id);
        let (mods, pat) = match &expr.kind {
            music_hir::HirExprKind::Let { mods, pat, .. } => (mods, *pat),
            _ => continue,
        };
        if !mods.exported || !mods.opaque {
            continue;
        }

        let pat = module.store.pats.get(pat);
        let music_hir::HirPatKind::Bind { name, .. } = &pat.kind else {
            continue;
        };
        out.push(String::from(interner.resolve(name.name)));
    }

    out
}
