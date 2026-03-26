use music_ast::data::AstData;
use music_shared::{Interner, SourceMap};

/// Central compiler database holding AST, interned strings, and source files.
///
/// Extension crates (e.g. `music_resolve`) wrap `Db` with additional
/// side-tables rather than adding fields here, avoiding circular deps.
pub struct Db {
    pub ast: AstData,
    pub interner: Interner,
    pub source: SourceMap,
}

impl Db {
    #[must_use]
    pub const fn new(ast: AstData, interner: Interner, source: SourceMap) -> Self {
        Self {
            ast,
            interner,
            source,
        }
    }
}
