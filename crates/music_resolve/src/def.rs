use music_arena::Idx;
use music_builtins::types::BuiltinType;
use music_found::{Span, Symbol};

/// Typed index into the definition arena.
pub type DefId = Idx<DefInfo>;

/// Metadata about a resolved definition.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DefInfo {
    pub name: Symbol,
    pub span: Span,
    pub kind: DefKind,
    pub vis: Visibility,
}

/// What kind of entity a definition represents.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum DefKind {
    Value,
    Function,
    Type,
    TypeClass,
    Effect,
    TypeParam,
    Variant,
    Field,
    Method,
    Law,
    Builtin(BuiltinType),
    Import,
}

/// Visibility level of a definition.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum Visibility {
    #[default]
    Private,
    Exported,
    Opaque,
}

#[cfg(test)]
#[allow(clippy::unwrap_used, clippy::panic)]
mod tests;
