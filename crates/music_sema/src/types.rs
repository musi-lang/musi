use music_arena::Idx;
use music_builtins::types::BuiltinType;
use music_found::Symbol;

/// Interned handle for a semantic type in the type arena.
pub type SemaTypeId = Idx<Ty>;

/// Identity of a unification variable.
pub type TyVarId = u32;

/// Semantic type representation used during type checking.
///
/// Unlike the syntactic `TyKind` from the AST, this IR supports unification
/// variables, gradual types, and interned references to other types.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Ty {
    // -- Concrete --
    Builtin(BuiltinType),
    Record {
        fields: Vec<(Symbol, SemaTypeId)>,
    },
    Choice {
        variants: Vec<(Symbol, Option<SemaTypeId>)>,
    },
    Arrow {
        param: SemaTypeId,
        ret: SemaTypeId,
    },
    EffectArrow {
        param: SemaTypeId,
        ret: SemaTypeId,
        effects: Vec<SemaTypeId>,
    },
    Tuple(Vec<SemaTypeId>),
    Array(SemaTypeId),
    List(SemaTypeId),
    Union(Vec<SemaTypeId>),
    Mut(SemaTypeId),

    // -- Inference --
    Var(TyVarId),

    // -- Generics --
    Param(Symbol),
    App(SemaTypeId, Vec<SemaTypeId>),

    // -- Gradual --
    Any,
    Unknown,
    Never,
    Unit,

    // -- Class / effect references --
    Class(Symbol),
    Effect(Symbol),
}

#[cfg(test)]
#[allow(clippy::unwrap_used, clippy::panic)]
mod tests;
