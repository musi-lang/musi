use music_arena::Idx;
use music_builtins::types::BuiltinType;
use music_shared::Symbol;

/// Interned handle for a semantic type in the type arena.
pub type SemaTypeId = Idx<Ty>;

/// Identity of a unification variable.
pub type TyVarId = u32;

pub type SemaTypeList = Vec<SemaTypeId>;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct NominalKey {
    pub module_name: Option<String>,
    pub name: Symbol,
}

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
    Named(NominalKey),
    Arrow {
        param: SemaTypeId,
        ret: SemaTypeId,
    },
    EffectArrow {
        param: SemaTypeId,
        ret: SemaTypeId,
        effects: SemaTypeList,
    },
    Tuple(SemaTypeList),
    Array(SemaTypeId),
    List(SemaTypeId),
    Union(SemaTypeList),
    Mut(SemaTypeId),

    // -- Inference --
    Var(TyVarId),

    // -- Generics --
    Param(Symbol),
    App(SemaTypeId, SemaTypeList),

    // -- Gradual --
    Any,
    Unknown,
    Empty,
    Unit,

    // -- Class / effect references --
    Class(Symbol),
    Effect(Symbol),
    EffectOp {
        effect: Symbol,
        op: Symbol,
        ret: SemaTypeId,
    },
}

#[cfg(test)]
#[allow(clippy::unwrap_used, clippy::panic)]
mod tests;
