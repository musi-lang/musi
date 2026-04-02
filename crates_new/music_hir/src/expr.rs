use music_arena::SliceRange;
use music_names::Ident;

use crate::module::{HirExprId, HirLitId, HirPatId, HirTyId};
use crate::origin::HirOrigin;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct HirExpr {
    pub origin: HirOrigin,
    pub kind: HirExprKind,
    pub ty: HirTyId,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum HirExprKind {
    Error,
    Name {
        name: Ident,
    },
    Lit {
        lit: HirLitId,
    },
    Template {
        parts: SliceRange<HirTemplatePart>,
    },

    Sequence {
        exprs: SliceRange<HirExprId>,
    },
    Tuple {
        items: SliceRange<HirExprId>,
    },
    Array {
        items: SliceRange<HirArrayItem>,
    },
    Record {
        items: SliceRange<HirRecordItem>,
    },

    Call {
        callee: HirExprId,
        args: SliceRange<HirArg>,
    },
    Apply {
        callee: HirExprId,
        args: SliceRange<HirExprId>,
    },
    Index {
        base: HirExprId,
        args: SliceRange<HirExprId>,
    },
    Field {
        base: HirExprId,
        name: Ident,
    },
    RecordUpdate {
        base: HirExprId,
        items: SliceRange<HirRecordItem>,
    },

    Prefix {
        op: HirPrefixOp,
        expr: HirExprId,
    },
    Binary {
        op: HirBinaryOp,
        left: HirExprId,
        right: HirExprId,
    },

    Let {
        pat: HirPatId,
        value: HirExprId,
        body: HirExprId,
    },
    Import {
        arg: HirExprId,
    },
    Export {
        opaque: bool,
        expr: HirExprId,
    },

    Case {
        scrutinee: HirExprId,
        arms: SliceRange<HirCaseArm>,
    },

    Quote {
        expr: HirExprId,
    },
    Splice {
        expr: HirExprId,
    },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum HirPrefixOp {
    Neg,
    Not,
    Mut,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum HirBinaryOp {
    Assign,
    Pipe,
    Arrow,
    EffectArrow,
    Or,
    Xor,
    And,
    Eq,
    Ne,
    Lt,
    Gt,
    Le,
    Ge,
    In,
    Shl,
    Shr,
    Add,
    Sub,
    Mul,
    Div,
    Rem,
    UserOp(Ident),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct HirArg {
    pub spread: bool,
    pub expr: HirExprId,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct HirArrayItem {
    pub spread: bool,
    pub expr: HirExprId,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct HirRecordItem {
    pub spread: bool,
    pub name: Option<Ident>,
    pub value: HirExprId,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum HirTemplatePart {
    Text { value: Box<str> },
    Expr { expr: HirExprId },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct HirCaseArm {
    pub pat: HirPatId,
    pub guard: Option<HirExprId>,
    pub expr: HirExprId,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct HirLit {
    pub origin: HirOrigin,
    pub kind: HirLitKind,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum HirLitKind {
    Int { raw: Box<str> },
    Float { raw: Box<str> },
    String { value: Box<str> },
    Rune { value: u32 },
}
