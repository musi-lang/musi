use music_arena::SliceRange;
use music_names::{Ident, Symbol};

use crate::module::{HirExprId, HirLitId, HirPatId};
use crate::origin::HirOrigin;
use crate::ty::HirDim;

type ConstraintRange = SliceRange<HirConstraint>;
type MemberDefRange = SliceRange<HirMemberDef>;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct HirExportMod {
    pub opaque: bool,
}

impl HirExportMod {
    #[must_use]
    pub const fn new(opaque: bool) -> Self {
        Self { opaque }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct HirForeignMod {
    pub abi: Option<Symbol>,
}

impl HirForeignMod {
    #[must_use]
    pub const fn new(abi: Option<Symbol>) -> Self {
        Self { abi }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct HirMods {
    pub attrs: SliceRange<HirAttr>,
    pub export: Option<HirExportMod>,
    pub foreign: Option<HirForeignMod>,
    pub partial: bool,
}

impl HirMods {
    #[must_use]
    pub const fn new(
        attrs: SliceRange<HirAttr>,
        export: Option<HirExportMod>,
        foreign: Option<HirForeignMod>,
        partial: bool,
    ) -> Self {
        Self {
            attrs,
            export,
            foreign,
            partial,
        }
    }

    pub const EMPTY: Self = Self {
        attrs: SliceRange::EMPTY,
        export: None,
        foreign: None,
        partial: false,
    };

    #[must_use]
    pub const fn with_attrs(mut self, attrs: SliceRange<HirAttr>) -> Self {
        self.attrs = attrs;
        self
    }

    #[must_use]
    pub const fn with_export(mut self, export: HirExportMod) -> Self {
        self.export = Some(export);
        self
    }

    #[must_use]
    pub const fn with_foreign(mut self, foreign: HirForeignMod) -> Self {
        self.foreign = Some(foreign);
        self
    }

    #[must_use]
    pub const fn with_partial(mut self) -> Self {
        self.partial = true;
        self
    }

    #[must_use]
    pub const fn is_empty(&self) -> bool {
        self.attrs.is_empty() && self.export.is_none() && self.foreign.is_none() && !self.partial
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct HirBinder {
    pub name: Ident,
    pub ty: Option<HirExprId>,
}

impl HirBinder {
    #[must_use]
    pub const fn new(name: Ident, ty: Option<HirExprId>) -> Self {
        Self { name, ty }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct HirExpr {
    pub origin: HirOrigin,
    pub mods: HirMods,
    pub kind: HirExprKind,
}

impl HirExpr {
    #[must_use]
    pub const fn new(origin: HirOrigin, kind: HirExprKind) -> Self {
        Self {
            origin,
            mods: HirMods::EMPTY,
            kind,
        }
    }

    #[must_use]
    pub const fn with_mods(origin: HirOrigin, mods: HirMods, kind: HirExprKind) -> Self {
        Self { origin, mods, kind }
    }
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
    ArrayTy {
        dims: SliceRange<HirDim>,
        item: HirExprId,
    },
    HandlerTy {
        effect: HirExprId,
        input: HirExprId,
        output: HirExprId,
    },
    Record {
        items: SliceRange<HirRecordItem>,
    },
    Variant {
        tag: Ident,
        args: SliceRange<HirArg>,
    },

    Pi {
        binder: Ident,
        binder_ty: HirExprId,
        ret: HirExprId,
        is_effectful: bool,
    },
    Lambda {
        params: SliceRange<HirParam>,
        ret_ty: Option<HirExprId>,
        body: HirExprId,
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
        access: HirAccessKind,
        name: Ident,
    },
    RecordUpdate {
        base: HirExprId,
        items: SliceRange<HirRecordItem>,
    },
    TypeTest {
        base: HirExprId,
        ty: HirExprId,
        as_name: Option<Ident>,
    },
    TypeCast {
        base: HirExprId,
        ty: HirExprId,
    },

    Prefix {
        op: HirPrefixOp,
        expr: HirExprId,
    },
    PartialRange {
        kind: HirPartialRangeKind,
        expr: HirExprId,
    },
    Binary {
        op: HirBinaryOp,
        left: HirExprId,
        right: HirExprId,
    },

    Let {
        mods: HirLetMods,
        pat: HirPatId,
        type_params: SliceRange<HirBinder>,
        has_param_clause: bool,
        params: SliceRange<HirParam>,
        constraints: ConstraintRange,
        effects: Option<HirEffectSet>,
        sig: Option<HirExprId>,
        value: HirExprId,
    },
    Import {
        arg: HirExprId,
    },

    Match {
        scrutinee: HirExprId,
        arms: SliceRange<HirMatchArm>,
    },
    Data {
        variants: SliceRange<HirVariantDef>,
        fields: SliceRange<HirFieldDef>,
    },
    Effect {
        members: MemberDefRange,
    },
    Class {
        constraints: ConstraintRange,
        members: MemberDefRange,
    },
    Instance {
        type_params: SliceRange<HirBinder>,
        constraints: ConstraintRange,
        class: HirExprId,
        members: MemberDefRange,
    },
    Request {
        expr: HirExprId,
    },
    Unsafe {
        body: HirExprId,
    },
    HandlerLit {
        effect: Ident,
        clauses: SliceRange<HirHandleClause>,
    },
    Handle {
        expr: HirExprId,
        handler: HirExprId,
    },
    Resume {
        expr: Option<HirExprId>,
    },

    Quote {
        kind: HirQuoteKind,
    },
    Splice {
        kind: HirSpliceKind,
    },
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum HirAccessKind {
    Direct,
    Optional,
    Unwrap,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum HirPrefixOp {
    Neg,
    Not,
    Mut,
    Comptime,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum HirPartialRangeKind {
    From,
    UpTo,
    Thru,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum HirBinaryOp {
    Assign,
    Arrow,
    EffectArrow,
    TypeEq,
    Or,
    Xor,
    And,
    Eq,
    Ne,
    Lt,
    Gt,
    Le,
    Ge,
    ClosedRange,
    OpenRange,
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
    pub name: Option<Ident>,
    pub expr: HirExprId,
}

impl HirArg {
    #[must_use]
    pub const fn new(spread: bool, name: Option<Ident>, expr: HirExprId) -> Self {
        Self { spread, name, expr }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct HirParam {
    pub name: Ident,
    pub ty: Option<HirExprId>,
    pub default: Option<HirExprId>,
    pub is_comptime: bool,
}

impl HirParam {
    #[must_use]
    pub const fn new(
        name: Ident,
        ty: Option<HirExprId>,
        default: Option<HirExprId>,
        is_comptime: bool,
    ) -> Self {
        Self {
            name,
            ty,
            default,
            is_comptime,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct HirLetMods {
    pub is_rec: bool,
    pub receiver: Option<HirLetReceiver>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct HirLetReceiver {
    pub is_mut: bool,
    pub binder: Ident,
    pub ty: HirExprId,
    pub member: Ident,
}

impl HirLetReceiver {
    #[must_use]
    pub const fn new(is_mut: bool, binder: Ident, ty: HirExprId, member: Ident) -> Self {
        Self {
            is_mut,
            binder,
            ty,
            member,
        }
    }
}

impl HirLetMods {
    #[must_use]
    pub const fn new(is_rec: bool) -> Self {
        Self {
            is_rec,
            receiver: None,
        }
    }

    #[must_use]
    pub const fn with_receiver(mut self, receiver: HirLetReceiver) -> Self {
        self.receiver = Some(receiver);
        self
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct HirArrayItem {
    pub spread: bool,
    pub expr: HirExprId,
}

impl HirArrayItem {
    #[must_use]
    pub const fn new(spread: bool, expr: HirExprId) -> Self {
        Self { spread, expr }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct HirRecordItem {
    pub spread: bool,
    pub name: Option<Ident>,
    pub value: HirExprId,
}

impl HirRecordItem {
    #[must_use]
    pub const fn new(spread: bool, name: Option<Ident>, value: HirExprId) -> Self {
        Self {
            spread,
            name,
            value,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum HirTemplatePart {
    Text { value: Box<str> },
    Expr { expr: HirExprId },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct HirAttr {
    pub origin: HirOrigin,
    pub path: SliceRange<Ident>,
    pub args: SliceRange<HirAttrArg>,
}

impl HirAttr {
    #[must_use]
    pub const fn new(
        origin: HirOrigin,
        path: SliceRange<Ident>,
        args: SliceRange<HirAttrArg>,
    ) -> Self {
        Self { origin, path, args }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct HirAttrArg {
    pub name: Option<Ident>,
    pub value: HirExprId,
}

impl HirAttrArg {
    #[must_use]
    pub const fn new(name: Option<Ident>, value: HirExprId) -> Self {
        Self { name, value }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct HirMatchArm {
    pub attrs: SliceRange<HirAttr>,
    pub pat: HirPatId,
    pub guard: Option<HirExprId>,
    pub expr: HirExprId,
}

impl HirMatchArm {
    #[must_use]
    pub const fn new(
        attrs: SliceRange<HirAttr>,
        pat: HirPatId,
        guard: Option<HirExprId>,
        expr: HirExprId,
    ) -> Self {
        Self {
            attrs,
            pat,
            guard,
            expr,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum HirConstraintKind {
    Subtype,
    Implements,
    TypeEq,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct HirConstraint {
    pub name: Ident,
    pub kind: HirConstraintKind,
    pub value: HirExprId,
}

impl HirConstraint {
    #[must_use]
    pub const fn new(name: Ident, kind: HirConstraintKind, value: HirExprId) -> Self {
        Self { name, kind, value }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct HirEffectSet {
    pub items: SliceRange<HirEffectItem>,
    pub open: Option<Ident>,
}

impl HirEffectSet {
    #[must_use]
    pub const fn new(items: SliceRange<HirEffectItem>, open: Option<Ident>) -> Self {
        Self { items, open }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct HirEffectItem {
    pub name: Ident,
    pub arg: Option<HirExprId>,
}

impl HirEffectItem {
    #[must_use]
    pub const fn new(name: Ident, arg: Option<HirExprId>) -> Self {
        Self { name, arg }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum HirMemberKind {
    Let,
    Law,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct HirMemberDef {
    pub origin: HirOrigin,
    pub attrs: SliceRange<HirAttr>,
    pub kind: HirMemberKind,
    pub name: Ident,
    pub params: SliceRange<HirParam>,
    pub sig: Option<HirExprId>,
    pub value: Option<HirExprId>,
}

impl HirMemberDef {
    #[must_use]
    pub const fn new(
        origin: HirOrigin,
        attrs: SliceRange<HirAttr>,
        kind: HirMemberKind,
        name: Ident,
        params: SliceRange<HirParam>,
        sig: Option<HirExprId>,
        value: Option<HirExprId>,
    ) -> Self {
        Self {
            origin,
            attrs,
            kind,
            name,
            params,
            sig,
            value,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct HirVariantDef {
    pub origin: HirOrigin,
    pub attrs: SliceRange<HirAttr>,
    pub name: Ident,
    pub fields: SliceRange<HirVariantFieldDef>,
    pub result: Option<HirExprId>,
    pub value: Option<HirExprId>,
}

impl HirVariantDef {
    #[must_use]
    pub const fn new(
        origin: HirOrigin,
        attrs: SliceRange<HirAttr>,
        name: Ident,
        fields: SliceRange<HirVariantFieldDef>,
        result: Option<HirExprId>,
        value: Option<HirExprId>,
    ) -> Self {
        Self {
            origin,
            attrs,
            name,
            fields,
            result,
            value,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct HirVariantFieldDef {
    pub name: Option<Ident>,
    pub ty: HirExprId,
}

impl HirVariantFieldDef {
    #[must_use]
    pub const fn new(name: Option<Ident>, ty: HirExprId) -> Self {
        Self { name, ty }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct HirFieldDef {
    pub origin: HirOrigin,
    pub attrs: SliceRange<HirAttr>,
    pub name: Ident,
    pub ty: HirExprId,
    pub value: Option<HirExprId>,
}

impl HirFieldDef {
    #[must_use]
    pub const fn new(
        origin: HirOrigin,
        attrs: SliceRange<HirAttr>,
        name: Ident,
        ty: HirExprId,
        value: Option<HirExprId>,
    ) -> Self {
        Self {
            origin,
            attrs,
            name,
            ty,
            value,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct HirHandleClause {
    pub op: Ident,
    pub params: SliceRange<Ident>,
    pub body: HirExprId,
}

impl HirHandleClause {
    #[must_use]
    pub const fn new(op: Ident, params: SliceRange<Ident>, body: HirExprId) -> Self {
        Self { op, params, body }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum HirQuoteKind {
    Expr {
        expr: HirExprId,
        raw: Box<str>,
    },
    Block {
        exprs: SliceRange<HirExprId>,
        raw: Box<str>,
    },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum HirSpliceKind {
    Name {
        name: Ident,
        raw: Box<str>,
    },
    Expr {
        expr: HirExprId,
        raw: Box<str>,
    },
    Exprs {
        exprs: SliceRange<HirExprId>,
        raw: Box<str>,
    },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct HirLit {
    pub origin: HirOrigin,
    pub kind: HirLitKind,
}

impl HirLit {
    #[must_use]
    pub const fn new(origin: HirOrigin, kind: HirLitKind) -> Self {
        Self { origin, kind }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum HirLitKind {
    Int { raw: Box<str> },
    Float { raw: Box<str> },
    String { value: Box<str> },
    Rune { value: u32 },
}
