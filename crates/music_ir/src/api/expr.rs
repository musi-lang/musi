use music_base::{SourceId, Span};
use music_module::ModuleKey;
use music_names::NameBindingId;
use music_sema::DefinitionKey;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct IrOrigin {
    pub source_id: SourceId,
    pub span: Span,
}

impl IrOrigin {
    #[must_use]
    pub const fn new(source_id: SourceId, span: Span) -> Self {
        Self { source_id, span }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct IrTempId(u32);

impl IrTempId {
    #[must_use]
    pub const fn from_raw(raw: u32) -> Self {
        Self(raw)
    }

    #[must_use]
    pub const fn raw(self) -> u32 {
        self.0
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct IrParam {
    pub binding: Option<NameBindingId>,
    pub name: Box<str>,
}

impl IrParam {
    #[must_use]
    pub fn new<Name>(binding: NameBindingId, name: Name) -> Self
    where
        Name: Into<Box<str>>,
    {
        Self {
            binding: Some(binding),
            name: name.into(),
        }
    }

    #[must_use]
    pub fn synthetic<Name>(name: Name) -> Self
    where
        Name: Into<Box<str>>,
    {
        Self {
            binding: None,
            name: name.into(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct IrArg {
    pub spread: bool,
    pub expr: IrExpr,
}

impl IrArg {
    #[must_use]
    pub const fn new(spread: bool, expr: IrExpr) -> Self {
        Self { spread, expr }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum IrSeqPart {
    Expr(IrExpr),
    Spread(IrExpr),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum IrLit {
    Int { raw: Box<str> },
    Float { raw: Box<str> },
    String { value: Box<str> },
    Rune { value: u32 },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum IrBinaryOp {
    IAdd,
    ISub,
    IMul,
    IDiv,
    IRem,
    FAdd,
    FSub,
    FMul,
    FDiv,
    FRem,
    StrCat,
    Eq,
    Ne,
    Lt,
    Gt,
    Le,
    Ge,
    LogicalXor,
    BitsAnd,
    BitsOr,
    BitsXor,
    Other(Box<str>),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct IrRangeKind {
    pub lower: IrRangeEndpoint,
    pub upper: IrRangeEndpoint,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum IrRangeEndpoint {
    Missing,
    Included,
    Excluded,
}

impl IrRangeKind {
    #[must_use]
    pub const fn bounded(include_lower: bool, include_upper: bool) -> Self {
        Self {
            lower: if include_lower {
                IrRangeEndpoint::Included
            } else {
                IrRangeEndpoint::Excluded
            },
            upper: if include_upper {
                IrRangeEndpoint::Included
            } else {
                IrRangeEndpoint::Excluded
            },
        }
    }

    #[must_use]
    pub const fn from(include_lower: bool) -> Self {
        Self {
            lower: if include_lower {
                IrRangeEndpoint::Included
            } else {
                IrRangeEndpoint::Excluded
            },
            upper: IrRangeEndpoint::Missing,
        }
    }

    #[must_use]
    pub const fn up_to(include_upper: bool) -> Self {
        Self {
            lower: IrRangeEndpoint::Missing,
            upper: if include_upper {
                IrRangeEndpoint::Included
            } else {
                IrRangeEndpoint::Excluded
            },
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum IrIntrinsicKind {
    FloatTotalCompare,
    FfiPtrNull,
    FfiPtrIsNull,
    FfiPtrOffset,
    FfiPtrSize,
    FfiPtrRead,
    FfiPtrWrite,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct IrExpr {
    pub origin: IrOrigin,
    pub kind: IrExprKind,
}

impl IrExpr {
    #[must_use]
    pub const fn new(origin: IrOrigin, kind: IrExprKind) -> Self {
        Self { origin, kind }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct IrRecordField {
    pub name: Box<str>,
    pub index: u16,
    pub expr: IrExpr,
}

impl IrRecordField {
    #[must_use]
    pub fn new<Name>(name: Name, index: u16, expr: IrExpr) -> Self
    where
        Name: Into<Box<str>>,
    {
        Self {
            name: name.into(),
            index,
            expr,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct IrRecordLayoutField {
    pub name: Box<str>,
    pub index: u16,
}

impl IrRecordLayoutField {
    #[must_use]
    pub fn new<Name>(name: Name, index: u16) -> Self
    where
        Name: Into<Box<str>>,
    {
        Self {
            name: name.into(),
            index,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct IrNameRef {
    pub binding: Option<NameBindingId>,
    pub name: Box<str>,
    pub import_record_target: Option<ModuleKey>,
}

impl IrNameRef {
    #[must_use]
    pub fn new<Name>(name: Name) -> Self
    where
        Name: Into<Box<str>>,
    {
        Self {
            binding: None,
            name: name.into(),
            import_record_target: None,
        }
    }

    #[must_use]
    pub const fn with_binding(mut self, binding: NameBindingId) -> Self {
        self.binding = Some(binding);
        self
    }

    #[must_use]
    pub const fn with_binding_opt(mut self, binding: Option<NameBindingId>) -> Self {
        self.binding = binding;
        self
    }

    #[must_use]
    pub fn with_import_record_target(mut self, import_record_target: ModuleKey) -> Self {
        self.import_record_target = Some(import_record_target);
        self
    }

    #[must_use]
    pub fn with_import_record_target_opt(
        mut self,
        import_record_target: Option<ModuleKey>,
    ) -> Self {
        self.import_record_target = import_record_target;
        self
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum IrAssignTarget {
    Binding {
        binding: Option<NameBindingId>,
        name: Box<str>,
        import_record_target: Option<ModuleKey>,
    },
    Index {
        base: Box<IrExpr>,
        indices: Box<[IrExpr]>,
    },
    RecordField {
        base: Box<IrExpr>,
        index: u16,
    },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum IrCasePattern {
    Wildcard,
    Bind {
        binding: NameBindingId,
        name: Box<str>,
    },
    Lit(IrLit),
    Tuple {
        items: Box<[Self]>,
    },
    Array {
        items: Box<[Self]>,
    },
    Record {
        fields: Box<[IrCaseRecordField]>,
    },
    Variant {
        data_key: DefinitionKey,
        variant_count: u16,
        tag_index: u16,
        tag_value: i64,
        args: Box<[Self]>,
    },
    As {
        pat: Box<Self>,
        binding: NameBindingId,
        name: Box<str>,
    },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct IrMatchArm {
    pub pattern: IrCasePattern,
    pub guard: Option<IrExpr>,
    pub expr: IrExpr,
}

impl IrMatchArm {
    #[must_use]
    pub const fn new(pattern: IrCasePattern, expr: IrExpr) -> Self {
        Self {
            pattern,
            guard: None,
            expr,
        }
    }

    #[must_use]
    pub fn with_guard(mut self, guard: IrExpr) -> Self {
        self.guard = Some(guard);
        self
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct IrCaseRecordField {
    pub index: u16,
    pub pat: Box<IrCasePattern>,
}

impl IrCaseRecordField {
    #[must_use]
    pub fn new(index: u16, pat: IrCasePattern) -> Self {
        Self {
            index,
            pat: Box::new(pat),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum IrExprKind {
    Unit,
    Name {
        binding: Option<NameBindingId>,
        name: Box<str>,
        import_record_target: Option<ModuleKey>,
    },
    Temp {
        temp: IrTempId,
    },
    Lit(IrLit),
    Sequence {
        exprs: Box<[IrExpr]>,
    },
    Tuple {
        ty_name: Box<str>,
        items: Box<[IrExpr]>,
    },
    Array {
        ty_name: Box<str>,
        items: Box<[IrExpr]>,
    },
    ArrayCat {
        ty_name: Box<str>,
        parts: Box<[IrSeqPart]>,
    },
    Record {
        ty_name: Box<str>,
        field_count: u16,
        fields: Box<[IrRecordField]>,
    },
    RecordGet {
        base: Box<IrExpr>,
        index: u16,
    },
    RecordUpdate {
        ty_name: Box<str>,
        field_count: u16,
        base: Box<IrExpr>,
        base_fields: Box<[IrRecordLayoutField]>,
        result_fields: Box<[IrRecordLayoutField]>,
        updates: Box<[IrRecordField]>,
    },
    Let {
        binding: Option<NameBindingId>,
        name: Box<str>,
        value: Box<IrExpr>,
    },
    TempLet {
        temp: IrTempId,
        value: Box<IrExpr>,
    },
    Assign {
        target: Box<IrAssignTarget>,
        value: Box<IrExpr>,
    },
    Index {
        base: Box<IrExpr>,
        indices: Box<[IrExpr]>,
    },
    ModuleLoad {
        spec: Box<IrExpr>,
    },
    ModuleGet {
        base: Box<IrExpr>,
        name: Box<str>,
    },
    TypeValue {
        ty_name: Box<str>,
    },
    TypeApply {
        callee: Box<IrExpr>,
        type_args: Box<[Box<str>]>,
    },
    SyntaxValue {
        raw: Box<str>,
    },
    ClosureNew {
        callee: IrNameRef,
        captures: Box<[IrExpr]>,
    },
    Binary {
        op: IrBinaryOp,
        left: Box<IrExpr>,
        right: Box<IrExpr>,
    },
    BoolAnd {
        left: Box<IrExpr>,
        right: Box<IrExpr>,
    },
    BoolOr {
        left: Box<IrExpr>,
        right: Box<IrExpr>,
    },
    Range {
        ty_name: Box<str>,
        kind: IrRangeKind,
        lower: Box<IrExpr>,
        upper: Box<IrExpr>,
        bounds_evidence: Option<Box<IrExpr>>,
    },
    RangeContains {
        value: Box<IrExpr>,
        range: Box<IrExpr>,
        evidence: Box<IrExpr>,
    },
    RangeMaterialize {
        range: Box<IrExpr>,
        evidence: Box<IrExpr>,
        result_ty_name: Box<str>,
    },
    Not {
        expr: Box<IrExpr>,
    },
    TyTest {
        base: Box<IrExpr>,
        ty_name: Box<str>,
    },
    TyCast {
        base: Box<IrExpr>,
        ty_name: Box<str>,
    },
    Match {
        scrutinee: Box<IrExpr>,
        arms: Box<[IrMatchArm]>,
    },
    VariantNew {
        data_key: DefinitionKey,
        tag_index: u16,
        tag_value: i64,
        field_count: u16,
        args: Box<[IrExpr]>,
    },
    Call {
        callee: Box<IrExpr>,
        args: Box<[IrArg]>,
    },
    IntrinsicCall {
        kind: IrIntrinsicKind,
        symbol: Box<str>,
        param_tys: Box<[Box<str>]>,
        result_ty: Box<str>,
        args: Box<[IrArg]>,
    },
    CallParts {
        callee: Box<IrExpr>,
        args: Box<[IrSeqPart]>,
    },
    Request {
        effect_key: DefinitionKey,
        op_index: u16,
        args: Box<[IrExpr]>,
    },
    RequestSeq {
        effect_key: DefinitionKey,
        op_index: u16,
        args: Box<[IrSeqPart]>,
    },
    AnswerLit {
        effect_key: DefinitionKey,
        value: Box<IrExpr>,
        ops: Box<[IrHandleOp]>,
    },
    Handle {
        effect_key: DefinitionKey,
        answer: Box<IrExpr>,
        body: Box<IrExpr>,
    },
    Resume {
        expr: Option<Box<IrExpr>>,
    },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct IrHandleOp {
    pub op_index: u16,
    pub name: Box<str>,
    pub closure: IrExpr,
}

impl IrHandleOp {
    #[must_use]
    pub fn new<Name>(op_index: u16, name: Name, closure: IrExpr) -> Self
    where
        Name: Into<Box<str>>,
    {
        Self {
            op_index,
            name: name.into(),
            closure,
        }
    }
}
