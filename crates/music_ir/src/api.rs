use music_base::{SourceId, Span, diag::Diag};
use music_module::ModuleKey;
use music_names::NameBindingId;
use music_sema::{
    ClassSurface, DefinitionKey, EffectRow, ExportedValue, InstanceSurface, SurfaceDim, SurfaceTy,
    SurfaceTyId, SurfaceTyKind,
};
use music_term::{TypeDim, TypeField, TypeModuleRef, TypeTerm, TypeTermKind};

use crate::IrDiagKind;

pub type IrDiagList = Vec<Diag>;
pub type IrNameList = Box<[Box<str>]>;

#[must_use]
pub fn ir_diag_kind(diag: &Diag) -> Option<IrDiagKind> {
    IrDiagKind::from_diag(diag)
}

#[must_use]
pub fn lower_surface_type_term(types: &[SurfaceTy], ty: &SurfaceTy) -> TypeTerm {
    match &ty.kind {
        SurfaceTyKind::Error => TypeTerm::new(TypeTermKind::Error),
        SurfaceTyKind::Unknown => TypeTerm::new(TypeTermKind::Unknown),
        SurfaceTyKind::Type => TypeTerm::new(TypeTermKind::Type),
        SurfaceTyKind::Syntax => TypeTerm::new(TypeTermKind::Syntax),
        SurfaceTyKind::Any => TypeTerm::new(TypeTermKind::Any),
        SurfaceTyKind::Empty => TypeTerm::new(TypeTermKind::Empty),
        SurfaceTyKind::Unit => TypeTerm::new(TypeTermKind::Unit),
        SurfaceTyKind::Bool => TypeTerm::new(TypeTermKind::Bool),
        SurfaceTyKind::Nat => TypeTerm::new(TypeTermKind::Nat),
        SurfaceTyKind::Int => TypeTerm::new(TypeTermKind::Int),
        SurfaceTyKind::Float => TypeTerm::new(TypeTermKind::Float),
        SurfaceTyKind::String => TypeTerm::new(TypeTermKind::String),
        SurfaceTyKind::CString => TypeTerm::new(TypeTermKind::CString),
        SurfaceTyKind::CPtr => TypeTerm::new(TypeTermKind::CPtr),
        SurfaceTyKind::Module => TypeTerm::new(TypeTermKind::Module),
        SurfaceTyKind::NatLit(value) => TypeTerm::new(TypeTermKind::NatLit(*value)),
        SurfaceTyKind::Named { name, args } => lower_named_term(
            name,
            args.iter()
                .map(|arg| lower_surface_type_term_id(types, *arg))
                .collect::<Vec<_>>()
                .into_boxed_slice(),
        ),
        SurfaceTyKind::Pi {
            binder,
            binder_ty,
            body,
            is_effectful,
        } => TypeTerm::new(TypeTermKind::Pi {
            binder: binder.clone(),
            binder_ty: Box::new(lower_surface_type_term_id(types, *binder_ty)),
            body: Box::new(lower_surface_type_term_id(types, *body)),
            is_effectful: *is_effectful,
        }),
        SurfaceTyKind::Arrow {
            params,
            ret,
            is_effectful,
        } => TypeTerm::new(TypeTermKind::Arrow {
            params: params
                .iter()
                .map(|param| lower_surface_type_term_id(types, *param))
                .collect::<Vec<_>>()
                .into_boxed_slice(),
            ret: Box::new(lower_surface_type_term_id(types, *ret)),
            is_effectful: *is_effectful,
        }),
        SurfaceTyKind::Sum { left, right } => TypeTerm::new(TypeTermKind::Sum {
            left: Box::new(lower_surface_type_term_id(types, *left)),
            right: Box::new(lower_surface_type_term_id(types, *right)),
        }),
        SurfaceTyKind::Tuple { items } => TypeTerm::new(TypeTermKind::Tuple {
            items: items
                .iter()
                .map(|item| lower_surface_type_term_id(types, *item))
                .collect::<Vec<_>>()
                .into_boxed_slice(),
        }),
        SurfaceTyKind::Seq { item } => TypeTerm::new(TypeTermKind::Seq {
            item: Box::new(lower_surface_type_term_id(types, *item)),
        }),
        SurfaceTyKind::Array { dims, item } => TypeTerm::new(TypeTermKind::Array {
            dims: dims
                .iter()
                .map(|dim| match dim {
                    SurfaceDim::Unknown => TypeDim::Unknown,
                    SurfaceDim::Name(name) => TypeDim::Name(name.clone()),
                    SurfaceDim::Int(value) => TypeDim::Int(*value),
                })
                .collect::<Vec<_>>()
                .into_boxed_slice(),
            item: Box::new(lower_surface_type_term_id(types, *item)),
        }),
        SurfaceTyKind::Range { item } => TypeTerm::new(TypeTermKind::Range {
            item: Box::new(lower_surface_type_term_id(types, *item)),
        }),
        SurfaceTyKind::Handler {
            effect,
            input,
            output,
        } => TypeTerm::new(TypeTermKind::Handler {
            effect: Box::new(lower_surface_type_term_id(types, *effect)),
            input: Box::new(lower_surface_type_term_id(types, *input)),
            output: Box::new(lower_surface_type_term_id(types, *output)),
        }),
        SurfaceTyKind::Mut { inner } => TypeTerm::new(TypeTermKind::Mut {
            inner: Box::new(lower_surface_type_term_id(types, *inner)),
        }),
        SurfaceTyKind::Record { fields } => TypeTerm::new(TypeTermKind::Record {
            fields: fields
                .iter()
                .map(|field| TypeField {
                    name: field.name.clone(),
                    ty: lower_surface_type_term_id(types, field.ty),
                })
                .collect::<Vec<_>>()
                .into_boxed_slice(),
        }),
    }
}

fn lower_surface_type_term_id(types: &[SurfaceTy], ty: SurfaceTyId) -> TypeTerm {
    let index = usize::try_from(ty.raw()).unwrap_or(usize::MAX);
    types.get(index).map_or_else(
        || TypeTerm::new(TypeTermKind::Error),
        |item| lower_surface_type_term(types, item),
    )
}

fn lower_named_term(name: &str, args: Box<[TypeTerm]>) -> TypeTerm {
    let (module, local_name) = name
        .rsplit_once("::")
        .map_or((None, name), |(module, tail)| {
            (
                Some(TypeModuleRef {
                    spec: module.into(),
                }),
                tail,
            )
        });
    TypeTerm::new(TypeTermKind::Named {
        module,
        name: local_name.into(),
        args,
    })
}

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
    Other(Box<str>),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum IrRangeEndBound {
    Inclusive,
    Exclusive,
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
    pub module_target: Option<ModuleKey>,
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
            module_target: None,
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
    pub fn with_module_target(mut self, module_target: ModuleKey) -> Self {
        self.module_target = Some(module_target);
        self
    }

    #[must_use]
    pub fn with_module_target_opt(mut self, module_target: Option<ModuleKey>) -> Self {
        self.module_target = module_target;
        self
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum IrAssignTarget {
    Binding {
        binding: Option<NameBindingId>,
        name: Box<str>,
        module_target: Option<ModuleKey>,
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
        args: Box<[Self]>,
    },
    As {
        pat: Box<Self>,
        binding: NameBindingId,
        name: Box<str>,
    },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct IrCaseArm {
    pub pattern: IrCasePattern,
    pub guard: Option<IrExpr>,
    pub expr: IrExpr,
}

impl IrCaseArm {
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
        module_target: Option<ModuleKey>,
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
    DynamicImport {
        spec: Box<IrExpr>,
    },
    ModuleGet {
        base: Box<IrExpr>,
        name: Box<str>,
    },
    TypeValue {
        ty_name: Box<str>,
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
    Range {
        ty_name: Box<str>,
        start: Box<IrExpr>,
        end: Box<IrExpr>,
        end_bound: IrRangeEndBound,
    },
    RangeContains {
        value: Box<IrExpr>,
        range: Box<IrExpr>,
        evidence: Box<IrExpr>,
    },
    RangeMaterialize {
        range: Box<IrExpr>,
        evidence: Box<IrExpr>,
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
    Case {
        scrutinee: Box<IrExpr>,
        arms: Box<[IrCaseArm]>,
    },
    VariantNew {
        data_key: DefinitionKey,
        tag_index: u16,
        field_count: u16,
        args: Box<[IrExpr]>,
    },
    Call {
        callee: Box<IrExpr>,
        args: Box<[IrArg]>,
    },
    CallSeq {
        callee: Box<IrExpr>,
        args: Box<[IrSeqPart]>,
    },
    Perform {
        effect_key: DefinitionKey,
        op_index: u16,
        args: Box<[IrExpr]>,
    },
    PerformSeq {
        effect_key: DefinitionKey,
        op_index: u16,
        args: Box<[IrSeqPart]>,
    },
    HandlerLit {
        effect_key: DefinitionKey,
        value: Box<IrExpr>,
        ops: Box<[IrHandleOp]>,
    },
    Handle {
        effect_key: DefinitionKey,
        handler: Box<IrExpr>,
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

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct IrCallable {
    pub binding: Option<NameBindingId>,
    pub name: Box<str>,
    pub params: Box<[IrParam]>,
    pub body: IrExpr,
    pub exported: bool,
    pub effects: EffectRow,
    pub module_target: Option<ModuleKey>,
}

impl IrCallable {
    #[must_use]
    pub fn new<Name>(name: Name, params: Box<[IrParam]>, body: IrExpr) -> Self
    where
        Name: Into<Box<str>>,
    {
        Self {
            binding: None,
            name: name.into(),
            params,
            body,
            exported: false,
            effects: EffectRow::default(),
            module_target: None,
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
    pub const fn with_exported(mut self, exported: bool) -> Self {
        self.exported = exported;
        self
    }

    #[must_use]
    pub fn with_effects(mut self, effects: EffectRow) -> Self {
        self.effects = effects;
        self
    }

    #[must_use]
    pub fn with_module_target(mut self, module_target: ModuleKey) -> Self {
        self.module_target = Some(module_target);
        self
    }

    #[must_use]
    pub fn with_module_target_opt(mut self, module_target: Option<ModuleKey>) -> Self {
        self.module_target = module_target;
        self
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct IrDataVariantDef {
    pub name: Box<str>,
    pub field_tys: IrNameList,
}

impl IrDataVariantDef {
    #[must_use]
    pub fn new<Name>(name: Name, field_tys: IrNameList) -> Self
    where
        Name: Into<Box<str>>,
    {
        Self {
            name: name.into(),
            field_tys,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct IrDataDef {
    pub key: DefinitionKey,
    pub variant_count: u32,
    pub field_count: u32,
    pub variants: Box<[IrDataVariantDef]>,
    pub repr_kind: Option<Box<str>>,
    pub layout_align: Option<u32>,
    pub layout_pack: Option<u32>,
}

impl IrDataDef {
    /// # Panics
    ///
    /// Panics if the variant count or max field count does not fit in `u32`.
    #[must_use]
    pub fn new(key: DefinitionKey, variants: Box<[IrDataVariantDef]>) -> Self {
        let variant_count =
            u32::try_from(variants.len()).expect("IR data variant count should fit in u32");
        let field_count = variants
            .iter()
            .map(|variant| u32::try_from(variant.field_tys.len()).unwrap_or(u32::MAX))
            .max()
            .unwrap_or(0);
        Self {
            key,
            variant_count,
            field_count,
            variants,
            repr_kind: None,
            layout_align: None,
            layout_pack: None,
        }
    }

    #[must_use]
    pub fn with_repr_kind<ReprKind>(mut self, repr_kind: ReprKind) -> Self
    where
        ReprKind: Into<Box<str>>,
    {
        self.repr_kind = Some(repr_kind.into());
        self
    }

    #[must_use]
    pub const fn with_layout_align(mut self, layout_align: u32) -> Self {
        self.layout_align = Some(layout_align);
        self
    }

    #[must_use]
    pub const fn with_layout_pack(mut self, layout_pack: u32) -> Self {
        self.layout_pack = Some(layout_pack);
        self
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct IrForeignDef {
    pub binding: Option<NameBindingId>,
    pub name: Box<str>,
    pub abi: Box<str>,
    pub symbol: Box<str>,
    pub link: Option<Box<str>>,
    pub param_tys: IrNameList,
    pub result_ty: Box<str>,
    pub exported: bool,
}

impl IrForeignDef {
    #[must_use]
    pub fn new<Name, Abi, SymbolName, ResultTy>(
        name: Name,
        abi: Abi,
        symbol: SymbolName,
        param_tys: IrNameList,
        result_ty: ResultTy,
    ) -> Self
    where
        Name: Into<Box<str>>,
        Abi: Into<Box<str>>,
        SymbolName: Into<Box<str>>,
        ResultTy: Into<Box<str>>,
    {
        Self {
            binding: None,
            name: name.into(),
            abi: abi.into(),
            symbol: symbol.into(),
            link: None,
            param_tys,
            result_ty: result_ty.into(),
            exported: false,
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
    pub fn with_link<Link>(mut self, link: Link) -> Self
    where
        Link: Into<Box<str>>,
    {
        self.link = Some(link.into());
        self
    }

    #[must_use]
    pub fn with_link_opt(mut self, link: Option<Box<str>>) -> Self {
        self.link = link;
        self
    }

    #[must_use]
    pub const fn with_exported(mut self, exported: bool) -> Self {
        self.exported = exported;
        self
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct IrGlobal {
    pub binding: Option<NameBindingId>,
    pub name: Box<str>,
    pub body: IrExpr,
    pub exported: bool,
    pub effects: EffectRow,
    pub module_target: Option<ModuleKey>,
}

impl IrGlobal {
    #[must_use]
    pub fn new<Name>(name: Name, body: IrExpr) -> Self
    where
        Name: Into<Box<str>>,
    {
        Self {
            binding: None,
            name: name.into(),
            body,
            exported: false,
            effects: EffectRow::default(),
            module_target: None,
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
    pub const fn with_exported(mut self, exported: bool) -> Self {
        self.exported = exported;
        self
    }

    #[must_use]
    pub fn with_effects(mut self, effects: EffectRow) -> Self {
        self.effects = effects;
        self
    }

    #[must_use]
    pub fn with_module_target(mut self, module_target: ModuleKey) -> Self {
        self.module_target = Some(module_target);
        self
    }

    #[must_use]
    pub fn with_module_target_opt(mut self, module_target: Option<ModuleKey>) -> Self {
        self.module_target = module_target;
        self
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct IrEffectDef {
    pub key: DefinitionKey,
    pub ops: Box<[IrEffectOpDef]>,
}

impl IrEffectDef {
    #[must_use]
    pub const fn new(key: DefinitionKey, ops: Box<[IrEffectOpDef]>) -> Self {
        Self { key, ops }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct IrEffectOpDef {
    pub name: Box<str>,
    pub param_tys: IrNameList,
    pub result_ty: Box<str>,
}

impl IrEffectOpDef {
    #[must_use]
    pub fn new<Name, ResultTy>(name: Name, param_tys: IrNameList, result_ty: ResultTy) -> Self
    where
        Name: Into<Box<str>>,
        ResultTy: Into<Box<str>>,
    {
        Self {
            name: name.into(),
            param_tys,
            result_ty: result_ty.into(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct IrClassDef {
    pub key: DefinitionKey,
    pub member_names: IrNameList,
}

impl IrClassDef {
    #[must_use]
    pub const fn new(key: DefinitionKey, member_names: IrNameList) -> Self {
        Self { key, member_names }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct IrInstanceDef {
    pub class_key: DefinitionKey,
    pub member_names: IrNameList,
}

impl IrInstanceDef {
    #[must_use]
    pub const fn new(class_key: DefinitionKey, member_names: IrNameList) -> Self {
        Self {
            class_key,
            member_names,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct IrMetaRecord {
    pub target: Box<str>,
    pub key: Box<str>,
    pub values: IrNameList,
}

impl IrMetaRecord {
    #[must_use]
    pub fn new<Target, Key>(target: Target, key: Key, values: IrNameList) -> Self
    where
        Target: Into<Box<str>>,
        Key: Into<Box<str>>,
    {
        Self {
            target: target.into(),
            key: key.into(),
            values,
        }
    }
}

#[derive(Debug, Clone)]
pub struct IrModule {
    module_key: ModuleKey,
    static_imports: Box<[ModuleKey]>,
    types: Box<[SurfaceTy]>,
    exports: Box<[ExportedValue]>,
    callables: Box<[IrCallable]>,
    globals: Box<[IrGlobal]>,
    data_defs: Box<[IrDataDef]>,
    foreigns: Box<[IrForeignDef]>,
    effects: Box<[IrEffectDef]>,
    classes: Box<[IrClassDef]>,
    instances: Box<[IrInstanceDef]>,
    meta: Box<[IrMetaRecord]>,
}

type IrModuleCollections = (
    Box<[ExportedValue]>,
    Box<[IrCallable]>,
    Box<[IrGlobal]>,
    Box<[IrDataDef]>,
    Box<[IrForeignDef]>,
    Box<[IrEffectDef]>,
    Box<[IrClassDef]>,
    Box<[IrInstanceDef]>,
    Box<[IrMetaRecord]>,
);

impl IrModule {
    #[must_use]
    pub(crate) fn new(
        module_key: ModuleKey,
        static_imports: Box<[ModuleKey]>,
        types: Box<[SurfaceTy]>,
        collections: IrModuleCollections,
    ) -> Self {
        Self {
            module_key,
            static_imports,
            types,
            exports: collections.0,
            callables: collections.1,
            globals: collections.2,
            data_defs: collections.3,
            foreigns: collections.4,
            effects: collections.5,
            classes: collections.6,
            instances: collections.7,
            meta: collections.8,
        }
    }

    #[must_use]
    pub const fn module_key(&self) -> &ModuleKey {
        &self.module_key
    }

    #[must_use]
    pub fn static_imports(&self) -> &[ModuleKey] {
        &self.static_imports
    }

    #[must_use]
    pub fn types(&self) -> &[SurfaceTy] {
        &self.types
    }

    #[must_use]
    pub fn exports(&self) -> &[ExportedValue] {
        &self.exports
    }

    #[must_use]
    pub fn callables(&self) -> &[IrCallable] {
        &self.callables
    }

    #[must_use]
    pub fn globals(&self) -> &[IrGlobal] {
        &self.globals
    }

    #[must_use]
    pub fn data_defs(&self) -> &[IrDataDef] {
        &self.data_defs
    }

    #[must_use]
    pub fn foreigns(&self) -> &[IrForeignDef] {
        &self.foreigns
    }

    #[must_use]
    pub fn effects(&self) -> &[IrEffectDef] {
        &self.effects
    }

    #[must_use]
    pub fn classes(&self) -> &[IrClassDef] {
        &self.classes
    }

    #[must_use]
    pub fn instances(&self) -> &[IrInstanceDef] {
        &self.instances
    }

    #[must_use]
    pub fn meta(&self) -> &[IrMetaRecord] {
        &self.meta
    }

    #[must_use]
    pub fn exported_value(&self, name: &str) -> Option<&ExportedValue> {
        self.exports
            .iter()
            .find(|value| value.name.as_ref() == name)
    }

    #[must_use]
    pub fn effect(&self, key: &DefinitionKey) -> Option<&IrEffectDef> {
        self.effects.iter().find(|effect| &effect.key == key)
    }

    #[must_use]
    pub fn class(&self, key: &DefinitionKey) -> Option<&IrClassDef> {
        self.classes.iter().find(|class| &class.key == key)
    }
}

impl From<&ClassSurface> for IrClassDef {
    fn from(value: &ClassSurface) -> Self {
        Self::new(
            value.key.clone(),
            value
                .members
                .iter()
                .map(|member| member.name.clone())
                .collect::<Vec<_>>()
                .into_boxed_slice(),
        )
    }
}

impl From<&InstanceSurface> for IrInstanceDef {
    fn from(value: &InstanceSurface) -> Self {
        Self::new(value.class_key.clone(), value.member_names.clone())
    }
}
