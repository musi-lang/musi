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
    pub binding: NameBindingId,
    pub name: Box<str>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct IrArg {
    pub spread: bool,
    pub expr: IrExpr,
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

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct IrExpr {
    pub origin: IrOrigin,
    pub kind: IrExprKind,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct IrRecordField {
    pub name: Box<str>,
    pub index: u16,
    pub expr: IrExpr,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct IrRecordLayoutField {
    pub name: Box<str>,
    pub index: u16,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct IrNameRef {
    pub binding: Option<NameBindingId>,
    pub name: Box<str>,
    pub module_target: Option<ModuleKey>,
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

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct IrCaseRecordField {
    pub index: u16,
    pub pat: Box<IrCasePattern>,
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
    Handle {
        effect_key: DefinitionKey,
        value: Box<IrExpr>,
        ops: Box<[IrHandleOp]>,
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

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct IrDataDef {
    pub key: DefinitionKey,
    pub variant_count: u32,
    pub field_count: u32,
    pub repr_kind: Option<Box<str>>,
    pub layout_align: Option<u32>,
    pub layout_pack: Option<u32>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct IrForeignDef {
    pub binding: Option<NameBindingId>,
    pub name: Box<str>,
    pub abi: Box<str>,
    pub symbol: Box<str>,
    pub link: Option<Box<str>>,
    pub param_tys: Box<[Box<str>]>,
    pub result_ty: Box<str>,
    pub exported: bool,
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

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct IrEffectDef {
    pub key: DefinitionKey,
    pub ops: Box<[IrEffectOpDef]>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct IrEffectOpDef {
    pub name: Box<str>,
    pub param_tys: Box<[Box<str>]>,
    pub result_ty: Box<str>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct IrClassDef {
    pub key: DefinitionKey,
    pub member_names: Box<[Box<str>]>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct IrInstanceDef {
    pub class_key: DefinitionKey,
    pub member_names: Box<[Box<str>]>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct IrMetaRecord {
    pub target: Box<str>,
    pub key: Box<str>,
    pub values: Box<[Box<str>]>,
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
        Self {
            key: value.key.clone(),
            member_names: value
                .members
                .iter()
                .map(|member| member.name.clone())
                .collect::<Vec<_>>()
                .into_boxed_slice(),
        }
    }
}

impl From<&InstanceSurface> for IrInstanceDef {
    fn from(value: &InstanceSurface) -> Self {
        Self {
            class_key: value.class_key.clone(),
            member_names: value.member_names.clone(),
        }
    }
}
