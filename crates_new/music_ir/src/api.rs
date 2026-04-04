use music_base::{SourceId, Span, diag::Diag};
use music_module::ModuleKey;
use music_names::NameBindingId;
use music_sema::{
    ClassSurface, DefinitionKey, EffectRow, EffectSurface, ExportedValue, InstanceSurface,
    SurfaceTy,
};

pub type IrDiagList = Vec<Diag>;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct IrOrigin {
    pub source_id: SourceId,
    pub span: Span,
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
pub enum IrLit {
    Int { raw: Box<str> },
    Float { raw: Box<str> },
    String { value: Box<str> },
    Rune { value: u32 },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum IrBinaryOp {
    Add,
    Sub,
    Mul,
    Div,
    Rem,
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
        index: Box<IrExpr>,
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
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct IrCaseArm {
    pub pattern: IrCasePattern,
    pub guard: Option<IrExpr>,
    pub expr: IrExpr,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum IrExprKind {
    Unit,
    Name {
        binding: Option<NameBindingId>,
        name: Box<str>,
        module_target: Option<ModuleKey>,
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
    Let {
        binding: Option<NameBindingId>,
        name: Box<str>,
        is_mut: bool,
        value: Box<IrExpr>,
    },
    Assign {
        target: Box<IrAssignTarget>,
        value: Box<IrExpr>,
    },
    Index {
        base: Box<IrExpr>,
        index: Box<IrExpr>,
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
    Case {
        scrutinee: Box<IrExpr>,
        arms: Box<[IrCaseArm]>,
    },
    Call {
        callee: Box<IrExpr>,
        args: Box<[IrArg]>,
    },
    Unsupported {
        description: Box<str>,
    },
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
    pub name: Box<str>,
    pub variant_count: u32,
    pub field_count: u32,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct IrForeignDef {
    pub binding: Option<NameBindingId>,
    pub name: Box<str>,
    pub abi: Box<str>,
    pub param_count: u32,
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
    pub ops: Box<[Box<str>]>,
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

#[derive(Debug, Clone)]
pub struct IrModule {
    pub module_key: ModuleKey,
    pub static_imports: Box<[ModuleKey]>,
    pub types: Box<[SurfaceTy]>,
    pub exports: Box<[ExportedValue]>,
    pub callables: Box<[IrCallable]>,
    pub globals: Box<[IrGlobal]>,
    pub data_defs: Box<[IrDataDef]>,
    pub foreigns: Box<[IrForeignDef]>,
    pub effects: Box<[IrEffectDef]>,
    pub classes: Box<[IrClassDef]>,
    pub instances: Box<[IrInstanceDef]>,
}

impl IrModule {
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

impl From<&EffectSurface> for IrEffectDef {
    fn from(value: &EffectSurface) -> Self {
        Self {
            key: value.key.clone(),
            ops: value
                .ops
                .iter()
                .map(|op| op.name.clone())
                .collect::<Vec<_>>()
                .into_boxed_slice(),
        }
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
            member_names: Box::new([]),
        }
    }
}
