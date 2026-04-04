use music_base::diag::Diag;
use music_hir::{HirExprId, HirModule, HirTyId};
use music_module::ModuleKey;
use music_names::Symbol;
use music_sema::{
    ClassSurface, DefinitionKey, EffectRow, EffectSurface, ExportedValue, InstanceSurface,
    SurfaceTy,
};

pub type IrDiagList = Vec<Diag>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct IrCallable {
    pub symbol: Symbol,
    pub name: Box<str>,
    pub expr: HirExprId,
    pub ty: HirTyId,
    pub effects: EffectRow,
    pub module_target: Option<ModuleKey>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct IrDataDef {
    pub symbol: Symbol,
    pub name: Box<str>,
    pub expr: HirExprId,
    pub variant_count: u32,
    pub field_count: u32,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct IrForeignDef {
    pub symbol: Symbol,
    pub name: Box<str>,
    pub abi: Box<str>,
    pub sig: Option<HirExprId>,
    pub param_count: u32,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct IrGlobal {
    pub symbol: Symbol,
    pub name: Box<str>,
    pub expr: HirExprId,
    pub ty: HirTyId,
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
    pub hir: HirModule,
    pub root: HirExprId,
    pub root_ty: HirTyId,
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
