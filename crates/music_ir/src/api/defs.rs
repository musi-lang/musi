use music_module::ModuleKey;
use music_names::NameBindingId;
use music_sema::{DefinitionKey, EffectRow, GivenSurface, ShapeSurface};

use super::{IrExpr, IrParam};

pub type IrNameList = Box<[Box<str>]>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct IrCallable {
    pub binding: Option<NameBindingId>,
    pub name: Box<str>,
    pub params: Box<[IrParam]>,
    pub body: IrExpr,
    pub exported: bool,
    pub hot: bool,
    pub cold: bool,
    pub effects: EffectRow,
    pub import_record_target: Option<ModuleKey>,
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
            hot: false,
            cold: false,
            effects: EffectRow::default(),
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
    pub const fn with_exported(mut self, exported: bool) -> Self {
        self.exported = exported;
        self
    }

    #[must_use]
    pub const fn with_hot(mut self, hot: bool) -> Self {
        self.hot = hot;
        self
    }

    #[must_use]
    pub const fn with_cold(mut self, cold: bool) -> Self {
        self.cold = cold;
        self
    }

    #[must_use]
    pub fn with_effects(mut self, effects: EffectRow) -> Self {
        self.effects = effects;
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
pub struct IrDataVariantDef {
    pub name: Box<str>,
    pub tag: i64,
    pub field_tys: IrNameList,
}

impl IrDataVariantDef {
    #[must_use]
    pub fn new<Name>(name: Name, tag: i64, field_tys: IrNameList) -> Self
    where
        Name: Into<Box<str>>,
    {
        Self {
            name: name.into(),
            tag,
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
    pub frozen: bool,
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
            frozen: false,
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

    #[must_use]
    pub const fn with_frozen(mut self, frozen: bool) -> Self {
        self.frozen = frozen;
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
    pub hot: bool,
    pub cold: bool,
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
            hot: false,
            cold: false,
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

    #[must_use]
    pub const fn with_hot(mut self, hot: bool) -> Self {
        self.hot = hot;
        self
    }

    #[must_use]
    pub const fn with_cold(mut self, cold: bool) -> Self {
        self.cold = cold;
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
    pub import_record_target: Option<ModuleKey>,
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
    pub is_comptime_safe: bool,
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
            is_comptime_safe: false,
        }
    }

    #[must_use]
    pub const fn with_comptime_safe(mut self, is_comptime_safe: bool) -> Self {
        self.is_comptime_safe = is_comptime_safe;
        self
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum IrModuleInitPart {
    Global { name: Box<str> },
    Expr(IrExpr),
}

impl IrModuleInitPart {
    #[must_use]
    pub fn global(name: impl Into<Box<str>>) -> Self {
        Self::Global { name: name.into() }
    }

    #[must_use]
    pub const fn expr(expr: IrExpr) -> Self {
        Self::Expr(expr)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct IrShapeDef {
    pub key: DefinitionKey,
    pub member_names: IrNameList,
}

impl IrShapeDef {
    #[must_use]
    pub const fn new(key: DefinitionKey, member_names: IrNameList) -> Self {
        Self { key, member_names }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct IrGivenDef {
    pub shape_key: DefinitionKey,
    pub member_names: IrNameList,
}

impl IrGivenDef {
    #[must_use]
    pub const fn new(shape_key: DefinitionKey, member_names: IrNameList) -> Self {
        Self {
            shape_key,
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

impl From<&ShapeSurface> for IrShapeDef {
    fn from(value: &ShapeSurface) -> Self {
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

impl From<&GivenSurface> for IrGivenDef {
    fn from(value: &GivenSurface) -> Self {
        Self::new(value.shape_key.clone(), value.member_names.clone())
    }
}
