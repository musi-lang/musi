use std::collections::HashMap;

use music_arena::SliceRange;
use music_base::Span;
use music_hir::{HirDim, HirStore, HirTyField, HirTyId, HirTyKind};
use music_module::ModuleKey;
use music_names::{Ident, Interner, Symbol};

use super::super::PassBase;
use crate::api::{
    DefinitionKey, ModuleSurface, SurfaceDim, SurfaceEffectItem, SurfaceEffectRow, SurfaceTy,
    SurfaceTyField, SurfaceTyId, SurfaceTyKind,
};
use crate::effects::EffectRow;

pub(super) fn lower_surface_effect_row(
    tys: &mut SurfaceTyBuilder<'_>,
    row: &EffectRow,
) -> SurfaceEffectRow {
    let items = row
        .items
        .iter()
        .map(|item| SurfaceEffectItem::new(item.name.clone(), item.arg.map(|ty| tys.lower(ty))))
        .collect::<Vec<_>>()
        .into_boxed_slice();
    if let Some(open) = row.open.clone() {
        SurfaceEffectRow::new(items).with_open(open)
    } else {
        SurfaceEffectRow::new(items)
    }
}

pub fn import_surface_ty(
    ctx: &mut PassBase<'_, '_, '_>,
    surface: &ModuleSurface,
    ty: SurfaceTyId,
) -> HirTyId {
    let mut importer = SurfaceTyImporter::new(ctx, surface);
    importer.import(ty)
}

pub fn surface_key(module_key: &ModuleKey, interner: &Interner, name: Symbol) -> DefinitionKey {
    DefinitionKey::new(module_key.clone(), interner.resolve(name))
}

pub fn canonical_surface_ty(surface: &ModuleSurface, ty: SurfaceTyId) -> String {
    let kind = &surface
        .try_ty(ty)
        .expect("surface type missing while formatting")
        .kind;
    canonical_surface_ty_kind(surface, kind)
}

fn canonical_surface_ty_kind(surface: &ModuleSurface, kind: &SurfaceTyKind) -> String {
    if let Some(rendered) = canonical_simple_surface_ty(kind) {
        return rendered;
    }
    match kind {
        SurfaceTyKind::Named { name, args } => canonical_surface_named(surface, name, args),
        SurfaceTyKind::Pi {
            binder,
            binder_ty,
            body,
            is_effectful,
        } => canonical_surface_pi(surface, binder, *binder_ty, *body, *is_effectful),
        SurfaceTyKind::Arrow {
            params,
            ret,
            is_effectful,
        } => canonical_surface_arrow(surface, params, *ret, *is_effectful),
        SurfaceTyKind::Sum { left, right } => {
            format!(
                "{} + {}",
                canonical_surface_ty(surface, *left),
                canonical_surface_ty(surface, *right)
            )
        }
        SurfaceTyKind::Tuple { items } => canonical_surface_tuple(surface, items),
        SurfaceTyKind::Seq { item } => format!("[]{}", canonical_surface_ty(surface, *item)),
        SurfaceTyKind::Array { dims, item } => canonical_surface_array(surface, dims, *item),
        SurfaceTyKind::Bits { width } => format!("Bits[{width}]"),
        SurfaceTyKind::Range { bound } => canonical_surface_wrapped(surface, "Range", *bound),
        SurfaceTyKind::Handler {
            effect,
            input,
            output,
        } => canonical_surface_handler(surface, *effect, *input, *output),
        SurfaceTyKind::Mut { inner } => canonical_surface_prefixed(surface, "mut", *inner),
        SurfaceTyKind::AnyShape { capability } => {
            canonical_surface_prefixed(surface, "any", *capability)
        }
        SurfaceTyKind::SomeShape { capability } => {
            canonical_surface_prefixed(surface, "some", *capability)
        }
        SurfaceTyKind::Record { fields } => canonical_surface_record(surface, fields),
        _ => canonical_simple_surface_ty(kind).expect("simple surface type should render"),
    }
}

fn canonical_surface_wrapped(surface: &ModuleSurface, name: &str, inner: SurfaceTyId) -> String {
    format!("{name}[{}]", canonical_surface_ty(surface, inner))
}

fn canonical_surface_handler(
    surface: &ModuleSurface,
    effect: SurfaceTyId,
    input: SurfaceTyId,
    output: SurfaceTyId,
) -> String {
    format!(
        "answer {} ({} -> {})",
        canonical_surface_ty(surface, effect),
        canonical_surface_ty(surface, input),
        canonical_surface_ty(surface, output)
    )
}

fn canonical_surface_prefixed(surface: &ModuleSurface, prefix: &str, inner: SurfaceTyId) -> String {
    format!("{prefix} {}", canonical_surface_ty(surface, inner))
}

fn canonical_simple_surface_ty(kind: &SurfaceTyKind) -> Option<String> {
    let simple = SimpleTyKind::from_surface(kind)?;
    if let SimpleTyKind::NatLit(value) = simple {
        return Some(value.to_string());
    }
    simple_ty_info(simple).map(|ty| ty.display_name.to_owned())
}

fn canonical_surface_named(surface: &ModuleSurface, name: &str, args: &[SurfaceTyId]) -> String {
    if args.is_empty() {
        return name.to_owned();
    }
    format!(
        "{}[{}]",
        name,
        args.iter()
            .copied()
            .map(|arg| canonical_surface_ty(surface, arg))
            .collect::<Vec<_>>()
            .join(", ")
    )
}

fn canonical_surface_arrow(
    surface: &ModuleSurface,
    params: &[SurfaceTyId],
    ret: SurfaceTyId,
    is_effectful: bool,
) -> String {
    let params = params
        .iter()
        .copied()
        .map(|param| canonical_surface_ty(surface, param))
        .collect::<Vec<_>>();
    let left = if params.len() == 1 {
        params[0].clone()
    } else {
        format!("({})", params.join(", "))
    };
    let arrow = if is_effectful { " ~> " } else { " -> " };
    format!("{left}{arrow}{}", canonical_surface_ty(surface, ret))
}

fn canonical_surface_pi(
    surface: &ModuleSurface,
    binder: &str,
    binder_ty: SurfaceTyId,
    body: SurfaceTyId,
    is_effectful: bool,
) -> String {
    let arrow = if is_effectful { " ~> " } else { " -> " };
    format!(
        "forall ({binder} : {}){arrow}{}",
        canonical_surface_ty(surface, binder_ty),
        canonical_surface_ty(surface, body)
    )
}

fn canonical_surface_tuple(surface: &ModuleSurface, items: &[SurfaceTyId]) -> String {
    format!(
        "({})",
        items
            .iter()
            .copied()
            .map(|item| canonical_surface_ty(surface, item))
            .collect::<Vec<_>>()
            .join(", ")
    )
}

fn canonical_surface_array(
    surface: &ModuleSurface,
    dims: &[SurfaceDim],
    item: SurfaceTyId,
) -> String {
    let dims = dims
        .iter()
        .map(|dim| match dim {
            SurfaceDim::Unknown => "_".into(),
            SurfaceDim::Name(name) => name.to_string(),
            SurfaceDim::Int(value) => value.to_string(),
        })
        .collect::<Vec<_>>()
        .join(", ");
    format!("[{dims}]{}", canonical_surface_ty(surface, item))
}

fn canonical_surface_record(surface: &ModuleSurface, fields: &[SurfaceTyField]) -> String {
    format!(
        "{{{}}}",
        fields
            .iter()
            .map(|field| format!(
                "{} = {}",
                field.name,
                canonical_surface_ty(surface, field.ty)
            ))
            .collect::<Vec<_>>()
            .join(", ")
    )
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum RangeTyTag {
    Open,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct RangeTyForm<Id> {
    tag: RangeTyTag,
    bound: Id,
}

impl RangeTyTag {
    const fn to_surface_kind(bound: SurfaceTyId) -> SurfaceTyKind {
        SurfaceTyKind::Range { bound }
    }

    const fn to_hir_kind(bound: HirTyId) -> HirTyKind {
        HirTyKind::Range { bound }
    }
}

const fn hir_range_form(kind: &HirTyKind) -> Option<RangeTyForm<HirTyId>> {
    match kind {
        HirTyKind::Range { bound } => Some(RangeTyForm {
            tag: RangeTyTag::Open,
            bound: *bound,
        }),
        _ => None,
    }
}

const fn surface_range_form(kind: &SurfaceTyKind) -> Option<RangeTyForm<SurfaceTyId>> {
    if let SurfaceTyKind::Range { bound } = kind {
        return Some(RangeTyForm {
            tag: RangeTyTag::Open,
            bound: *bound,
        });
    }
    None
}
pub(super) struct SurfaceTyBuilder<'a> {
    hir: &'a HirStore,
    pub(super) interner: &'a Interner,
    cache: HashMap<HirTyId, SurfaceTyId>,
    tys: Vec<SurfaceTy>,
}

impl<'a> SurfaceTyBuilder<'a> {
    pub(super) fn new(hir: &'a HirStore, interner: &'a Interner) -> Self {
        Self {
            hir,
            interner,
            cache: HashMap::new(),
            tys: Vec::new(),
        }
    }

    pub(super) fn lower(&mut self, id: HirTyId) -> SurfaceTyId {
        if let Some(id) = self.cache.get(&id).copied() {
            return id;
        }
        let kind = self.lower_kind(id);
        let next = SurfaceTyId::new(u32::try_from(self.tys.len()).unwrap_or(u32::MAX));
        self.tys.push(SurfaceTy::new(kind));
        let _ = self.cache.insert(id, next);
        next
    }

    fn lower_kind(&mut self, id: HirTyId) -> SurfaceTyKind {
        let kind = &self.hir.tys.get(id).kind;
        self.lower_ty_kind(kind)
    }

    fn lower_ty_kind(&mut self, kind: &HirTyKind) -> SurfaceTyKind {
        if let Some(simple) = simple_surface_ty_kind(kind) {
            return simple;
        }
        match kind {
            HirTyKind::Named { name, args } => SurfaceTyKind::Named {
                name: self.interner.resolve(*name).into(),
                args: self.lower_ty_ids(*args),
            },
            HirTyKind::Pi {
                binder,
                binder_ty,
                body,
                is_effectful,
            } => SurfaceTyKind::Pi {
                binder: self.interner.resolve(*binder).into(),
                binder_ty: self.lower(*binder_ty),
                body: self.lower(*body),
                is_effectful: *is_effectful,
            },
            HirTyKind::Arrow {
                params,
                ret,
                is_effectful,
            } => SurfaceTyKind::Arrow {
                params: self.lower_ty_ids(*params),
                ret: self.lower(*ret),
                is_effectful: *is_effectful,
            },
            HirTyKind::Sum { left, right } => SurfaceTyKind::Sum {
                left: self.lower(*left),
                right: self.lower(*right),
            },
            HirTyKind::Tuple { items } => SurfaceTyKind::Tuple {
                items: self.lower_ty_ids(*items),
            },
            HirTyKind::Seq { item } => SurfaceTyKind::Seq {
                item: self.lower(*item),
            },
            HirTyKind::Array { dims, item } => SurfaceTyKind::Array {
                dims: self.lower_dims(dims.clone()),
                item: self.lower(*item),
            },
            HirTyKind::Bits { width } => SurfaceTyKind::Bits { width: *width },
            HirTyKind::Range { .. } => self.lower_range_kind(kind),
            HirTyKind::Handler {
                effect,
                input,
                output,
            } => SurfaceTyKind::Handler {
                effect: self.lower(*effect),
                input: self.lower(*input),
                output: self.lower(*output),
            },
            HirTyKind::Mut { inner } => SurfaceTyKind::Mut {
                inner: self.lower(*inner),
            },
            HirTyKind::AnyShape { capability } => SurfaceTyKind::AnyShape {
                capability: self.lower(*capability),
            },
            HirTyKind::SomeShape { capability } => SurfaceTyKind::SomeShape {
                capability: self.lower(*capability),
            },
            HirTyKind::Record { fields } => SurfaceTyKind::Record {
                fields: self.lower_fields(fields.clone()),
            },
            other => simple_surface_ty_kind(other)
                .expect("expected primitive type kind after composite matches"),
        }
    }

    fn lower_range_kind(&mut self, kind: &HirTyKind) -> SurfaceTyKind {
        let Some(form) = hir_range_form(kind) else {
            return simple_surface_ty_kind(kind)
                .expect("expected primitive type kind after range matches");
        };
        RangeTyTag::to_surface_kind(self.lower(form.bound))
    }

    fn lower_ty_ids(&mut self, tys: SliceRange<HirTyId>) -> Box<[SurfaceTyId]> {
        self.hir
            .ty_ids
            .get(tys)
            .iter()
            .copied()
            .map(|ty| self.lower(ty))
            .collect::<Vec<_>>()
            .into_boxed_slice()
    }

    fn lower_dims(&self, dims: SliceRange<HirDim>) -> Box<[SurfaceDim]> {
        self.hir
            .dims
            .get(dims)
            .iter()
            .map(|dim| match dim {
                HirDim::Unknown => SurfaceDim::Unknown,
                HirDim::Name(name) => SurfaceDim::Name(self.interner.resolve(name.name).into()),
                HirDim::Int(value) => SurfaceDim::Int(*value),
            })
            .collect::<Vec<_>>()
            .into_boxed_slice()
    }

    fn lower_fields(&mut self, fields: SliceRange<HirTyField>) -> Box<[SurfaceTyField]> {
        self.hir
            .ty_fields
            .get(fields)
            .iter()
            .map(|field| {
                SurfaceTyField::new(self.interner.resolve(field.name), self.lower(field.ty))
            })
            .collect::<Vec<_>>()
            .into_boxed_slice()
    }

    pub(super) fn finish(self) -> Box<[SurfaceTy]> {
        self.tys.into_boxed_slice()
    }
}

fn simple_surface_ty_kind(kind: &HirTyKind) -> Option<SurfaceTyKind> {
    SimpleTyKind::from_hir(kind).map(SimpleTyKind::into_surface)
}

struct SurfaceTyImporter<'ctx, 'ctx_state, 'interner, 'env> {
    ctx: &'ctx mut PassBase<'ctx_state, 'interner, 'env>,
    surface: &'ctx ModuleSurface,
    cache: HashMap<SurfaceTyId, HirTyId>,
}

impl<'ctx, 'ctx_state, 'interner, 'env> SurfaceTyImporter<'ctx, 'ctx_state, 'interner, 'env> {
    fn new(
        ctx: &'ctx mut PassBase<'ctx_state, 'interner, 'env>,
        surface: &'ctx ModuleSurface,
    ) -> Self {
        Self {
            ctx,
            surface,
            cache: HashMap::new(),
        }
    }

    fn import(&mut self, id: SurfaceTyId) -> HirTyId {
        if let Some(id) = self.cache.get(&id).copied() {
            return id;
        }
        let kind = self.import_kind(id);
        let local = self.ctx.alloc_ty(kind);
        let _ = self.cache.insert(id, local);
        local
    }

    fn import_kind(&mut self, id: SurfaceTyId) -> HirTyKind {
        let kind = &self
            .surface
            .try_ty(id)
            .expect("surface type missing while reading")
            .kind;
        self.import_ty_kind(kind)
    }

    fn import_ty_kind(&mut self, kind: &SurfaceTyKind) -> HirTyKind {
        if let Some(simple) = simple_hir_ty_kind(kind) {
            return simple;
        }
        match kind {
            SurfaceTyKind::Named { name, args } => HirTyKind::Named {
                name: self.ctx.intern(name),
                args: self.import_ty_list(args),
            },
            SurfaceTyKind::Pi {
                binder,
                binder_ty,
                body,
                is_effectful,
            } => HirTyKind::Pi {
                binder: self.ctx.intern(binder),
                binder_ty: self.import(*binder_ty),
                body: self.import(*body),
                is_effectful: *is_effectful,
            },
            SurfaceTyKind::Arrow {
                params,
                ret,
                is_effectful,
            } => HirTyKind::Arrow {
                params: self.import_ty_list(params),
                ret: self.import(*ret),
                is_effectful: *is_effectful,
            },
            SurfaceTyKind::Sum { left, right } => HirTyKind::Sum {
                left: self.import(*left),
                right: self.import(*right),
            },
            SurfaceTyKind::Tuple { items } => HirTyKind::Tuple {
                items: self.import_ty_list(items),
            },
            SurfaceTyKind::Seq { item } => HirTyKind::Seq {
                item: self.import(*item),
            },
            SurfaceTyKind::Array { dims, item } => HirTyKind::Array {
                dims: self.import_dims(dims),
                item: self.import(*item),
            },
            SurfaceTyKind::Bits { width } => HirTyKind::Bits { width: *width },
            SurfaceTyKind::Range { .. } => self.import_range_kind(kind),
            SurfaceTyKind::Handler {
                effect,
                input,
                output,
            } => HirTyKind::Handler {
                effect: self.import(*effect),
                input: self.import(*input),
                output: self.import(*output),
            },
            SurfaceTyKind::Mut { inner } => HirTyKind::Mut {
                inner: self.import(*inner),
            },
            SurfaceTyKind::AnyShape { capability } => HirTyKind::AnyShape {
                capability: self.import(*capability),
            },
            SurfaceTyKind::SomeShape { capability } => HirTyKind::SomeShape {
                capability: self.import(*capability),
            },
            SurfaceTyKind::Record { fields } => HirTyKind::Record {
                fields: self.import_fields(fields),
            },
            other => simple_hir_ty_kind(other)
                .expect("expected primitive surface type kind after composite matches"),
        }
    }

    fn import_range_kind(&mut self, kind: &SurfaceTyKind) -> HirTyKind {
        if let Some(form) = surface_range_form(kind) {
            return RangeTyTag::to_hir_kind(self.import(form.bound));
        }
        simple_hir_ty_kind(kind).expect("expected primitive surface type kind after range matches")
    }

    fn import_ty_list(&mut self, tys: &[SurfaceTyId]) -> SliceRange<HirTyId> {
        let tys = tys
            .iter()
            .copied()
            .map(|ty| self.import(ty))
            .collect::<Vec<_>>();
        self.ctx.alloc_ty_list(tys)
    }

    fn import_dims(&mut self, dims: &[SurfaceDim]) -> SliceRange<HirDim> {
        let dims = dims
            .iter()
            .map(|dim| match dim {
                SurfaceDim::Unknown => HirDim::Unknown,
                SurfaceDim::Name(name) => {
                    HirDim::Name(Ident::new(self.ctx.intern(name), Span::DUMMY))
                }
                SurfaceDim::Int(value) => HirDim::Int(*value),
            })
            .collect::<Vec<_>>();
        self.ctx.alloc_dims(dims)
    }

    fn import_fields(&mut self, fields: &[SurfaceTyField]) -> SliceRange<HirTyField> {
        let fields = fields
            .iter()
            .map(|field| HirTyField::new(self.ctx.intern(&field.name), self.import(field.ty)))
            .collect::<Vec<_>>();
        self.ctx.alloc_ty_fields(fields)
    }
}

fn simple_hir_ty_kind(kind: &SurfaceTyKind) -> Option<HirTyKind> {
    SimpleTyKind::from_surface(kind).map(SimpleTyKind::into_hir)
}

#[derive(Clone, Copy, PartialEq, Eq)]
enum SimpleTyKind {
    Error,
    Unknown,
    Type,
    Syntax,
    Any,
    Empty,
    Unit,
    Bool,
    Nat,
    Int,
    Int8,
    Int16,
    Int32,
    Int64,
    Nat8,
    Nat16,
    Nat32,
    Nat64,
    Float,
    Float32,
    Float64,
    String,
    Rune,
    CString,
    CPtr,
    NatLit(u64),
}

struct SimpleTyInfo {
    simple: SimpleTyKind,
    hir: HirTyKind,
    surface: SurfaceTyKind,
    display_name: &'static str,
}

const SIMPLE_TY_INFOS: &[SimpleTyInfo] = &[
    SimpleTyInfo {
        simple: SimpleTyKind::Error,
        hir: HirTyKind::Error,
        surface: SurfaceTyKind::Error,
        display_name: "<error>",
    },
    SimpleTyInfo {
        simple: SimpleTyKind::Unknown,
        hir: HirTyKind::Unknown,
        surface: SurfaceTyKind::Unknown,
        display_name: "Unknown",
    },
    SimpleTyInfo {
        simple: SimpleTyKind::Type,
        hir: HirTyKind::Type,
        surface: SurfaceTyKind::Type,
        display_name: "Type",
    },
    SimpleTyInfo {
        simple: SimpleTyKind::Syntax,
        hir: HirTyKind::Syntax,
        surface: SurfaceTyKind::Syntax,
        display_name: "Syntax",
    },
    SimpleTyInfo {
        simple: SimpleTyKind::Any,
        hir: HirTyKind::Any,
        surface: SurfaceTyKind::Any,
        display_name: "Any",
    },
    SimpleTyInfo {
        simple: SimpleTyKind::Empty,
        hir: HirTyKind::Empty,
        surface: SurfaceTyKind::Empty,
        display_name: "Empty",
    },
    SimpleTyInfo {
        simple: SimpleTyKind::Unit,
        hir: HirTyKind::Unit,
        surface: SurfaceTyKind::Unit,
        display_name: "Unit",
    },
    SimpleTyInfo {
        simple: SimpleTyKind::Bool,
        hir: HirTyKind::Bool,
        surface: SurfaceTyKind::Bool,
        display_name: "Bool",
    },
    SimpleTyInfo {
        simple: SimpleTyKind::Nat,
        hir: HirTyKind::Nat,
        surface: SurfaceTyKind::Nat,
        display_name: "Nat",
    },
    SimpleTyInfo {
        simple: SimpleTyKind::Int,
        hir: HirTyKind::Int,
        surface: SurfaceTyKind::Int,
        display_name: "Int",
    },
    SimpleTyInfo {
        simple: SimpleTyKind::Int8,
        hir: HirTyKind::Int8,
        surface: SurfaceTyKind::Int8,
        display_name: "Int8",
    },
    SimpleTyInfo {
        simple: SimpleTyKind::Int16,
        hir: HirTyKind::Int16,
        surface: SurfaceTyKind::Int16,
        display_name: "Int16",
    },
    SimpleTyInfo {
        simple: SimpleTyKind::Int32,
        hir: HirTyKind::Int32,
        surface: SurfaceTyKind::Int32,
        display_name: "Int32",
    },
    SimpleTyInfo {
        simple: SimpleTyKind::Int64,
        hir: HirTyKind::Int64,
        surface: SurfaceTyKind::Int64,
        display_name: "Int64",
    },
    SimpleTyInfo {
        simple: SimpleTyKind::Nat8,
        hir: HirTyKind::Nat8,
        surface: SurfaceTyKind::Nat8,
        display_name: "Nat8",
    },
    SimpleTyInfo {
        simple: SimpleTyKind::Nat16,
        hir: HirTyKind::Nat16,
        surface: SurfaceTyKind::Nat16,
        display_name: "Nat16",
    },
    SimpleTyInfo {
        simple: SimpleTyKind::Nat32,
        hir: HirTyKind::Nat32,
        surface: SurfaceTyKind::Nat32,
        display_name: "Nat32",
    },
    SimpleTyInfo {
        simple: SimpleTyKind::Nat64,
        hir: HirTyKind::Nat64,
        surface: SurfaceTyKind::Nat64,
        display_name: "Nat64",
    },
    SimpleTyInfo {
        simple: SimpleTyKind::Float,
        hir: HirTyKind::Float,
        surface: SurfaceTyKind::Float,
        display_name: "Float",
    },
    SimpleTyInfo {
        simple: SimpleTyKind::Float32,
        hir: HirTyKind::Float32,
        surface: SurfaceTyKind::Float32,
        display_name: "Float32",
    },
    SimpleTyInfo {
        simple: SimpleTyKind::Float64,
        hir: HirTyKind::Float64,
        surface: SurfaceTyKind::Float64,
        display_name: "Float64",
    },
    SimpleTyInfo {
        simple: SimpleTyKind::String,
        hir: HirTyKind::String,
        surface: SurfaceTyKind::String,
        display_name: "String",
    },
    SimpleTyInfo {
        simple: SimpleTyKind::Rune,
        hir: HirTyKind::Rune,
        surface: SurfaceTyKind::Rune,
        display_name: "Rune",
    },
    SimpleTyInfo {
        simple: SimpleTyKind::CString,
        hir: HirTyKind::CString,
        surface: SurfaceTyKind::CString,
        display_name: "CString",
    },
    SimpleTyInfo {
        simple: SimpleTyKind::CPtr,
        hir: HirTyKind::CPtr,
        surface: SurfaceTyKind::CPtr,
        display_name: "CPtr",
    },
];

fn simple_ty_info(kind: SimpleTyKind) -> Option<&'static SimpleTyInfo> {
    SIMPLE_TY_INFOS
        .iter()
        .find(|candidate| candidate.simple == kind)
}

impl SimpleTyKind {
    fn from_hir(kind: &HirTyKind) -> Option<Self> {
        if let HirTyKind::NatLit(value) = kind {
            return Some(Self::NatLit(*value));
        }
        SIMPLE_TY_INFOS
            .iter()
            .find_map(|ty| (&ty.hir == kind).then_some(ty.simple))
    }

    fn from_surface(kind: &SurfaceTyKind) -> Option<Self> {
        if let SurfaceTyKind::NatLit(value) = kind {
            return Some(Self::NatLit(*value));
        }
        SIMPLE_TY_INFOS
            .iter()
            .find_map(|ty| (&ty.surface == kind).then_some(ty.simple))
    }

    fn into_hir(self) -> HirTyKind {
        if let Self::NatLit(value) = self {
            return HirTyKind::NatLit(value);
        }
        simple_ty_info(self)
            .map(|ty| ty.hir.clone())
            .expect("simple type kind missing HIR mapping")
    }

    fn into_surface(self) -> SurfaceTyKind {
        if let Self::NatLit(value) = self {
            return SurfaceTyKind::NatLit(value);
        }
        simple_ty_info(self)
            .map(|ty| ty.surface.clone())
            .expect("simple type kind missing surface mapping")
    }
}
