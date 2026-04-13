use std::collections::HashMap;

use music_arena::SliceRange;
use music_base::Span;
use music_hir::{HirDim, HirStore, HirTyField, HirTyId, HirTyKind};
use music_module::ModuleKey;
use music_names::{Ident, Interner, Symbol};

use super::PassBase;
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
        SurfaceTyKind::Range { bound } => {
            format!("Range[{}]", canonical_surface_ty(surface, *bound))
        }
        SurfaceTyKind::ClosedRange { bound } => {
            format!("ClosedRange[{}]", canonical_surface_ty(surface, *bound))
        }
        SurfaceTyKind::PartialRangeFrom { bound } => {
            format!(
                "PartialRangeFrom[{}]",
                canonical_surface_ty(surface, *bound)
            )
        }
        SurfaceTyKind::PartialRangeUpTo { bound } => {
            format!(
                "PartialRangeUpTo[{}]",
                canonical_surface_ty(surface, *bound)
            )
        }
        SurfaceTyKind::PartialRangeThru { bound } => {
            format!(
                "PartialRangeThru[{}]",
                canonical_surface_ty(surface, *bound)
            )
        }
        SurfaceTyKind::Handler {
            effect,
            input,
            output,
        } => format!(
            "using {} ({} -> {})",
            canonical_surface_ty(surface, *effect),
            canonical_surface_ty(surface, *input),
            canonical_surface_ty(surface, *output)
        ),
        SurfaceTyKind::Mut { inner } => {
            format!("mut {}", canonical_surface_ty(surface, *inner))
        }
        SurfaceTyKind::Record { fields } => canonical_surface_record(surface, fields),
        SurfaceTyKind::Error
        | SurfaceTyKind::Unknown
        | SurfaceTyKind::Type
        | SurfaceTyKind::Syntax
        | SurfaceTyKind::Any
        | SurfaceTyKind::Empty
        | SurfaceTyKind::Unit
        | SurfaceTyKind::Bool
        | SurfaceTyKind::Nat
        | SurfaceTyKind::Int
        | SurfaceTyKind::Float
        | SurfaceTyKind::String
        | SurfaceTyKind::CString
        | SurfaceTyKind::CPtr
        | SurfaceTyKind::Module
        | SurfaceTyKind::NatLit(_) => {
            canonical_simple_surface_ty(kind).expect("simple surface type should render")
        }
    }
}

fn canonical_simple_surface_ty(kind: &SurfaceTyKind) -> Option<String> {
    let simple = SimpleTyKind::from_surface(kind)?;
    Some(match simple {
        SimpleTyKind::NatLit(value) => value.to_string(),
        SimpleTyKind::Error => "<error>".to_owned(),
        SimpleTyKind::Unknown => "Unknown".to_owned(),
        SimpleTyKind::Type => "Type".to_owned(),
        SimpleTyKind::Syntax => "Syntax".to_owned(),
        SimpleTyKind::Any => "Any".to_owned(),
        SimpleTyKind::Empty => "Empty".to_owned(),
        SimpleTyKind::Unit => "Unit".to_owned(),
        SimpleTyKind::Bool => "Bool".to_owned(),
        SimpleTyKind::Nat => "Nat".to_owned(),
        SimpleTyKind::Int => "Int".to_owned(),
        SimpleTyKind::Float => "Float".to_owned(),
        SimpleTyKind::String => "String".to_owned(),
        SimpleTyKind::CString => "CString".to_owned(),
        SimpleTyKind::CPtr => "CPtr".to_owned(),
        SimpleTyKind::Module => "Module".to_owned(),
    })
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
            HirTyKind::Range { bound } => SurfaceTyKind::Range {
                bound: self.lower(*bound),
            },
            HirTyKind::ClosedRange { bound } => SurfaceTyKind::ClosedRange {
                bound: self.lower(*bound),
            },
            HirTyKind::PartialRangeFrom { bound } => SurfaceTyKind::PartialRangeFrom {
                bound: self.lower(*bound),
            },
            HirTyKind::PartialRangeUpTo { bound } => SurfaceTyKind::PartialRangeUpTo {
                bound: self.lower(*bound),
            },
            HirTyKind::PartialRangeThru { bound } => SurfaceTyKind::PartialRangeThru {
                bound: self.lower(*bound),
            },
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
            HirTyKind::Record { fields } => SurfaceTyKind::Record {
                fields: self.lower_fields(fields.clone()),
            },
            other => simple_surface_ty_kind(other)
                .expect("expected primitive type kind after composite matches"),
        }
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

const fn simple_surface_ty_kind(kind: &HirTyKind) -> Option<SurfaceTyKind> {
    match SimpleTyKind::from_hir(kind) {
        Some(kind) => Some(kind.into_surface()),
        None => None,
    }
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
            SurfaceTyKind::Range { bound } => HirTyKind::Range {
                bound: self.import(*bound),
            },
            SurfaceTyKind::ClosedRange { bound } => HirTyKind::ClosedRange {
                bound: self.import(*bound),
            },
            SurfaceTyKind::PartialRangeFrom { bound } => HirTyKind::PartialRangeFrom {
                bound: self.import(*bound),
            },
            SurfaceTyKind::PartialRangeUpTo { bound } => HirTyKind::PartialRangeUpTo {
                bound: self.import(*bound),
            },
            SurfaceTyKind::PartialRangeThru { bound } => HirTyKind::PartialRangeThru {
                bound: self.import(*bound),
            },
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
            SurfaceTyKind::Record { fields } => HirTyKind::Record {
                fields: self.import_fields(fields),
            },
            other => simple_hir_ty_kind(other)
                .expect("expected primitive surface type kind after composite matches"),
        }
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

const fn simple_hir_ty_kind(kind: &SurfaceTyKind) -> Option<HirTyKind> {
    match SimpleTyKind::from_surface(kind) {
        Some(kind) => Some(kind.into_hir()),
        None => None,
    }
}

#[derive(Clone, Copy)]
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
    Float,
    String,
    CString,
    CPtr,
    Module,
    NatLit(u64),
}

impl SimpleTyKind {
    const fn from_hir(kind: &HirTyKind) -> Option<Self> {
        match kind {
            HirTyKind::Error => Some(Self::Error),
            HirTyKind::Unknown => Some(Self::Unknown),
            HirTyKind::Type => Some(Self::Type),
            HirTyKind::Syntax => Some(Self::Syntax),
            HirTyKind::Any => Some(Self::Any),
            HirTyKind::Empty => Some(Self::Empty),
            HirTyKind::Unit => Some(Self::Unit),
            HirTyKind::Bool => Some(Self::Bool),
            HirTyKind::Nat => Some(Self::Nat),
            HirTyKind::Int => Some(Self::Int),
            HirTyKind::Float => Some(Self::Float),
            HirTyKind::String => Some(Self::String),
            HirTyKind::CString => Some(Self::CString),
            HirTyKind::CPtr => Some(Self::CPtr),
            HirTyKind::Module => Some(Self::Module),
            HirTyKind::NatLit(value) => Some(Self::NatLit(*value)),
            HirTyKind::Named { .. }
            | HirTyKind::Pi { .. }
            | HirTyKind::Arrow { .. }
            | HirTyKind::Sum { .. }
            | HirTyKind::Tuple { .. }
            | HirTyKind::Seq { .. }
            | HirTyKind::Array { .. }
            | HirTyKind::Range { .. }
            | HirTyKind::ClosedRange { .. }
            | HirTyKind::PartialRangeFrom { .. }
            | HirTyKind::PartialRangeUpTo { .. }
            | HirTyKind::PartialRangeThru { .. }
            | HirTyKind::Handler { .. }
            | HirTyKind::Mut { .. }
            | HirTyKind::Record { .. } => None,
        }
    }

    const fn from_surface(kind: &SurfaceTyKind) -> Option<Self> {
        if matches!(
            kind,
            SurfaceTyKind::Named { .. }
                | SurfaceTyKind::Pi { .. }
                | SurfaceTyKind::Arrow { .. }
                | SurfaceTyKind::Sum { .. }
                | SurfaceTyKind::Tuple { .. }
                | SurfaceTyKind::Seq { .. }
                | SurfaceTyKind::Array { .. }
                | SurfaceTyKind::Range { .. }
                | SurfaceTyKind::ClosedRange { .. }
                | SurfaceTyKind::PartialRangeFrom { .. }
                | SurfaceTyKind::PartialRangeUpTo { .. }
                | SurfaceTyKind::PartialRangeThru { .. }
                | SurfaceTyKind::Handler { .. }
                | SurfaceTyKind::Mut { .. }
                | SurfaceTyKind::Record { .. }
        ) {
            return None;
        }
        match kind {
            SurfaceTyKind::Error => Some(Self::Error),
            SurfaceTyKind::Unknown => Some(Self::Unknown),
            SurfaceTyKind::Type => Some(Self::Type),
            SurfaceTyKind::Syntax => Some(Self::Syntax),
            SurfaceTyKind::Any => Some(Self::Any),
            SurfaceTyKind::Empty => Some(Self::Empty),
            SurfaceTyKind::Unit => Some(Self::Unit),
            SurfaceTyKind::Bool => Some(Self::Bool),
            SurfaceTyKind::Nat => Some(Self::Nat),
            SurfaceTyKind::Int => Some(Self::Int),
            SurfaceTyKind::Float => Some(Self::Float),
            SurfaceTyKind::String => Some(Self::String),
            SurfaceTyKind::CString => Some(Self::CString),
            SurfaceTyKind::CPtr => Some(Self::CPtr),
            SurfaceTyKind::Module => Some(Self::Module),
            SurfaceTyKind::NatLit(value) => Some(Self::NatLit(*value)),
            SurfaceTyKind::Named { .. }
            | SurfaceTyKind::Pi { .. }
            | SurfaceTyKind::Arrow { .. }
            | SurfaceTyKind::Sum { .. }
            | SurfaceTyKind::Tuple { .. }
            | SurfaceTyKind::Seq { .. }
            | SurfaceTyKind::Array { .. }
            | SurfaceTyKind::Range { .. }
            | SurfaceTyKind::ClosedRange { .. }
            | SurfaceTyKind::PartialRangeFrom { .. }
            | SurfaceTyKind::PartialRangeUpTo { .. }
            | SurfaceTyKind::PartialRangeThru { .. }
            | SurfaceTyKind::Handler { .. }
            | SurfaceTyKind::Mut { .. }
            | SurfaceTyKind::Record { .. } => None,
        }
    }

    const fn into_hir(self) -> HirTyKind {
        match self {
            Self::Error => HirTyKind::Error,
            Self::Unknown => HirTyKind::Unknown,
            Self::Type => HirTyKind::Type,
            Self::Syntax => HirTyKind::Syntax,
            Self::Any => HirTyKind::Any,
            Self::Empty => HirTyKind::Empty,
            Self::Unit => HirTyKind::Unit,
            Self::Bool => HirTyKind::Bool,
            Self::Nat => HirTyKind::Nat,
            Self::Int => HirTyKind::Int,
            Self::Float => HirTyKind::Float,
            Self::String => HirTyKind::String,
            Self::CString => HirTyKind::CString,
            Self::CPtr => HirTyKind::CPtr,
            Self::Module => HirTyKind::Module,
            Self::NatLit(value) => HirTyKind::NatLit(value),
        }
    }

    const fn into_surface(self) -> SurfaceTyKind {
        if let Self::NatLit(value) = self {
            return SurfaceTyKind::NatLit(value);
        }
        match self {
            Self::Error => SurfaceTyKind::Error,
            Self::Unknown => SurfaceTyKind::Unknown,
            Self::Type => SurfaceTyKind::Type,
            Self::Syntax => SurfaceTyKind::Syntax,
            Self::Any => SurfaceTyKind::Any,
            Self::Empty => SurfaceTyKind::Empty,
            Self::Unit => SurfaceTyKind::Unit,
            Self::Bool => SurfaceTyKind::Bool,
            Self::Nat => SurfaceTyKind::Nat,
            Self::Int => SurfaceTyKind::Int,
            Self::Float => SurfaceTyKind::Float,
            Self::String => SurfaceTyKind::String,
            Self::CString => SurfaceTyKind::CString,
            Self::CPtr => SurfaceTyKind::CPtr,
            Self::Module => SurfaceTyKind::Module,
            Self::NatLit(value) => SurfaceTyKind::NatLit(value),
        }
    }
}
