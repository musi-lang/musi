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
    SurfaceEffectRow {
        items: row
            .items
            .iter()
            .map(|item| SurfaceEffectItem {
                name: item.name.clone(),
                arg: item.arg.map(|ty| tys.lower(ty)),
            })
            .collect::<Vec<_>>()
            .into_boxed_slice(),
        open: row.open.clone(),
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
    match &surface.ty(ty).kind {
        SurfaceTyKind::Error => "<error>".into(),
        SurfaceTyKind::Unknown => "Unknown".into(),
        SurfaceTyKind::Type => "Type".into(),
        SurfaceTyKind::Syntax => "Syntax".into(),
        SurfaceTyKind::Any => "Any".into(),
        SurfaceTyKind::Empty => "Empty".into(),
        SurfaceTyKind::Unit => "Unit".into(),
        SurfaceTyKind::Bool => "Bool".into(),
        SurfaceTyKind::Nat => "Nat".into(),
        SurfaceTyKind::Int => "Int".into(),
        SurfaceTyKind::Float => "Float".into(),
        SurfaceTyKind::String => "String".into(),
        SurfaceTyKind::CString => "CString".into(),
        SurfaceTyKind::CPtr => "CPtr".into(),
        SurfaceTyKind::Module => "Module".into(),
        SurfaceTyKind::NatLit(value) => value.to_string(),
        SurfaceTyKind::Named { name, args } => canonical_surface_named(surface, name, args),
        SurfaceTyKind::Pi {
            binder,
            binder_ty,
            body,
            is_effectful,
        } => {
            let arrow = if *is_effectful { " ~> " } else { " -> " };
            format!(
                "forall ({binder} : {}){arrow}{}",
                canonical_surface_ty(surface, *binder_ty),
                canonical_surface_ty(surface, *body)
            )
        }
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
        SurfaceTyKind::Array { dims, item } => canonical_surface_array(surface, dims, *item),
        SurfaceTyKind::Mut { inner } => {
            format!("mut {}", canonical_surface_ty(surface, *inner))
        }
        SurfaceTyKind::Record { fields } => canonical_surface_record(surface, fields),
    }
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
        self.tys.push(SurfaceTy { kind });
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
            HirTyKind::Array { dims, item } => SurfaceTyKind::Array {
                dims: self.lower_dims(dims.clone()),
                item: self.lower(*item),
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
            .map(|field| SurfaceTyField {
                name: self.interner.resolve(field.name).into(),
                ty: self.lower(field.ty),
            })
            .collect::<Vec<_>>()
            .into_boxed_slice()
    }

    pub(super) fn finish(self) -> Box<[SurfaceTy]> {
        self.tys.into_boxed_slice()
    }
}

const fn simple_surface_ty_kind(kind: &HirTyKind) -> Option<SurfaceTyKind> {
    match kind {
        HirTyKind::Error => Some(SurfaceTyKind::Error),
        HirTyKind::Unknown => Some(SurfaceTyKind::Unknown),
        HirTyKind::Type => Some(SurfaceTyKind::Type),
        HirTyKind::Syntax => Some(SurfaceTyKind::Syntax),
        HirTyKind::Any => Some(SurfaceTyKind::Any),
        HirTyKind::Empty => Some(SurfaceTyKind::Empty),
        HirTyKind::Unit => Some(SurfaceTyKind::Unit),
        HirTyKind::Bool => Some(SurfaceTyKind::Bool),
        HirTyKind::Nat => Some(SurfaceTyKind::Nat),
        HirTyKind::Int => Some(SurfaceTyKind::Int),
        HirTyKind::Float => Some(SurfaceTyKind::Float),
        HirTyKind::String => Some(SurfaceTyKind::String),
        HirTyKind::CString => Some(SurfaceTyKind::CString),
        HirTyKind::CPtr => Some(SurfaceTyKind::CPtr),
        HirTyKind::Module => Some(SurfaceTyKind::Module),
        HirTyKind::NatLit(value) => Some(SurfaceTyKind::NatLit(*value)),
        HirTyKind::Named { .. }
        | HirTyKind::Pi { .. }
        | HirTyKind::Arrow { .. }
        | HirTyKind::Sum { .. }
        | HirTyKind::Tuple { .. }
        | HirTyKind::Array { .. }
        | HirTyKind::Mut { .. }
        | HirTyKind::Record { .. } => None,
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
        let kind = &self.surface.ty(id).kind;
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
            SurfaceTyKind::Array { dims, item } => HirTyKind::Array {
                dims: self.import_dims(dims),
                item: self.import(*item),
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
            .map(|field| HirTyField {
                name: self.ctx.intern(&field.name),
                ty: self.import(field.ty),
            })
            .collect::<Vec<_>>();
        self.ctx.alloc_ty_fields(fields)
    }
}

const fn simple_hir_ty_kind(kind: &SurfaceTyKind) -> Option<HirTyKind> {
    match kind {
        SurfaceTyKind::Error => Some(HirTyKind::Error),
        SurfaceTyKind::Unknown => Some(HirTyKind::Unknown),
        SurfaceTyKind::Type => Some(HirTyKind::Type),
        SurfaceTyKind::Syntax => Some(HirTyKind::Syntax),
        SurfaceTyKind::Any => Some(HirTyKind::Any),
        SurfaceTyKind::Empty => Some(HirTyKind::Empty),
        SurfaceTyKind::Unit => Some(HirTyKind::Unit),
        SurfaceTyKind::Bool => Some(HirTyKind::Bool),
        SurfaceTyKind::Nat => Some(HirTyKind::Nat),
        SurfaceTyKind::Int => Some(HirTyKind::Int),
        SurfaceTyKind::Float => Some(HirTyKind::Float),
        SurfaceTyKind::String => Some(HirTyKind::String),
        SurfaceTyKind::CString => Some(HirTyKind::CString),
        SurfaceTyKind::CPtr => Some(HirTyKind::CPtr),
        SurfaceTyKind::Module => Some(HirTyKind::Module),
        SurfaceTyKind::NatLit(value) => Some(HirTyKind::NatLit(*value)),
        SurfaceTyKind::Named { .. }
        | SurfaceTyKind::Pi { .. }
        | SurfaceTyKind::Arrow { .. }
        | SurfaceTyKind::Sum { .. }
        | SurfaceTyKind::Tuple { .. }
        | SurfaceTyKind::Array { .. }
        | SurfaceTyKind::Mut { .. }
        | SurfaceTyKind::Record { .. } => None,
    }
}
