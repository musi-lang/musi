use std::collections::HashMap;

use music_arena::SliceRange;
use music_hir::{HirDim, HirStore, HirTyField, HirTyId, HirTyKind};
use music_names::Interner;

use crate::api::{
    SurfaceDim, SurfaceEffectItem, SurfaceEffectRow, SurfaceTy, SurfaceTyField, SurfaceTyId,
    SurfaceTyKind,
};
use crate::effects::EffectRow;

use super::simple::{hir_range_form, range_to_surface_kind, simple_surface_ty_kind};

pub(in crate::checker::surface) fn lower_surface_effect_row(
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

pub(in crate::checker::surface) struct SurfaceTyBuilder<'a> {
    hir: &'a HirStore,
    pub(in crate::checker::surface) interner: &'a Interner,
    cache: HashMap<HirTyId, SurfaceTyId>,
    tys: Vec<SurfaceTy>,
}

impl<'a> SurfaceTyBuilder<'a> {
    pub(in crate::checker::surface) fn new(hir: &'a HirStore, interner: &'a Interner) -> Self {
        Self {
            hir,
            interner,
            cache: HashMap::new(),
            tys: Vec::new(),
        }
    }

    pub(in crate::checker::surface) fn lower(&mut self, id: HirTyId) -> SurfaceTyId {
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
        range_to_surface_kind(self.lower(form.bound))
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

    pub(in crate::checker::surface) fn finish(self) -> Box<[SurfaceTy]> {
        self.tys.into_boxed_slice()
    }
}
