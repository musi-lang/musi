use std::collections::HashMap;

use music_arena::SliceRange;
use music_base::Span;
use music_hir::{HirDim, HirTyField, HirTyId, HirTyKind};
use music_names::Ident;

use crate::api::{ModuleSurface, SurfaceDim, SurfaceTyField, SurfaceTyId, SurfaceTyKind};
use crate::checker::PassBase;

use super::simple::{range_to_hir_kind, simple_hir_ty_kind, surface_range_form};

pub fn import_surface_ty(
    ctx: &mut PassBase<'_, '_, '_>,
    surface: &ModuleSurface,
    ty: SurfaceTyId,
) -> HirTyId {
    let mut importer = SurfaceTyImporter::new(ctx, surface);
    importer.import(ty)
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
            return range_to_hir_kind(self.import(form.bound));
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
