use std::collections::HashMap;

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
        SurfaceTyKind::Int => "Int".into(),
        SurfaceTyKind::Float => "Float".into(),
        SurfaceTyKind::String => "String".into(),
        SurfaceTyKind::CString => "CString".into(),
        SurfaceTyKind::CPtr => "CPtr".into(),
        SurfaceTyKind::Module => "Module".into(),
        SurfaceTyKind::Named { name, args } => {
            if args.is_empty() {
                name.to_string()
            } else {
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
        }
        SurfaceTyKind::Arrow {
            params,
            ret,
            is_effectful,
        } => {
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
            let arrow = if *is_effectful { " ~> " } else { " -> " };
            format!("{left}{arrow}{}", canonical_surface_ty(surface, *ret))
        }
        SurfaceTyKind::Sum { left, right } => {
            format!(
                "{} + {}",
                canonical_surface_ty(surface, *left),
                canonical_surface_ty(surface, *right)
            )
        }
        SurfaceTyKind::Tuple { items } => format!(
            "({})",
            items
                .iter()
                .copied()
                .map(|item| canonical_surface_ty(surface, item))
                .collect::<Vec<_>>()
                .join(", ")
        ),
        SurfaceTyKind::Array { dims, item } => {
            let dims = dims
                .iter()
                .map(|dim| match dim {
                    SurfaceDim::Unknown => "_".into(),
                    SurfaceDim::Name(name) => name.to_string(),
                    SurfaceDim::Int(value) => value.to_string(),
                })
                .collect::<Vec<_>>()
                .join(", ");
            format!("[{dims}]{}", canonical_surface_ty(surface, *item))
        }
        SurfaceTyKind::Mut { inner } => {
            format!("mut {}", canonical_surface_ty(surface, *inner))
        }
        SurfaceTyKind::Record { fields } => format!(
            "{{{}}}",
            fields
                .iter()
                .map(|field| format!(
                    "{} := {}",
                    field.name,
                    canonical_surface_ty(surface, field.ty)
                ))
                .collect::<Vec<_>>()
                .join(", ")
        ),
    }
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
        let kind = match &self.hir.tys.get(id).kind {
            HirTyKind::Error => SurfaceTyKind::Error,
            HirTyKind::Unknown => SurfaceTyKind::Unknown,
            HirTyKind::Type => SurfaceTyKind::Type,
            HirTyKind::Syntax => SurfaceTyKind::Syntax,
            HirTyKind::Any => SurfaceTyKind::Any,
            HirTyKind::Empty => SurfaceTyKind::Empty,
            HirTyKind::Unit => SurfaceTyKind::Unit,
            HirTyKind::Bool => SurfaceTyKind::Bool,
            HirTyKind::Int => SurfaceTyKind::Int,
            HirTyKind::Float => SurfaceTyKind::Float,
            HirTyKind::String => SurfaceTyKind::String,
            HirTyKind::CString => SurfaceTyKind::CString,
            HirTyKind::CPtr => SurfaceTyKind::CPtr,
            HirTyKind::Module => SurfaceTyKind::Module,
            HirTyKind::Named { name, args } => SurfaceTyKind::Named {
                name: self.interner.resolve(*name).into(),
                args: self
                    .hir
                    .ty_ids
                    .get(*args)
                    .iter()
                    .copied()
                    .map(|ty| self.lower(ty))
                    .collect::<Vec<_>>()
                    .into_boxed_slice(),
            },
            HirTyKind::Arrow {
                params,
                ret,
                is_effectful,
            } => SurfaceTyKind::Arrow {
                params: self
                    .hir
                    .ty_ids
                    .get(*params)
                    .iter()
                    .copied()
                    .map(|ty| self.lower(ty))
                    .collect::<Vec<_>>()
                    .into_boxed_slice(),
                ret: self.lower(*ret),
                is_effectful: *is_effectful,
            },
            HirTyKind::Sum { left, right } => SurfaceTyKind::Sum {
                left: self.lower(*left),
                right: self.lower(*right),
            },
            HirTyKind::Tuple { items } => SurfaceTyKind::Tuple {
                items: self
                    .hir
                    .ty_ids
                    .get(*items)
                    .iter()
                    .copied()
                    .map(|ty| self.lower(ty))
                    .collect::<Vec<_>>()
                    .into_boxed_slice(),
            },
            HirTyKind::Array { dims, item } => SurfaceTyKind::Array {
                dims: self
                    .hir
                    .dims
                    .get(dims.clone())
                    .iter()
                    .map(|dim| match dim {
                        HirDim::Unknown => SurfaceDim::Unknown,
                        HirDim::Name(name) => {
                            SurfaceDim::Name(self.interner.resolve(name.name).into())
                        }
                        HirDim::Int(value) => SurfaceDim::Int(*value),
                    })
                    .collect::<Vec<_>>()
                    .into_boxed_slice(),
                item: self.lower(*item),
            },
            HirTyKind::Mut { inner } => SurfaceTyKind::Mut {
                inner: self.lower(*inner),
            },
            HirTyKind::Record { fields } => SurfaceTyKind::Record {
                fields: self
                    .hir
                    .ty_fields
                    .get(fields.clone())
                    .iter()
                    .map(|field| SurfaceTyField {
                        name: self.interner.resolve(field.name).into(),
                        ty: self.lower(field.ty),
                    })
                    .collect::<Vec<_>>()
                    .into_boxed_slice(),
            },
        };
        let next = SurfaceTyId::new(u32::try_from(self.tys.len()).unwrap_or(u32::MAX));
        self.tys.push(SurfaceTy { kind });
        let _ = self.cache.insert(id, next);
        next
    }

    pub(super) fn finish(self) -> Box<[SurfaceTy]> {
        self.tys.into_boxed_slice()
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
        let kind = match &self.surface.ty(id).kind {
            SurfaceTyKind::Error => HirTyKind::Error,
            SurfaceTyKind::Unknown => HirTyKind::Unknown,
            SurfaceTyKind::Type => HirTyKind::Type,
            SurfaceTyKind::Syntax => HirTyKind::Syntax,
            SurfaceTyKind::Any => HirTyKind::Any,
            SurfaceTyKind::Empty => HirTyKind::Empty,
            SurfaceTyKind::Unit => HirTyKind::Unit,
            SurfaceTyKind::Bool => HirTyKind::Bool,
            SurfaceTyKind::Int => HirTyKind::Int,
            SurfaceTyKind::Float => HirTyKind::Float,
            SurfaceTyKind::String => HirTyKind::String,
            SurfaceTyKind::CString => HirTyKind::CString,
            SurfaceTyKind::CPtr => HirTyKind::CPtr,
            SurfaceTyKind::Module => HirTyKind::Module,
            SurfaceTyKind::Named { name, args } => {
                let args = args
                    .iter()
                    .copied()
                    .map(|ty| self.import(ty))
                    .collect::<Vec<_>>();
                let args = self.ctx.alloc_ty_list(args);
                HirTyKind::Named {
                    name: self.ctx.intern(name),
                    args,
                }
            }
            SurfaceTyKind::Arrow {
                params,
                ret,
                is_effectful,
            } => {
                let params = params
                    .iter()
                    .copied()
                    .map(|ty| self.import(ty))
                    .collect::<Vec<_>>();
                let params = self.ctx.alloc_ty_list(params);
                HirTyKind::Arrow {
                    params,
                    ret: self.import(*ret),
                    is_effectful: *is_effectful,
                }
            }
            SurfaceTyKind::Sum { left, right } => HirTyKind::Sum {
                left: self.import(*left),
                right: self.import(*right),
            },
            SurfaceTyKind::Tuple { items } => {
                let items = items
                    .iter()
                    .copied()
                    .map(|ty| self.import(ty))
                    .collect::<Vec<_>>();
                let items = self.ctx.alloc_ty_list(items);
                HirTyKind::Tuple { items }
            }
            SurfaceTyKind::Array { dims, item } => {
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
                let dims = self.ctx.alloc_dims(dims);
                HirTyKind::Array {
                    dims,
                    item: self.import(*item),
                }
            }
            SurfaceTyKind::Mut { inner } => HirTyKind::Mut {
                inner: self.import(*inner),
            },
            SurfaceTyKind::Record { fields } => {
                let fields = fields
                    .iter()
                    .map(|field| HirTyField {
                        name: self.ctx.intern(&field.name),
                        ty: self.import(field.ty),
                    })
                    .collect::<Vec<_>>();
                let fields = self.ctx.alloc_ty_fields(fields);
                HirTyKind::Record { fields }
            }
        };
        let local = self.ctx.alloc_ty(kind);
        let _ = self.cache.insert(id, local);
        local
    }
}
