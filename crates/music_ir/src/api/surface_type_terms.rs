use music_sema::{SurfaceDim, SurfaceTy, SurfaceTyId, SurfaceTyKind};
use music_term::{TypeDim, TypeField, TypeModuleRef, TypeTerm, TypeTermKind};

struct SurfaceTypeTermPrimitive {
    surface: SurfaceTyKind,
    term: TypeTermKind,
}

const SURFACE_TYPE_TERM_PRIMITIVES: &[SurfaceTypeTermPrimitive] = &[
    SurfaceTypeTermPrimitive {
        surface: SurfaceTyKind::Error,
        term: TypeTermKind::Error,
    },
    SurfaceTypeTermPrimitive {
        surface: SurfaceTyKind::Unknown,
        term: TypeTermKind::Unknown,
    },
    SurfaceTypeTermPrimitive {
        surface: SurfaceTyKind::Type,
        term: TypeTermKind::Type,
    },
    SurfaceTypeTermPrimitive {
        surface: SurfaceTyKind::Syntax,
        term: TypeTermKind::Syntax,
    },
    SurfaceTypeTermPrimitive {
        surface: SurfaceTyKind::Any,
        term: TypeTermKind::Any,
    },
    SurfaceTypeTermPrimitive {
        surface: SurfaceTyKind::Empty,
        term: TypeTermKind::Empty,
    },
    SurfaceTypeTermPrimitive {
        surface: SurfaceTyKind::Unit,
        term: TypeTermKind::Unit,
    },
    SurfaceTypeTermPrimitive {
        surface: SurfaceTyKind::Bool,
        term: TypeTermKind::Bool,
    },
    SurfaceTypeTermPrimitive {
        surface: SurfaceTyKind::Nat,
        term: TypeTermKind::Nat,
    },
    SurfaceTypeTermPrimitive {
        surface: SurfaceTyKind::Int,
        term: TypeTermKind::Int,
    },
    SurfaceTypeTermPrimitive {
        surface: SurfaceTyKind::Int8,
        term: TypeTermKind::Int8,
    },
    SurfaceTypeTermPrimitive {
        surface: SurfaceTyKind::Int16,
        term: TypeTermKind::Int16,
    },
    SurfaceTypeTermPrimitive {
        surface: SurfaceTyKind::Int32,
        term: TypeTermKind::Int32,
    },
    SurfaceTypeTermPrimitive {
        surface: SurfaceTyKind::Int64,
        term: TypeTermKind::Int64,
    },
    SurfaceTypeTermPrimitive {
        surface: SurfaceTyKind::Nat8,
        term: TypeTermKind::Nat8,
    },
    SurfaceTypeTermPrimitive {
        surface: SurfaceTyKind::Nat16,
        term: TypeTermKind::Nat16,
    },
    SurfaceTypeTermPrimitive {
        surface: SurfaceTyKind::Nat32,
        term: TypeTermKind::Nat32,
    },
    SurfaceTypeTermPrimitive {
        surface: SurfaceTyKind::Nat64,
        term: TypeTermKind::Nat64,
    },
    SurfaceTypeTermPrimitive {
        surface: SurfaceTyKind::Float,
        term: TypeTermKind::Float,
    },
    SurfaceTypeTermPrimitive {
        surface: SurfaceTyKind::Float32,
        term: TypeTermKind::Float32,
    },
    SurfaceTypeTermPrimitive {
        surface: SurfaceTyKind::Float64,
        term: TypeTermKind::Float64,
    },
    SurfaceTypeTermPrimitive {
        surface: SurfaceTyKind::String,
        term: TypeTermKind::String,
    },
    SurfaceTypeTermPrimitive {
        surface: SurfaceTyKind::Rune,
        term: TypeTermKind::Rune,
    },
    SurfaceTypeTermPrimitive {
        surface: SurfaceTyKind::CString,
        term: TypeTermKind::CString,
    },
    SurfaceTypeTermPrimitive {
        surface: SurfaceTyKind::CPtr,
        term: TypeTermKind::CPtr,
    },
];

#[must_use]
pub fn lower_surface_type_term(types: &[SurfaceTy], ty: &SurfaceTy) -> TypeTerm {
    lower_surface_primitive_type_term(ty)
        .or_else(|| lower_surface_named_callable_type_term(types, ty))
        .or_else(|| lower_surface_collection_type_term(types, ty))
        .or_else(|| lower_surface_range_type_term(types, ty))
        .or_else(|| lower_surface_record_type_term(types, ty))
        .unwrap_or_else(|| TypeTerm::new(TypeTermKind::Error))
}

fn lower_surface_primitive_type_term(ty: &SurfaceTy) -> Option<TypeTerm> {
    if let SurfaceTyKind::NatLit(value) = &ty.kind {
        return Some(TypeTerm::new(TypeTermKind::NatLit(*value)));
    }
    SURFACE_TYPE_TERM_PRIMITIVES.iter().find_map(|primitive| {
        (primitive.surface == ty.kind).then(|| TypeTerm::new(primitive.term.clone()))
    })
}

fn lower_surface_named_callable_type_term(types: &[SurfaceTy], ty: &SurfaceTy) -> Option<TypeTerm> {
    Some(match &ty.kind {
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
        SurfaceTyKind::Bits { width } => lower_named_term(
            "Bits",
            vec![TypeTerm::new(TypeTermKind::NatLit(u64::from(*width)))].into_boxed_slice(),
        ),
        SurfaceTyKind::Handler {
            effect,
            input,
            output,
        } => TypeTerm::new(TypeTermKind::Handler {
            effect: Box::new(lower_surface_type_term_id(types, *effect)),
            input: Box::new(lower_surface_type_term_id(types, *input)),
            output: Box::new(lower_surface_type_term_id(types, *output)),
        }),
        SurfaceTyKind::Mut { inner } => TypeTerm::new(TypeTermKind::Mut {
            inner: Box::new(lower_surface_type_term_id(types, *inner)),
        }),
        SurfaceTyKind::AnyShape { capability: shape } => TypeTerm::new(TypeTermKind::AnyShape {
            capability: Box::new(lower_surface_type_term_id(types, *shape)),
        }),
        SurfaceTyKind::SomeShape { capability: shape } => TypeTerm::new(TypeTermKind::SomeShape {
            capability: Box::new(lower_surface_type_term_id(types, *shape)),
        }),
        _ => return None,
    })
}

fn lower_surface_collection_type_term(types: &[SurfaceTy], ty: &SurfaceTy) -> Option<TypeTerm> {
    Some(match &ty.kind {
        SurfaceTyKind::Tuple { items } => TypeTerm::new(TypeTermKind::Tuple {
            items: items
                .iter()
                .map(|item| lower_surface_type_term_id(types, *item))
                .collect::<Vec<_>>()
                .into_boxed_slice(),
        }),
        SurfaceTyKind::Seq { item } => TypeTerm::new(TypeTermKind::Seq {
            item: Box::new(lower_surface_type_term_id(types, *item)),
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
        _ => return None,
    })
}

fn lower_surface_range_type_term(types: &[SurfaceTy], ty: &SurfaceTy) -> Option<TypeTerm> {
    Some(match &ty.kind {
        SurfaceTyKind::Range { bound } => TypeTerm::new(TypeTermKind::Range {
            bound: Box::new(lower_surface_type_term_id(types, *bound)),
        }),
        _ => return None,
    })
}

fn lower_surface_record_type_term(types: &[SurfaceTy], ty: &SurfaceTy) -> Option<TypeTerm> {
    let SurfaceTyKind::Record { fields } = &ty.kind else {
        return None;
    };
    Some(TypeTerm::new(TypeTermKind::Record {
        fields: fields
            .iter()
            .map(|field| TypeField {
                name: field.name.clone(),
                ty: lower_surface_type_term_id(types, field.ty),
            })
            .collect::<Vec<_>>()
            .into_boxed_slice(),
    }))
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
