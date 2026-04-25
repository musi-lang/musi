use std::collections::HashMap;

use music_seam::descriptor::ExportTarget;
use music_seam::{Artifact, DataId, ExportId, TypeId};

use super::ProgramTypeAbiKind;

pub(super) type ExportMap = HashMap<Box<str>, ExportId>;
pub(super) type DataLayoutMap = HashMap<TypeId, ProgramDataLayout>;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ProgramExportKind {
    Procedure,
    Global,
    Foreign,
    Type,
    Effect,
    Shape,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ProgramExport {
    pub name: Box<str>,
    pub opaque: bool,
    pub kind: ProgramExportKind,
}

impl ProgramExport {
    #[must_use]
    pub const fn new(name: Box<str>, opaque: bool, kind: ProgramExportKind) -> Self {
        Self { name, opaque, kind }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ProgramDataVariantLayout {
    pub name: Box<str>,
    pub tag: i64,
    pub field_tys: Box<[TypeId]>,
    pub field_ty_names: Box<[Box<str>]>,
}

impl ProgramDataVariantLayout {
    #[must_use]
    pub const fn new(
        name: Box<str>,
        tag: i64,
        field_tys: Box<[TypeId]>,
        field_ty_names: Box<[Box<str>]>,
    ) -> Self {
        Self {
            name,
            tag,
            field_tys,
            field_ty_names,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ProgramDataLayout {
    pub data: DataId,
    pub ty: TypeId,
    pub name: Box<str>,
    pub variant_count: u32,
    pub field_count: u32,
    pub variants: Box<[ProgramDataVariantLayout]>,
    pub repr_kind: Option<Box<str>>,
    pub layout_align: Option<u32>,
    pub layout_pack: Option<u32>,
    pub frozen: bool,
}

impl ProgramDataLayout {
    /// # Panics
    ///
    /// Panics if the variant count or total field count does not fit in `u32`.
    #[must_use]
    pub fn new(
        data: DataId,
        ty: TypeId,
        name: Box<str>,
        variants: Box<[ProgramDataVariantLayout]>,
    ) -> Self {
        let variant_count =
            u32::try_from(variants.len()).expect("program data variant count should fit in u32");
        let field_count = variants
            .iter()
            .map(|variant| variant.field_tys.len())
            .sum::<usize>();
        let field_count =
            u32::try_from(field_count).expect("program data field count should fit in u32");
        Self {
            data,
            ty,
            name,
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
    pub fn with_repr_kind(mut self, repr_kind: impl Into<Box<str>>) -> Self {
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

    #[must_use]
    pub const fn is_single_variant_product(&self) -> bool {
        self.variant_count == 1
    }

    #[must_use]
    pub fn is_repr_c(&self) -> bool {
        self.repr_kind.as_deref() == Some("c")
    }

    #[must_use]
    pub fn is_repr_transparent(&self) -> bool {
        self.repr_kind.as_deref() == Some("transparent")
    }

    #[must_use]
    pub fn is_repr_c_single_variant_product(&self) -> bool {
        self.is_repr_c() && self.variant_count == 1
    }

    #[must_use]
    pub fn is_repr_transparent_wrapper(&self) -> bool {
        self.is_repr_transparent()
            && self.variant_count == 1
            && self
                .single_variant()
                .is_some_and(|variant| variant.field_tys.len() == 1)
    }

    #[must_use]
    pub fn native_abi_kind(&self) -> ProgramTypeAbiKind {
        if self.is_repr_transparent_wrapper() {
            ProgramTypeAbiKind::DataTransparent
        } else if self.is_repr_c_single_variant_product() {
            ProgramTypeAbiKind::DataReprCProduct
        } else {
            ProgramTypeAbiKind::Unsupported
        }
    }

    #[must_use]
    pub fn single_variant(&self) -> Option<&ProgramDataVariantLayout> {
        (self.variants.len() == 1).then_some(&self.variants[0])
    }
}

pub(super) fn build_exports(artifact: &Artifact) -> (ExportMap, Box<[ProgramExport]>) {
    let export_list = artifact
        .exports
        .iter()
        .map(|(_, export)| {
            ProgramExport::new(
                source_export_name(artifact.string_text(export.name)).into(),
                export.opaque,
                export_kind(export.target),
            )
        })
        .collect::<Vec<_>>()
        .into_boxed_slice();
    let exports = artifact
        .exports
        .iter()
        .map(|(id, export)| {
            (
                source_export_name(artifact.string_text(export.name)).into(),
                id,
            )
        })
        .collect();
    (exports, export_list)
}

pub(super) fn build_data_layouts(artifact: &Artifact) -> (DataLayoutMap, Box<[ProgramDataLayout]>) {
    let mut layout_map = DataLayoutMap::new();
    let mut layout_list = Vec::new();
    for (ty, _) in artifact.types.iter() {
        let Some((data_id, descriptor)) = artifact.data_for_type(ty) else {
            continue;
        };
        let name = artifact.string_text(descriptor.name);
        let variants = descriptor
            .variants
            .iter()
            .map(|variant| {
                ProgramDataVariantLayout::new(
                    artifact.string_text(variant.name).into(),
                    variant.tag,
                    variant.field_tys.clone(),
                    variant
                        .field_tys
                        .iter()
                        .map(|field_ty| artifact.type_name(*field_ty).into())
                        .collect::<Vec<_>>()
                        .into_boxed_slice(),
                )
            })
            .collect::<Vec<_>>()
            .into_boxed_slice();
        let mut layout = ProgramDataLayout::new(data_id, ty, name.into(), variants);
        debug_assert_eq!(layout.variant_count, descriptor.variant_count);
        debug_assert_eq!(layout.field_count, descriptor.field_count);
        if let Some(repr_kind) = descriptor.repr_kind.map(|id| artifact.string_text(id)) {
            layout = layout.with_repr_kind(repr_kind);
        }
        if let Some(layout_align) = descriptor.layout_align {
            layout = layout.with_layout_align(layout_align);
        }
        if let Some(layout_pack) = descriptor.layout_pack {
            layout = layout.with_layout_pack(layout_pack);
        }
        if descriptor.frozen {
            layout = layout.with_frozen(true);
        }
        let _ = layout_map.insert(ty, layout.clone());
        layout_list.push(layout);
    }
    (layout_map, layout_list.into_boxed_slice())
}

const fn export_kind(target: ExportTarget) -> ProgramExportKind {
    match target {
        ExportTarget::Procedure(_) => ProgramExportKind::Procedure,
        ExportTarget::Global(_) => ProgramExportKind::Global,
        ExportTarget::Foreign(_) => ProgramExportKind::Foreign,
        ExportTarget::Type(_) => ProgramExportKind::Type,
        ExportTarget::Effect(_) => ProgramExportKind::Effect,
        ExportTarget::Shape(_) => ProgramExportKind::Shape,
    }
}

pub(super) fn source_export_name(name: &str) -> &str {
    name.rsplit_once("::").map_or(name, |(_, tail)| tail)
}
