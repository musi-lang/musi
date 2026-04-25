use std::sync::Arc;

use music_seam::descriptor::ExportTarget;
use music_seam::{Artifact, EffectId, ForeignId, ProcedureId, ShapeId, StringId, TypeId};
use music_term::{TypeTerm, TypeTermKind};

use super::layout::{ProgramDataLayout, ProgramExport, source_export_name};
use super::model::{LoadedProcedure, Program};
use super::runtime::{RuntimeInstruction, RuntimeInstructionList};
use crate::{Value, VmError, VmErrorKind, VmIndexSpace, VmResult};

impl Program {
    #[must_use]
    pub fn string_text(&self, id: StringId) -> &str {
        self.inner.artifact.string_text(id)
    }

    #[must_use]
    pub fn type_name(&self, id: TypeId) -> &str {
        let descriptor = self.inner.artifact.types.get(id);
        self.string_text(descriptor.name)
    }

    #[must_use]
    pub fn procedure_name(&self, id: ProcedureId) -> &str {
        let descriptor = self.inner.artifact.procedures.get(id);
        self.string_text(descriptor.name)
    }

    #[must_use]
    pub fn procedure_source_name(&self, id: ProcedureId) -> &str {
        source_export_name(self.procedure_name(id))
    }

    #[must_use]
    pub fn foreign_name(&self, id: ForeignId) -> &str {
        let descriptor = self.inner.artifact.foreigns.get(id);
        self.string_text(descriptor.name)
    }

    #[must_use]
    pub fn foreign_source_name(&self, id: ForeignId) -> &str {
        source_export_name(self.foreign_name(id))
    }

    #[must_use]
    pub fn effect_name(&self, id: EffectId) -> &str {
        let descriptor = self.inner.artifact.effects.get(id);
        self.string_text(descriptor.name)
    }

    #[must_use]
    pub fn effect_source_name(&self, id: EffectId) -> &str {
        source_export_name(self.effect_name(id))
    }

    #[must_use]
    pub fn shape_name(&self, id: ShapeId) -> &str {
        let descriptor = self.inner.artifact.shapes.get(id);
        self.string_text(descriptor.name)
    }

    #[must_use]
    pub fn shape_source_name(&self, id: ShapeId) -> &str {
        source_export_name(self.shape_name(id))
    }
}

impl Program {
    /// # Errors
    ///
    /// Returns [`VmErrorKind::InvalidTypeTerm`] when the stored type-term JSON cannot be decoded.
    pub fn try_type_term(&self, id: TypeId) -> VmResult<TypeTerm> {
        TypeTerm::from_json(self.inner.artifact.type_term_json(id)).map_err(|detail| {
            VmError::new(VmErrorKind::InvalidTypeTerm {
                ty: self.type_name(id).into(),
                detail: detail.to_string().into(),
            })
        })
    }

    #[must_use]
    pub fn type_term(&self, id: TypeId) -> TypeTerm {
        self.try_type_term(id)
            .unwrap_or_else(|_| TypeTerm::new(TypeTermKind::Unknown))
    }

    #[must_use]
    pub fn export_count(&self) -> usize {
        self.inner.export_list.len()
    }

    #[must_use]
    pub fn exports(&self) -> &[ProgramExport] {
        &self.inner.export_list
    }

    #[must_use]
    pub fn data_layouts(&self) -> &[ProgramDataLayout] {
        &self.inner.data_layout_list
    }

    #[must_use]
    pub fn type_data_layout(&self, ty: TypeId) -> Option<&ProgramDataLayout> {
        self.inner.data_layouts.get(&ty)
    }
}

impl Program {
    #[must_use]
    pub(crate) fn artifact(&self) -> &Artifact {
        &self.inner.artifact
    }

    pub(crate) fn loaded_procedure(&self, id: ProcedureId) -> VmResult<&LoadedProcedure> {
        let len = self.inner.procedures.len();
        self.inner
            .procedures
            .get(usize::try_from(id.raw()).unwrap_or(usize::MAX))
            .ok_or_else(|| {
                VmError::new(VmErrorKind::IndexOutOfBounds {
                    space: VmIndexSpace::Procedure,
                    owner: None,
                    index: i64::from(id.raw()),
                    len,
                })
            })
    }

    pub(crate) fn loaded_runtime_code(&self, id: ProcedureId) -> VmResult<RuntimeInstructionList> {
        Ok(Arc::<[RuntimeInstruction]>::clone(
            &self.loaded_procedure(id)?.runtime_instructions,
        ))
    }

    #[must_use]
    pub(crate) fn export_target_with_opaque(&self, name: &str) -> Option<(ExportTarget, bool)> {
        let export_id = self.inner.exports.get(name).copied()?;
        let export = self.inner.artifact.exports.get(export_id);
        Some((export.target, export.opaque))
    }

    #[must_use]
    pub(crate) fn entry_procedure(&self) -> Option<ProcedureId> {
        self.inner
            .entry_procedure
            .or(self.inner.module_init_procedure)
    }

    #[must_use]
    pub(crate) fn global_init_image(&self) -> Option<&Arc<[Value]>> {
        self.inner.global_init_image.as_ref()
    }
}
