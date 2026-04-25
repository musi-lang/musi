use std::sync::Arc;

use music_seam::{Artifact, ProcedureId, decode_binary};

use super::decode::build_procedures;
use super::layout::{build_data_layouts, build_exports};
use super::model::{Program, ProgramInner};
use crate::VmResult;
use crate::program_init::{build_global_init_image, specialize_runtime_kernels};

impl Program {
    /// Loads validated SEAM bytes into an opaque runtime program.
    ///
    /// # Errors
    ///
    /// Returns [`VmError`] if decoding, validation, or runtime indexing fails.
    pub fn from_bytes(bytes: &[u8]) -> VmResult<Self> {
        let artifact = decode_binary(bytes)?;
        Self::from_artifact(artifact)
    }

    pub(crate) fn from_artifact(artifact: Artifact) -> VmResult<Self> {
        let mut procedures = build_procedures(&artifact)?;
        specialize_runtime_kernels(&mut procedures);
        let (exports, export_list) = build_exports(&artifact);
        let (data_layouts, data_layout_list) = build_data_layouts(&artifact);
        let entry_procedure = find_suffix_procedure(&artifact, "::__entry");
        let module_init_procedure = find_suffix_procedure(&artifact, "::__module_init");
        let global_init_image = build_global_init_image(
            &procedures,
            artifact.globals.len(),
            entry_procedure.or(module_init_procedure),
        )
        .map(Arc::from);
        Ok(Self {
            inner: Arc::new(ProgramInner {
                artifact,
                procedures,
                exports,
                export_list,
                data_layouts,
                data_layout_list,
                entry_procedure,
                module_init_procedure,
                global_init_image,
            }),
        })
    }
}

fn find_suffix_procedure(artifact: &Artifact, suffix: &str) -> Option<ProcedureId> {
    artifact.procedures.iter().find_map(|(id, procedure)| {
        artifact
            .string_text(procedure.name)
            .ends_with(suffix)
            .then_some(id)
    })
}
