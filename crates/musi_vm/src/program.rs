use std::collections::HashMap;
use std::sync::Arc;

use music_assembly::decode_binary;
use music_bc::descriptor::ExportTarget;
use music_bc::{Artifact, CodeEntry, ExportId, Instruction, LabelId, MethodId, StringId, TypeId};
use music_term::TypeTerm;

use super::opcode::classify_opcode;
use super::{VmError, VmErrorKind, VmResult};

type InstructionList = Box<[Instruction]>;
type LabelIndexMap = HashMap<LabelId, usize>;
type ExportMap = HashMap<Box<str>, ExportId>;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ProgramExportKind {
    Method,
    Global,
    Foreign,
    Type,
    Effect,
    Class,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ProgramExport {
    pub name: Box<str>,
    pub opaque: bool,
    pub kind: ProgramExportKind,
}

#[derive(Debug, Clone)]
pub struct Program {
    inner: Arc<ProgramInner>,
}

#[derive(Debug)]
pub struct ProgramInner {
    artifact: Artifact,
    methods: Box<[LoadedMethod]>,
    exports: ExportMap,
    export_list: Box<[ProgramExport]>,
    entry_method: Option<MethodId>,
    module_init_method: Option<MethodId>,
}

#[derive(Debug, Clone)]
pub struct LoadedMethod {
    pub name: Box<str>,
    pub params: u16,
    pub locals: u16,
    pub instructions: InstructionList,
    pub labels: LabelIndexMap,
}

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
        let methods = build_methods(&artifact)?;
        let (exports, export_list) = build_exports(&artifact);
        let entry_method = find_suffix_method(&artifact, "::__entry");
        let module_init_method = find_suffix_method(&artifact, "::__module_init");
        Ok(Self {
            inner: Arc::new(ProgramInner {
                artifact,
                methods,
                exports,
                export_list,
                entry_method,
                module_init_method,
            }),
        })
    }

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
    pub fn type_term(&self, id: TypeId) -> TypeTerm {
        TypeTerm::from_json(self.inner.artifact.type_term_json(id))
            .expect("artifact type descriptors must carry valid type terms")
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
    pub(crate) fn artifact(&self) -> &Artifact {
        &self.inner.artifact
    }

    pub(crate) fn loaded_method(&self, id: MethodId) -> VmResult<&LoadedMethod> {
        let len = self.inner.methods.len();
        self.inner
            .methods
            .get(usize::try_from(id.raw()).unwrap_or(usize::MAX))
            .ok_or_else(|| {
                VmError::new(VmErrorKind::MethodOutOfBounds {
                    method: id.raw(),
                    len,
                })
            })
    }

    #[must_use]
    pub(crate) fn export_target(&self, name: &str) -> Option<ExportTarget> {
        let export_id = self.inner.exports.get(name).copied()?;
        Some(self.inner.artifact.exports.get(export_id).target)
    }

    #[must_use]
    pub(crate) fn entry_method(&self) -> Option<MethodId> {
        self.inner.entry_method.or(self.inner.module_init_method)
    }
}

fn build_methods(artifact: &Artifact) -> VmResult<Box<[LoadedMethod]>> {
    artifact
        .methods
        .iter()
        .map(|(_, method)| {
            let method_name: Box<str> = artifact.string_text(method.name).into();
            let mut labels = LabelIndexMap::new();
            let mut instructions = Vec::new();
            for entry in &method.code {
                match entry {
                    CodeEntry::Label(label) => {
                        let _ = labels.insert(label.id, instructions.len());
                    }
                    CodeEntry::Instruction(instruction) => {
                        let _family = classify_opcode(instruction.opcode);
                        let _ = &method_name;
                        instructions.push(instruction.clone());
                    }
                }
            }
            Ok(LoadedMethod {
                name: method_name,
                params: method.params,
                locals: method.locals,
                instructions: instructions.into_boxed_slice(),
                labels,
            })
        })
        .collect::<VmResult<Vec<_>>>()
        .map(Vec::into_boxed_slice)
}

fn build_exports(artifact: &Artifact) -> (ExportMap, Box<[ProgramExport]>) {
    let export_list = artifact
        .exports
        .iter()
        .map(|(_, export)| ProgramExport {
            name: source_export_name(artifact.string_text(export.name)).into(),
            opaque: export.opaque,
            kind: export_kind(export.target),
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

const fn export_kind(target: ExportTarget) -> ProgramExportKind {
    match target {
        ExportTarget::Method(_) => ProgramExportKind::Method,
        ExportTarget::Global(_) => ProgramExportKind::Global,
        ExportTarget::Foreign(_) => ProgramExportKind::Foreign,
        ExportTarget::Type(_) => ProgramExportKind::Type,
        ExportTarget::Effect(_) => ProgramExportKind::Effect,
        ExportTarget::Class(_) => ProgramExportKind::Class,
    }
}

fn find_suffix_method(artifact: &Artifact, suffix: &str) -> Option<MethodId> {
    artifact.methods.iter().find_map(|(id, method)| {
        artifact
            .string_text(method.name)
            .ends_with(suffix)
            .then_some(id)
    })
}

fn source_export_name(name: &str) -> &str {
    name.rsplit_once("::").map_or(name, |(_, tail)| tail)
}
