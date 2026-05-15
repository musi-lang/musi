use crate::artifact::{BlockSignatureId, RootMapId, StringId, TypeId};
use crate::instruction::{CodeEntry, LabelId};
use std::str::FromStr;

pub type ProcedureTypeIdList = Box<[TypeId]>;
pub type ProcedureDomainList = Box<[StringId]>;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u8)]
pub enum ProcedureCallingConvention {
    Managed = 0,
    FfiWrapper = 1,
    RuntimeHelper = 2,
    LoaderGenerated = 3,
}

impl ProcedureCallingConvention {
    #[must_use]
    pub const fn as_str(self) -> &'static str {
        match self {
            Self::Managed => "managed",
            Self::FfiWrapper => "ffi-wrapper",
            Self::RuntimeHelper => "runtime-helper",
            Self::LoaderGenerated => "loader-generated",
        }
    }

    #[must_use]
    pub const fn from_wire(byte: u8) -> Option<Self> {
        match byte {
            0 => Some(Self::Managed),
            1 => Some(Self::FfiWrapper),
            2 => Some(Self::RuntimeHelper),
            3 => Some(Self::LoaderGenerated),
            _ => None,
        }
    }

    #[must_use]
    pub const fn wire_code(self) -> u8 {
        match self {
            Self::Managed => 0,
            Self::FfiWrapper => 1,
            Self::RuntimeHelper => 2,
            Self::LoaderGenerated => 3,
        }
    }
}

impl FromStr for ProcedureCallingConvention {
    type Err = ();

    fn from_str(text: &str) -> Result<Self, Self::Err> {
        match text {
            "managed" => Ok(Self::Managed),
            "ffi-wrapper" => Ok(Self::FfiWrapper),
            "runtime-helper" => Ok(Self::RuntimeHelper),
            "loader-generated" => Ok(Self::LoaderGenerated),
            _ => Err(()),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u8)]
pub enum ProcedureVisibility {
    Private = 0,
    ModuleExport = 1,
    ExternalExport = 2,
}

impl ProcedureVisibility {
    #[must_use]
    pub const fn as_str(self) -> &'static str {
        match self {
            Self::Private => "private",
            Self::ModuleExport => "module-export",
            Self::ExternalExport => "external-export",
        }
    }

    #[must_use]
    pub const fn from_wire(byte: u8) -> Option<Self> {
        match byte {
            0 => Some(Self::Private),
            1 => Some(Self::ModuleExport),
            2 => Some(Self::ExternalExport),
            _ => None,
        }
    }

    #[must_use]
    pub const fn wire_code(self) -> u8 {
        match self {
            Self::Private => 0,
            Self::ModuleExport => 1,
            Self::ExternalExport => 2,
        }
    }
}

impl FromStr for ProcedureVisibility {
    type Err = ();

    fn from_str(text: &str) -> Result<Self, Self::Err> {
        match text {
            "private" => Ok(Self::Private),
            "module-export" => Ok(Self::ModuleExport),
            "external-export" => Ok(Self::ExternalExport),
            _ => Err(()),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ProcedureDescriptor {
    pub name: StringId,
    pub params: u16,
    pub param_tys: ProcedureTypeIdList,
    pub locals: u16,
    pub local_tys: ProcedureTypeIdList,
    pub result_tys: ProcedureTypeIdList,
    pub entry_label: LabelId,
    pub bytecode_body: u32,
    pub block_signature_table: Option<BlockSignatureId>,
    pub root_map_table: Option<RootMapId>,
    pub domain_requirements: ProcedureDomainList,
    pub calling_convention: ProcedureCallingConvention,
    pub visibility: ProcedureVisibility,
    pub export: bool,
    pub hot: bool,
    pub cold: bool,
    pub labels: Box<[StringId]>,
    pub code: Box<[CodeEntry]>,
}

impl ProcedureDescriptor {
    #[must_use]
    pub fn new(name: StringId, params: u16, locals: u16, code: Box<[CodeEntry]>) -> Self {
        Self {
            name,
            params,
            param_tys: Box::new([]),
            locals,
            local_tys: Box::new([]),
            result_tys: Box::new([]),
            entry_label: 0,
            bytecode_body: 0,
            block_signature_table: None,
            root_map_table: None,
            domain_requirements: Box::new([]),
            calling_convention: ProcedureCallingConvention::Managed,
            visibility: ProcedureVisibility::Private,
            export: false,
            hot: false,
            cold: false,
            labels: Box::new([]),
            code,
        }
    }

    #[must_use]
    pub const fn with_export(mut self, export: bool) -> Self {
        self.export = export;
        self
    }

    #[must_use]
    pub const fn with_hot(mut self, hot: bool) -> Self {
        self.hot = hot;
        self
    }

    #[must_use]
    pub const fn with_cold(mut self, cold: bool) -> Self {
        self.cold = cold;
        self
    }

    #[must_use]
    pub fn with_labels(mut self, labels: Box<[StringId]>) -> Self {
        self.labels = labels;
        self
    }

    #[must_use]
    pub fn with_result_tys(mut self, result_tys: Box<[TypeId]>) -> Self {
        self.result_tys = result_tys;
        self
    }

    #[must_use]
    pub fn with_param_tys(mut self, param_tys: Box<[TypeId]>) -> Self {
        self.param_tys = param_tys;
        self
    }

    #[must_use]
    pub fn with_local_tys(mut self, local_tys: Box<[TypeId]>) -> Self {
        self.local_tys = local_tys;
        self
    }

    #[must_use]
    pub const fn with_entry_label(mut self, entry_label: LabelId) -> Self {
        self.entry_label = entry_label;
        self
    }

    #[must_use]
    pub const fn with_bytecode_body(mut self, bytecode_body: u32) -> Self {
        self.bytecode_body = bytecode_body;
        self
    }

    #[must_use]
    pub const fn with_block_signature_table(
        mut self,
        block_signature_table: BlockSignatureId,
    ) -> Self {
        self.block_signature_table = Some(block_signature_table);
        self
    }

    #[must_use]
    pub const fn with_root_map_table(mut self, root_map_table: RootMapId) -> Self {
        self.root_map_table = Some(root_map_table);
        self
    }

    #[must_use]
    pub fn with_domain_requirements(mut self, domain_requirements: Box<[StringId]>) -> Self {
        self.domain_requirements = domain_requirements;
        self
    }

    #[must_use]
    pub const fn with_calling_convention(
        mut self,
        calling_convention: ProcedureCallingConvention,
    ) -> Self {
        self.calling_convention = calling_convention;
        self
    }

    #[must_use]
    pub const fn with_visibility(mut self, visibility: ProcedureVisibility) -> Self {
        self.visibility = visibility;
        self
    }
}
