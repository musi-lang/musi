use std::error::Error;
use std::fmt::{self, Display, Formatter};

use crate::SessionDiagKind;
use crate::diag::{session_error_kind, session_source_map_error_kind};
use music_base::diag::{CatalogDiagnostic, DiagContext};
use music_base::{Diag, SourceId, SourceMapError};
use music_emit::EmitOptions;
use music_module::{ImportMap, ImportSite, ModuleExportSummary, ModuleKey};
use music_seam::Artifact;
use music_seam::AssemblyError;
use music_sema::TargetInfo;
use music_syntax::{LexError, ParseError};

pub type SessionDiagList = Box<[Diag]>;

#[derive(Debug, Clone, Default)]
pub struct SessionSyntaxErrors {
    lex: Box<[LexError]>,
    parse: Box<[ParseError]>,
    diags: SessionDiagList,
}

impl SessionSyntaxErrors {
    #[must_use]
    pub fn new(
        lex: impl Into<Box<[LexError]>>,
        parse: impl Into<Box<[ParseError]>>,
        diags: impl Into<SessionDiagList>,
    ) -> Self {
        Self {
            lex: lex.into(),
            parse: parse.into(),
            diags: diags.into(),
        }
    }

    #[must_use]
    pub fn lex_errors(&self) -> &[LexError] {
        &self.lex
    }

    #[must_use]
    pub fn parse_errors(&self) -> &[ParseError] {
        &self.parse
    }

    #[must_use]
    pub fn diags(&self) -> &[Diag] {
        &self.diags
    }

    #[must_use]
    pub fn is_empty(&self) -> bool {
        self.lex.is_empty() && self.parse.is_empty()
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SessionOptions {
    pub emit: EmitOptions,
    pub import_map: ImportMap,
    pub target: Option<TargetInfo>,
    pub enable_ctfe: bool,
}

impl SessionOptions {
    #[must_use]
    pub fn new() -> Self {
        Self {
            emit: EmitOptions,
            import_map: ImportMap::default(),
            target: None,
            enable_ctfe: true,
        }
    }

    #[must_use]
    pub const fn with_emit(mut self, emit: EmitOptions) -> Self {
        self.emit = emit;
        self
    }

    #[must_use]
    pub fn with_import_map(mut self, import_map: ImportMap) -> Self {
        self.import_map = import_map;
        self
    }

    #[must_use]
    pub fn with_target(mut self, target: TargetInfo) -> Self {
        self.target = Some(target);
        self
    }

    #[must_use]
    pub const fn with_ctfe(mut self, enable_ctfe: bool) -> Self {
        self.enable_ctfe = enable_ctfe;
        self
    }
}

impl Default for SessionOptions {
    fn default() -> Self {
        Self::new()
    }
}

#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct SessionStats {
    pub parse_runs: u64,
    pub resolve_runs: u64,
    pub sema_runs: u64,
    pub ir_runs: u64,
    pub emit_runs: u64,
}

impl SessionStats {
    #[must_use]
    pub const fn new() -> Self {
        Self {
            parse_runs: 0,
            resolve_runs: 0,
            sema_runs: 0,
            ir_runs: 0,
            emit_runs: 0,
        }
    }
}

#[derive(Debug, Clone)]
pub struct ParsedModule {
    pub module_key: ModuleKey,
    pub source_id: SourceId,
    pub import_sites: Box<[ImportSite]>,
    pub export_summary: ModuleExportSummary,
    pub syntax: SessionSyntaxErrors,
}

impl ParsedModule {
    #[must_use]
    pub fn new(module_key: ModuleKey, source_id: SourceId) -> Self {
        Self {
            module_key,
            source_id,
            import_sites: Box::default(),
            export_summary: ModuleExportSummary::new(),
            syntax: SessionSyntaxErrors::default(),
        }
    }

    #[must_use]
    pub fn with_import_sites(mut self, import_sites: impl Into<Box<[ImportSite]>>) -> Self {
        self.import_sites = import_sites.into();
        self
    }

    #[must_use]
    pub fn with_export_summary(mut self, export_summary: ModuleExportSummary) -> Self {
        self.export_summary = export_summary;
        self
    }

    #[must_use]
    pub fn with_syntax(mut self, syntax: SessionSyntaxErrors) -> Self {
        self.syntax = syntax;
        self
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CompiledOutput {
    pub artifact: Artifact,
    pub bytes: Vec<u8>,
    pub text: String,
}

impl CompiledOutput {
    #[must_use]
    pub const fn new(artifact: Artifact, bytes: Vec<u8>, text: String) -> Self {
        Self {
            artifact,
            bytes,
            text,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct LawSuiteModule {
    pub source_module_key: ModuleKey,
    pub suite_module_key: ModuleKey,
    pub export_name: Box<str>,
    pub law_count: usize,
}

impl LawSuiteModule {
    #[must_use]
    pub fn new(
        source_module_key: ModuleKey,
        suite_module_key: ModuleKey,
        export_name: impl Into<Box<str>>,
        law_count: usize,
    ) -> Self {
        Self {
            source_module_key,
            suite_module_key,
            export_name: export_name.into(),
            law_count,
        }
    }
}

#[derive(Debug)]
pub enum SessionError {
    ModuleNotRegistered {
        key: ModuleKey,
    },
    SourceMapUpdateFailed {
        kind: SessionSourceMapError,
    },
    ModuleParseFailed {
        module: ModuleKey,
        syntax: SessionSyntaxErrors,
    },
    ModuleResolveFailed {
        module: ModuleKey,
        diags: SessionDiagList,
    },
    ModuleSemanticCheckFailed {
        module: ModuleKey,
        diags: SessionDiagList,
    },
    ModuleLoweringFailed {
        module: ModuleKey,
        diags: SessionDiagList,
    },
    ModuleEmissionFailed {
        module: ModuleKey,
        diags: SessionDiagList,
    },
    LawSuiteSynthesisFailed {
        module: ModuleKey,
        reason: Box<str>,
    },
    ArtifactTransportFailed(AssemblyError),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum SessionSourceMapError {
    SourceRegistryOverflow,
    SourceTooLarge { len: usize },
}

impl From<SourceMapError> for SessionError {
    fn from(value: SourceMapError) -> Self {
        Self::SourceMapUpdateFailed {
            kind: SessionSourceMapError::from(value),
        }
    }
}

impl From<SourceMapError> for SessionSourceMapError {
    fn from(value: SourceMapError) -> Self {
        match value {
            SourceMapError::Overflow => Self::SourceRegistryOverflow,
            SourceMapError::SourceTooLarge { len } => Self::SourceTooLarge { len },
        }
    }
}

impl SessionError {
    #[must_use]
    pub const fn diag_kind(&self) -> SessionDiagKind {
        session_error_kind(self)
    }

    #[must_use]
    pub fn diagnostic(&self) -> CatalogDiagnostic<SessionDiagKind> {
        CatalogDiagnostic::new(self.diag_kind(), self.diag_context())
    }

    fn diag_context(&self) -> DiagContext {
        match self {
            Self::ModuleNotRegistered { key } => DiagContext::new().with("key", key.as_str()),
            Self::SourceMapUpdateFailed { kind } => DiagContext::new().with("kind", kind),
            Self::ModuleParseFailed { module, .. }
            | Self::ModuleResolveFailed { module, .. }
            | Self::ModuleSemanticCheckFailed { module, .. }
            | Self::ModuleLoweringFailed { module, .. }
            | Self::ModuleEmissionFailed { module, .. } => {
                DiagContext::new().with("module", module.as_str())
            }
            Self::LawSuiteSynthesisFailed { module, reason } => DiagContext::new()
                .with("module", module.as_str())
                .with("reason", reason.as_ref()),
            Self::ArtifactTransportFailed(source) => DiagContext::new().with("source", source),
        }
    }
}

impl Display for SessionError {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        Display::fmt(&self.diagnostic(), f)
    }
}

impl Error for SessionError {
    fn source(&self) -> Option<&(dyn Error + 'static)> {
        match self {
            Self::ArtifactTransportFailed(source) => Some(source),
            _ => None,
        }
    }
}

impl From<AssemblyError> for SessionError {
    fn from(value: AssemblyError) -> Self {
        Self::ArtifactTransportFailed(value)
    }
}

impl SessionSourceMapError {
    #[must_use]
    pub const fn diag_kind(&self) -> SessionDiagKind {
        session_source_map_error_kind(self)
    }

    #[must_use]
    pub fn diagnostic(&self) -> CatalogDiagnostic<SessionDiagKind> {
        let context = match self {
            Self::SourceRegistryOverflow => DiagContext::new(),
            Self::SourceTooLarge { len } => DiagContext::new().with("len", *len),
        };
        CatalogDiagnostic::new(self.diag_kind(), context)
    }
}

impl Display for SessionSourceMapError {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        Display::fmt(&self.diagnostic(), f)
    }
}

impl Error for SessionSourceMapError {}
