use music_base::{Diag, SourceId, SourceMapError};
use music_emit::EmitOptions;
use music_module::{ImportMap, ImportSite, ModuleExportSummary, ModuleKey};
use music_seam::Artifact;
use music_seam::AssemblyError;
use music_sema::TargetInfo;
use music_syntax::{LexError, ParseError};
use thiserror::Error;

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

#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct SessionOptions {
    pub emit: EmitOptions,
    pub import_map: ImportMap,
    pub target: Option<TargetInfo>,
}

#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct SessionStats {
    pub parse_runs: u64,
    pub resolve_runs: u64,
    pub sema_runs: u64,
    pub ir_runs: u64,
    pub emit_runs: u64,
}

#[derive(Debug, Clone)]
pub struct ParsedModule {
    pub module_key: ModuleKey,
    pub source_id: SourceId,
    pub import_sites: Box<[ImportSite]>,
    pub export_summary: ModuleExportSummary,
    pub syntax: SessionSyntaxErrors,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CompiledOutput {
    pub artifact: Artifact,
    pub bytes: Vec<u8>,
    pub text: String,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct LawSuiteModule {
    pub source_module_key: ModuleKey,
    pub suite_module_key: ModuleKey,
    pub export_name: Box<str>,
    pub law_count: usize,
}

#[derive(Debug, Error)]
pub enum SessionError {
    #[error("module `{key}` not registered")]
    ModuleNotRegistered { key: ModuleKey },
    #[error("source map update failed")]
    SourceMapUpdateFailed { kind: SessionSourceMapError },
    #[error("module `{module}` parse failed")]
    ModuleParseFailed {
        module: ModuleKey,
        syntax: SessionSyntaxErrors,
    },
    #[error("module `{module}` resolve failed")]
    ModuleResolveFailed {
        module: ModuleKey,
        diags: SessionDiagList,
    },
    #[error("module `{module}` semantic check failed")]
    ModuleSemanticCheckFailed {
        module: ModuleKey,
        diags: SessionDiagList,
    },
    #[error("module `{module}` lowering failed")]
    ModuleLoweringFailed {
        module: ModuleKey,
        diags: SessionDiagList,
    },
    #[error("module `{module}` emission failed")]
    ModuleEmissionFailed {
        module: ModuleKey,
        diags: SessionDiagList,
    },
    #[error("module `{module}` law-suite synthesis failed")]
    LawSuiteSynthesisFailed { module: ModuleKey, reason: Box<str> },
    #[error("artifact transport failed")]
    ArtifactTransportFailed(#[from] AssemblyError),
}

#[derive(Debug, Error, Clone, PartialEq, Eq)]
pub enum SessionSourceMapError {
    #[error("source registry overflow")]
    SourceRegistryOverflow,
    #[error("source text too large ({len} bytes)")]
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
