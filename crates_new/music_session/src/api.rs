use music_assembly::AssemblyError;
use music_base::{Diag, SourceId, SourceMapError};
use music_bc::Artifact;
use music_emit::EmitOptions;
use music_module::{ImportMap, ImportSite, ModuleExportSummary, ModuleKey};
use music_sema::TargetInfo;
use music_syntax::{LexError, ParseError};
use thiserror::Error;

pub type SessionDiagList = Box<[Diag]>;

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

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ParsedModule {
    pub module_key: ModuleKey,
    pub source_id: SourceId,
    pub import_sites: Box<[ImportSite]>,
    pub export_summary: ModuleExportSummary,
    pub lex_errors: Box<[LexError]>,
    pub parse_errors: Box<[ParseError]>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CompiledOutput {
    pub artifact: Artifact,
    pub bytes: Vec<u8>,
    pub text: String,
}

#[derive(Debug, Error)]
pub enum SessionError {
    #[error("unknown module `{key}`")]
    UnknownModule { key: ModuleKey },
    #[error("source map error")]
    SourceMap { kind: SessionSourceMapError },
    #[error("parse failed for module `{module}`")]
    Parse {
        module: ModuleKey,
        lex_errors: Box<[LexError]>,
        parse_errors: Box<[ParseError]>,
    },
    #[error("resolve failed for module `{module}`")]
    Resolve {
        module: ModuleKey,
        diags: SessionDiagList,
    },
    #[error("semantic analysis failed for module `{module}`")]
    Sema {
        module: ModuleKey,
        diags: SessionDiagList,
    },
    #[error("ir lowering failed for module `{module}`")]
    Ir {
        module: ModuleKey,
        diags: SessionDiagList,
    },
    #[error("emit failed for module `{module}`")]
    Emit {
        module: ModuleKey,
        diags: SessionDiagList,
    },
    #[error("assembly error")]
    Assembly(#[from] AssemblyError),
}

#[derive(Debug, Error, Clone, PartialEq, Eq)]
pub enum SessionSourceMapError {
    #[error("source map overflow")]
    Overflow,
    #[error("source text too large ({len} bytes)")]
    SourceTooLarge { len: usize },
}

impl From<SourceMapError> for SessionError {
    fn from(value: SourceMapError) -> Self {
        Self::SourceMap {
            kind: SessionSourceMapError::from(value),
        }
    }
}

impl From<SourceMapError> for SessionSourceMapError {
    fn from(value: SourceMapError) -> Self {
        match value {
            SourceMapError::Overflow => Self::Overflow,
            SourceMapError::SourceTooLarge { len } => Self::SourceTooLarge { len },
        }
    }
}
