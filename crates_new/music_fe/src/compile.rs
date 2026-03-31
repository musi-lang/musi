use music_basic::{SourceId, path};
use music_codegen::{EmitModule, EmitProgram, emit_program};
use music_il::SeamArtifact;
use music_session::Session;

use crate::errors::{FrontendError, FrontendErrorKind, FrontendResult};
use crate::graph::build_module_graph;

pub struct CompiledProgram {
    pub artifact: SeamArtifact,
}

/// Compiles module graph rooted at `entry` into a SEAM artifact.
///
/// # Errors
/// Returns `Err` when parse, analysis, or emission fails.
pub fn compile_entry(session: &mut Session, entry: SourceId) -> FrontendResult<CompiledProgram> {
    let graph = build_module_graph(session.sources(), entry)?;

    let entry_path = session
        .sources()
        .get(entry)
        .ok_or(FrontendError {
            kind: FrontendErrorKind::EntrySourceMissing,
        })?
        .path();
    let entry_path = path::normalize_path(entry_path)
        .to_string_lossy()
        .into_owned();

    let mut analyzed_modules = Vec::with_capacity(graph.nodes_in_order.len());
    for node in &graph.nodes_in_order {
        let parsed = session.parse(node.source_id).ok_or(FrontendError {
            kind: FrontendErrorKind::ParseFailed,
        })?;
        if !parsed.lex_errors().is_empty() || !parsed.parse_errors().is_empty() {
            return Err(FrontendError {
                kind: FrontendErrorKind::ParseFailed,
            });
        }

        let analyzed = session
            .analyze_module(node.source_id)
            .ok_or(FrontendError {
                kind: FrontendErrorKind::AnalyzeFailed,
            })?;
        if !analyzed.resolve_errors.is_empty() || !analyzed.check_errors.is_empty() {
            return Err(FrontendError {
                kind: FrontendErrorKind::AnalyzeFailed,
            });
        }

        analyzed_modules.push((node.path.clone(), analyzed));
    }

    let modules_in_order = analyzed_modules
        .iter()
        .map(|(path, analyzed)| EmitModule {
            path: path.as_str(),
            analyzed,
        })
        .collect::<Vec<_>>()
        .into_boxed_slice();

    let program = EmitProgram {
        interner: session.interner(),
        sources: session.sources(),
        known: session.known(),
        modules_in_order,
        entry_path: entry_path.as_str(),
    };

    let artifact = emit_program(&program)
        .map_err(|_| FrontendError {
            kind: FrontendErrorKind::EmitFailed,
        })?
        .artifact;

    Ok(CompiledProgram { artifact })
}

/// Compiles `entry` into SEAM binary.
///
/// # Errors
/// Returns `Err` when compilation or encoding fails.
pub fn compile_entry_binary(session: &mut Session, entry: SourceId) -> FrontendResult<Vec<u8>> {
    let program = compile_entry(session, entry)?;
    music_assembly::encode_binary(&program.artifact).map_err(|_| FrontendError {
        kind: FrontendErrorKind::EncodeFailed,
    })
}
