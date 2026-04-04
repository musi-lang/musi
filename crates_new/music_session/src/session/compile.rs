use music_assembly::{encode_binary, format_text};
use music_bc::Artifact;
use music_emit::lower_ir_program;
use music_module::ModuleKey;

use crate::api::{CompiledOutput, SessionError};

use super::Session;

impl Session {
    /// Compiles one module into the in-memory artifact plus binary and text encodings.
    ///
    /// # Errors
    ///
    /// Returns any parse, resolve, semantic, IR, emit, or assembly error for the target module.
    pub fn compile_module(&mut self, key: &ModuleKey) -> Result<CompiledOutput, SessionError> {
        let artifact = self.compile_module_artifact(key)?;
        Self::build_output(artifact)
    }

    /// Emits one module to a validated in-memory artifact.
    ///
    /// # Errors
    ///
    /// Returns any parse, resolve, semantic, IR, or emit error for the target module.
    pub fn compile_module_artifact(&mut self, key: &ModuleKey) -> Result<Artifact, SessionError> {
        Ok(self.emit_module(key)?.artifact.clone())
    }

    /// Compiles one module and returns its SEAM binary encoding.
    ///
    /// # Errors
    ///
    /// Returns any parse, resolve, semantic, IR, emit, or assembly error for the target module.
    pub fn compile_module_bytes(&mut self, key: &ModuleKey) -> Result<Vec<u8>, SessionError> {
        Ok(self.compile_module(key)?.bytes)
    }

    /// Compiles one module and returns its text assembly form.
    ///
    /// # Errors
    ///
    /// Returns any parse, resolve, semantic, IR, emit, or assembly error for the target module.
    pub fn compile_module_text(&mut self, key: &ModuleKey) -> Result<String, SessionError> {
        Ok(self.compile_module(key)?.text)
    }

    /// Compiles the reachable static-import graph rooted at `key`.
    ///
    /// # Errors
    ///
    /// Returns any parse, resolve, semantic, IR, emit, or assembly error from the reachable graph.
    pub fn compile_entry(&mut self, key: &ModuleKey) -> Result<CompiledOutput, SessionError> {
        let artifact = self.compile_entry_artifact(key)?;
        Self::build_output(artifact)
    }

    /// Emits the reachable static-import graph rooted at `key` to an in-memory artifact.
    ///
    /// # Errors
    ///
    /// Returns any parse, resolve, semantic, IR, or emit error from the reachable graph.
    ///
    /// # Panics
    ///
    /// Panics if the entry-program cache is missing immediately after successful cache construction.
    pub fn compile_entry_artifact(&mut self, key: &ModuleKey) -> Result<Artifact, SessionError> {
        if !self.graph.entry_programs.contains_key(key) {
            let modules = self.collect_reachable_ir_modules(key)?;
            let program = lower_ir_program(&modules, key, self.options.emit).map_err(|diags| {
                SessionError::Emit {
                    module: key.clone(),
                    diags: diags.into_boxed_slice(),
                }
            })?;
            let _ = self.graph.entry_programs.insert(key.clone(), program);
            self.stats.emit_runs = self.stats.emit_runs.saturating_add(1);
        }
        Ok(self
            .graph
            .entry_programs
            .get(key)
            .expect("entry program cache missing after construction")
            .artifact
            .clone())
    }

    /// Compiles the reachable static-import graph rooted at `key` and returns its SEAM bytes.
    ///
    /// # Errors
    ///
    /// Returns any parse, resolve, semantic, IR, emit, or assembly error from the reachable graph.
    pub fn compile_entry_bytes(&mut self, key: &ModuleKey) -> Result<Vec<u8>, SessionError> {
        Ok(self.compile_entry(key)?.bytes)
    }

    /// Compiles the reachable static-import graph rooted at `key` and returns its text assembly.
    ///
    /// # Errors
    ///
    /// Returns any parse, resolve, semantic, IR, emit, or assembly error from the reachable graph.
    pub fn compile_entry_text(&mut self, key: &ModuleKey) -> Result<String, SessionError> {
        Ok(self.compile_entry(key)?.text)
    }

    fn build_output(artifact: Artifact) -> Result<CompiledOutput, SessionError> {
        Ok(CompiledOutput {
            bytes: encode_binary(&artifact)?,
            text: format_text(&artifact),
            artifact,
        })
    }
}
