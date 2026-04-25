use musi_foundation::{extend_import_map, register_modules};
use music_module::ModuleKey;
use music_seam::Artifact;
use music_session::{CompiledOutput, Session, SessionError, SessionOptions};

use crate::ProjectResult;
use crate::project::model::{CompiledOutputResult, Project};

impl Project {
    /// # Errors
    ///
    /// Returns [`ProjectError`] when the project modules cannot be registered into a configured
    /// [`Session`].
    pub fn build_session(&self) -> ProjectResult<Session> {
        let mut import_map = self.import_map.clone();
        extend_import_map(&mut import_map);
        let mut session_options = SessionOptions::new()
            .with_emit(self.options.emit)
            .with_import_map(import_map);
        if let Some(target) = self.options.target.clone() {
            session_options = session_options.with_target(target);
        }
        let mut session = Session::new(session_options);
        register_modules(&mut session)?;
        for (key, text) in &self.module_texts {
            session.set_module_text(key, text.clone())?;
        }
        Ok(session)
    }

    fn with_entry_session<T, F>(&self, entry: &ModuleKey, compile: F) -> ProjectResult<T>
    where
        F: FnOnce(&mut Session, &ModuleKey) -> ProjectResult<T>,
    {
        let mut session = self.build_session()?;
        compile(&mut session, entry)
    }

    fn with_root_entry_session<T, F>(&self, compile: F) -> ProjectResult<T>
    where
        F: FnOnce(&mut Session, &ModuleKey) -> ProjectResult<T>,
    {
        let entry = self.root_entry()?;
        self.with_entry_session(&entry.module_key, compile)
    }

    fn compile_root_entry_with<T, F>(&self, compile: F) -> ProjectResult<T>
    where
        F: FnOnce(&mut Session, &ModuleKey) -> Result<T, SessionError>,
    {
        self.with_root_entry_session(|session, entry| Ok(compile(session, entry)?))
    }

    /// # Errors
    ///
    /// Returns [`ProjectError`] when the root package entry cannot be compiled through
    /// [`Session`].
    pub fn compile_root_entry(&self) -> CompiledOutputResult {
        self.compile_root_entry_with(Session::compile_entry)
    }

    /// # Errors
    ///
    /// Returns [`ProjectError`] when the root package entry cannot be emitted to an artifact.
    pub fn compile_root_entry_artifact(&self) -> ProjectResult<Artifact> {
        self.compile_root_entry_with(Session::compile_entry_artifact)
    }

    /// # Errors
    ///
    /// Returns [`ProjectError`] when the root package entry cannot be compiled to bytes.
    pub fn compile_root_entry_bytes(&self) -> ProjectResult<Vec<u8>> {
        self.compile_root_entry_with(Session::compile_entry_bytes)
    }

    /// # Errors
    ///
    /// Returns [`ProjectError`] when the root package entry cannot be compiled to text.
    pub fn compile_root_entry_text(&self) -> ProjectResult<String> {
        self.compile_root_entry_with(Session::compile_entry_text)
    }

    /// # Errors
    ///
    /// Returns [`ProjectError`] when the named package entry cannot be compiled.
    pub fn compile_package_entry(&self, name: &str) -> ProjectResult<CompiledOutput> {
        let entry = self.package_entry(name)?;
        self.with_entry_session(&entry.module_key, |session, module_key| {
            Ok(session.compile_entry(module_key)?)
        })
    }

    #[cfg(test)]
    pub(crate) fn compile_module(&self, module_key: &ModuleKey) -> ProjectResult<CompiledOutput> {
        self.with_entry_session(module_key, |session, entry| {
            Ok(session.compile_entry(entry)?)
        })
    }
}
