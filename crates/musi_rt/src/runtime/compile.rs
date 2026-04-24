use std::collections::BTreeMap;
use std::sync::MutexGuard;

use musi_foundation::register_modules;
use musi_vm::{Program, VmDiagKind, VmError, VmErrorKind, VmLoader, VmResult};
use music_base::diag::DiagContext;
use music_module::{ModuleKey, ModuleSpecifier};
use music_session::Session;

use super::{Runtime, RuntimeProgramResult, RuntimeStore, RuntimeStoreCell};
use crate::error::{RuntimeError, RuntimeErrorKind, RuntimeResult};
use crate::runtime::session_errors::{runtime_session_error, vm_session_error};

#[derive(Clone)]
pub(super) struct SessionLoader {
    store: RuntimeStoreCell,
}

impl SessionLoader {
    pub(super) const fn new(store: RuntimeStoreCell) -> Self {
        Self { store }
    }
}

impl Runtime {
    pub(super) fn compile_registered_program(&self, spec: &str) -> RuntimeProgramResult {
        let spec = self.resolve_registered_spec(spec);
        let cached_program = {
            let store = lock_runtime_store(&self.store)?;
            store.programs.get(spec.as_ref()).cloned()
        };
        if let Some(program) = cached_program {
            return Ok(program);
        }
        let Some(program) = self.compile_registered_program_uncached(spec.as_ref())? else {
            return Err(RuntimeError::new(RuntimeErrorKind::MissingModuleSource {
                spec,
            }));
        };
        let _ = lock_runtime_store(&self.store)?
            .programs
            .insert(spec, program.clone());
        Ok(program)
    }

    pub(super) fn compile_synthetic_program(
        &self,
        spec: &str,
        source: &str,
    ) -> RuntimeProgramResult {
        compile_synthetic_program_from_store(&self.store, spec, source)
    }

    fn resolve_registered_spec(&self, spec: &str) -> Box<str> {
        let Ok(store) = self.store.lock() else {
            return spec.into();
        };
        resolve_store_spec(&store, spec)
    }

    #[allow(clippy::significant_drop_tightening)]
    fn compile_registered_program_uncached(&self, spec: &str) -> RuntimeResult<Option<Program>> {
        let store = lock_runtime_store(&self.store)?;
        if !store.module_texts.contains_key(spec) {
            return Ok(None);
        }
        compile_registered_texts(spec, &store).map(Some)
    }
}

impl VmLoader for SessionLoader {
    fn load_program(&mut self, spec: &str) -> VmResult<Program> {
        let spec = {
            let store = lock_vm_store(&self.store)?;
            resolve_store_spec(&store, spec)
        };
        let cached_program = {
            let store = lock_vm_store(&self.store)?;
            store.programs.get(spec.as_ref()).cloned()
        };
        if let Some(program) = cached_program {
            return Ok(program);
        }

        let has_source = {
            let store = lock_vm_store(&self.store)?;
            store.module_texts.contains_key(spec.as_ref())
        };
        if !has_source {
            return Err(VmError::new(VmErrorKind::MissingModuleSource { spec }));
        }

        let program = {
            let store = lock_vm_store(&self.store)?;
            compile_registered_texts_for_vm(spec.as_ref(), &store)?
        };
        let _ = lock_vm_store(&self.store)?
            .programs
            .insert(spec, program.clone());
        Ok(program)
    }
}

pub(super) fn compile_synthetic_program_from_store(
    store: &RuntimeStoreCell,
    spec: &str,
    source: &str,
) -> RuntimeProgramResult {
    let store = lock_runtime_store(store)?;
    let session_options = store.session_options.clone();
    let module_texts = store.module_texts.clone();
    drop(store);

    let mut session = Session::new(session_options);
    register_modules(&mut session).map_err(runtime_session_error)?;
    for (module, text) in &module_texts {
        if module.as_ref() == spec {
            continue;
        }
        session
            .set_module_text(&ModuleKey::new(module.as_ref()), text.as_str())
            .map_err(runtime_session_error)?;
    }
    session
        .set_module_text(&ModuleKey::new(spec), source.to_owned())
        .map_err(runtime_session_error)?;
    let output = session
        .compile_module(&ModuleKey::new(spec))
        .map_err(runtime_session_error)?;
    Program::from_bytes(&output.bytes).map_err(Into::into)
}

fn lock_runtime_store(store: &RuntimeStoreCell) -> RuntimeResult<MutexGuard<'_, RuntimeStore>> {
    store.lock().map_err(|_| {
        RuntimeError::new(RuntimeErrorKind::VmExecutionFailed(
            runtime_store_lock_error(),
        ))
    })
}

fn lock_vm_store(store: &RuntimeStoreCell) -> VmResult<MutexGuard<'_, RuntimeStore>> {
    store.lock().map_err(|_| runtime_store_lock_error())
}

fn runtime_store_lock_error() -> VmError {
    VmError::new(VmErrorKind::InvalidProgramShape {
        detail: VmDiagKind::RuntimeHostUnavailable
            .message_with(&DiagContext::new().with("subject", "runtime store lock"))
            .into(),
    })
}

fn compile_registered_texts(spec: &str, store: &RuntimeStore) -> RuntimeProgramResult {
    let session_options = store.session_options.clone();
    let module_texts = store.module_texts.clone();
    let mut session = Session::new(session_options);
    register_modules(&mut session).map_err(runtime_session_error)?;
    for (module, text) in &module_texts {
        session
            .set_module_text(&ModuleKey::new(module.as_ref()), text.as_str())
            .map_err(runtime_session_error)?;
    }
    let output = session
        .compile_entry(&ModuleKey::new(spec))
        .map_err(runtime_session_error)?;
    Program::from_bytes(&output.bytes).map_err(Into::into)
}

fn compile_registered_texts_for_vm(spec: &str, store: &RuntimeStore) -> VmResult<Program> {
    let session_options = store.session_options.clone();
    let module_texts = store.module_texts.clone();
    let mut session = Session::new(session_options);
    register_modules(&mut session).map_err(|err| vm_session_error(&err))?;
    for (module, text) in &module_texts {
        session
            .set_module_text(&ModuleKey::new(module.as_ref()), text.as_str())
            .map_err(|err| vm_session_error(&err))?;
    }
    let output = session
        .compile_entry(&ModuleKey::new(spec))
        .map_err(|err| vm_session_error(&err))?;
    Program::from_bytes(&output.bytes)
}

fn resolve_store_spec(store: &RuntimeStore, spec: &str) -> Box<str> {
    if store.module_texts.contains_key(spec) || store.programs.contains_key(spec) {
        return spec.into();
    }
    let import_map = &store.session_options.import_map;
    if let Some(mapped) = import_map
        .resolve(&ModuleKey::new(spec), &ModuleSpecifier::new(spec))
        .filter(|mapped| {
            store.module_texts.contains_key(mapped.as_str())
                || store.programs.contains_key(mapped.as_str())
        })
    {
        return mapped.as_str().into();
    }
    for scope in import_map.scopes.values() {
        if let Some(mapped) = resolve_runtime_map_spec(scope, spec)
            && (store.module_texts.contains_key(mapped.as_ref())
                || store.programs.contains_key(mapped.as_ref()))
        {
            return mapped;
        }
    }
    spec.into()
}

fn resolve_runtime_map_spec(map: &BTreeMap<String, String>, spec: &str) -> Option<Box<str>> {
    if let Some(target) = map.get(spec) {
        return Some(target.as_str().into());
    }
    let mut best_key = None::<&str>;
    let mut best_len = 0usize;
    for key in map.keys().map(String::as_str) {
        if key.ends_with('/') && spec.starts_with(key) && key.len() >= best_len {
            best_key = Some(key);
            best_len = key.len();
        }
    }
    let prefix = best_key?;
    let target = map.get(prefix)?;
    let rest = spec.strip_prefix(prefix).unwrap_or("");
    Some(format!("{target}{rest}").into_boxed_str())
}
