use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

use musi_vm::{
    NativeHost, Program, Value, ValueView, Vm, VmError, VmErrorKind, VmHost, VmLoader, VmResult,
};
use music_module::ModuleKey;
use music_session::{Session, SessionError, SessionOptions};

use crate::api::RuntimeOptions;
use crate::error::{RuntimeError, RuntimeErrorKind, RuntimeResult};

type ModuleTextMap = HashMap<Box<str>, String>;
type ProgramMap = HashMap<Box<str>, Program>;
type SharedHostCell = Rc<RefCell<Box<dyn VmHost>>>;

#[derive(Default)]
struct RuntimeStore {
    module_texts: ModuleTextMap,
    programs: ProgramMap,
    session_options: SessionOptions,
}

#[derive(Clone)]
struct SharedHost {
    inner: SharedHostCell,
}

#[derive(Clone)]
struct SessionLoader {
    store: Rc<RefCell<RuntimeStore>>,
}

pub struct Runtime {
    store: Rc<RefCell<RuntimeStore>>,
    host: SharedHostCell,
    vm: Option<Vm>,
    options: RuntimeOptions,
    root_spec: Option<Box<str>>,
}

impl Runtime {
    #[must_use]
    pub fn new(host: impl VmHost + 'static, options: RuntimeOptions) -> Self {
        let store = Rc::new(RefCell::new(RuntimeStore {
            session_options: options.session.clone(),
            ..RuntimeStore::default()
        }));
        Self {
            store,
            host: Rc::new(RefCell::new(Box::new(host))),
            vm: None,
            options,
            root_spec: None,
        }
    }

    #[must_use]
    pub fn with_native_host(options: RuntimeOptions) -> Self {
        Self::new(NativeHost, options)
    }

    /// Registers source text for one runtime module spec.
    ///
    /// # Errors
    ///
    /// Returns [`RuntimeError`] if runtime state cannot accept updated module text.
    pub fn register_module_text(
        &mut self,
        spec: impl Into<Box<str>>,
        text: impl Into<String>,
    ) -> RuntimeResult {
        let spec = spec.into();
        let _ = self
            .store
            .borrow_mut()
            .module_texts
            .insert(spec, text.into());
        self.invalidate_loaded_state();
        Ok(())
    }

    /// Registers one precompiled runtime program for one runtime module spec.
    ///
    /// # Errors
    ///
    /// Returns [`RuntimeError`] if runtime state cannot accept updated program bytes.
    pub fn register_program(
        &mut self,
        spec: impl Into<Box<str>>,
        program: Program,
    ) -> RuntimeResult {
        let spec = spec.into();
        let _ = self.store.borrow_mut().programs.insert(spec, program);
        self.invalidate_loaded_state();
        Ok(())
    }

    /// Loads one root module into one fresh VM instance and initializes it.
    ///
    /// # Errors
    ///
    /// Returns [`RuntimeError`] if source or program lookup fails, compilation fails, or VM initialization fails.
    pub fn load_root(&mut self, spec: &str) -> RuntimeResult {
        let program = self.compile_program(spec)?;
        let loader = SessionLoader {
            store: Rc::clone(&self.store),
        };
        let host = SharedHost {
            inner: Rc::clone(&self.host),
        };
        let mut vm = Vm::new(program, loader, host, self.options.vm.clone());
        vm.initialize()?;
        self.root_spec = Some(spec.into());
        self.vm = Some(vm);
        Ok(())
    }

    /// Looks up one root export from loaded runtime state.
    ///
    /// # Errors
    ///
    /// Returns [`RuntimeError`] if root runtime state is missing or export lookup fails.
    pub fn lookup_export(&mut self, name: &str) -> RuntimeResult<Value> {
        Ok(self.vm_mut()?.lookup_export(name)?)
    }

    /// Calls one root export from loaded runtime state.
    ///
    /// # Errors
    ///
    /// Returns [`RuntimeError`] if root runtime state is missing or export call fails.
    pub fn call_export(&mut self, name: &str, args: &[Value]) -> RuntimeResult<Value> {
        Ok(self.vm_mut()?.call_export(name, args)?)
    }

    /// Loads one runtime module through registered source or program state.
    ///
    /// # Errors
    ///
    /// Returns [`RuntimeError`] if root runtime state is missing or module loading fails.
    pub fn load_module(&mut self, spec: &str) -> RuntimeResult<Value> {
        Ok(self.vm_mut()?.load_module(spec)?)
    }

    /// Calls one export from one loaded module handle.
    ///
    /// # Errors
    ///
    /// Returns [`RuntimeError`] if root runtime state is missing or module export call fails.
    pub fn call_module_export(
        &mut self,
        module: &Value,
        name: &str,
        args: &[Value],
    ) -> RuntimeResult<Value> {
        Ok(self.vm_mut()?.call_module_export(module, name, args)?)
    }

    /// Compiles one expression-syntax string into one synthetic runtime module and returns its value.
    ///
    /// # Errors
    ///
    /// Returns [`RuntimeError`] if syntax value is invalid, compilation fails, or runtime execution fails.
    pub fn eval_expr_syntax(&mut self, syntax: &Value, result_ty: &str) -> RuntimeResult<Value> {
        let body = Self::syntax_text(syntax)?;
        let source = format!("export let answer () : {result_ty} := {body};");
        let program = self.compile_synthetic_program("main", &source)?;
        let loader = SessionLoader {
            store: Rc::clone(&self.store),
        };
        let host = SharedHost {
            inner: Rc::clone(&self.host),
        };
        let mut vm = Vm::new(program, loader, host, self.options.vm.clone());
        vm.initialize()?;
        Ok(vm.call_export("answer", &[])?)
    }

    /// Compiles one module-syntax string under one explicit runtime spec and returns its module handle.
    ///
    /// # Errors
    ///
    /// Returns [`RuntimeError`] if syntax value is invalid, compilation fails, or runtime loading fails.
    pub fn load_module_syntax(&mut self, spec: &str, syntax: &Value) -> RuntimeResult<Value> {
        let source = Self::syntax_text(syntax)?.to_owned();
        let _ = self
            .store
            .borrow_mut()
            .module_texts
            .insert(spec.into(), source);
        let _ = self.store.borrow_mut().programs.remove(spec);
        Ok(self.vm_mut()?.load_module(spec)?)
    }

    /// Inspects one runtime value through one stable VM view.
    ///
    /// # Errors
    ///
    /// Returns [`RuntimeError`] if root runtime state is missing.
    pub fn inspect<'a>(&'a self, value: &'a Value) -> RuntimeResult<ValueView<'a>> {
        let vm = self
            .vm
            .as_ref()
            .ok_or_else(|| RuntimeError::new(RuntimeErrorKind::RootModuleRequired))?;
        Ok(vm.inspect(value))
    }

    #[must_use]
    pub fn root_spec(&self) -> Option<&str> {
        self.root_spec.as_deref()
    }

    fn vm_mut(&mut self) -> RuntimeResult<&mut Vm> {
        self.vm
            .as_mut()
            .ok_or_else(|| RuntimeError::new(RuntimeErrorKind::RootModuleRequired))
    }

    fn compile_program(&self, spec: &str) -> RuntimeResult<Program> {
        let mut loader = SessionLoader {
            store: Rc::clone(&self.store),
        };
        loader.load_program(spec).map_err(Into::into)
    }

    fn compile_synthetic_program(&self, spec: &str, source: &str) -> RuntimeResult<Program> {
        let store = self.store.borrow();
        let mut session = Session::new(store.session_options.clone());
        for (module, text) in &store.module_texts {
            if module.as_ref() == spec {
                continue;
            }
            session
                .set_module_text(&ModuleKey::new(module.as_ref()), text.clone())
                .map_err(RuntimeError::from)?;
        }
        session
            .set_module_text(&ModuleKey::new(spec), source.to_owned())
            .map_err(RuntimeError::from)?;
        let output = session
            .compile_module(&ModuleKey::new(spec))
            .map_err(RuntimeError::from)?;
        Program::from_bytes(&output.bytes).map_err(Into::into)
    }

    fn syntax_text(value: &Value) -> RuntimeResult<&str> {
        match value {
            Value::String(text) => Ok(text.as_ref()),
            _ => Err(RuntimeError::new(RuntimeErrorKind::InvalidSyntaxValue {
                found: value.kind(),
            })),
        }
    }
    fn invalidate_loaded_state(&mut self) {
        self.store.borrow_mut().programs.clear();
        self.vm = None;
        self.root_spec = None;
    }
}

impl SharedHost {
    fn with<R>(&self, f: impl FnOnce(&mut dyn VmHost) -> VmResult<R>) -> VmResult<R> {
        let mut host = self.inner.borrow_mut();
        f(host.as_mut())
    }
}

impl VmHost for SharedHost {
    fn call_foreign(&mut self, foreign: &musi_vm::ForeignCall, args: &[Value]) -> VmResult<Value> {
        self.with(|host| host.call_foreign(foreign, args))
    }

    fn handle_effect(&mut self, effect: &musi_vm::EffectCall, args: &[Value]) -> VmResult<Value> {
        self.with(|host| host.handle_effect(effect, args))
    }
}

impl VmLoader for SessionLoader {
    fn load_program(&mut self, spec: &str) -> VmResult<Program> {
        if let Some(program) = self.store.borrow().programs.get(spec).cloned() {
            return Ok(program);
        }

        if !self.store.borrow().module_texts.contains_key(spec) {
            return Err(VmError::new(VmErrorKind::ModuleSourceMissing {
                spec: spec.into(),
            }));
        }

        let store = self.store.borrow();
        let mut session = Session::new(store.session_options.clone());
        for (module, text) in &store.module_texts {
            session
                .set_module_text(&ModuleKey::new(module.as_ref()), text.clone())
                .map_err(|err| session_setup_error(&err))?;
        }
        let output = session
            .compile_entry(&ModuleKey::new(spec))
            .map_err(|err| session_compile_error(&err))?;
        drop(store);

        let program = Program::from_bytes(&output.bytes)?;
        let _ = self
            .store
            .borrow_mut()
            .programs
            .insert(spec.into(), program.clone());
        Ok(program)
    }
}

fn session_setup_error(err: &SessionError) -> VmError {
    VmError::new(VmErrorKind::InvalidProgram {
        detail: err.to_string().into(),
    })
}

fn session_compile_error(err: &SessionError) -> VmError {
    let detail: Box<str> = match err {
        SessionError::Parse { syntax, .. } => syntax
            .diags()
            .first()
            .map_or_else(|| err.to_string().into(), |diag| diag.message().into()),
        SessionError::Resolve { diags, .. }
        | SessionError::Sema { diags, .. }
        | SessionError::Ir { diags, .. }
        | SessionError::Emit { diags, .. } => diags
            .first()
            .map_or_else(|| err.to_string().into(), |diag| diag.message().into()),
        _ => err.to_string().into(),
    };
    VmError::new(VmErrorKind::InvalidProgram { detail })
}
