use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

use musi_native::NativeHost;
use musi_vm::{
    EffectCall, ForeignCall, Program, RejectingHost, Value, ValueView, Vm, VmError, VmErrorKind,
    VmHost, VmLoader, VmResult,
};
use music_module::{ImportMap, ModuleKey};
use music_session::{Session, SessionError, SessionOptions};

use crate::api::RuntimeOptions;
use crate::error::{RuntimeError, RuntimeErrorKind, RuntimeResult};
use crate::testing::{RuntimeTestReport, TestCollector, TestRuntimeHost};

type ModuleTextMap = HashMap<Box<str>, String>;
type ProgramMap = HashMap<Box<str>, Program>;

const TEST_INTRINSIC_SPEC: &str = "musi:test";
const TEST_INTRINSIC_MODULE: &str = r"
export let Test := effect {
  let suiteStart (name : String) : Unit;
  let suiteEnd () : Unit;
  let testCase (name : String, passed : Bool) : Unit;
};
";

#[derive(Default)]
struct RuntimeStore {
    module_texts: ModuleTextMap,
    programs: ProgramMap,
    session_options: SessionOptions,
}

#[derive(Clone)]
struct SessionLoader {
    store: Rc<RefCell<RuntimeStore>>,
}

pub struct Runtime {
    store: Rc<RefCell<RuntimeStore>>,
    host: NativeHost,
    vm: Option<Vm>,
    options: RuntimeOptions,
    root_spec: Option<Box<str>>,
}

impl Runtime {
    #[must_use]
    pub fn new(options: RuntimeOptions) -> Self {
        Self::with_native_host(NativeHost::with_fallback(RejectingHost), options)
    }

    #[must_use]
    pub fn with_host(host: impl VmHost + 'static, options: RuntimeOptions) -> Self {
        Self::with_native_host(NativeHost::with_fallback(host), options)
    }

    #[must_use]
    pub fn with_native_host(host: NativeHost, options: RuntimeOptions) -> Self {
        let mut session_options = options.session.clone();
        extend_import_map(&mut session_options.import_map);
        let store = Rc::new(RefCell::new(RuntimeStore {
            session_options,
            ..RuntimeStore::default()
        }));
        Self {
            store,
            host,
            vm: None,
            options,
            root_spec: None,
        }
    }

    pub fn register_foreign_handler(
        &mut self,
        name: impl Into<Box<str>>,
        handler: impl FnMut(&ForeignCall, &[Value]) -> VmResult<Value> + 'static,
    ) {
        self.host.register_foreign_handler(name, handler);
    }

    pub fn register_effect_handler(
        &mut self,
        effect: impl Into<Box<str>>,
        op: impl Into<Box<str>>,
        handler: impl FnMut(&EffectCall, &[Value]) -> VmResult<Value> + 'static,
    ) {
        self.host.register_effect_handler(effect, op, handler);
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
        let _ = self.store.borrow_mut().module_texts.insert(spec, text.into());
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
        let program = self.compile_registered_program(spec)?;
        let loader = SessionLoader {
            store: Rc::clone(&self.store),
        };
        let host = self.host.clone();
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

    /// Calls one runtime value from loaded root runtime state.
    ///
    /// # Errors
    ///
    /// Returns [`RuntimeError`] if root runtime state is missing or the value call fails.
    pub fn call_value(&mut self, value: Value, args: &[Value]) -> RuntimeResult<Value> {
        Ok(self.vm_mut()?.call_value(value, args)?)
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
        let host = self.host.clone();
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
        let program = self.compile_registered_program(spec)?;
        let _ = self
            .store
            .borrow_mut()
            .programs
            .insert(spec.into(), program);
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

    /// Runs one registered test module and returns structured case results.
    ///
    /// # Errors
    ///
    /// Returns [`RuntimeError`] if compilation, intrinsic test-event collection, or test-body execution fails.
    pub fn run_test_module(&mut self, spec: &str) -> RuntimeResult<RuntimeTestReport> {
        let program = self.compile_registered_program(spec)?;
        let loader = SessionLoader {
            store: Rc::clone(&self.store),
        };
        let collector = Rc::new(RefCell::new(TestCollector::default()));
        let host = TestRuntimeHost::new(self.host.clone(), Rc::clone(&collector));
        let mut vm = Vm::new(program, loader, host, self.options.vm.clone());
        vm.initialize()?;
        let _ = vm.call_export("test", &[])?;
        Ok(collector.borrow_mut().finish_report(spec))
    }

    fn vm_mut(&mut self) -> RuntimeResult<&mut Vm> {
        self.vm
            .as_mut()
            .ok_or_else(|| RuntimeError::new(RuntimeErrorKind::RootModuleRequired))
    }

    fn compile_registered_program(&self, spec: &str) -> RuntimeResult<Program> {
        if let Some(program) = self.store.borrow().programs.get(spec).cloned() {
            return Ok(program);
        }
        let Some(program) = self.compile_registered_program_uncached(spec)? else {
            return Err(RuntimeError::new(RuntimeErrorKind::ModuleSourceMissing {
                spec: spec.into(),
            }));
        };
        let _ = self
            .store
            .borrow_mut()
            .programs
            .insert(spec.into(), program.clone());
        Ok(program)
    }

    fn compile_registered_program_uncached(&self, spec: &str) -> RuntimeResult<Option<Program>> {
        let store = self.store.borrow();
        if !store.module_texts.contains_key(spec) {
            return Ok(None);
        }
        let session_options = store.session_options.clone();
        let module_texts = store.module_texts.clone();
        drop(store);

        let mut session = Session::new(session_options);
        register_intrinsic_modules(&mut session).map_err(runtime_session_error)?;
        for (module, text) in &module_texts {
            session
                .set_module_text(&ModuleKey::new(module.as_ref()), text.clone())
                .map_err(runtime_session_error)?;
        }
        let output = session
            .compile_entry(&ModuleKey::new(spec))
            .map_err(runtime_session_error)?;
        let program = Program::from_bytes(&output.bytes)?;
        Ok(Some(program))
    }

    fn compile_synthetic_program(&self, spec: &str, source: &str) -> RuntimeResult<Program> {
        let store = self.store.borrow();
        let session_options = store.session_options.clone();
        let module_texts = store.module_texts.clone();
        drop(store);

        let mut session = Session::new(session_options);
        register_intrinsic_modules(&mut session).map_err(runtime_session_error)?;
        for (module, text) in &module_texts {
            if module.as_ref() == spec {
                continue;
            }
            session
                .set_module_text(&ModuleKey::new(module.as_ref()), text.clone())
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
        register_intrinsic_modules(&mut session).map_err(|err| vm_session_error(&err))?;
        for (module, text) in &store.module_texts {
            session
                .set_module_text(&ModuleKey::new(module.as_ref()), text.clone())
                .map_err(|err| vm_session_error(&err))?;
        }
        let output = session
            .compile_entry(&ModuleKey::new(spec))
            .map_err(|err| vm_session_error(&err))?;
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

fn runtime_session_error(err: SessionError) -> RuntimeError {
    RuntimeError::from(err)
}

fn session_error_detail(err: &SessionError) -> Box<str> {
    match err {
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
    }
}

fn vm_session_error(err: &SessionError) -> VmError {
    let stage = match &err {
        SessionError::Parse { .. } => "parse failed",
        SessionError::Resolve { .. } => "resolve failed",
        SessionError::Sema { .. } => "sema failed",
        SessionError::Ir { .. } => "ir failed",
        SessionError::Emit { .. } => "emit failed",
        _ => "session setup failed",
    };
    let detail = session_error_detail(err);
    VmError::new(VmErrorKind::InvalidProgram {
        detail: format!("{stage} (`{detail}`)").into(),
    })
}

fn extend_import_map(import_map: &mut ImportMap) {
    let _ = import_map
        .imports
        .insert(TEST_INTRINSIC_SPEC.into(), TEST_INTRINSIC_SPEC.into());
}

fn register_intrinsic_modules(session: &mut Session) -> Result<(), SessionError> {
    session.set_module_text(
        &ModuleKey::new(TEST_INTRINSIC_SPEC),
        TEST_INTRINSIC_MODULE.to_owned(),
    )
}
