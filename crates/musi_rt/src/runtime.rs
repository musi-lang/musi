use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

use musi_foundation::{extend_import_map, register_modules, syntax};
use musi_native::{NativeHost, NativeTestReport, WeakNativeHost};
use musi_vm::{
    EffectCall, Program, Value, ValueView, Vm, VmError, VmErrorKind, VmLoader, VmOptions, VmResult,
    VmValueKind,
};
use music_module::ModuleKey;
use music_session::{Session, SessionError, SessionOptions};
use music_term::SyntaxTerm;

use crate::api::RuntimeOptions;
use crate::error::{RuntimeError, RuntimeErrorKind, RuntimeResult};
use crate::runtime_handlers::register_runtime_handlers;

type ModuleTextMap = HashMap<Box<str>, String>;
type ProgramMap = HashMap<Box<str>, Program>;
type RuntimeStoreCell = Rc<RefCell<RuntimeStore>>;
type RuntimeProgramResult = RuntimeResult<Program>;

#[derive(Default)]
struct RuntimeStore {
    module_texts: ModuleTextMap,
    programs: ProgramMap,
    session_options: SessionOptions,
}

#[derive(Clone)]
struct SessionLoader {
    store: RuntimeStoreCell,
}

pub struct Runtime {
    store: RuntimeStoreCell,
    host: NativeHost,
    vm: Option<Vm>,
    options: RuntimeOptions,
    root_spec: Option<Box<str>>,
}

impl Runtime {
    #[must_use]
    pub fn new(mut host: NativeHost, options: RuntimeOptions) -> Self {
        let mut session_options = options.session.clone();
        extend_import_map(&mut session_options.import_map);
        let store = Rc::new(RefCell::new(RuntimeStore {
            session_options,
            ..RuntimeStore::default()
        }));
        let nested_host = host.downgrade();
        register_syntax_handlers(&mut host, Rc::clone(&store), &nested_host, &options.vm);
        register_runtime_handlers(&mut host);
        Self {
            store,
            host,
            vm: None,
            options,
            root_spec: None,
        }
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
        let body = Self::syntax_term(syntax)?;
        let source = format!("export let answer () : {result_ty} := {};", body.text());
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
        let source = Self::syntax_term(syntax)?.text().to_owned();
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
    pub fn run_test_module(&mut self, spec: &str) -> RuntimeResult<NativeTestReport> {
        self.run_test_export(spec, "test")
    }

    /// Runs one registered test export and returns structured case results.
    ///
    /// # Errors
    ///
    /// Returns [`RuntimeError`] if compilation, intrinsic test-event collection, or test-body execution fails.
    pub fn run_test_export(
        &mut self,
        spec: &str,
        export_name: &str,
    ) -> RuntimeResult<NativeTestReport> {
        let program = self.compile_registered_program(spec)?;
        let loader = SessionLoader {
            store: Rc::clone(&self.store),
        };
        let host = self.host.clone();
        let mut vm = Vm::new(program, loader, host, self.options.vm.clone());
        vm.initialize()?;
        self.host.begin_test_session();
        let result = vm.call_export(export_name, &[]);
        let report = self.host.finish_test_session(spec);
        let _ = result?;
        Ok(report)
    }

    fn vm_mut(&mut self) -> RuntimeResult<&mut Vm> {
        self.vm
            .as_mut()
            .ok_or_else(|| RuntimeError::new(RuntimeErrorKind::RootModuleRequired))
    }

    fn compile_registered_program(&self, spec: &str) -> RuntimeProgramResult {
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
        register_modules(&mut session).map_err(runtime_session_error)?;
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

    fn compile_synthetic_program(&self, spec: &str, source: &str) -> RuntimeProgramResult {
        compile_synthetic_program_from_store(&self.store, spec, source)
    }

    fn syntax_term(value: &Value) -> RuntimeResult<&SyntaxTerm> {
        match value {
            Value::Syntax(term) => Ok(term.as_ref()),
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
        register_modules(&mut session).map_err(|err| vm_session_error(&err))?;
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

fn register_syntax_handlers(
    host: &mut NativeHost,
    store: Rc<RefCell<RuntimeStore>>,
    nested_host: &WeakNativeHost,
    vm_options: &VmOptions,
) {
    let eval_store = Rc::clone(&store);
    let eval_host = nested_host.clone();
    let eval_vm_options = vm_options.clone();
    host.register_effect_handler(syntax::EFFECT, syntax::EVAL_OP, move |effect, args| {
        let [Value::Syntax(body), Value::Type(result_ty)] = args else {
            return Err(invalid_syntax_effect(effect, "invalid syntax eval args"));
        };
        let result_ty_name = effect.type_term(*result_ty).to_string();
        eval_syntax_value(
            &eval_store,
            &eval_host,
            &eval_vm_options,
            body.text(),
            result_ty_name.as_str(),
        )
    });

    host.register_effect_handler(
        syntax::EFFECT,
        syntax::REGISTER_MODULE_OP,
        move |effect, args| {
            let [Value::String(spec), Value::Syntax(body)] = args else {
                return Err(invalid_syntax_effect(
                    effect,
                    "invalid syntax register args",
                ));
            };
            let mut store = store.borrow_mut();
            let _ = store
                .module_texts
                .insert(spec.as_ref().into(), body.text().into());
            store.programs.clear();
            Ok(Value::Unit)
        },
    );
}

fn eval_syntax_value(
    store: &RuntimeStoreCell,
    nested_host: &WeakNativeHost,
    vm_options: &VmOptions,
    body: &str,
    result_ty: &str,
) -> VmResult<Value> {
    let source = format!("export let answer () : {result_ty} := {body};");
    let program = compile_synthetic_program_from_store(store, "main", &source)
        .map_err(|error| vm_runtime_error(&error))?;
    let Some(host) = nested_host.upgrade() else {
        return Err(VmError::new(VmErrorKind::EffectRejected {
            effect: syntax::EFFECT.into(),
            op: Some(syntax::EVAL_OP.into()),
            reason: "runtime host unavailable".into(),
        }));
    };
    let loader = SessionLoader {
        store: Rc::clone(store),
    };
    let mut vm = Vm::new(program, loader, host, vm_options.clone());
    vm.initialize()?;
    vm.call_export("answer", &[])
}

fn compile_synthetic_program_from_store(
    store: &RuntimeStoreCell,
    spec: &str,
    source: &str,
) -> RuntimeProgramResult {
    let store = store.borrow();
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

fn invalid_syntax_effect(effect: &EffectCall, reason: &'static str) -> VmError {
    VmError::new(VmErrorKind::EffectRejected {
        effect: effect.effect_name().into(),
        op: Some(effect.op_name().into()),
        reason: reason.into(),
    })
}

fn vm_runtime_error(err: &RuntimeError) -> VmError {
    match err.kind() {
        RuntimeErrorKind::SessionSetupFailed { detail }
        | RuntimeErrorKind::SessionParseFailed { detail }
        | RuntimeErrorKind::SessionResolveFailed { detail }
        | RuntimeErrorKind::SessionSemanticCheckFailed { detail }
        | RuntimeErrorKind::SessionLoweringFailed { detail }
        | RuntimeErrorKind::SessionEmitFailed { detail } => {
            VmError::new(VmErrorKind::EffectRejected {
                effect: syntax::EFFECT.into(),
                op: Some(syntax::EVAL_OP.into()),
                reason: detail.clone(),
            })
        }
        RuntimeErrorKind::ModuleSourceMissing { spec } => {
            VmError::new(VmErrorKind::ModuleSourceMissing { spec: spec.clone() })
        }
        RuntimeErrorKind::InvalidSyntaxValue { found } => {
            VmError::new(VmErrorKind::InvalidValueKind {
                expected: VmValueKind::Syntax,
                found: *found,
            })
        }
        RuntimeErrorKind::RootModuleRequired => VmError::new(VmErrorKind::ProgramShapeInvalid {
            detail: "root module required".into(),
        }),
        RuntimeErrorKind::VmExecutionFailed(err) => err.clone(),
    }
}

fn session_error_detail(err: &SessionError) -> Box<str> {
    match err {
        SessionError::ModuleParseFailed { syntax, .. } => {
            first_diag_message_or_error(syntax.diags(), err, |diag| -> &str { diag.message() })
        }
        SessionError::ModuleResolveFailed { diags, .. }
        | SessionError::ModuleSemanticCheckFailed { diags, .. }
        | SessionError::ModuleLoweringFailed { diags, .. }
        | SessionError::ModuleEmissionFailed { diags, .. } => {
            first_diag_message_or_error(diags, err, |diag| -> &str { diag.message() })
        }
        _ => err.to_string().into(),
    }
}

fn first_diag_message_or_error<T>(
    diags: &[T],
    err: &SessionError,
    message: impl Fn(&T) -> &str,
) -> Box<str> {
    diags
        .first()
        .map_or_else(|| err.to_string().into(), |diag| message(diag).into())
}

fn vm_session_error(err: &SessionError) -> VmError {
    let stage = match &err {
        SessionError::ModuleParseFailed { .. } => "parse failed",
        SessionError::ModuleResolveFailed { .. } => "resolve failed",
        SessionError::ModuleSemanticCheckFailed { .. } => "semantic check failed",
        SessionError::ModuleLoweringFailed { .. } => "lowering failed",
        SessionError::ModuleEmissionFailed { .. } => "emission failed",
        _ => "session setup failed",
    };
    let detail = session_error_detail(err);
    VmError::new(VmErrorKind::ProgramShapeInvalid {
        detail: format!("{stage} (`{detail}`)").into(),
    })
}
