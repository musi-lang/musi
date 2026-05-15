use std::fmt::Display;
use std::sync::{Arc, Mutex};

use musi_foundation::syntax;
use musi_native::{NativeHost, WeakNativeHost};
use musi_vm::{
    ForeignCall, Value, ValueView, Vm, VmError, VmErrorKind, VmOptions, VmResult, VmValueKind,
};
use music_term::SyntaxTerm;

use super::compile::{SessionLoader, compile_synthetic_program_from_store};
use super::{Runtime, RuntimeStore, RuntimeStoreCell};
use crate::error::{RuntimeError, RuntimeErrorKind, RuntimeResult};
use crate::runtime::session_errors::vm_runtime_error;

impl Runtime {
    /// Compiles one expression-syntax string into one synthetic runtime module and returns its value.
    ///
    /// # Errors
    ///
    /// Returns [`crate::RuntimeError`] if syntax value is invalid, compilation fails, or runtime execution fails.
    pub fn eval_expr_syntax(&mut self, syntax: &Value, result_ty: &str) -> RuntimeResult<Value> {
        let body = self.syntax_term(syntax)?;
        let source = format!("export let result () : {result_ty} := {};", body.text());
        let program = self.compile_synthetic_program("main", &source)?;
        let loader = SessionLoader::new(Arc::clone(&self.store));
        let host = self.host.clone();
        let mut vm = Vm::new(program, loader, host, self.options.vm.clone());
        vm.initialize()?;
        Ok(vm.call_export("result", &[])?)
    }

    /// Compiles one module-syntax string under one explicit runtime spec and returns its module handle.
    ///
    /// # Errors
    ///
    /// Returns [`crate::RuntimeError`] if syntax value is invalid, compilation fails, or runtime loading fails.
    pub fn load_module_syntax(&mut self, spec: &str, syntax: &Value) -> RuntimeResult<Value> {
        let source = self.syntax_term(syntax)?.text().to_owned();
        let _ = self
            .store
            .lock()
            .map_err(|_| runtime_store_lock_error())?
            .module_texts
            .insert(spec.into(), source);
        let program = self.compile_registered_program(spec)?;
        let _ = self
            .store
            .lock()
            .map_err(|_| runtime_store_lock_error())?
            .programs
            .insert(spec.into(), program);
        Ok(self.vm_mut()?.load_module(spec)?)
    }

    fn syntax_term<'a>(&'a self, value: &'a Value) -> RuntimeResult<&'a SyntaxTerm> {
        let Some(vm) = self.vm.as_ref() else {
            return Err(RuntimeError::new(RuntimeErrorKind::RootModuleRequired));
        };
        match vm.inspect(value) {
            ValueView::Syntax(term) => Ok(term.term()),
            _ => Err(RuntimeError::new(RuntimeErrorKind::InvalidSyntaxValue {
                found: value.kind(),
            })),
        }
    }
}

pub(super) fn register_syntax_handlers(
    host: &mut NativeHost,
    store: Arc<Mutex<RuntimeStore>>,
    nested_host: &WeakNativeHost,
    vm_options: &VmOptions,
) {
    let eval_store = Arc::clone(&store);
    let eval_host = nested_host.clone();
    let eval_vm_options = vm_options.clone();
    host.register_foundation_handler_with_context(
        syntax::EFFECT,
        syntax::EVAL_OP,
        move |ctx, foreign, args| {
            let [body, Value::Type(result_ty)] = args else {
                return Err(invalid_syntax_foreign_args(
                    foreign,
                    "syntax body and result type",
                    args.len(),
                ));
            };
            let body = ctx
                .syntax(body)
                .ok_or_else(|| invalid_syntax_foreign_args(foreign, "syntax body", body.kind()))?;
            let result_ty_name = foreign.type_term(*result_ty).to_string();
            eval_syntax_value(
                &eval_store,
                &eval_host,
                &eval_vm_options,
                body.term().text(),
                result_ty_name.as_str(),
            )
        },
    );

    host.register_foundation_handler_with_context(
        syntax::EFFECT,
        syntax::REGISTER_MODULE_OP,
        move |ctx, foreign, args| {
            let [spec, body] = args else {
                return Err(invalid_syntax_foreign_args(
                    foreign,
                    "module spec and syntax body",
                    args.len(),
                ));
            };
            let spec = ctx.string(spec).ok_or_else(|| {
                invalid_syntax_foreign_args(foreign, "module spec string", spec.kind())
            })?;
            let body = ctx
                .syntax(body)
                .ok_or_else(|| invalid_syntax_foreign_args(foreign, "syntax body", body.kind()))?;
            let mut store = store
                .lock()
                .map_err(|_| runtime_host_unavailable(foreign, "runtime store lock"))?;
            let _ = store
                .module_texts
                .insert(spec.as_str().into(), body.term().text().into());
            store.programs.clear();
            drop(store);
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
    let source = format!("export let result () : {result_ty} := {body};");
    let program = compile_synthetic_program_from_store(store, "main", &source)
        .map_err(|error| vm_runtime_error(&error))?;
    let Some(host) = nested_host.upgrade() else {
        return Err(runtime_host_unavailable_name("runtime host"));
    };
    let loader = SessionLoader::new(Arc::clone(store));
    let mut vm = Vm::new(program, loader, host, vm_options.clone());
    vm.initialize()?;
    vm.call_export("result", &[])
}

fn syntax_foreign_rejected_reason(foreign: &ForeignCall, reason: impl AsRef<str>) -> VmError {
    VmError::new(VmErrorKind::ForeignCallRejected {
        foreign: format!("{} ({})", foreign.name(), reason.as_ref()).into_boxed_str(),
    })
}

fn invalid_syntax_foreign_args(
    foreign: &ForeignCall,
    expected: &str,
    found: impl Display,
) -> VmError {
    syntax_foreign_rejected_reason(foreign, format!("expected {expected}, found {found}"))
}

fn runtime_host_unavailable(foreign: &ForeignCall, subject: &str) -> VmError {
    syntax_foreign_rejected_reason(foreign, format!("host unavailable: {subject}"))
}

fn runtime_host_unavailable_name(subject: &str) -> VmError {
    VmError::new(VmErrorKind::ForeignCallRejected {
        foreign: format!("musi:syntax::Musi__eval (host unavailable: {subject})").into_boxed_str(),
    })
}

pub(super) const fn vm_syntax_value_kind_error(found: VmValueKind) -> VmError {
    VmError::new(VmErrorKind::InvalidValueKind {
        expected: VmValueKind::Syntax,
        found,
    })
}

fn runtime_store_lock_error() -> RuntimeError {
    RuntimeError::new(RuntimeErrorKind::VmExecutionFailed(VmError::new(
        VmErrorKind::InvalidProgramShape {
            detail: "runtime store lock unavailable".into(),
        },
    )))
}
