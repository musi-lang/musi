use std::cell::RefCell;
use std::rc::Rc;

use musi_foundation::syntax;
use musi_native::{NativeHost, WeakNativeHost};
use musi_vm::{EffectCall, Value, Vm, VmError, VmErrorKind, VmOptions, VmResult, VmValueKind};
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
        let body = Self::syntax_term(syntax)?;
        let source = format!("export let answer () : {result_ty} := {};", body.text());
        let program = self.compile_synthetic_program("main", &source)?;
        let loader = SessionLoader::new(Rc::clone(&self.store));
        let host = self.host.clone();
        let mut vm = Vm::new(program, loader, host, self.options.vm.clone());
        vm.initialize()?;
        Ok(vm.call_export("answer", &[])?)
    }

    /// Compiles one module-syntax string under one explicit runtime spec and returns its module handle.
    ///
    /// # Errors
    ///
    /// Returns [`crate::RuntimeError`] if syntax value is invalid, compilation fails, or runtime loading fails.
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

    fn syntax_term(value: &Value) -> RuntimeResult<&SyntaxTerm> {
        match value {
            Value::Syntax(term) => Ok(term.as_ref()),
            _ => Err(RuntimeError::new(RuntimeErrorKind::InvalidSyntaxValue {
                found: value.kind(),
            })),
        }
    }
}

pub(super) fn register_syntax_handlers(
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
    let loader = SessionLoader::new(Rc::clone(store));
    let mut vm = Vm::new(program, loader, host, vm_options.clone());
    vm.initialize()?;
    vm.call_export("answer", &[])
}

fn invalid_syntax_effect(effect: &EffectCall, reason: &'static str) -> VmError {
    VmError::new(VmErrorKind::EffectRejected {
        effect: effect.effect_name().into(),
        op: Some(effect.op_name().into()),
        reason: reason.into(),
    })
}

pub(super) const fn vm_syntax_value_kind_error(found: VmValueKind) -> VmError {
    VmError::new(VmErrorKind::InvalidValueKind {
        expected: VmValueKind::Syntax,
        found,
    })
}
