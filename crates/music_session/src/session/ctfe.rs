use std::sync::{Arc, Mutex};

use musi_vm::{
    EffectCall, ForeignCall, Program, RejectingHost, RejectingLoader, Value, ValueView, Vm,
    VmError, VmErrorKind, VmHost, VmHostCallContext, VmOptions, VmResult,
};
use music_base::{Diag, SourceId};
use music_hir::{HirExprId, HirExprKind, HirPrefixOp};
use music_module::ModuleKey;
use music_names::{NameBindingId, NameSite};
use music_sema::{
    ComptimeClosureValue, ComptimeDataValue, ComptimeEffectValue, ComptimeForeignValue,
    ComptimeImportRecordValue, ComptimeSeqValue, ComptimeShapeValue, ComptimeTypeValue,
    ComptimeValue, SemaModule,
};
use music_term::TypeTerm;

use crate::api::SessionError;

use super::Session;

const CTFE_EXPORT_PREFIX: &str = "musiCtfe";

struct CtfeJob {
    expr: HirExprId,
    source: String,
    prefix_end: usize,
    export: String,
}

impl Session {
    pub(super) fn evaluate_module_comptime(
        &self,
        key: &ModuleKey,
        sema: &mut SemaModule,
    ) -> Result<(), SessionError> {
        let jobs = self.collect_ctfe_jobs(key, sema)?;
        if jobs.is_empty() {
            return Ok(());
        }
        for job in jobs {
            let source = self.ctfe_source(key, &job)?;
            let mut session = self.ctfe_session(key, &source)?;
            let output = session.compile_entry(key)?;
            let program = Program::from_bytes(&output.bytes).map_err(|error| {
                SessionError::ModuleLoweringFailed {
                    module: key.clone(),
                    diags: Box::new([ctfe_diag(format!("program load failed `{error}`"))]),
                }
            })?;
            let host = SafeCtfeHost::new(self.ctfe_host.clone());
            let mut vm = Vm::new(program, RejectingLoader, host, VmOptions);
            vm.initialize()
                .map_err(|error| SessionError::ModuleLoweringFailed {
                    module: key.clone(),
                    diags: Box::new([ctfe_diag(format!("initialization failed `{error}`"))]),
                })?;
            let value = vm.call_export(&job.export, &[]).map_err(|error| {
                SessionError::ModuleLoweringFailed {
                    module: key.clone(),
                    diags: Box::new([ctfe_diag(format!(
                        "`{}` evaluation failed `{error}`",
                        job.export
                    ))]),
                }
            })?;
            let comptime = value_to_comptime(key, &vm, &value).map_err(|detail| {
                SessionError::ModuleLoweringFailed {
                    module: key.clone(),
                    diags: Box::new([ctfe_diag(format!(
                        "`{}` produced unsupported compile-time value `{detail}`",
                        job.export
                    ))]),
                }
            })?;
            sema.set_expr_comptime_value(job.expr, comptime);
        }
        Ok(())
    }

    fn collect_ctfe_jobs(
        &self,
        key: &ModuleKey,
        sema: &SemaModule,
    ) -> Result<Vec<CtfeJob>, SessionError> {
        let (_, source) = self.expanded_source_text(key)?;
        let mut jobs = Vec::new();
        for (expr_id, expr) in &sema.module().store.exprs {
            if let HirExprKind::Prefix {
                op: HirPrefixOp::Comptime,
                expr: inner,
            } = &expr.kind
            {
                Self::push_ctfe_job(sema, &source, expr_id, *inner, &mut jobs);
            }
            if let HirExprKind::Call { callee, args } = &expr.kind {
                let Some(binding) = direct_callee_binding(sema, *callee) else {
                    continue;
                };
                let Some(scheme) = sema.binding_scheme(binding) else {
                    continue;
                };
                for (index, arg) in sema
                    .module()
                    .store
                    .args
                    .get(args.clone())
                    .iter()
                    .enumerate()
                {
                    if scheme.comptime_params.get(index).copied().unwrap_or(false) {
                        Self::push_ctfe_job(sema, &source, arg.expr, arg.expr, &mut jobs);
                    }
                }
            }
        }
        Ok(jobs)
    }

    fn push_ctfe_job(
        sema: &SemaModule,
        source: &str,
        target: HirExprId,
        source_expr: HirExprId,
        jobs: &mut Vec<CtfeJob>,
    ) {
        if sema.expr_comptime_value(target).is_some() || jobs.iter().any(|job| job.expr == target) {
            return;
        }
        let span = sema.module().store.exprs.get(source_expr).origin.span;
        let start = usize::try_from(span.start).unwrap_or(usize::MAX);
        let end = usize::try_from(span.end).unwrap_or(usize::MAX);
        let Some(expr_source) = source
            .get(start..end)
            .map(str::trim)
            .filter(|text| !text.is_empty())
        else {
            return;
        };
        jobs.push(CtfeJob {
            expr: target,
            source: expr_source.to_owned(),
            prefix_end: source
                .get(..start)
                .and_then(|prefix| prefix.rfind('\n').map(|index| index + 1))
                .unwrap_or(0),
            export: format!("{CTFE_EXPORT_PREFIX}{}", jobs.len()),
        });
    }

    fn expanded_source_text(&self, key: &ModuleKey) -> Result<(SourceId, String), SessionError> {
        let record = self.module_record(key)?;
        let source_id = record
            .expanded_source_id
            .or(record.source_id)
            .ok_or_else(|| SessionError::ModuleNotRegistered { key: key.clone() })?;
        let text = record
            .expanded_text
            .as_ref()
            .unwrap_or(&record.text)
            .clone();
        Ok((source_id, text))
    }

    fn ctfe_source(&self, key: &ModuleKey, job: &CtfeJob) -> Result<String, SessionError> {
        let (_, base) = self.expanded_source_text(key)?;
        let mut source = base.get(..job.prefix_end).unwrap_or_default().to_owned();
        source.push_str("\nexport let ");
        source.push_str(&job.export);
        source.push_str(" () := ");
        source.push_str(&job.source);
        source.push_str(";\n");
        Ok(source)
    }

    fn ctfe_session(&self, key: &ModuleKey, source: &str) -> Result<Self, SessionError> {
        let mut options = self.options.clone().with_ctfe(false);
        options.emit = self.options.emit;
        let mut session = Self::new(options);
        for (module_key, record) in &self.store.modules {
            let text = if module_key == key {
                source.to_owned()
            } else {
                record.text.clone()
            };
            session.set_module_text(module_key, text)?;
        }
        Ok(session)
    }
}

#[derive(Clone)]
struct SafeCtfeHost {
    host: Option<Arc<Mutex<Box<dyn VmHost>>>>,
}

impl SafeCtfeHost {
    const fn new(host: Option<Arc<Mutex<Box<dyn VmHost>>>>) -> Self {
        Self { host }
    }
}

impl VmHost for SafeCtfeHost {
    fn call_foreign(
        &mut self,
        _ctx: VmHostCallContext<'_, '_>,
        foreign: &ForeignCall,
        _args: &[Value],
    ) -> VmResult<Value> {
        Err(VmError::new(VmErrorKind::ForeignCallRejected {
            foreign: foreign.name().into(),
        }))
    }

    fn handle_effect(
        &mut self,
        ctx: VmHostCallContext<'_, '_>,
        effect: &EffectCall,
        args: &[Value],
    ) -> VmResult<Value> {
        if !effect.is_comptime_safe() {
            return Err(VmError::new(VmErrorKind::EffectRejected {
                effect: effect.effect_name().into(),
                op: Some(effect.op_name().into()),
                reason: "effect op lacks comptimeSafe marker".into(),
            }));
        }
        let Some(host) = &self.host else {
            return RejectingHost.handle_effect(ctx, effect, args);
        };
        let Ok(mut host) = host.lock() else {
            return Err(VmError::new(VmErrorKind::EffectRejected {
                effect: effect.effect_name().into(),
                op: Some(effect.op_name().into()),
                reason: "ctfe host lock poisoned".into(),
            }));
        };
        host.handle_effect(ctx, effect, args)
    }
}

fn value_to_comptime(
    root_key: &ModuleKey,
    vm: &Vm,
    value: &Value,
) -> Result<ComptimeValue, Box<str>> {
    match value {
        Value::Unit => Ok(ComptimeValue::Unit),
        Value::Int(value) => Ok(ComptimeValue::Int(*value)),
        Value::Nat(value) => Ok(ComptimeValue::Nat(*value)),
        Value::Float(value) => Ok(ComptimeValue::Float(value.to_string().into_boxed_str())),
        Value::String(_) => match vm.inspect(value) {
            ValueView::String(text) => Ok(ComptimeValue::String(text.as_str().into())),
            _ => Err("string view missing".into()),
        },
        Value::CPtr(value) => Ok(ComptimeValue::CPtr(*value)),
        Value::Syntax(_) => match vm.inspect(value) {
            ValueView::Syntax(term) => Ok(ComptimeValue::Syntax(term.term().clone())),
            _ => Err("syntax view missing".into()),
        },
        Value::Seq(_) => seq_to_comptime(root_key, vm, value),
        Value::Data(_) => data_to_comptime(root_key, vm, value),
        Value::Closure(_) => closure_to_comptime(root_key, vm, value),
        Value::Procedure(_) => procedure_to_comptime(root_key, vm, value),
        Value::Type(value) => Ok(ComptimeValue::Type(ComptimeTypeValue {
            term: root_program(vm)?.type_term(*value),
        })),
        Value::Module(_) => module_to_comptime(root_key, vm, value),
        Value::Foreign(_) => foreign_to_comptime(root_key, vm, value),
        Value::Effect(value) => Ok(ComptimeValue::Effect(ComptimeEffectValue {
            module: root_key.clone(),
            name: root_program(vm)?.effect_source_name(*value).into(),
        })),
        Value::Shape(value) => Ok(ComptimeValue::Shape(ComptimeShapeValue {
            module: root_key.clone(),
            name: root_program(vm)?.shape_source_name(*value).into(),
        })),
        Value::Continuation(_) => Err("continuation escape rejected".into()),
    }
}

fn procedure_to_comptime(
    root_key: &ModuleKey,
    vm: &Vm,
    value: &Value,
) -> Result<ComptimeValue, Box<str>> {
    let ValueView::Procedure(procedure) = vm.inspect(value) else {
        return Err("procedure view missing".into());
    };
    let program = vm
        .module_program(procedure.module_slot())
        .ok_or_else(|| format!("module slot `{}` missing", procedure.module_slot()))?;
    let module = module_key_for_slot(root_key, vm, procedure.module_slot())?;
    Ok(ComptimeValue::Closure(ComptimeClosureValue {
        module,
        name: program.procedure_source_name(procedure.procedure()).into(),
        captures: Box::default(),
    }))
}

fn seq_to_comptime(
    root_key: &ModuleKey,
    vm: &Vm,
    value: &Value,
) -> Result<ComptimeValue, Box<str>> {
    let ValueView::Seq(seq) = vm.inspect(value) else {
        return Err("sequence view missing".into());
    };
    let ty = root_program(vm)?.type_term(seq.ty());
    let items = (0..seq.len())
        .map(|index| {
            seq.get(index)
                .ok_or_else(|| "sequence item missing".into())
                .and_then(|item| value_to_comptime(root_key, vm, &item))
        })
        .collect::<Result<Vec<_>, _>>()?
        .into_boxed_slice();
    Ok(ComptimeValue::Seq(ComptimeSeqValue { ty, items }))
}

fn data_to_comptime(
    root_key: &ModuleKey,
    vm: &Vm,
    value: &Value,
) -> Result<ComptimeValue, Box<str>> {
    let (ValueView::Record(data) | ValueView::Data(data)) = vm.inspect(value) else {
        return Err("data view missing".into());
    };
    let program = root_program(vm)?;
    let ty = program.type_term(data.ty());
    let variant = program
        .type_data_layout(data.ty())
        .and_then(|layout| {
            layout
                .variants
                .iter()
                .find(|variant| variant.tag == data.tag())
        })
        .map(|variant| variant.name.clone())
        .ok_or_else(|| format!("data variant tag `{}` missing", data.tag()))?;
    let fields = (0..data.len())
        .map(|index| {
            data.get(index)
                .cloned()
                .ok_or_else(|| "data field missing".into())
                .and_then(|field| value_to_comptime(root_key, vm, &field))
        })
        .collect::<Result<Vec<_>, _>>()?
        .into_boxed_slice();
    Ok(ComptimeValue::Data(ComptimeDataValue {
        ty,
        tag: data.tag(),
        variant,
        fields,
    }))
}

fn closure_to_comptime(
    root_key: &ModuleKey,
    vm: &Vm,
    value: &Value,
) -> Result<ComptimeValue, Box<str>> {
    let ValueView::Closure(closure) = vm.inspect(value) else {
        return Err("closure view missing".into());
    };
    let program = vm
        .module_program(closure.module_slot())
        .ok_or_else(|| format!("module slot `{}` missing", closure.module_slot()))?;
    let module = module_key_for_slot(root_key, vm, closure.module_slot())?;
    let captures = closure
        .captures()
        .iter()
        .map(|capture| value_to_comptime(root_key, vm, capture))
        .collect::<Result<Vec<_>, _>>()?
        .into_boxed_slice();
    Ok(ComptimeValue::Closure(ComptimeClosureValue {
        module,
        name: program.procedure_source_name(closure.procedure()).into(),
        captures,
    }))
}

fn module_to_comptime(
    root_key: &ModuleKey,
    vm: &Vm,
    value: &Value,
) -> Result<ComptimeValue, Box<str>> {
    let ValueView::Module(module) = vm.inspect(value) else {
        return Err("module view missing".into());
    };
    let key = if module.slot() == 0 {
        root_key.clone()
    } else {
        ModuleKey::new(module.spec())
    };
    Ok(ComptimeValue::ImportRecord(ComptimeImportRecordValue {
        key,
    }))
}

fn foreign_to_comptime(
    root_key: &ModuleKey,
    vm: &Vm,
    value: &Value,
) -> Result<ComptimeValue, Box<str>> {
    let foreign = value
        .as_foreign()
        .ok_or_else(|| "foreign view missing".to_owned())?;
    let program = vm
        .module_program(foreign.module_slot())
        .ok_or_else(|| format!("module slot `{}` missing", foreign.module_slot()))?;
    let module = module_key_for_slot(root_key, vm, foreign.module_slot())?;
    let type_args = foreign
        .type_args()
        .iter()
        .copied()
        .map(|ty| program.type_term(ty))
        .collect::<Vec<TypeTerm>>()
        .into_boxed_slice();
    Ok(ComptimeValue::Foreign(ComptimeForeignValue {
        module,
        name: program.foreign_source_name(foreign.foreign()).into(),
        type_args,
    }))
}

fn module_key_for_slot(root_key: &ModuleKey, vm: &Vm, slot: usize) -> Result<ModuleKey, Box<str>> {
    if slot == 0 {
        return Ok(root_key.clone());
    }
    vm.module_spec(slot)
        .map(ModuleKey::new)
        .ok_or_else(|| format!("module slot `{slot}` missing").into_boxed_str())
}

fn root_program(vm: &Vm) -> Result<&Program, Box<str>> {
    vm.module_program(0)
        .ok_or_else(|| "root module missing".into())
}

fn ctfe_diag(message: String) -> Diag {
    Diag::error("compile-time evaluation failed").with_note(message)
}

fn direct_callee_binding(sema: &SemaModule, callee: HirExprId) -> Option<NameBindingId> {
    let HirExprKind::Name { name } = sema.module().store.exprs.get(callee).kind else {
        return None;
    };
    sema.resolved()
        .names
        .refs
        .get(&NameSite::new(sema.module().source_id, name.span))
        .copied()
}
