use std::collections::HashMap;

use music_base::Span;
use music_hir::{
    HirExprId, HirExprKind, HirMemberDef, HirMemberKind, HirPatKind, HirTyId, HirTyKind,
    simple_hir_ty_display_name,
};
use music_module::ModuleKey;
use music_names::Symbol;
use music_sema::{SemaModule, SurfaceTyId, SurfaceTyKind};

use crate::api::{LawSuiteModule, SessionError};

use super::Session;

const LAW_TEST_EXPORT_NAME: &str = "musiLawsTest";

#[derive(Debug, Clone)]
struct SampleCase {
    label: String,
    expr: String,
}

#[derive(Debug, Clone)]
struct ExecutableLawCase {
    name: String,
    helpers: BindingList,
    bindings: BindingList,
    body: String,
}

#[derive(Debug, Clone)]
struct ClassDecl {
    expr_id: HirExprId,
    name: String,
    type_params: Box<[Symbol]>,
    laws: Box<[HirMemberDef]>,
}

#[derive(Debug, Clone)]
struct ExportedEffectDecl {
    name: String,
    laws: Box<[HirMemberDef]>,
}

#[derive(Debug, Clone)]
struct InstanceDecl {
    expr_id: HirExprId,
    member_defs: Box<[HirMemberDef]>,
}

#[derive(Debug, Clone)]
struct InstanceMemberBinding {
    name: String,
    source: String,
}

type TopLevelLetBinding = (HirExprId, String, Box<[Symbol]>, HirExprId);
type BindingList = Vec<String>;
type TopLevelLetBindingList = Vec<TopLevelLetBinding>;
type ExecutableLawCaseList = Vec<ExecutableLawCase>;
type TopLevelExprIdList = Vec<HirExprId>;
type ExecutableLawCaseListMut<'a> = &'a mut ExecutableLawCaseList;

struct SampleCaseBuild<'a> {
    prefix: &'a str,
    param_names: &'a [String],
    sample_sets: &'a [Vec<SampleCase>],
    member_bindings: &'a [InstanceMemberBinding],
    body: &'a str,
}

impl Session {
    /// Synthesizes runnable runtime test modules for every registered module that exports class or
    /// effect laws.
    ///
    /// # Errors
    ///
    /// Returns any earlier parse, resolve, or semantic error needed to inspect exported law
    /// surfaces.
    pub fn law_suite_modules(&mut self) -> Result<Box<[LawSuiteModule]>, SessionError> {
        let mut candidates = Vec::new();
        for module_key in self.store.modules.keys() {
            let name = module_key.as_str();
            if name.ends_with("::__laws") || name.ends_with(".test.ms") {
                continue;
            }
            if self.module_might_define_laws(module_key) {
                candidates.push(module_key.clone());
            }
        }
        let mut suites = candidates
            .into_iter()
            .map(|module_key| self.build_law_suite_module(&module_key))
            .collect::<Result<Vec<_>, _>>()?
            .into_iter()
            .flatten()
            .collect::<Vec<_>>();
        suites.sort_by(|left, right| left.suite_module_key.cmp(&right.suite_module_key));
        Ok(suites.into_boxed_slice())
    }

    /// Synthesizes runtime test modules for law-bearing modules reachable from one entry module.
    ///
    /// # Errors
    ///
    /// Returns any earlier parse, resolve, or semantic error needed to inspect the reachable graph.
    pub fn law_suite_modules_for_entry(
        &mut self,
        key: &ModuleKey,
    ) -> Result<Box<[LawSuiteModule]>, SessionError> {
        let reachable = self.collect_reachable_module_keys(key)?;
        let mut candidates = Vec::new();
        for module_key in reachable {
            let name = module_key.as_str();
            if name.ends_with("::__laws") || name.ends_with(".test.ms") {
                continue;
            }
            if self.module_might_define_laws(&module_key) {
                candidates.push(module_key);
            }
        }
        let mut suites = candidates
            .into_iter()
            .map(|module_key| self.build_law_suite_module(&module_key))
            .collect::<Result<Vec<_>, _>>()?
            .into_iter()
            .flatten()
            .collect::<Vec<_>>();
        suites.sort_by(|left, right| left.suite_module_key.cmp(&right.suite_module_key));
        Ok(suites.into_boxed_slice())
    }

    fn build_law_suite_module(
        &mut self,
        module_key: &ModuleKey,
    ) -> Result<Option<LawSuiteModule>, SessionError> {
        let source = self
            .module_text(module_key)
            .ok_or_else(|| SessionError::ModuleNotRegistered {
                key: module_key.clone(),
            })?
            .to_owned();
        let sema = self.check_module(module_key)?;
        let cases = executable_law_cases(module_key, sema, &source)?;
        if cases.is_empty() {
            return Ok(None);
        }
        let suite_module_key = ModuleKey::new(format!("{}::__laws", module_key.as_str()));
        let suite_source = render_law_suite_module_source(&source, module_key, &cases);
        self.set_module_text(&suite_module_key, suite_source)?;
        Ok(Some(LawSuiteModule::new(
            module_key.clone(),
            suite_module_key,
            LAW_TEST_EXPORT_NAME,
            cases.len(),
        )))
    }
}

fn executable_law_cases(
    module_key: &ModuleKey,
    sema: &SemaModule,
    source: &str,
) -> Result<ExecutableLawCaseList, SessionError> {
    let mut cases = ExecutableLawCaseList::new();
    let classes = class_decls(module_key, sema, source)?;
    let exported_effects = exported_effect_decls(module_key, sema, source)?;
    let instances = instance_decls(sema);

    extend_effect_law_cases(&mut cases, module_key, sema, source, &exported_effects)?;
    extend_class_law_cases(&mut cases, module_key, sema, source, &classes, &instances)?;

    cases.sort_by(|left, right| left.name.cmp(&right.name));
    Ok(cases)
}

fn extend_effect_law_cases(
    cases: ExecutableLawCaseListMut<'_>,
    module_key: &ModuleKey,
    sema: &SemaModule,
    source: &str,
    exported_effects: &[ExportedEffectDecl],
) -> Result<(), SessionError> {
    for effect in exported_effects {
        let Some(surface) = sema
            .surface()
            .exported_effects()
            .iter()
            .find(|item| item.key.name.as_ref() == effect.name)
        else {
            continue;
        };
        for (law, surface_law) in effect.laws.iter().zip(surface.laws.iter()) {
            let body = member_body_text(module_key, sema, source, law)?;
            let sample_sets = surface_law
                .params
                .iter()
                .map(|param| sample_cases_for_surface_ty(module_key, sema, param.ty))
                .collect::<Result<Vec<_>, _>>()?;
            let param_names = sema
                .module()
                .store
                .params
                .get(law.params.clone())
                .iter()
                .map(|param| snippet_for_span(module_key, source, param.name.span))
                .collect::<Result<Vec<_>, _>>()?;
            let prefix = format!(
                "{}.{}",
                effect.name,
                snippet_for_span(module_key, source, law.name.span)?
            );
            push_sampled_cases(cases, &prefix, &param_names, &sample_sets, &[], &body);
        }
    }
    Ok(())
}

fn extend_class_law_cases(
    cases: ExecutableLawCaseListMut<'_>,
    module_key: &ModuleKey,
    sema: &SemaModule,
    source: &str,
    classes: &[ClassDecl],
    instances: &[InstanceDecl],
) -> Result<(), SessionError> {
    for class in classes {
        let class_facts = sema
            .class_facts(class.expr_id)
            .expect("class facts missing for class-law declaration");
        let class_instances = instances.iter().filter(|instance| {
            sema.instance_facts(instance.expr_id)
                .is_some_and(|facts| facts.class_key == class_facts.key)
        });
        for instance in class_instances {
            let instance_facts = sema
                .instance_facts(instance.expr_id)
                .expect("instance facts missing for class-law instance");
            if !instance_facts.type_params.is_empty() {
                return Err(law_suite_error(
                    module_key,
                    format!(
                        "instance `{}` remains polymorphic",
                        render_instance_head(&class.name, &instance_facts.class_args, sema)
                    ),
                ));
            }
            let subst = class_type_subst(class, instance_facts.class_args.as_ref());
            let member_bindings = instance
                .member_defs
                .iter()
                .filter(|member| member.kind == HirMemberKind::Let)
                .map(|member| {
                    Ok::<InstanceMemberBinding, SessionError>(InstanceMemberBinding {
                        name: snippet_for_span(module_key, source, member.name.span)?,
                        source: snippet_for_span(module_key, source, member.origin.span)?,
                    })
                })
                .collect::<Result<Vec<_>, _>>()?;
            for (law, law_facts) in class.laws.iter().zip(class_facts.laws.iter()) {
                let body = member_body_text(module_key, sema, source, law)?;
                let sample_sets = law_facts
                    .params
                    .iter()
                    .map(|param| {
                        let ty = substitute_class_ty(sema, param.ty, &subst);
                        sample_cases_for_hir_ty(module_key, sema, ty)
                    })
                    .collect::<Result<Vec<_>, _>>()?;
                let param_names = sema
                    .module()
                    .store
                    .params
                    .get(law.params.clone())
                    .iter()
                    .map(|param| snippet_for_span(module_key, source, param.name.span))
                    .collect::<Result<Vec<_>, _>>()?;
                let prefix = format!(
                    "{}.{}",
                    render_instance_head(&class.name, &instance_facts.class_args, sema),
                    snippet_for_span(module_key, source, law.name.span)?
                );
                push_sampled_cases(
                    cases,
                    &prefix,
                    &param_names,
                    &sample_sets,
                    &member_bindings,
                    &body,
                );
            }
        }
    }
    Ok(())
}

impl Session {
    fn module_might_define_laws(&self, module_key: &ModuleKey) -> bool {
        self.module_text(module_key)
            .is_some_and(|text| text.contains("law "))
    }
}

fn class_decls(
    module_key: &ModuleKey,
    sema: &SemaModule,
    source: &str,
) -> Result<Vec<ClassDecl>, SessionError> {
    Ok(top_level_let_bindings(module_key, sema, source, false)?
        .into_iter()
        .filter_map(|(_expr_id, name, type_params, value)| {
            match &sema.module().store.exprs.get(value).kind {
                HirExprKind::Class { members, .. } => sema.class_facts(value).map(|_| ClassDecl {
                    expr_id: value,
                    name,
                    type_params,
                    laws: sema
                        .module()
                        .store
                        .members
                        .get(members.clone())
                        .iter()
                        .filter(|member| member.kind == HirMemberKind::Law)
                        .cloned()
                        .collect::<Vec<_>>()
                        .into_boxed_slice(),
                }),
                _ => None,
            }
        })
        .collect::<Vec<_>>())
}

fn exported_effect_decls(
    module_key: &ModuleKey,
    sema: &SemaModule,
    source: &str,
) -> Result<Vec<ExportedEffectDecl>, SessionError> {
    Ok(top_level_let_bindings(module_key, sema, source, true)?
        .into_iter()
        .filter_map(
            |(_, name, _, value)| match &sema.module().store.exprs.get(value).kind {
                HirExprKind::Effect { members } => Some(ExportedEffectDecl {
                    name,
                    laws: sema
                        .module()
                        .store
                        .members
                        .get(members.clone())
                        .iter()
                        .filter(|member| member.kind == HirMemberKind::Law)
                        .cloned()
                        .collect::<Vec<_>>()
                        .into_boxed_slice(),
                }),
                _ => None,
            },
        )
        .collect::<Vec<_>>())
}

fn instance_decls(sema: &SemaModule) -> Vec<InstanceDecl> {
    let store = &sema.module().store;
    top_level_expr_ids(sema)
        .into_iter()
        .filter_map(|expr_id| {
            let expr = store.exprs.get(expr_id);
            let (instance_expr_id, members) = match &expr.kind {
                HirExprKind::Let { value, .. } => match &store.exprs.get(*value).kind {
                    HirExprKind::Instance { members, .. } => (*value, members.clone()),
                    _ => return None,
                },
                HirExprKind::Instance { members, .. } => (expr_id, members.clone()),
                _ => return None,
            };
            Some(InstanceDecl {
                expr_id: instance_expr_id,
                member_defs: store.members.get(members).to_vec().into_boxed_slice(),
            })
        })
        .collect()
}

fn top_level_let_bindings(
    module_key: &ModuleKey,
    sema: &SemaModule,
    source: &str,
    require_export: bool,
) -> Result<TopLevelLetBindingList, SessionError> {
    let store = &sema.module().store;
    top_level_expr_ids(sema)
        .into_iter()
        .filter_map(|expr_id| {
            let expr = store.exprs.get(expr_id);
            let HirExprKind::Let {
                pat,
                type_params,
                value,
                ..
            } = expr.kind
            else {
                return None;
            };
            if require_export && expr.mods.export.is_none() {
                return None;
            }
            let HirPatKind::Bind { name } = store.pats.get(pat).kind else {
                return None;
            };
            Some(
                snippet_for_span(module_key, source, name.span).map(|binding_name| {
                    (
                        expr_id,
                        binding_name,
                        store
                            .binders
                            .get(type_params)
                            .iter()
                            .map(|binder| binder.name.name)
                            .collect::<Vec<_>>()
                            .into_boxed_slice(),
                        value,
                    )
                }),
            )
        })
        .collect()
}

fn top_level_expr_ids(sema: &SemaModule) -> TopLevelExprIdList {
    let root = sema.module().root;
    match &sema.module().store.exprs.get(root).kind {
        HirExprKind::Sequence { exprs } => sema.module().store.expr_ids.get(*exprs).to_vec(),
        _ => vec![root],
    }
}

fn class_type_subst(class: &ClassDecl, class_args: &[HirTyId]) -> HashMap<Symbol, HirTyId> {
    class
        .type_params
        .iter()
        .copied()
        .zip(class_args.iter().copied())
        .collect()
}

fn substitute_class_ty(
    sema: &SemaModule,
    ty: HirTyId,
    subst: &HashMap<Symbol, HirTyId>,
) -> HirTyId {
    match &sema.ty(ty).kind {
        HirTyKind::Named { name, args } if args.is_empty() => {
            subst.get(name).copied().unwrap_or(ty)
        }
        _ => ty,
    }
}

fn sample_cases_for_hir_ty(
    module_key: &ModuleKey,
    sema: &SemaModule,
    ty: HirTyId,
) -> Result<Vec<SampleCase>, SessionError> {
    match &sema.ty(ty).kind {
        HirTyKind::Unit => Ok(vec![SampleCase {
            label: "unit".into(),
            expr: "()".into(),
        }]),
        HirTyKind::Bool => Ok(vec![
            SampleCase {
                label: "False".into(),
                expr: "0 = 1".into(),
            },
            SampleCase {
                label: "True".into(),
                expr: "0 = 0".into(),
            },
        ]),
        HirTyKind::Int => Ok(int_samples()),
        HirTyKind::Float => Ok(float_samples()),
        HirTyKind::String | HirTyKind::CString => Ok(string_samples()),
        HirTyKind::Rune => Ok(vec![SampleCase {
            label: "rune".into(),
            expr: "'a'".into(),
        }]),
        HirTyKind::Named { name, .. } => Err(law_suite_error(
            module_key,
            format!(
                "law parameter type `{}` has no built-in sample set",
                render_named_type_fallback(sema, *name)
            ),
        )),
        other => Err(law_suite_error(
            module_key,
            format!(
                "law parameter type `{}` has no built-in sample set",
                render_hir_ty(other)
            ),
        )),
    }
}

fn sample_cases_for_surface_ty(
    module_key: &ModuleKey,
    sema: &SemaModule,
    ty: SurfaceTyId,
) -> Result<Vec<SampleCase>, SessionError> {
    match &sema
        .surface()
        .try_ty(ty)
        .expect("surface law param type missing")
        .kind
    {
        SurfaceTyKind::Unit => Ok(vec![SampleCase {
            label: "unit".into(),
            expr: "()".into(),
        }]),
        SurfaceTyKind::Bool => Ok(vec![
            SampleCase {
                label: "False".into(),
                expr: "0 = 1".into(),
            },
            SampleCase {
                label: "True".into(),
                expr: "0 = 0".into(),
            },
        ]),
        SurfaceTyKind::Int => Ok(int_samples()),
        SurfaceTyKind::Float => Ok(float_samples()),
        SurfaceTyKind::String | SurfaceTyKind::CString => Ok(string_samples()),
        SurfaceTyKind::Rune => Ok(vec![SampleCase {
            label: "rune".into(),
            expr: "'a'".into(),
        }]),
        other => Err(law_suite_error(
            module_key,
            format!(
                "law parameter type `{}` has no built-in sample set",
                render_surface_ty(other)
            ),
        )),
    }
}

fn int_samples() -> Vec<SampleCase> {
    [-2, -1, 0, 1, 2]
        .into_iter()
        .map(|value| SampleCase {
            label: value.to_string(),
            expr: value.to_string(),
        })
        .collect()
}

fn float_samples() -> Vec<SampleCase> {
    [
        ("negative", "-1.0"),
        ("negativeZero", "0.0 / -1.0"),
        ("zero", "0.0"),
        ("positive", "1.0"),
        ("negativeInfinity", "-1.0 / 0.0"),
        ("positiveInfinity", "1.0 / 0.0"),
        ("nan", "0.0 / 0.0"),
    ]
    .into_iter()
    .map(|(label, expr)| SampleCase {
        label: label.into(),
        expr: expr.into(),
    })
    .collect()
}

fn string_samples() -> Vec<SampleCase> {
    [("empty", "\"\""), ("a", "\"a\""), ("musi", "\"musi\"")]
        .into_iter()
        .map(|(label, expr)| SampleCase {
            label: label.into(),
            expr: expr.into(),
        })
        .collect()
}

fn push_sampled_cases(
    out: &mut ExecutableLawCaseList,
    prefix: &str,
    param_names: &[String],
    sample_sets: &[Vec<SampleCase>],
    member_bindings: &[InstanceMemberBinding],
    body: &str,
) {
    let mut current = Vec::<SampleCase>::new();
    let build = SampleCaseBuild {
        prefix,
        param_names,
        sample_sets,
        member_bindings,
        body,
    };
    push_sampled_cases_rec(out, &build, 0, &mut current);
}

fn push_sampled_cases_rec(
    out: &mut ExecutableLawCaseList,
    build: &SampleCaseBuild<'_>,
    index: usize,
    current: &mut Vec<SampleCase>,
) {
    if index == build.sample_sets.len() {
        let case_index = out.len();
        let helpers = helper_bindings(case_index, build);
        let body = rewrite_member_calls(case_index, build);
        let mut bindings = BindingList::new();
        bindings.extend(
            build
                .param_names
                .iter()
                .zip(current.iter())
                .map(|(name, sample)| format!("let {name} := {};", sample.expr)),
        );
        let case_name = if current.is_empty() {
            build.prefix.to_owned()
        } else {
            let labels = current
                .iter()
                .map(|sample| sample.label.as_str())
                .collect::<Vec<_>>()
                .join(", ");
            format!("{}[{labels}]", build.prefix)
        };
        out.push(ExecutableLawCase {
            name: case_name,
            helpers,
            bindings,
            body,
        });
        return;
    }
    for sample in &build.sample_sets[index] {
        current.push(sample.clone());
        push_sampled_cases_rec(out, build, index + 1, current);
        let _ = current.pop();
    }
}

fn helper_bindings(case_index: usize, build: &SampleCaseBuild<'_>) -> BindingList {
    build
        .member_bindings
        .iter()
        .map(|binding| {
            let helper = law_helper_name(case_index, &binding.name);
            replace_member_decl_name(&binding.source, &binding.name, &helper)
        })
        .collect()
}

fn replace_member_decl_name(source: &str, name: &str, helper: &str) -> String {
    let needle = format!("let {name}");
    source.replacen(&needle, &format!("let {helper}"), 1)
}

fn rewrite_member_calls(case_index: usize, build: &SampleCaseBuild<'_>) -> String {
    build
        .member_bindings
        .iter()
        .fold(build.body.to_owned(), |body, binding| {
            body.replace(
                &format!("{}(", binding.name),
                &format!("{}(", law_helper_name(case_index, &binding.name)),
            )
        })
}

fn law_helper_name(case_index: usize, member_name: &str) -> String {
    let sanitized = member_name
        .chars()
        .map(|ch| if ch.is_ascii_alphanumeric() { ch } else { '_' })
        .collect::<String>();
    format!("musiLawCase{case_index}{sanitized}")
}

fn member_body_text(
    module_key: &ModuleKey,
    sema: &SemaModule,
    source: &str,
    member: &HirMemberDef,
) -> Result<String, SessionError> {
    let value = member
        .value
        .ok_or_else(|| law_suite_error(module_key, "law body is missing"))?;
    let span = sema.module().store.exprs.get(value).origin.span;
    snippet_for_span(module_key, source, span)
}

fn snippet_for_span(
    module_key: &ModuleKey,
    source: &str,
    span: Span,
) -> Result<String, SessionError> {
    let start = usize::try_from(span.start).unwrap_or(source.len());
    let end = usize::try_from(span.end).unwrap_or(source.len());
    source
        .get(start..end)
        .map(str::trim)
        .map(str::to_owned)
        .ok_or_else(|| law_suite_error(module_key, format!("source slice `{span}` is invalid")))
}

fn render_law_suite_module_source(
    source: &str,
    module_key: &ModuleKey,
    cases: &[ExecutableLawCase],
) -> String {
    let mut out = String::new();
    out.push_str(source);
    if !source.ends_with('\n') {
        out.push('\n');
    }
    out.push_str("\nlet musiLawTest := import \"musi:test\";\n\n");
    for test_case in cases {
        for helper in &test_case.helpers {
            out.push_str(helper);
            if !helper.trim_end().ends_with(';') {
                out.push(';');
            }
            if !helper.ends_with('\n') {
                out.push('\n');
            }
        }
    }
    if cases.iter().any(|case| !case.helpers.is_empty()) {
        out.push('\n');
    }
    out.push_str("export let ");
    out.push_str(LAW_TEST_EXPORT_NAME);
    out.push_str(" () : Unit :=\n    (\n      musiLawTest.suiteStart(");
    out.push_str(&string_lit(&format!("{} laws", module_key.as_str())));
    out.push_str(");\n");
    for test_case in cases {
        out.push_str("      musiLawTest.testCase(");
        out.push_str(&string_lit(&test_case.name));
        out.push_str(", (\n");
        for binding in &test_case.bindings {
            out.push_str("        ");
            out.push_str(binding);
            out.push('\n');
        }
        out.push_str("        ");
        out.push_str(test_case.body.trim());
        out.push_str("\n      ));\n");
    }
    out.push_str("      musiLawTest.suiteEnd()\n    );\n");
    out
}

fn render_instance_head(class_name: &str, class_args: &[HirTyId], sema: &SemaModule) -> String {
    if class_args.is_empty() {
        return class_name.to_owned();
    }
    let args = class_args
        .iter()
        .copied()
        .map(|ty| render_ty_id(sema, ty))
        .collect::<Vec<_>>()
        .join(", ");
    format!("{class_name}[{args}]")
}

fn render_ty_id(sema: &SemaModule, ty: HirTyId) -> String {
    match &sema.ty(ty).kind {
        HirTyKind::NatLit(value) => value.to_string(),
        kind if simple_hir_ty_display_name(kind).is_some() => {
            simple_hir_ty_display_name(kind).unwrap_or("<error>").into()
        }
        HirTyKind::Named { name, args } => {
            let name = render_named_type_fallback(sema, *name);
            if args.is_empty() {
                name
            } else {
                let args = sema
                    .module()
                    .store
                    .ty_ids
                    .get(*args)
                    .iter()
                    .copied()
                    .map(|arg| render_ty_id(sema, arg))
                    .collect::<Vec<_>>()
                    .join(", ");
                format!("{name}[{args}]")
            }
        }
        _ => "<unsupported>".into(),
    }
}

fn render_hir_ty(kind: &HirTyKind) -> String {
    if let HirTyKind::NatLit(value) = kind {
        return value.to_string();
    }
    if let Some(name) = simple_hir_ty_display_name(kind) {
        return name.into();
    }
    match kind {
        HirTyKind::Named { .. } => "<named>".into(),
        _ => "<unsupported>".into(),
    }
}

fn render_surface_ty(kind: &SurfaceTyKind) -> String {
    match kind {
        SurfaceTyKind::Unit => "Unit".into(),
        SurfaceTyKind::Bool => "Bool".into(),
        SurfaceTyKind::Int => "Int".into(),
        SurfaceTyKind::Float => "Float".into(),
        SurfaceTyKind::String => "String".into(),
        SurfaceTyKind::Rune => "Rune".into(),
        SurfaceTyKind::CString => "CString".into(),
        SurfaceTyKind::CPtr => "CPtr".into(),
        SurfaceTyKind::Named { name, .. } => name.to_string(),
        _ => "<unsupported>".into(),
    }
}

fn render_named_type_fallback(_sema: &SemaModule, _symbol: Symbol) -> String {
    "<named>".into()
}

fn string_lit(value: &str) -> String {
    let mut out = String::with_capacity(value.len() + 2);
    out.push('"');
    for ch in value.chars() {
        match ch {
            '\\' => out.push_str("\\\\"),
            '"' => out.push_str("\\\""),
            '\n' => out.push_str("\\n"),
            '\r' => out.push_str("\\r"),
            '\t' => out.push_str("\\t"),
            _ => out.push(ch),
        }
    }
    out.push('"');
    out
}

fn law_suite_error(module_key: &ModuleKey, reason: impl Into<Box<str>>) -> SessionError {
    SessionError::LawSuiteSynthesisFailed {
        module: module_key.clone(),
        reason: reason.into(),
    }
}
