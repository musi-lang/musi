use music_base::Span;
use music_hir::{
    HirExprId, HirExprKind, HirMemberDef, HirMemberKind, HirPatKind, HirTyId, HirTyKind,
    simple_hir_ty_display_name,
};
use music_module::ModuleKey;
use music_names::Symbol;
use music_sema::SemaModule;

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
struct ShapeDecl {
    expr_id: HirExprId,
    name: String,
    laws: Box<[HirMemberDef]>,
}

#[derive(Debug, Clone)]
struct LawMemberBinding {
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
    member_bindings: &'a [LawMemberBinding],
    body: &'a str,
}

impl Session {
    /// Synthesizes runnable runtime test modules for every registered module that exports shape
    /// laws.
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
    let shapes = shape_decls(module_key, sema, source)?;

    extend_shape_law_cases(&mut cases, module_key, sema, source, &shapes)?;

    cases.sort_by(|left, right| left.name.cmp(&right.name));
    Ok(cases)
}

fn extend_shape_law_cases(
    cases: ExecutableLawCaseListMut<'_>,
    module_key: &ModuleKey,
    sema: &SemaModule,
    source: &str,
    shapes: &[ShapeDecl],
) -> Result<(), SessionError> {
    for shape in shapes {
        let shape_facts = sema
            .shape_facts(shape.expr_id)
            .expect("shape facts missing for shape-law declaration");
        for (law, law_facts) in shape.laws.iter().zip(shape_facts.laws.iter()) {
            let body = member_body_text(module_key, sema, source, law)?;
            let sample_sets = law_facts
                .params
                .iter()
                .map(|param| sample_cases_for_hir_ty(module_key, sema, param.ty))
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
                shape.name,
                snippet_for_span(module_key, source, law.name.span)?
            );
            push_sampled_cases(cases, &prefix, &param_names, &sample_sets, &[], &body);
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

fn shape_decls(
    module_key: &ModuleKey,
    sema: &SemaModule,
    source: &str,
) -> Result<Vec<ShapeDecl>, SessionError> {
    Ok(top_level_let_bindings(module_key, sema, source, false)?
        .into_iter()
        .filter_map(|(_expr_id, name, _type_params, value)| {
            match &sema.module().store.exprs.get(value).kind {
                HirExprKind::Shape { members, .. } => sema.shape_facts(value).map(|_| ShapeDecl {
                    expr_id: value,
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
            }
        })
        .collect::<Vec<_>>())
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
    member_bindings: &[LawMemberBinding],
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
            let helper_name = law_helper_name(case_index, &binding.name);
            replace_member_decl_name(&binding.source, &binding.name, &helper_name)
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
    let body_expr = member
        .value
        .ok_or_else(|| law_suite_error(module_key, "law body is missing"))?;
    let span = sema.module().store.exprs.get(body_expr).origin.span;
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
