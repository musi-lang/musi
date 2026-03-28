use std::collections::{HashMap, HashSet};
use std::path::PathBuf;

use music_ast::common::{Attr, AttrArg, MemberDecl};
use music_ast::ExprId;
use music_ast::expr::{DataBody, ExprKind, InstanceBody};
use music_db::Db;
use music_owned::modules::is_compiler_owned_path;
use music_resolve::def::{DefId, DefInfo};
use music_resolve::queries::ResolutionMap;
use music_resolve::{ModuleGraph, ModuleId, ModuleLoader, ModuleResult, ProjectResolution};
use music_sema::env::{DispatchInfo, TypeEnv, VariantInfo};
use music_sema::types::SemaTypeId;
use music_sema::type_check;
use music_shared::diag::{Diag, DiagCode};
use music_shared::Interner;

use crate::lower;

/// The complete typed state for one module.
///
/// This is the canonical handoff from resolution/sema into later compiler
/// stages. It owns the module database, resolution results, and type facts.
pub struct TypedModule {
    pub db: Db,
    pub resolution: ResolutionMap,
    pub type_env: TypeEnv,
    pub diagnostics: Vec<Diag>,
    pub has_errors: bool,
    pub module_id: Option<ModuleId>,
    pub path: PathBuf,
}

/// The complete typed state for a resolved project.
pub struct TypedProject {
    pub loader: ModuleLoader,
    pub graph: ModuleGraph,
    pub modules: HashMap<ModuleId, TypedModule>,
    pub order: Vec<ModuleId>,
}

impl TypedModule {
    #[must_use]
    pub fn new(db: Db, resolution: ResolutionMap, type_env: TypeEnv) -> Self {
        Self {
            db,
            resolution,
            type_env,
            diagnostics: Vec::new(),
            has_errors: false,
            module_id: None,
            path: PathBuf::new(),
        }
    }

    #[must_use]
    pub fn with_status(
        db: Db,
        resolution: ResolutionMap,
        type_env: TypeEnv,
        diagnostics: Vec<Diag>,
        has_errors: bool,
        module_id: Option<ModuleId>,
        path: PathBuf,
    ) -> Self {
        let mut module = Self::new(db, resolution, type_env);
        module.diagnostics = diagnostics;
        module.has_errors = has_errors;
        module.module_id = module_id;
        module.path = path;
        module
    }

    #[must_use]
    pub fn expr_type(&self, expr_id: ExprId) -> Option<SemaTypeId> {
        self.type_env.type_map.get(&expr_id).copied()
    }

    #[must_use]
    pub fn dispatch(&self, expr_id: ExprId) -> Option<&DispatchInfo> {
        self.type_env.dispatch.get(&expr_id)
    }

    #[must_use]
    pub fn variant_info(&self, expr_id: ExprId) -> Option<&VariantInfo> {
        self.type_env.variant_info.get(&expr_id)
    }

    /// Returns the stable class ID for a type class by name, if assigned.
    #[must_use]
    pub fn class_id(&self, name: music_shared::Symbol) -> Option<u16> {
        self.type_env.class_ids.get(&name).copied()
    }

    /// # Panics
    ///
    /// Panics if `def_id` is not present in the resolution arena.
    #[must_use]
    pub fn def_info(&self, def_id: DefId) -> &DefInfo {
        self.resolution.defs.get(def_id)
    }
}

/// Convert a resolved project into the canonical typed-project artifact.
#[must_use]
pub fn type_project(project: ProjectResolution, loader: ModuleLoader) -> TypedProject {
    let ProjectResolution {
        graph,
        modules,
        order,
    } = project;
    let mut typed_modules = HashMap::with_capacity(modules.len());

    for (module_id, module) in modules {
        let path = graph.path(module_id).to_path_buf();
        let typed = type_module(module, Some(module_id), path);
        let _ = typed_modules.insert(module_id, typed);
    }

    TypedProject {
        loader,
        graph,
        modules: typed_modules,
        order,
    }
}

/// Convert one resolved module into the canonical typed-module artifact.
#[must_use]
pub fn type_module(module: ModuleResult, module_id: Option<ModuleId>, path: PathBuf) -> TypedModule {
    let ModuleResult {
        mut db,
        resolution,
        errors: _,
        mut diagnostics,
        has_errors,
    } = module;

    let source_id = db.source.iter().next().map(|source| source.id());
    if let Some(source_id) = source_id {
        diagnostics.extend(validate_attributes(&db, &path, source_id));
    }

    let suppressed_codes = collect_suppressed_codes(&db, &db.interner);
    if !suppressed_codes.is_empty() {
        diagnostics.retain(|diag| {
            diag.code
                .map(|code| !suppressed_codes.contains(&code))
                .unwrap_or(true)
        });
    }

    let has_errors = has_errors || !diagnostics.is_empty();

    if has_errors {
        return TypedModule::with_status(
            db,
            resolution,
            TypeEnv::new(),
            diagnostics,
            true,
            module_id,
            path,
        );
    }

    lower(&mut db.ast);
    let (db, resolution, type_env, sema_errors) = type_check(db, resolution, None);
    let has_sema_errors = !sema_errors.is_empty();
    let mut typed_diagnostics = diagnostics;
    let source_id = db.source.iter().next().map(|source| source.id());
    if let Some(source_id) = source_id {
        for err in &sema_errors {
            typed_diagnostics.push(err.diagnostic(&db.interner, source_id));
        }
    }

    if !suppressed_codes.is_empty() {
        typed_diagnostics.retain(|diag| {
            diag.code
                .map(|code| !suppressed_codes.contains(&code))
                .unwrap_or(true)
        });
    }

    TypedModule::with_status(
        db,
        resolution,
        type_env,
        typed_diagnostics,
        has_sema_errors,
        module_id,
        path,
    )
}

fn collect_suppressed_codes(db: &Db, interner: &Interner) -> HashSet<DiagCode> {
    let mut codes = HashSet::new();
    for (_, attr) in &db.ast.attrs {
        let path = attr_path_strings(db, &attr.kind);
        if path.len() != 2 || path[0] != "diagnostic" || path[1] != "allow" {
            continue;
        }
        for arg in &attr.kind.args {
            if let Some(code) = attr_arg_code(db, interner, arg) {
                let _ = codes.insert(code);
            }
        }
    }
    codes
}

fn validate_attributes(db: &Db, path: &PathBuf, source_id: music_shared::SourceId) -> Vec<Diag> {
    let mut diagnostics = Vec::new();
    let compiler_owned = is_compiler_owned_path(path);
    for &expr_id in &db.ast.root {
        validate_expr_attributes(
            db,
            expr_id,
            compiler_owned,
            source_id,
            &mut diagnostics,
        );
    }
    diagnostics
}

fn validate_expr_attributes(
    db: &Db,
    expr_id: ExprId,
    compiler_owned: bool,
    source_id: music_shared::SourceId,
    diagnostics: &mut Vec<Diag>,
) {
    match &db.ast.exprs.get(expr_id).kind {
        ExprKind::Let(binding) => {
            validate_attr_list(
                db,
                &binding.attrs,
                AttrTarget::Let {
                    foreign: binding.modifiers.foreign_abi.is_some(),
                },
                compiler_owned,
                source_id,
                diagnostics,
            );
            if let Some(value) = binding.value {
                validate_expr_attributes(db, value, compiler_owned, source_id, diagnostics);
            }
        }
        ExprKind::DataDef(body) => {
            if let DataBody::Sum(variants) = body.as_ref() {
                for variant in variants {
                    validate_attr_list(
                        db,
                        &variant.attrs,
                        AttrTarget::Variant,
                        compiler_owned,
                        source_id,
                        diagnostics,
                    );
                }
            }
        }
        ExprKind::ClassDef(data) => {
            validate_member_attributes(
                db,
                &data.members,
                compiler_owned,
                source_id,
                diagnostics,
            );
        }
        ExprKind::EffectDef(members) => {
            validate_member_attributes(db, members, compiler_owned, source_id, diagnostics);
        }
        ExprKind::InstanceDef(instance) => {
            validate_attr_list(
                db,
                &instance.attrs,
                AttrTarget::Instance,
                compiler_owned,
                source_id,
                diagnostics,
            );
            if let InstanceBody::Methods(members) = &instance.body {
                validate_member_attributes(db, members, compiler_owned, source_id, diagnostics);
            }
        }
        ExprKind::Seq(items) => {
            for &item in items {
                validate_expr_attributes(db, item, compiler_owned, source_id, diagnostics);
            }
        }
        _ => {}
    }
}

fn validate_member_attributes(
    db: &Db,
    members: &[MemberDecl],
    compiler_owned: bool,
    source_id: music_shared::SourceId,
    diagnostics: &mut Vec<Diag>,
) {
    for member in members {
        if let MemberDecl::Fn(decl) = member {
            validate_attr_list(
                db,
                &decl.attrs,
                AttrTarget::Member,
                compiler_owned,
                source_id,
                diagnostics,
            );
        }
    }
}

#[derive(Clone, Copy)]
enum AttrTarget {
    Let { foreign: bool },
    Instance,
    Member,
    Variant,
}

fn validate_attr_list(
    db: &Db,
    attrs: &[music_ast::AttrId],
    target: AttrTarget,
    compiler_owned: bool,
    source_id: music_shared::SourceId,
    diagnostics: &mut Vec<Diag>,
) {
    for &attr_id in attrs {
        let attr = db.ast.attrs.get(attr_id);
        let path = attr_path_strings(db, &attr.kind);
        let Some(root) = path.first().map(String::as_str) else {
            continue;
        };

        match root {
            "musi" => validate_compiler_attr(
                &path,
                compiler_owned,
                source_id,
                diagnostics,
                attr.span,
            ),
            "ffi" => validate_ffi_attr(&path, target, source_id, diagnostics, attr.span),
            "diagnostic" => validate_diagnostic_attr(
                db,
                &attr.kind,
                &path,
                source_id,
                diagnostics,
                attr.span,
            ),
            "builtin" | "lib" => diagnostics.push(
                Diag::error(format!("legacy attribute '@{root}' is not supported"))
                    .with_code(DiagCode::new(2501))
                    .with_hint(match root {
                        "builtin" => "use '@musi.lang', '@musi.intrinsic', or '@musi.variant'".to_owned(),
                        "lib" => "use '@ffi.link(...)'".to_owned(),
                        _ => String::new(),
                    })
                    .with_label(attr.span, source_id, ""),
            ),
            _ => {}
        }
    }
}

fn validate_compiler_attr(
    path: &[String],
    compiler_owned: bool,
    source_id: music_shared::SourceId,
    diagnostics: &mut Vec<Diag>,
    span: music_shared::Span,
) {
    if !compiler_owned {
        diagnostics.push(
            Diag::error(format!(
                "reserved compiler attribute '{}' is not allowed here",
                attr_path_display(path)
            ))
            .with_code(DiagCode::new(2502))
            .with_hint("compiler-owned attributes are only allowed in compiler modules")
            .with_label(span, source_id, ""),
        );
        return;
    }

    let valid = matches!(
        path,
        [root, kind] if root == "musi"
            && matches!(
                kind.as_str(),
                "lang" | "intrinsic" | "variant" | "layout" | "codegen"
            )
    );
    if !valid {
        diagnostics.push(
            Diag::error(format!(
                "unknown compiler attribute '{}'",
                attr_path_display(path)
            ))
            .with_code(DiagCode::new(2503))
            .with_label(span, source_id, ""),
        );
    }
}

fn validate_ffi_attr(
    path: &[String],
    target: AttrTarget,
    source_id: music_shared::SourceId,
    diagnostics: &mut Vec<Diag>,
    span: music_shared::Span,
) {
    match path {
        [root, link] if root == "ffi" && link == "link" => {
            if !matches!(target, AttrTarget::Let { foreign: true }) {
                diagnostics.push(
                    Diag::error("attribute '@ffi.link' is only allowed on foreign let declarations")
                        .with_code(DiagCode::new(2504))
                        .with_label(span, source_id, ""),
                );
            }
        }
        [root, _] if root == "ffi" => diagnostics.push(
            Diag::error(format!("unknown FFI attribute '{}'", attr_path_display(path)))
                .with_code(DiagCode::new(2505))
                .with_label(span, source_id, ""),
        ),
        _ => {}
    }
}

fn validate_diagnostic_attr(
    db: &Db,
    attr: &Attr,
    path: &[String],
    source_id: music_shared::SourceId,
    diagnostics: &mut Vec<Diag>,
    span: music_shared::Span,
) {
    let valid = matches!(
        path,
        [root, kind] if root == "diagnostic"
            && matches!(kind.as_str(), "allow" | "warn" | "deny" | "expect")
    );
    if !valid {
        diagnostics.push(
            Diag::error(format!(
                "unknown diagnostic attribute '{}'",
                attr_path_display(path)
            ))
            .with_code(DiagCode::new(2506))
            .with_label(span, source_id, ""),
        );
        return;
    }

    for arg in &attr.args {
        if attr_arg_code(db, &db.interner, arg).is_none() {
            diagnostics.push(
                Diag::error("diagnostic attributes require codes like ms1234")
                    .with_code(DiagCode::new(2507))
                    .with_label(span, source_id, ""),
            );
            return;
        }
    }
}

fn attr_arg_code(db: &Db, interner: &Interner, arg: &AttrArg) -> Option<DiagCode> {
    let expr_id = match arg {
        AttrArg::Named { value, .. } => *value,
        AttrArg::Positional(expr_id) => *expr_id,
    };
    match &db.ast.exprs.get(expr_id).kind {
        ExprKind::Var(ident) => DiagCode::parse(interner.resolve(ident.name)),
        ExprKind::Lit(music_shared::Literal::Str(code)) => DiagCode::parse(code),
        _ => None,
    }
}

fn attr_path_strings(db: &Db, attr: &Attr) -> Vec<String> {
    attr.path
        .iter()
        .map(|ident| db.interner.resolve(ident.name).to_owned())
        .collect()
}

fn attr_path_display(path: &[String]) -> String {
    path.join(".")
}
