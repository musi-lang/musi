use std::collections::{HashMap, HashSet};

use music_ast::common::{Attr, AttrArg, MemberDecl};
use music_ast::expr::{DataBody, ExprKind, InstanceBody};
use music_ast::{AttrId, ExprId};
use music_db::Db;
use music_shared::diag::DiagLevel;
use music_shared::diag::{Diag, DiagCode};
use music_shared::{Interner, Literal, SourceId, Span};

use crate::{AttrBindError, AttrSpec, attr_expr_string, attr_path_matches, bind_attr};

const LINK_ATTR: AttrSpec = AttrSpec {
    path: &["link"],
    params: &["name", "symbol"],
    required: &[],
};

#[derive(Default)]
pub(super) struct DiagPolicy {
    allow: HashSet<DiagCode>,
    warn: HashSet<DiagCode>,
    deny: HashSet<DiagCode>,
    expect: HashMap<DiagCode, Vec<Span>>,
}

pub(super) fn collect_diag_policy(db: &Db, interner: &Interner) -> DiagPolicy {
    let mut policy = DiagPolicy::default();
    for (_, attr) in &db.ast.attrs {
        let path = attr_path_strings(db, &attr.kind);
        if path.len() != 2 || path[0] != "diag" {
            continue;
        }
        for arg in &attr.kind.args {
            if let Some(code) = attr_arg_code(db, interner, arg) {
                match path[1].as_str() {
                    "allow" => {
                        let _ = policy.allow.insert(code);
                    }
                    "warn" => {
                        let _ = policy.warn.insert(code);
                    }
                    "deny" => {
                        let _ = policy.deny.insert(code);
                    }
                    "expect" => {
                        policy.expect.entry(code).or_default().push(attr.span);
                    }
                    _ => {}
                }
            }
        }
    }
    policy
}

pub(super) fn apply_diag_policy(
    diagnostics: &mut Vec<Diag>,
    policy: &mut DiagPolicy,
    _source_id: SourceId,
) {
    let mut filtered = Vec::with_capacity(diagnostics.len());
    for mut diag in diagnostics.drain(..) {
        let Some(code) = diag.code else {
            filtered.push(diag);
            continue;
        };

        if let Some(pending) = policy.expect.get_mut(&code)
            && pending.pop().is_some()
        {
            continue;
        }

        if policy.allow.contains(&code) {
            continue;
        }

        if policy.deny.contains(&code) {
            diag.level = DiagLevel::Error;
        } else if policy.warn.contains(&code) {
            diag.level = DiagLevel::Warning;
        }

        filtered.push(diag);
    }
    *diagnostics = filtered;
}

pub(super) fn finalize_diag_policy(
    diagnostics: &mut Vec<Diag>,
    policy: &mut DiagPolicy,
    source_id: SourceId,
) {
    for (code, spans) in std::mem::take(&mut policy.expect) {
        for span in spans {
            diagnostics.push(
                Diag::error(format!("expected diagnostic '{code}' was not produced"))
                    .with_code(DiagCode::new(2510))
                    .with_label(span, source_id, ""),
            );
        }
    }
}

pub(super) fn validate_attributes(db: &Db, compiler_owned: bool, source_id: SourceId) -> Vec<Diag> {
    let mut diagnostics = Vec::new();
    for &expr_id in &db.ast.root {
        validate_expr_attributes(db, expr_id, compiler_owned, source_id, &mut diagnostics);
    }
    diagnostics
}

fn validate_expr_attributes(
    db: &Db,
    expr_id: ExprId,
    compiler_owned: bool,
    source_id: SourceId,
    diagnostics: &mut Vec<Diag>,
) {
    match &db.ast.exprs.get(expr_id).kind {
        ExprKind::Let(binding) => {
            validate_attr_list(
                db,
                &binding.attrs,
                AttrTarget::Let {
                    foreign: binding.modifiers.foreign,
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
            validate_member_attributes(db, &data.members, compiler_owned, source_id, diagnostics);
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
    source_id: SourceId,
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
    attrs: &[AttrId],
    target: AttrTarget,
    compiler_owned: bool,
    source_id: SourceId,
    diagnostics: &mut Vec<Diag>,
) {
    for &attr_id in attrs {
        let attr = db.ast.attrs.get(attr_id);
        let path = attr_path_strings(db, &attr.kind);
        let Some(root) = path.first().map(String::as_str) else {
            continue;
        };

        match root {
            "musi" => {
                validate_compiler_attr(&path, compiler_owned, source_id, diagnostics, attr.span)
            }
            "link" => {
                if attr_path_matches(db, &attr.kind, LINK_ATTR.path) {
                    validate_link_attr(db, &attr.kind, target, source_id, diagnostics, attr.span);
                } else {
                    diagnostics.push(
                        Diag::error(format!(
                            "unknown public attribute '{}'",
                            attr_path_display(&path)
                        ))
                        .with_code(DiagCode::new(2509))
                        .with_label(attr.span, source_id, ""),
                    );
                }
            }
            "diag" => {
                validate_diag_attr(db, &attr.kind, &path, source_id, diagnostics, attr.span)
            }
            "when" | "repr" | "layout" => diagnostics.push(
                Diag::error(format!(
                    "attribute '@{}' is reserved but not implemented",
                    attr_path_display(&path)
                ))
                .with_code(DiagCode::new(2508))
                .with_label(attr.span, source_id, ""),
            ),
            "builtin" | "lib" | "ffi" | "diagnostic" => diagnostics.push(
                Diag::error(format!("legacy attribute '@{root}' is not supported"))
                    .with_code(DiagCode::new(2501))
                    .with_hint(match root {
                        "builtin" => {
                            "use '@musi.lang', '@musi.intrinsic', or '@musi.variant'".to_owned()
                        }
                        "lib" | "ffi" => "use '@link(...)'".to_owned(),
                        "diagnostic" => "use '@diag.allow(...)', '@diag.warn(...)', '@diag.deny(...)', or '@diag.expect(...)'".to_owned(),
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
    source_id: SourceId,
    diagnostics: &mut Vec<Diag>,
    span: Span,
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

fn validate_link_attr(
    db: &Db,
    attr: &Attr,
    target: AttrTarget,
    source_id: SourceId,
    diagnostics: &mut Vec<Diag>,
    span: Span,
) {
    if !matches!(target, AttrTarget::Let { foreign: true }) {
        diagnostics.push(
            Diag::error("attribute '@link' is only allowed on foreign let declarations")
                .with_code(DiagCode::new(2504))
                .with_label(span, source_id, ""),
        );
        return;
    }

    match bind_attr(db, attr, &LINK_ATTR) {
        Ok(bound) => {
            if bound.get("name").is_none() && bound.get("symbol").is_none() {
                diagnostics.push(
                    Diag::error("attribute '@link' requires at least one argument")
                        .with_code(DiagCode::new(2505))
                        .with_hint("write '@link(name := \"lib\")', '@link(symbol := \"native_name\")', or both")
                        .with_label(span, source_id, ""),
                );
            }
            if let Some(expr_id) = bound.get("name")
                && attr_expr_string(db, expr_id).is_none()
            {
                diagnostics.push(
                    Diag::error("attribute '@link' argument 'name' requires a string literal")
                        .with_code(DiagCode::new(2511))
                        .with_label(span, source_id, ""),
                );
            }
            if let Some(expr_id) = bound.get("symbol")
                && attr_expr_string(db, expr_id).is_none()
            {
                diagnostics.push(
                    Diag::error("attribute '@link' argument 'symbol' requires a string literal")
                        .with_code(DiagCode::new(2512))
                        .with_label(span, source_id, ""),
                );
            }
        }
        Err(error) => diagnostics.push(
            Diag::error(link_attr_error_message(error))
                .with_code(DiagCode::new(2505))
                .with_label(span, source_id, ""),
        ),
    }
}

fn validate_diag_attr(
    db: &Db,
    attr: &Attr,
    path: &[String],
    source_id: SourceId,
    diagnostics: &mut Vec<Diag>,
    span: Span,
) {
    let valid = matches!(
        path,
        [root, kind] if root == "diag"
            && matches!(kind.as_str(), "allow" | "warn" | "deny" | "expect")
    );
    if !valid {
        diagnostics.push(
            Diag::error(format!(
                "unknown diagnostic control attribute '{}'",
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
                Diag::error("diagnostic control attributes require codes like ms1234")
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
        ExprKind::Lit(Literal::Str(code)) => DiagCode::parse(code),
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

fn link_attr_error_message(error: AttrBindError) -> String {
    match error {
        AttrBindError::UnknownArgument { name } => {
            format!("unknown argument '{name}' in attribute '@link'")
        }
        AttrBindError::DuplicateArgument { name } => {
            format!("duplicate argument '{name}' in attribute '@link'")
        }
        AttrBindError::TooManyArguments { expected, found } => {
            format!("attribute '@link' accepts {expected} argument(s), found {found}")
        }
        AttrBindError::MissingArgument { name } => {
            format!("missing argument '{name}' in attribute '@link'")
        }
    }
}
