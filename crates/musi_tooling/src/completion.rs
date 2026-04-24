use std::collections::HashSet;
use std::path::Path;

use music_base::{Source, SourceId};
use music_hir::{HirExprId, HirExprKind, HirTyId, HirTyKind};
use music_module::ModuleKey;
use music_names::{NameBinding, NameBindingKind, Symbol};
use music_sema::SemaModule;
use music_session::Session;
use music_syntax::{Lexer, TokenKind};

use crate::ToolRange;
use crate::analysis_support::analysis_session;

const COMPLETION_PROBE: &str = "musiCompletionProbe";

pub type ToolCompletionList = Vec<ToolCompletion>;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ToolCompletionKind {
    Keyword,
    Function,
    Procedure,
    Variable,
    Parameter,
    TypeParameter,
    Type,
    Module,
    Property,
    EnumMember,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ToolCompletion {
    pub label: String,
    pub kind: ToolCompletionKind,
    pub detail: Option<String>,
    pub documentation: Option<String>,
    pub insert_text: Option<String>,
    pub sort_text: Option<String>,
    pub filter_text: Option<String>,
    pub replace_range: ToolRange,
}

impl ToolCompletion {
    #[must_use]
    pub fn new(
        label: impl Into<String>,
        kind: ToolCompletionKind,
        detail: Option<String>,
        replace_range: ToolRange,
    ) -> Self {
        Self {
            label: label.into(),
            kind,
            detail,
            documentation: None,
            insert_text: None,
            sort_text: None,
            filter_text: None,
            replace_range,
        }
    }

    #[must_use]
    pub fn with_sort_text(mut self, sort_text: impl Into<String>) -> Self {
        self.sort_text = Some(sort_text.into());
        self
    }
}

#[derive(Debug, Clone, Copy)]
struct CompletionContext {
    offset: u32,
    replace_start: u32,
    replace_end: u32,
    dot_offset: Option<u32>,
}

#[must_use]
pub fn completions_for_project_file(
    path: &Path,
    line: usize,
    character: usize,
) -> ToolCompletionList {
    completions_for_project_file_with_overlay(path, None, line, character)
}

#[must_use]
pub fn completions_for_project_file_with_overlay(
    path: &Path,
    overlay_text: Option<&str>,
    line: usize,
    character: usize,
) -> ToolCompletionList {
    let Some((session, module_key)) = analysis_session(path, overlay_text) else {
        return Vec::new();
    };
    let Some(parsed) = session.parsed_module_cached(&module_key).ok().flatten() else {
        return Vec::new();
    };
    let Some(source) = session.source(parsed.source_id) else {
        return Vec::new();
    };
    let Some(offset) = source.offset(line, character) else {
        return Vec::new();
    };
    if !allows_completion_at_offset(source, offset) {
        return Vec::new();
    }
    let Some(context) = completion_context(source, offset) else {
        return Vec::new();
    };
    if context.dot_offset.is_some() {
        return dot_completions(path, source, &context, line, character);
    }
    global_completions(&session, parsed.source_id, source, &context, &module_key)
}

fn global_completions(
    session: &Session,
    source_id: SourceId,
    source: &Source,
    context: &CompletionContext,
    module_key: &ModuleKey,
) -> ToolCompletionList {
    let replace_range = range_from_offsets(source, context.replace_start, context.replace_end);
    let mut completions = Vec::new();
    let mut seen = HashSet::new();
    for (index, keyword) in COMPLETION_KEYWORDS.iter().enumerate() {
        push_completion(
            &mut completions,
            &mut seen,
            ToolCompletion::new(
                *keyword,
                ToolCompletionKind::Keyword,
                Some("keyword".to_owned()),
                replace_range,
            )
            .with_sort_text(format!("2_{index:03}_{keyword}")),
        );
    }

    if let Some(resolved) = session.resolved_module_cached(module_key).ok().flatten() {
        for (_, binding) in &resolved.names.bindings {
            if !binding_visible_at_completion(source_id, binding, context.offset) {
                continue;
            }
            let label = session.resolve_symbol(binding.name).to_owned();
            if label == "_" {
                continue;
            }
            push_completion(
                &mut completions,
                &mut seen,
                ToolCompletion::new(
                    label.clone(),
                    completion_kind_for_binding(binding.kind),
                    Some(binding_kind_label(binding.kind).to_owned()),
                    replace_range,
                )
                .with_sort_text(format!("1_{label}")),
            );
        }
    }

    sort_completions(completions)
}

fn dot_completions(
    path: &Path,
    source: &Source,
    context: &CompletionContext,
    line: usize,
    character: usize,
) -> ToolCompletionList {
    let text = source.text();
    let Ok(start) = usize::try_from(context.replace_start) else {
        return Vec::new();
    };
    let Ok(end) = usize::try_from(context.replace_end) else {
        return Vec::new();
    };
    let Some(prefix) = text.get(start..end) else {
        return Vec::new();
    };
    let mut overlay = String::with_capacity(
        text.len()
            .saturating_sub(end.saturating_sub(start))
            .saturating_add(COMPLETION_PROBE.len()),
    );
    let Some(before_completion) = text.get(..start) else {
        return Vec::new();
    };
    let Some(after_completion) = text.get(end..) else {
        return Vec::new();
    };
    overlay.push_str(before_completion);
    overlay.push_str(COMPLETION_PROBE);
    if needs_completion_statement_terminator(text, end) {
        overlay.push(';');
    }
    overlay.push_str(after_completion);

    let probe_character = character
        .saturating_sub(prefix.chars().count())
        .saturating_add(COMPLETION_PROBE.len());
    let Some((session, module_key)) = analysis_session(path, Some(&overlay)) else {
        return Vec::new();
    };
    let Some(sema) = session.sema_module_cached(&module_key).ok().flatten() else {
        return Vec::new();
    };
    let Some(parsed) = session.parsed_module_cached(&module_key).ok().flatten() else {
        return Vec::new();
    };
    let Some(probe_source) = session.source(parsed.source_id) else {
        return Vec::new();
    };
    let Some(_probe_offset) = probe_source.offset(line, probe_character) else {
        return Vec::new();
    };
    let Some(base) = field_base_for_probe(&session, sema) else {
        return Vec::new();
    };
    let replace_range = range_from_offsets(source, context.replace_start, context.replace_end);
    let mut completions = member_completions_for_base(&session, sema, base, replace_range);
    completions.retain(|completion| completion.label.starts_with(prefix));
    sort_completions(completions)
}

fn field_base_for_probe(session: &Session, sema: &SemaModule) -> Option<HirExprId> {
    sema.module().store.exprs.iter().find_map(|(_, expr)| {
        let HirExprKind::Field { base, name, .. } = expr.kind else {
            return None;
        };
        if session.resolve_symbol(name.name) == COMPLETION_PROBE {
            return Some(base);
        }
        None
    })
}

fn member_completions_for_base(
    session: &Session,
    sema: &SemaModule,
    base: HirExprId,
    replace_range: ToolRange,
) -> ToolCompletionList {
    let mut completions = Vec::new();
    let mut seen = HashSet::new();
    if let HirExprKind::Name { name } = sema.module().store.exprs.get(base).kind {
        push_effect_operation_completions(
            session,
            sema,
            name.name,
            &replace_range,
            &mut completions,
            &mut seen,
        );
        push_shape_member_completions(
            session,
            sema,
            name.name,
            &replace_range,
            &mut completions,
            &mut seen,
        );
    }
    if let Some(ty) = sema.try_expr_ty(base) {
        push_type_member_completions(
            session,
            sema,
            ty,
            &replace_range,
            &mut completions,
            &mut seen,
        );
    }
    completions
}

fn push_effect_operation_completions(
    session: &Session,
    sema: &SemaModule,
    name: Symbol,
    replace_range: &ToolRange,
    completions: &mut ToolCompletionList,
    seen: &mut HashSet<String>,
) {
    let Some(effect) = sema.effect_def(session.resolve_symbol(name)) else {
        return;
    };
    for (index, (label, _)) in effect.ops().enumerate() {
        push_completion(
            completions,
            seen,
            ToolCompletion::new(
                label.to_owned(),
                ToolCompletionKind::Procedure,
                Some("effect operation".to_owned()),
                *replace_range,
            )
            .with_sort_text(format!("1_{index:03}_{label}")),
        );
    }
}

fn push_shape_member_completions(
    session: &Session,
    sema: &SemaModule,
    name: Symbol,
    replace_range: &ToolRange,
    completions: &mut ToolCompletionList,
    seen: &mut HashSet<String>,
) {
    let Some(shape) = sema.shape_facts_by_name(name) else {
        return;
    };
    for (index, member) in shape.members.iter().enumerate() {
        let label = session.resolve_symbol(member.name).to_owned();
        push_completion(
            completions,
            seen,
            ToolCompletion::new(
                label.clone(),
                ToolCompletionKind::Procedure,
                Some("shape member".to_owned()),
                *replace_range,
            )
            .with_sort_text(format!("1_{index:03}_{label}")),
        );
    }
}

fn push_type_member_completions(
    session: &Session,
    sema: &SemaModule,
    ty: HirTyId,
    replace_range: &ToolRange,
    completions: &mut ToolCompletionList,
    seen: &mut HashSet<String>,
) {
    match sema.ty(ty).kind.clone() {
        HirTyKind::Mut { inner } => {
            push_type_member_completions(session, sema, inner, replace_range, completions, seen);
        }
        HirTyKind::Record { fields } => {
            for (index, field) in sema.module().store.ty_fields.get(fields).iter().enumerate() {
                let label = session.resolve_symbol(field.name).to_owned();
                push_property_completion(&label, index, replace_range, completions, seen);
            }
        }
        HirTyKind::Range { .. } => {
            for (index, label) in ["lowerBound", "upperBound", "includeLower", "includeUpper"]
                .iter()
                .enumerate()
            {
                push_property_completion(label, index, replace_range, completions, seen);
            }
        }
        HirTyKind::Named { name, .. } => {
            if let Some(data) = sema.data_def(session.resolve_symbol(name))
                && let Some(variant) = data.record_shape_variant()
            {
                for (index, field_name) in variant.field_names().iter().flatten().enumerate() {
                    push_property_completion(field_name, index, replace_range, completions, seen);
                }
            }
        }
        _ => {}
    }
}

fn push_property_completion(
    label: &str,
    index: usize,
    replace_range: &ToolRange,
    completions: &mut ToolCompletionList,
    seen: &mut HashSet<String>,
) {
    push_completion(
        completions,
        seen,
        ToolCompletion::new(
            label.to_owned(),
            ToolCompletionKind::Property,
            Some("property".to_owned()),
            *replace_range,
        )
        .with_sort_text(format!("1_{index:03}_{label}")),
    );
}

const COMPLETION_KEYWORDS: &[&str] = &[
    "answer", "any", "ask", "as", "catch", "comptime", "data", "effect", "export", "given",
    "handle", "if", "import", "in", "known", "law", "let", "match", "mut", "native", "opaque",
    "partial", "pin", "quote", "rec", "require", "resume", "shape", "some", "unsafe", "where",
];

fn push_completion(
    completions: &mut ToolCompletionList,
    seen: &mut HashSet<String>,
    completion: ToolCompletion,
) {
    if seen.insert(completion.label.clone()) {
        completions.push(completion);
    }
}

fn sort_completions(mut completions: Vec<ToolCompletion>) -> ToolCompletionList {
    completions.sort_by(|left, right| {
        left.sort_text
            .as_deref()
            .unwrap_or(&left.label)
            .cmp(right.sort_text.as_deref().unwrap_or(&right.label))
            .then_with(|| completion_sort_group(left.kind).cmp(&completion_sort_group(right.kind)))
            .then_with(|| left.label.cmp(&right.label))
    });
    completions
}

fn binding_visible_at_completion(source_id: SourceId, binding: &NameBinding, offset: u32) -> bool {
    if binding.site.source_id != source_id {
        return matches!(binding.kind, NameBindingKind::Prelude);
    }
    binding.site.span.is_empty() || binding.site.span.start <= offset
}

const fn binding_kind_label(kind: NameBindingKind) -> &'static str {
    match kind {
        NameBindingKind::Prelude => "prelude",
        NameBindingKind::Import => "import",
        NameBindingKind::Let => "binding",
        NameBindingKind::AttachedMethod => "method",
        NameBindingKind::Param => "parameter",
        NameBindingKind::PiBinder | NameBindingKind::TypeParam => "type parameter",
        NameBindingKind::PatternBind => "pattern binding",
        NameBindingKind::Pin => "pin",
        NameBindingKind::HandleClauseResult => "answer result",
        NameBindingKind::HandleClauseParam => "answer parameter",
    }
}

const fn completion_sort_group(kind: ToolCompletionKind) -> u8 {
    match kind {
        ToolCompletionKind::Keyword => 2,
        _ => 1,
    }
}

const fn completion_kind_for_binding(kind: NameBindingKind) -> ToolCompletionKind {
    match kind {
        NameBindingKind::Prelude | NameBindingKind::Import => ToolCompletionKind::Module,
        NameBindingKind::Let | NameBindingKind::Pin | NameBindingKind::HandleClauseResult => {
            ToolCompletionKind::Variable
        }
        NameBindingKind::AttachedMethod => ToolCompletionKind::Function,
        NameBindingKind::Param
        | NameBindingKind::PatternBind
        | NameBindingKind::HandleClauseParam => ToolCompletionKind::Parameter,
        NameBindingKind::PiBinder | NameBindingKind::TypeParam => ToolCompletionKind::TypeParameter,
    }
}

fn completion_context(source: &Source, offset: u32) -> Option<CompletionContext> {
    let text = source.text();
    let cursor = usize::try_from(offset).ok()?;
    if cursor > text.len() || !text.is_char_boundary(cursor) {
        return None;
    }
    let replace_start = identifier_start(text, cursor)?;
    let replace_end = identifier_end(text, cursor)?;
    let dot_offset = previous_non_whitespace(text, replace_start)
        .filter(|(_, ch)| *ch == '.')
        .and_then(|(index, _)| u32::try_from(index).ok());
    Some(CompletionContext {
        offset,
        replace_start: u32::try_from(replace_start).ok()?,
        replace_end: u32::try_from(replace_end).ok()?,
        dot_offset,
    })
}

fn identifier_start(text: &str, cursor: usize) -> Option<usize> {
    let mut start = cursor;
    for (index, ch) in text.get(..cursor)?.char_indices().rev() {
        if is_ident_continue(ch) {
            start = index;
        } else {
            break;
        }
    }
    Some(start)
}

fn identifier_end(text: &str, cursor: usize) -> Option<usize> {
    let mut end = cursor;
    for (index, ch) in text.get(cursor..)?.char_indices() {
        if is_ident_continue(ch) {
            end = cursor.saturating_add(index).saturating_add(ch.len_utf8());
        } else {
            break;
        }
    }
    Some(end)
}

fn previous_non_whitespace(text: &str, cursor: usize) -> Option<(usize, char)> {
    text.get(..cursor)?
        .char_indices()
        .rev()
        .find(|(_, ch)| !ch.is_whitespace())
}

fn needs_completion_statement_terminator(text: &str, cursor: usize) -> bool {
    text.get(cursor..)
        .is_some_and(|tail| tail.chars().all(char::is_whitespace))
        || text
            .get(cursor..)
            .and_then(|tail| tail.chars().find(|ch| !ch.is_whitespace()))
            == Some('\n')
}

const fn is_ident_continue(ch: char) -> bool {
    ch == '_' || ch.is_ascii_alphanumeric()
}

fn range_from_offsets(source: &Source, start: u32, end: u32) -> ToolRange {
    let start = source.line_col(start);
    let end = source.line_col(end);
    ToolRange::new(start.0, start.1, end.0, end.1)
}

fn allows_completion_at_offset(source: &Source, offset: u32) -> bool {
    let lexed = Lexer::new(source.text()).lex();
    for index in 0..lexed.tokens().len() {
        for trivia in lexed.token_trivia(index) {
            if trivia.kind.is_comment() && trivia.span.contains(offset) {
                return false;
            }
        }
        let token = lexed.tokens()[index];
        if !token.span.contains(offset) {
            continue;
        }
        return !matches!(
            token.kind,
            TokenKind::String
                | TokenKind::Rune
                | TokenKind::TemplateNoSubst
                | TokenKind::TemplateHead
                | TokenKind::TemplateMiddle
                | TokenKind::TemplateTail
        );
    }
    true
}
