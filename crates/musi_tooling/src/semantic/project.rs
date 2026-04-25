use std::collections::BTreeSet;
use std::fs;
use std::path::Path;

use music_base::SourceMap;

use super::binding::{binding_token, binding_token_kind, member_tokens};
use super::model::ToolSemanticTokenList;
use super::ranges::{
    normalize_tokens, range_key, token_priority, token_range_priorities, tool_range_key,
};
use super::syntax::syntax_type_tokens;
use crate::analysis_support::analysis_session;

#[must_use]
pub fn semantic_tokens_for_project_file(path: &Path) -> ToolSemanticTokenList {
    semantic_tokens_for_project_file_with_overlay(path, None)
}

#[must_use]
pub fn semantic_tokens_for_project_file_with_overlay(
    path: &Path,
    overlay_text: Option<&str>,
) -> ToolSemanticTokenList {
    let Some((session, module_key)) = analysis_session(path, overlay_text) else {
        return lexical_and_syntax_tokens(path, overlay_text);
    };
    let Some(parsed) = session.parsed_module_cached(&module_key).ok().flatten() else {
        return lexical_and_syntax_tokens(path, overlay_text);
    };
    let Some(source) = session.source(parsed.source_id) else {
        return lexical_and_syntax_tokens(path, overlay_text);
    };
    let mut tokens = Vec::new();
    let mut syntax_tokens = syntax_type_tokens(source);
    if let Some(resolved) = session.resolved_module_cached(&module_key).ok().flatten() {
        let sema = session.sema_module_cached(&module_key).ok().flatten();
        let callable_binding_ranges = resolved
            .names
            .bindings
            .iter()
            .filter(|(binding_id, binding)| {
                binding.site.source_id == parsed.source_id
                    && binding_token_kind(*binding_id, binding.kind, sema).is_callable()
            })
            .map(|(_, binding)| range_key(source, binding.site.span))
            .collect::<BTreeSet<_>>();
        syntax_tokens
            .retain(|token| !callable_binding_ranges.contains(&tool_range_key(token.range)));
        let syntax_priorities = token_range_priorities(&syntax_tokens);
        tokens.extend(syntax_tokens);
        for (binding_id, binding) in &resolved.names.bindings {
            if binding.site.source_id != parsed.source_id {
                continue;
            }
            let token_kind = binding_token_kind(binding_id, binding.kind, sema);
            if syntax_priorities
                .get(&range_key(source, binding.site.span))
                .is_some_and(|priority| *priority >= token_priority(token_kind))
            {
                continue;
            }
            tokens.push(binding_token(
                source,
                binding.site.span,
                binding_id,
                binding.kind,
                sema,
                true,
            ));
        }
        for (site, binding_id) in &resolved.names.refs {
            if site.source_id != parsed.source_id {
                continue;
            }
            let binding = resolved.names.bindings.get(*binding_id);
            let token_kind = binding_token_kind(*binding_id, binding.kind, sema);
            if syntax_priorities
                .get(&range_key(source, site.span))
                .is_some_and(|priority| *priority >= token_priority(token_kind))
            {
                continue;
            }
            tokens.push(binding_token(
                source,
                site.span,
                *binding_id,
                binding.kind,
                sema,
                false,
            ));
        }
        if let Some(sema) = sema {
            tokens.extend(member_tokens(source, sema));
        }
    } else {
        tokens.extend(syntax_tokens);
    }
    normalize_tokens(tokens)
}
fn lexical_and_syntax_tokens(path: &Path, overlay_text: Option<&str>) -> ToolSemanticTokenList {
    let source_text = overlay_text
        .map(str::to_owned)
        .or_else(|| fs::read_to_string(path).ok())
        .unwrap_or_default();
    let mut sources = SourceMap::new();
    let Ok(source_id) = sources.add(path.to_path_buf(), source_text) else {
        return Vec::new();
    };
    let Some(source) = sources.get(source_id) else {
        return Vec::new();
    };
    let mut tokens = Vec::new();
    tokens.extend(syntax_type_tokens(source));
    normalize_tokens(tokens)
}
