use std::collections::hash_map::DefaultHasher;
use std::fs::read_to_string;
use std::hash::{Hash, Hasher};

use async_lsp::lsp_types::{
    Range, SemanticToken, SemanticTokens, SemanticTokensDelta, SemanticTokensDeltaParams,
    SemanticTokensEdit, SemanticTokensFullDeltaResult, SemanticTokensParams,
    SemanticTokensRangeParams, Url,
};
use musi_tooling::semantic_tokens_for_project_file_with_overlay;

use super::MusiLanguageServer;
use super::convert::encode_semantic_tokens;

#[derive(Debug, Clone)]
pub(super) struct SemanticTokenSnapshot {
    pub(super) result_id: String,
    pub(super) data: Vec<SemanticToken>,
}

impl MusiLanguageServer {
    pub(super) fn semantic_tokens(&self, params: &SemanticTokensParams) -> Option<SemanticTokens> {
        self.semantic_tokens_for_uri(&params.text_document.uri, None)
    }

    pub(super) fn semantic_range_tokens(
        &self,
        params: &SemanticTokensRangeParams,
    ) -> Option<SemanticTokens> {
        self.semantic_tokens_for_uri(&params.text_document.uri, Some(params.range))
    }

    pub(super) fn semantic_tokens_full_response(
        &mut self,
        params: &SemanticTokensParams,
    ) -> Option<SemanticTokens> {
        let uri = &params.text_document.uri;
        let tokens = self.semantic_tokens(params)?;
        let result_id = tokens.result_id.clone()?;
        let snapshot = SemanticTokenSnapshot {
            result_id,
            data: tokens.data.clone(),
        };
        let _ = self.semantic_token_cache.insert(uri.clone(), snapshot);
        Some(tokens)
    }

    pub(super) fn semantic_token_delta(
        &mut self,
        params: &SemanticTokensDeltaParams,
    ) -> Option<SemanticTokensFullDeltaResult> {
        let uri = &params.text_document.uri;
        let tokens = self.semantic_tokens_for_uri(uri, None)?;
        let result_id = tokens.result_id.clone()?;
        let next = SemanticTokenSnapshot {
            result_id,
            data: tokens.data.clone(),
        };
        let response = self
            .semantic_token_cache
            .get(uri)
            .filter(|previous| previous.result_id == params.previous_result_id)
            .map_or_else(
                || SemanticTokensFullDeltaResult::Tokens(tokens),
                |previous| {
                    SemanticTokensFullDeltaResult::TokensDelta(semantic_tokens_delta(
                        previous, &next,
                    ))
                },
            );
        let _ = self.semantic_token_cache.insert(uri.clone(), next);
        Some(response)
    }

    fn semantic_tokens_for_uri(&self, uri: &Url, range: Option<Range>) -> Option<SemanticTokens> {
        let path = uri.to_file_path().ok()?;
        if path.file_name().is_some_and(|name| name == "musi.json") {
            return None;
        }
        let file_text;
        let text = if let Some(text) = self.open_documents.get(uri) {
            text.as_str()
        } else {
            file_text = read_to_string(&path).ok()?;
            file_text.as_str()
        };
        let tokens = semantic_tokens_for_project_file_with_overlay(&path, Some(text));
        let data = encode_semantic_tokens(text, &tokens, range.as_ref());
        Some(SemanticTokens {
            result_id: range.is_none().then(|| semantic_tokens_result_id(&data)),
            data,
        })
    }
}

pub(super) fn semantic_tokens_result_id(tokens: &[SemanticToken]) -> String {
    let mut hasher = DefaultHasher::new();
    for token in tokens {
        token.delta_line.hash(&mut hasher);
        token.delta_start.hash(&mut hasher);
        token.length.hash(&mut hasher);
        token.token_type.hash(&mut hasher);
        token.token_modifiers_bitset.hash(&mut hasher);
    }
    format!("{:016x}", hasher.finish())
}

pub(super) fn semantic_tokens_delta(
    previous: &SemanticTokenSnapshot,
    next: &SemanticTokenSnapshot,
) -> SemanticTokensDelta {
    const TOKEN_COMPONENTS: usize = 5;
    let mut prefix_len = 0usize;
    while prefix_len < previous.data.len()
        && prefix_len < next.data.len()
        && previous.data.get(prefix_len) == next.data.get(prefix_len)
    {
        prefix_len = prefix_len.saturating_add(1);
    }
    let mut suffix_len = 0usize;
    while suffix_len < previous.data.len().saturating_sub(prefix_len)
        && suffix_len < next.data.len().saturating_sub(prefix_len)
        && previous.data[previous.data.len() - suffix_len - 1]
            == next.data[next.data.len() - suffix_len - 1]
    {
        suffix_len = suffix_len.saturating_add(1);
    }
    let inserted = next.data[prefix_len..next.data.len() - suffix_len].to_vec();
    SemanticTokensDelta {
        result_id: Some(next.result_id.clone()),
        edits: vec![SemanticTokensEdit {
            start: len_to_u32(prefix_len.saturating_mul(TOKEN_COMPONENTS)),
            delete_count: len_to_u32(
                previous
                    .data
                    .len()
                    .saturating_sub(prefix_len)
                    .saturating_sub(suffix_len)
                    .saturating_mul(TOKEN_COMPONENTS),
            ),
            data: (!inserted.is_empty()).then_some(inserted),
        }],
    }
}

fn len_to_u32(value: usize) -> u32 {
    u32::try_from(value).expect("semantic token vector length should fit u32")
}
