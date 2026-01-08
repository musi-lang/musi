use std::collections::HashMap;

use musi_ast::{AstArena, Prog};
use musi_core::{Span, Symbol};
use tower_lsp_server::ls_types::{
    SemanticToken, SemanticTokenModifier, SemanticTokenType, SemanticTokensLegend,
};

use crate::bindings::{BindingCollector, BindingInfo, BindingKind};
use crate::diagnostics::offset_to_position;

pub const TOKEN_TYPES: &[SemanticTokenType] = &[
    SemanticTokenType::VARIABLE,
    SemanticTokenType::FUNCTION,
    SemanticTokenType::TYPE,
    SemanticTokenType::PARAMETER,
    SemanticTokenType::PROPERTY,
    SemanticTokenType::ENUM_MEMBER,
];

pub const TOKEN_MODIFIERS: &[SemanticTokenModifier] = &[
    SemanticTokenModifier::DECLARATION,
    SemanticTokenModifier::READONLY,
    SemanticTokenModifier::new("mutable"),
];

pub fn legend() -> SemanticTokensLegend {
    SemanticTokensLegend {
        token_types: TOKEN_TYPES.to_vec(),
        token_modifiers: TOKEN_MODIFIERS.to_vec(),
    }
}

pub fn compute_tokens(text: &str, arena: &AstArena, prog: &Prog) -> Vec<SemanticToken> {
    let collector = BindingCollector::new(arena);
    let bindings = collector.collect(prog);

    let mut raw_tokens = collect_semantic_tokens(&bindings);
    raw_tokens.sort_by_key(|(span, _, _)| span.lo);

    build_lsp_tokens(&raw_tokens, text)
}

fn collect_semantic_tokens(
    bindings: &HashMap<Symbol, BindingInfo>,
) -> Vec<(Span, SemanticTokenType, Vec<SemanticTokenModifier>)> {
    let mut tokens = Vec::new();

    for info in bindings.values() {
        let (ty, decl_mods) = kind_to_token_info(info.kind, true);
        if info.def_span.lo != 0 || info.def_span.hi != 0 {
            tokens.push((info.def_span, ty.clone(), decl_mods));
        }

        let (ty, usage_mods) = kind_to_token_info(info.kind, false);
        for usage in &info.usages {
            tokens.push((*usage, ty.clone(), usage_mods.clone()));
        }
    }

    tokens
}

fn kind_to_token_info(
    kind: BindingKind,
    is_decl: bool,
) -> (SemanticTokenType, Vec<SemanticTokenModifier>) {
    let base_mods = if is_decl {
        vec![SemanticTokenModifier::DECLARATION]
    } else {
        vec![]
    };

    match kind {
        BindingKind::ValBinding => {
            let mut mods = base_mods;
            mods.push(SemanticTokenModifier::READONLY);
            (SemanticTokenType::VARIABLE, mods)
        }
        BindingKind::VarBinding => {
            let mut mods = base_mods;
            mods.push(SemanticTokenModifier::new("mutable"));
            (SemanticTokenType::VARIABLE, mods)
        }
        BindingKind::Function => (SemanticTokenType::FUNCTION, base_mods),
        BindingKind::TypeDef => (SemanticTokenType::TYPE, base_mods),
        BindingKind::Parameter { mutable } => {
            let mut mods = base_mods;
            if mutable {
                mods.push(SemanticTokenModifier::new("mutable"));
            } else {
                mods.push(SemanticTokenModifier::READONLY);
            }
            (SemanticTokenType::PARAMETER, mods)
        }
    }
}

fn build_lsp_tokens(
    raw_tokens: &[(Span, SemanticTokenType, Vec<SemanticTokenModifier>)],
    text: &str,
) -> Vec<SemanticToken> {
    let mut builder = TokenBuilder::new();
    for (span, ty, modifiers) in raw_tokens {
        let lo: usize = span.lo.try_into().expect("span.lo overflow");
        let hi: usize = span.hi.try_into().expect("span.hi overflow");
        let pos = offset_to_position(lo, text);
        let length: u32 = (hi - lo).try_into().expect("token length overflow");
        builder.push(
            pos.line,
            pos.character,
            length,
            token_type_index(ty),
            modifier_bits(modifiers),
        );
    }
    builder.build()
}

fn token_type_index(ty: &SemanticTokenType) -> u32 {
    TOKEN_TYPES
        .iter()
        .position(|t| t == ty)
        .map_or(0, |i| i.try_into().expect("token type index overflow"))
}

fn modifier_bits(modifiers: &[SemanticTokenModifier]) -> u32 {
    let mut bits = 0u32;
    for modifier in modifiers {
        if let Some(idx) = TOKEN_MODIFIERS.iter().position(|m| m == modifier) {
            bits |= 1 << idx;
        }
    }
    bits
}

struct TokenBuilder {
    tokens: Vec<SemanticToken>,
    prev_line: u32,
    prev_start: u32,
}

impl TokenBuilder {
    const fn new() -> Self {
        Self {
            tokens: Vec::new(),
            prev_line: 0,
            prev_start: 0,
        }
    }

    fn push(&mut self, line: u32, start: u32, length: u32, token_type: u32, token_modifiers: u32) {
        let delta_line = line - self.prev_line;
        let delta_start = if delta_line == 0 {
            start - self.prev_start
        } else {
            start
        };
        self.tokens.push(SemanticToken {
            delta_line,
            delta_start,
            length,
            token_type,
            token_modifiers_bitset: token_modifiers,
        });
        self.prev_line = line;
        self.prev_start = start;
    }

    fn build(self) -> Vec<SemanticToken> {
        self.tokens
    }
}
