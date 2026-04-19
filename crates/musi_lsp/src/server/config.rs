use async_lsp::lsp_types::{InitializeParams, LSPAny};
use musi_tooling::{ToolInlayHint, ToolInlayHintKind};

#[derive(Debug, Clone, Copy)]
pub(super) struct LspConfig {
    pub(super) inlay_hints: InlayHintConfig,
    pub(super) hover_maximum_length: usize,
}

impl Default for LspConfig {
    fn default() -> Self {
        Self {
            inlay_hints: InlayHintConfig::default(),
            hover_maximum_length: 500,
        }
    }
}

impl LspConfig {
    pub(super) fn from_initialize_params(params: &InitializeParams) -> Self {
        let mut config = Self::default();
        let Some(options) = &params.initialization_options else {
            return config;
        };
        config.hover_maximum_length = options
            .get("hover")
            .and_then(|hover| hover.get("maximumLength"))
            .and_then(number_option)
            .and_then(|value| usize::try_from(value).ok())
            .filter(|value| *value > 0)
            .unwrap_or(config.hover_maximum_length);
        let Some(inlay_hints) = options.get("inlayHints") else {
            return config;
        };
        config.inlay_hints.enabled = bool_option(inlay_hints, "enabled").unwrap_or(true);
        config.inlay_hints.parameter_names = string_option(inlay_hints, "parameterNames")
            .map_or(ParameterNameHints::None, ParameterNameHints::from_setting);
        config.inlay_hints.variable_types =
            bool_option(inlay_hints, "variableTypes").unwrap_or(false);
        config
    }
}

#[derive(Debug, Clone, Copy)]
pub(super) struct InlayHintConfig {
    pub(super) enabled: bool,
    parameter_names: ParameterNameHints,
    variable_types: bool,
}

impl Default for InlayHintConfig {
    fn default() -> Self {
        Self {
            enabled: true,
            parameter_names: ParameterNameHints::None,
            variable_types: false,
        }
    }
}

impl InlayHintConfig {
    pub(super) const fn allows(self, hint: &ToolInlayHint) -> bool {
        match hint.kind {
            ToolInlayHintKind::Type => self.variable_types,
            ToolInlayHintKind::Parameter => {
                !matches!(self.parameter_names, ParameterNameHints::None)
            }
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum ParameterNameHints {
    None,
    Literals,
    All,
}

impl ParameterNameHints {
    fn from_setting(value: &str) -> Self {
        match value {
            "literals" => Self::Literals,
            "all" => Self::All,
            _ => Self::None,
        }
    }
}

fn bool_option(value: &LSPAny, key: &str) -> Option<bool> {
    value.get(key)?.as_bool()
}

fn string_option<'a>(value: &'a LSPAny, key: &str) -> Option<&'a str> {
    value.get(key)?.as_str()
}

fn number_option(value: &LSPAny) -> Option<u64> {
    value.as_u64()
}
