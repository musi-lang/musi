use self::symbols::tokenize;
use super::*;
use crate::SeamDiagKind;
use music_base::diag::DiagContext;
use std::fmt::Display;

mod directives;
mod operands;
mod procedures;
mod symbols;

impl TextBuilder {
    pub(super) fn new() -> Self {
        Self::default()
    }

    pub(super) fn finish(self) -> Artifact {
        self.artifact
    }

    pub(super) fn parse_directive(&mut self, line: &str) -> AssemblyResult {
        let parts = tokenize(line)?;
        let Some(head) = parts.first() else {
            return Ok(());
        };
        match head.as_str() {
            ".type" => self.parse_type(&parts),
            ".data" => self.parse_data(&parts),
            ".const" => self.parse_const(&parts),
            ".global" => self.parse_global(&parts),
            ".effect" => self.parse_effect(&parts),
            ".capability" => self.parse_capability(&parts),
            ".native" => self.parse_foreign(&parts),
            ".export" => self.parse_export(&parts),
            ".meta" => self.parse_meta(&parts),
            other => Err(text_unknown_directive(other)),
        }
    }
}

pub(super) fn text_expected_form(form: &'static str) -> AssemblyError {
    AssemblyError::text_parse(
        SeamDiagKind::TextExpectedForm,
        &DiagContext::new().with("form", form),
    )
}

pub(super) fn text_missing_operand(operand: &str) -> AssemblyError {
    AssemblyError::text_parse(
        SeamDiagKind::TextMissingOperand,
        &DiagContext::new().with("operand", operand),
    )
}

pub(super) fn text_invalid_operand(operand: &str, value: impl Display) -> AssemblyError {
    AssemblyError::text_parse(
        SeamDiagKind::TextInvalidOperand,
        &DiagContext::new()
            .with("operand", operand)
            .with("value", value),
    )
}

pub(super) fn text_unknown_directive(directive: &str) -> AssemblyError {
    AssemblyError::text_parse(
        SeamDiagKind::TextUnknownDirective,
        &DiagContext::new().with("directive", directive),
    )
}

pub(super) fn text_unknown_opcode(opcode: &str) -> AssemblyError {
    AssemblyError::text_parse(
        SeamDiagKind::TextUnknownOpcode,
        &DiagContext::new().with("opcode", opcode),
    )
}

pub(super) fn text_unknown_symbol(kind: &str, symbol: &str) -> AssemblyError {
    AssemblyError::text_parse(
        SeamDiagKind::TextUnknownSymbol,
        &DiagContext::new().with("kind", kind).with("symbol", symbol),
    )
}

pub(super) fn text_duplicate_symbol(kind: &str, symbol: &str) -> AssemblyError {
    AssemblyError::text_parse(
        SeamDiagKind::TextDuplicateSymbol,
        &DiagContext::new().with("kind", kind).with("symbol", symbol),
    )
}

pub(super) fn text_unterminated_string(literal: &str) -> AssemblyError {
    AssemblyError::text_parse(
        SeamDiagKind::TextUnterminatedString,
        &DiagContext::new().with("literal", literal),
    )
}

pub(super) fn text_count_mismatch(kind: &str, expected: u32, found: u32) -> AssemblyError {
    AssemblyError::text_parse(
        SeamDiagKind::TextCountMismatch,
        &DiagContext::new()
            .with("kind", kind)
            .with("expected", expected)
            .with("found", found),
    )
}
