use self::symbols::tokenize;
use super::*;

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
            ".class" => self.parse_class(&parts),
            ".foreign" => self.parse_foreign(&parts),
            ".export" => self.parse_export(&parts),
            ".meta" => self.parse_meta(&parts),
            other => Err(AssemblyError::TextParseFailed(format!(
                "unknown directive {other}"
            ))),
        }
    }
}
