use std::collections::HashMap;

use music_names::{Symbol, SymbolSlice};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct IrDataLayout {
    pub record_fields: Option<SymbolSlice>,
    pub choice_variants: Option<SymbolSlice>,
}

impl IrDataLayout {
    #[must_use]
    pub fn record_field_index(&self, field: Symbol) -> Option<u16> {
        let fields = self.record_fields.as_ref()?;
        fields
            .iter()
            .position(|&sym| sym == field)
            .and_then(|i| u16::try_from(i).ok())
    }

    #[must_use]
    pub fn choice_variant_tag(&self, variant: Symbol) -> Option<u16> {
        let variants = self.choice_variants.as_ref()?;
        variants
            .iter()
            .position(|&sym| sym == variant)
            .and_then(|i| u16::try_from(i).ok())
    }
}

pub type IrDataLayouts = HashMap<Symbol, IrDataLayout>;

#[cfg(test)]
#[allow(clippy::unwrap_used, clippy::panic)]
mod tests;
